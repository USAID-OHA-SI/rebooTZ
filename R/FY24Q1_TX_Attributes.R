# PROJECT:  rebooTZ
# PURPOSE:  Combine TX + Site Attributes
# AUTHOR:   A.Chafetz | USAID
# REF ID:   a487bcd2 
# LICENSE:  MIT
# DATE:     2024-05-13
# UPDATED:  2024-07-11


# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(janitor, warn.conflicts = FALSE)
  library(googledrive)
  library(aws.s3)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  library(grabr)  
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(waffle)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets("datim")
  
  ref_id <- "a487bcd2"  #a reference to be places in viz captions 
  
  cntry <- "Tanzania"
  
  fy <- 2023
  
  path_pdap <- si_path() %>%
    return_latest("Site_IM_Recent_Tanz.*parquet")
  
  s3_bucket_prefix <- 'usaid/'
  
  path_pdap_hrh <- s3_objects(bucket = pdap_bucket("write"),
                              prefix = s3_bucket_prefix,
                              access_key = pdap_access(),
                              secret_key = pdap_secret()) %>%
    filter(str_detect(key, "HRH.*parquet")) %>%
    pull(key)
  
  meta <- get_metadata(path_pdap)  #extract MSD metadata
  # meta <- get_metadata()  #extract MSD metadata

  #fill color by facility type
  fill_types <- si_rampr("orchid_bloom_c", 5)
  names(fill_types) <- c("Hospital", "Primary Health Center", "Dispensary/Pharmacy",  "Health Post", "Other Facility")

  
# PDAP WAVE API -----------------------------------------------------------

  # #get TZA UID
  # cntry_uid <- pepfar_country_list %>%
  #   filter(country == cntry) %>%
  #   pull(country_uid)
  # 
  # #establish parameters to pass into POST API
  # post_body <- list(
  #   daily_frozen='daily',
  #   fiscal_year=list(2024),
  #   funding_agency = list("USAID"),
  #   indicator=list("TX_CURR"),
  #   standardizeddisaggregate = list("Total Numerator"),
  #   uid_hierarchy_list=list(stringr::str_glue('-|-|{cntry_uid}')))
  # 
  # #run API
  # wave_process_query(post_body, psd_type = "site_im", request_type = "GET")
  # 
  # genie_path <- return_latest("Data", "Genie")
  # 
  # meta <- get_metadata(genie_path, caption_note = "USAID")  

# DATIM ATTRIBUTES --------------------------------------------------------
  
  #get sql view table uid
  sqlview_name <- datim_sqlviews() %>%
    filter(str_detect(name, "Attributes")) %>%
    pull(name)

  #get iso code
  cntry_iso <- pepfar_country_list %>%
    filter(country == cntry) %>%
    pull(country_iso)

  # Extract Organisation Units data for a specific country - All levels with child / parent links
  df_attr <- datim_sqlviews(view_name = {sqlview_name},
                             dataset = TRUE,
                             query = list(type = "variable", params = list("OU" = cntry_iso)))
  
# IMPORT ------------------------------------------------------------------
  
  df_msd <- read_psd(path_pdap)

  df_hrh <- read_psd(path_pdap_hrh)
  

# MUNGE -------------------------------------------------------------------

  #aggregate to one observation by site and indicator
  df_sites <- df_msd %>%
    filter(fiscal_year == fy,
           indicator %in% c("TX_CURR", "HTS_TST"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, country, funding_agency, snu1, sitetype, sitename, orgunituid, indicator) %>%
    summarise(cumulative = sum(cumulative, na.rm = TRUE),
              .groups = "drop") %>% 
    filter(cumulative != 0)

  #pivot wide to have only one observation per site
  df_sites_w <- df_sites %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_from = cumulative)

  #clean attribute columns to make them easier to work with
  df_attr <- df_attr %>%
    clean_names() %>%
    select(orgunituid = datim_uid,
           facility_type:clinic_hours)%>%
    mutate(across(facility_type:clinic_hours,  \(x) na_if(x, "")))

  #join MER with site attributes
  df_full <- tidylog::left_join(df_sites_w, df_attr)
  
  #clean
  df_full <- df_full %>% 
    clean_agency() %>% 
    relocate(sitetype, facility_type:clinic_hours, .after = orgunituid) %>% 
    mutate(ownership_type = ifelse(is.na(ownership_type), "Not Classified", ownership_type))

  #subset HRH data to FY23 in TDA
  df_hrh <- df_hrh %>% 
    filter(country == cntry,
           fiscal_year == 2023)

  #aggregate HRH data by orgunit and cadre  
  df_hrh <- df_hrh %>% 
    group_by(fiscal_year, orgunituid, funding_agency, cadre) %>% 
    summarise(across(c(annual_fte, individual_count), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    clean_agency()
  
  #join with MER and attributes
  df_full_hrh <- tidylog::left_join(df_full, df_hrh)
  
# EXPORT ------------------------------------------------------------------

  output_path <- glue("Dataout/TZA_TX_SA_{str_remove_all(Sys.Date(), '-')}.csv")
  
  df_full %>% 
    write_csv(output_path, na  = "")

  output_path_hrh <- glue("Dataout/TZA_TX_SA-HRH_{str_remove_all(Sys.Date(), '-')}.csv")
  
  df_full_hrh %>% 
    write_csv(output_path_hrh, na  = "")
  

# UPLOAD TO GDRIVE --------------------------------------------------------

  return_latest("Dataout", "TZA_TX_SA_") %>% 
    drive_upload(output_path,
                 as_id("1hqyy8aCwBZih0YQfI160WA9Hdrc3lBSl"),
                 basename(output_path),
                 type = "spreadsheet",
                 overwrite = TRUE)
  
  return_latest("Dataout", "TZA_TX_SA-HRH_") %>% 
  drive_upload(output_path_hrh,
               as_id("1hqyy8aCwBZih0YQfI160WA9Hdrc3lBSl"),
               basename(output_path),
               type = "spreadsheet",
               overwrite = TRUE)

# VIZ ---------------------------------------------------------------------
  
 df_full %>% 
    count(funding_agency, ownership_type) %>% 
    mutate(ownership_type = fct_reorder(ownership_type, n, sum)) %>% 
    arrange(desc(ownership_type)) %>% 
    pivot_wider(names_from = funding_agency, values_from = n)
  
  df_viz_sites <- df_full %>% 
    group_by(country, ownership_type) %>%
    summarise(tx_curr_fy24 = sum(tx_curr_fy24 , na.rm = TRUE),
              n = n(),
              .groups = "drop") %>% 
    mutate(ownership_type = fct_reorder(ownership_type, n, .desc = TRUE),
           ownership_type = fct_relevel(ownership_type, "Private", after = 0),
           share_tx = tx_curr_fy24 / sum(tx_curr_fy24),
           share_n = n / sum(n),
           fill_color = case_when(ownership_type == "Private" ~ hw_orchid_bloom,
                                  .default = "#A1A3AB"
    ))
  
  
  v1 <- df_viz_sites %>% 
    ggplot(aes(n, country, fill = fill_color)) +
    geom_col(color = "white") +
    scale_fill_identity() +
    facet_wrap(~country) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         ) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_blank()
          )
  
  v2 <- df_full %>% 
    mutate(fill_color = case_when(ownership_type == "Private" ~ hw_orchid_bloom,
                                  .default = "#A1A3AB"
                                  )) %>% 
    count(funding_agency,fill_color, ownership_type) %>% 
    ggplot(aes(n, fct_reorder(ownership_type, n, sum), fill = fill_color)) +
    geom_col() +
    geom_text(aes(label = label_comma()(n)), hjust = -.3,
              family = "Source Sans Pro",
              color = matterhorn) +
    facet_wrap(~fct_reorder(funding_agency, n, sum, .desc = TRUE)) +
    coord_cartesian(clip = "off") +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank())
  
  v1 / v2 + 
    plot_layout(height = c(1, 6)) +
    plot_annotation(title = glue("{label_percent(1)(filter(df_viz_sites, ownership_type == 'Private')$share_n)} OF PEPFAR/TANZANIA's {label_comma(1)(sum(df_viz_sites$n))} SITES ARE <span style='color:{hw_orchid_bloom}'>PRIVATELY OWNED</span>"),
                    subtitle = "FY24 facilities with patients currently on treatment",
                    caption = str_replace(meta$caption, "Genie", "Genie + DATIM Site Attribute API"),
                    theme = si_style()) & theme(plot.title = element_markdown())
  
  si_save("Images/FY24Q1_TZA_SA-sites.png")
  
  
  
  v3 <- df_viz_sites %>% 
    ggplot(aes(tx_curr_fy24, country, fill = fill_color)) +
    geom_col(color = "white") +
    scale_fill_identity() +
    facet_wrap(~country) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
    ) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_blank()
    )
  
  
  v4 <- df_full %>% 
    mutate(fill_color = case_when(ownership_type == "Private" ~ hw_orchid_bloom,
                                  .default = "#A1A3AB"
    )) %>% 
    count(funding_agency,fill_color, ownership_type, wt = tx_curr_fy24) %>% 
    ggplot(aes(n, fct_reorder(ownership_type, n, sum), fill = fill_color)) +
    geom_col() +
    geom_text(aes(label = label_comma()(n)), hjust = -.2,
              family = "Source Sans Pro",
              color = matterhorn) +
    facet_wrap(~fct_reorder(funding_agency, n, sum, .desc = TRUE)) +
    coord_cartesian(clip = "off") +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank())

  
  v3 / v4 + 
    plot_layout(height = c(1, 6)) +
    plot_annotation(title = glue("{label_percent(1)(filter(df_viz_sites, ownership_type == 'Private')$share_tx)} OF PEPFAR/TANZANIA's {label_number(.1, scale_cut = cut_si(''))(sum(df_viz_sites$tx_curr_fy24))} PATIENTS ON TREATMENT ARE IN <span style='color:{hw_orchid_bloom}'>PRIVATELY OWNED FACILITIES</span>"),
                    subtitle = "FY24 patients currently on treatment",
                    caption = str_replace(meta$caption, "Genie", "Genie + DATIM Site Attribute API"),
                    theme = si_style()) & theme(plot.title = element_markdown())
  
  
  si_save("Images/FY24Q1_TZA_SA-tx.png")
  
  
  
# VIZ 2 -------------------------------------------------------------------

  df_full <- return_latest("Dataout", "TZA_TX_SA") %>% 
    read_csv()
  
##site count x agency
  
  df_viz_agg <- df_full %>% 
    filter(sitetype == "Facility") %>% 
    pivot_longer(c(hts_tst, tx_curr),
                 names_to = "indicator",
                 values_drop_na = TRUE) %>% 
    mutate(indicator = toupper(indicator)) %>% 
    group_by(funding_agency, ownership_type, indicator) %>% 
    summarise(cumulative = sum(value, na.rm = TRUE),
              sites = n(),
              .groups = "drop") %>% 
    mutate(ownership_type = factor(ownership_type,
                                    c("Private",
                                      "Government: MOH", "Government: Other",
                                      "NGO or Non-Profit", "Not Classified")),
           funding_agency = factor(funding_agency, c("CDC","USAID", "DOD"))) %>%
    group_by(funding_agency, indicator) %>% 
    mutate(agency_private = ifelse(ownership_type == "Private", sites, 0),
           agency_private = max(agency_private),
           agency_label = glue("<span style = 'font-size:14pt'>**{funding_agency}**</span><br>{agency_private} out of {number_format(big.mark = ',')(sum(sites))} total sites are <span style = 'color:{orchid_bloom};'>privately owned</span>")
           ) %>% 
    ungroup() %>% 
    arrange(funding_agency) %>% 
    mutate(agency_label = fct_inorder(agency_label)) %>% 
    arrange(ownership_type)
  
  
  df_viz_agg %>% 
    filter(indicator == "TX_CURR") %>% 
    ggplot(aes(fill = ownership_type, values = sites)) +
    geom_waffle(color = "white",
                n_rows = 20,
                size = .1) +
    facet_wrap(~agency_label, ncol = 1) +
    scale_fill_manual(values = c("Private" = orchid_bloom,
                                 "Government: MOH" = si_palettes$slate_t[2], 
                                 "Government: Other" = si_palettes$slate_t[3],
                                 "NGO or Non-Profit" = si_palettes$slate_t[4], 
                                 "Not Classified" = si_palettes$tango_t[4])) +
    # coord_equal(clip = "off") +
    coord_cartesian(clip = "off") +
    labs(title = "CDC has the largest share of private owernship type for treatment services" %>% toupper,
         subtitle = "Tanzania | FY23 | TX_CURR",
         caption = glue("Note: Other ownership type categories include Government: MOH or Other and NGO/Non-Profit. Sites without ownership type are shaded orange 
                        {meta$caption} + DATIM Data Exchange: Org Unit Attributes [{Sys.Date()}]")) +
    si_style_nolines() +
    theme(axis.text = element_blank(),
          legend.position = "none",
          strip.text = element_markdown())
  
  si_save("Images/FY23Q4_TZA_SA_ownership-tx.png")
  
  df_viz_agg %>% 
    filter(indicator == "HTS_TST") %>% 
    ggplot(aes(fill = ownership_type, values = sites)) +
    geom_waffle(color = "white",
                n_rows = 20,
                size = .1) +
    facet_wrap(~agency_label, ncol = 1) +
    scale_fill_manual(values = c("Private" = orchid_bloom,
                                 "Government: MOH" = si_palettes$slate_t[2], 
                                 "Government: Other" = si_palettes$slate_t[3],
                                 "NGO or Non-Profit" = si_palettes$slate_t[4], 
                                 "Not Classified" = si_palettes$tango_t[4])) +
    # coord_equal(clip = "off") +
    coord_cartesian(clip = "off") +
    labs(title = "CDC has the largest share of private owernship type for treatment services" %>% toupper,
         subtitle = "Tanzania | FY23 | TX_CURR",
         caption = glue("Note: Other ownership type categories include Government: MOH or Other and NGO/Non-Profit. Sites without ownership type are shaded orange 
                       Communities do not have ownership types and are excluded
                         {meta$caption} + DATIM Data Exchange: Org Unit Attributes [{Sys.Date()}]")) +
    si_style_nolines() +
    theme(axis.text = element_blank(),
          legend.position = "none",
          strip.text = element_markdown())

  si_save("Images/FY23Q4_TZA_SA_ownership-hts.png")

  ##private sites by type
  df_viz_pr_type <- df_full %>% 
    filter(sitetype == "Facility") %>% 
    pivot_longer(c(hts_tst, tx_curr),
                 names_to = "indicator",
                 values_drop_na = TRUE) %>% 
    mutate(indicator = toupper(indicator)) %>% 
    filter(ownership_type == "Private") %>% 
    group_by(indicator, funding_agency, facility_type) %>% 
    summarise(cumulative = sum(value, na.rm = TRUE),
              sites = n(),
              .groups = "drop") %>% 
    mutate(funding_agency = factor(funding_agency, c("CDC","USAID", "DOD")),
           type_order = cumulative) %>% 
    pivot_longer(c(cumulative, sites),
                 names_to = "type")
  
  #TX_CURR
  v1_type <- df_viz_pr_type %>% 
    filter(type == "cumulative",
           indicator == "TX_CURR") %>% 
    mutate(type = "patients currently<br>receiving ART") %>% 
    ggplot(aes(value, fct_rev(funding_agency), fill = facility_type)) +
    geom_col() +
    # geom_vline(xintercept = seq(1:30), color = "white") +
    geom_text(aes(label = label_number(big.mark = ",")(value)), hjust = 0,
              family = "Source Sans Pro", color = matterhorn) +
    facet_grid(type ~ fct_reorder(facility_type, value, sum) %>% fct_rev, switch = "y") +
    # scale_fill_si("orchid_bloom_c", discrete = TRUE) +
    scale_fill_manual(values = fill_types) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          strip.text.y = element_markdown(face = "bold", hjust = .5),
          strip.placement = "outside")
  
  
  v2_type <- df_viz_pr_type %>% 
    filter(type == "sites",
           indicator == "TX_CURR") %>% 
    mutate(type = "sites providing<br>ART") %>% 
    ggplot(aes(value, fct_rev(funding_agency), fill = facility_type)) +
    geom_col() +
    geom_text(aes(label = label_number(big.mark = ",")(value)), hjust = -.25,
              family = "Source Sans Pro", color = matterhorn) +
    facet_grid(type ~ fct_reorder(facility_type, value, sum) %>% fct_rev, switch = "y") +
    # scale_fill_si("orchid_bloom_c", discrete = TRUE) +
    scale_fill_manual(values = fill_types) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          strip.text.y = element_markdown(face = "bold", hjust = .5),
          strip.placement = "outside")
  
  v1_type / v2_type +
    plot_annotation(title = "The plurality of ART provided by the private sector are offered by CDC in hospitals" %>% toupper,
                    subtitle = "Tanzania | FY23 | TX_CURR | Private funding type only",
                    caption = glue("{meta$caption} + DATIM Data Exchange: Org Unit Attributes [{Sys.Date()}]"),
                    theme = si_style())

  si_save("Images/FY23Q4_TZA_SA_facility-tx.png")
  
  #HTS
  v3_type <- df_viz_pr_type %>% 
    filter(type == "cumulative",
           indicator == "HTS_TST") %>% 
    mutate(type = "individuals who received<br>testing Services") %>% 
    ggplot(aes(value, fct_rev(funding_agency), fill = facility_type)) +
    geom_col() +
    geom_text(aes(label = label_number(big.mark = ",")(value)), hjust = -.1,
              family = "Source Sans Pro", color = matterhorn) +
    facet_grid(type ~ fct_reorder(facility_type, type_order, sum) %>% fct_rev, switch = "y") +
    # scale_fill_si("orchid_bloom_c", discrete = TRUE) +
    scale_fill_manual(values = fill_types) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          strip.text.y = element_markdown(face = "bold", hjust = .5),
          strip.placement = "outside")
  
  
  v4_type <- df_viz_pr_type %>% 
    filter(type == "sites",
           indicator == "HTS_TST") %>% 
    mutate(type = "sites providing<br>testing") %>% 
    ggplot(aes(value, fct_rev(funding_agency), fill = facility_type)) +
    geom_col() +
    geom_text(aes(label = label_number(big.mark = ",")(value)), hjust = -.25,
              family = "Source Sans Pro", color = matterhorn) +
    facet_grid(type ~ fct_reorder(facility_type, type_order, sum) %>% fct_rev, switch = "y") +
    # scale_fill_si("orchid_bloom_c", discrete = TRUE) +
    scale_fill_manual(values = fill_types) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          strip.text.y = element_markdown(face = "bold", hjust = .5),
          strip.placement = "outside")

  v3_type / v4_type +
    plot_annotation(title = "The plurality of testing by the private sector are offered by CDC in hospitals" %>% toupper,
                    subtitle = "Tanzania | FY23 | HTS_TST | Private funding type only",
                    caption = glue("{meta$caption} + DATIM Data Exchange: Org Unit Attributes [{Sys.Date()}]"),
                    theme = si_style())
  
  si_save("Images/FY23Q4_TZA_SA_facility-hts.png")
  
 