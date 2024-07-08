# PROJECT:  rebooTZ
# PURPOSE:  Combine TX + Site Attributes
# AUTHOR:   A.Chafetz | USAID
# REF ID:   a487bcd2 
# LICENSE:  MIT
# DATE:     2024-05-13
# UPDATED:  2024-07-08



# DATIM METADATA ----------------------------------------------------------

# Site By IM Extract
# DATIM data as of: 3/15/2024, 19:44:38 UTC
# Genie report updated: 3/16/2024, 05:46:27 UTC
# Current period(s): 2022 Q1, 2022 Q2, 2022 Q3, 2022 Q4, 2022 Target, 2023 Q1, 2023 Q2, 2023 Q3, 2023 Q4, 2023 Target, 2024 Q1, 2024 Target

# Operating Unit: Tanzania
# Daily/Frozen: Frozen
# Indicator: TX_CURR
# Standardized Disaggregate: Total Numerator
# Fiscal Year: 2024, 2023, 2022


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
  
  # path_genie <- return_latest("Data","Genie")
  # 
  # meta <- get_metadata(path_genie)  #extract MSD metadata
  
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

 
# DATIM API ---------------------------------------------------------------
  # 
  # info <- get_outable() %>% 
  #   filter(country == cntry) %>% 
  #   select(country, country_uid, facility_lvl) %>% 
  #   as.list()
  # 
  # 
  # url <- paste0("https://final.datim.org/api/analytics.json?",
  #               "dimension=ou:", info$country_uid, ";LEVEL-", info$facility_lvl, "&", #hierarchy
  #               "filter=RUkVjD3BsS1&", #top level
  #               "filter=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results: Results
  #               "dimension=bw8KHXzxd9i&", #Funding Agency
  #               "dimension=CH5v24DUJO0&", #Site Attribute: Ownership Type
  #               "dimension=SbeNLojYo3t&", #Site Attribute: Facility Type
  #               "dimension=LxhLO68FcXm:MvszPTQrUhy&", #indicator: TX_CURR
  #               "dimension=pe:2021Oct;2022Oct;2023Oct&", #periods
  #               "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=false")
  # 
  # df_datim <- datim_process_query(url)
  # 
  # #check
  # # df_datim %>% 
  # #   janitor::clean_names() %>% 
  # #   clean_agency() %>% 
  # #   mutate(period = period %>% str_sub(-4) %>% as.numeric(), .before = value) %>% 
  # #   count(funding_agency, period, wt = value) %>% 
  # #   pivot_wider(names_from = period, 
  # #               names_glue = "fy{str_sub(period, -2)}",
  # #               values_from = n)
  # 
  # df_datim_clean <- df_datim %>% 
  #   rename(orgunit = `Organisation unit`,
  #          funding_agency = `Funding Agency`,
  #          ownership_type = `SA Own Type`,
  #          facility_type = `SA Facility Type`,
  #          indicator = `Technical Area`
  #          ) %>% 
  #   rename_with(tolower) %>% 
  #   clean_agency() %>% 
  #   mutate(ownership_type = str_remove(ownership_type, "^OWN "),
  #          facility_type = str_remove(facility_type, "^FT ")
  #   ) %>%
  #   mutate(fiscal_year = period %>% str_sub(-4) %>% as.numeric(), .before = value) %>% 
  #   arrange(fiscal_year) %>% 
  #   select(-period) %>% 
  #   pivot_wider(names_from = c(indicator, fiscal_year),
  #               names_glue = "{tolower(indicator)}_fy{str_sub(fiscal_year, -2)}") %>% 
  #   arrange(funding_agency)
  # 
  # df_orgs <- datim_orgunits(info$country, reshape = TRUE) %>% 
  #   select(orgunituid, country, snu1)
  # 
  # df_datim_clean <- df_datim_clean %>% 
  #   tidylog::right_join(df_orgs, .) %>% 
  #   relocate(orgunit, 1)
  # 
  # write_csv(df_datim_clean, "Dataout/TZA_TX_SA.csv", na  = "")
  

# REVIEW ------------------------------------------------------------------

  # df_datim_clean %>% 
  #   count(funding_agency, ownership_type) %>% 
  #   mutate(ownership_type = fct_reorder(ownership_type, n, sum)) %>% 
  #   arrange(desc(ownership_type)) %>% 
  #   pivot_wider(names_from = funding_agency, values_from = n)
  # 
  # df_datim_clean %>% 
  #   count(funding_agency, ownership_type, wt = tx_curr_fy24) %>% 
  #   mutate(ownership_type = fct_reorder(ownership_type, n, sum)) %>% 
  #   arrange(desc(ownership_type)) %>%
  #   pivot_wider(names_from = funding_agency, values_from = n)
  # 
  # 
  # df_datim_clean %>% 
  #   count(funding_agency, wt = tx_curr_fy24) 
  
  
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

  #aggregate to one observation by site
  df_sites <- df_msd %>%
    filter(fiscal_year == fy,
           indicator %in% c("TX_CURR", "HTS_TST"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, country, funding_agency, snu1, sitetype, sitename, orgunituid, indicator) %>%
    summarise(cumulative = sum(cumulative, na.rm = TRUE),
              .groups = "drop") %>% 
    filter(cumulative != 0)
  
  # df_sites_w <- df_sites %>% 
  #   pivot_wider(names_from = c(indicator, fiscal_year),
  #                             names_glue = "{tolower(indicator)}_fy{str_sub(fiscal_year, -2)}",
  #               values_from = cumulative)
  
  df_sites_w <- df_sites %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_from = cumulative)

  #clean columns
  df_attr <- df_attr %>%
    clean_names() %>%
    select(orgunituid = datim_uid,
           facility_type:clinic_hours)%>%
    mutate(across(facility_type:clinic_hours,  \(x) na_if(x, "")))

  #join with attributes
  df_full <- tidylog::left_join(df_sites_w, df_attr)
  
  #clean
  df_full <- df_full %>% 
    clean_agency() %>% 
    relocate(sitetype, facility_type:clinic_hours, .after = orgunituid) %>% 
    mutate(ownership_type = ifelse(is.na(ownership_type), "Not Classified", ownership_type))

  df_hrh %>% 
    glimpse()
  
# EXPORT ------------------------------------------------------------------

  output_path <- glue("Dataout/TZA_TX_SA_{str_remove_all(Sys.Date(), '-')}.csv")
  
  df_full %>% 
    write_csv(output_path, na  = "")

  
  
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
  
  
  df_full %>% 
    count(facility_type, ownership_type) %>% 
    mutate(ownership_type = fct_reorder(ownership_type, n, sum)) %>% 
    arrange(desc(ownership_type)) %>% 
    pivot_wider(names_from = facility_type, values_from = n)
  
  
  output_path <- "Dataout/TZA_TX_SA_20240515.csv"
  
  df_full %>% 
    write_csv(output_path, na  = "")
  
  drive_upload(output_path,
               as_id("18IyAdidfynvJEqfdITNg5fOjai7VxiFW"),
               basename(output_path),
               type = "spreadsheet",
               overwrite = TRUE)

# VIZ 2 -------------------------------------------------------------------

  df_viz_agg <- df_full %>% 
    filter(!is.na(tx_curr)) %>% 
    group_by(funding_agency, ownership_type) %>% 
    summarise(cumulative = sum(tx_curr, na.rm = TRUE),
              sites = n(),
              .groups = "drop") %>% 
    mutate(ownership_type = factor(ownership_type,
                                    c("Private",
                                      "Government: MOH", "Government: Other",
                                      "NGO or Non-Profit", "Not Classified")),
           funding_agency = factor(funding_agency, c("USAID", "CDC", "DOD"))) %>%
    group_by(funding_agency) %>% 
    mutate(agency_private = ifelse(ownership_type == "Private", sites, 0),
           agency_private = max(agency_private),
           agency_label = glue("<span style = 'font-size:14pt'>**{funding_agency}**</span><br>{agency_private} out of {number_format(big.mark = ',')(sum(sites))} total sites are <span style = 'color:{orchid_bloom};'>privately owned</span>")
           ) %>% 
    ungroup() %>% 
    arrange(funding_agency) %>% 
    mutate(agency_label = fct_inorder(agency_label)) %>% 
    arrange(ownership_type)
  
  
  df_viz_agg %>% 
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
    coord_equal() +
    coord_cartesian(clip = "off") +
    labs(subtitle = "Number of sites reporting patients on treatment (TX_CURR) by ownership type in FY23",
         caption = glue("Note: Other ownership type categories include Government: MOH or Other and NGO/Non-Profit. Sites without ownership type are shaded orange 
                        {meta$caption} + DATIM Data Exchange: Org Unit Attributes [{Sys.Date()}]")) +
    si_style_nolines() +
    theme(axis.text = element_blank(),
          legend.position = "none",
          strip.text = element_markdown())

