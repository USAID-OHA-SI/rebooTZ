# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  provide additional FY21Q3 partner review slides
# LICENSE:  MIT
# DATE:     2021-08-19
# UPDATED: 


# SOURCE META DATA --------------------------------------------------------

# DATIM DATA GENIE
# PSNU By IM
# DATIM data as of: 08/14/2021 21:59:04 UTC
# Genie report updated: 08/19/2021 01:43:13 UTC
# 
# Current period(s): 2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4,  2021 Target,  2021 Q1,  2021 Q2,  2021 Q3 

# Operating Unit: Tanzania
# Daily/Frozen: Daily

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(gisr)
  library(sf)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  #path to genie output file
  genie_path <- si_path("path_downloads") %>% 
    file.path("Genie-PSNUByIMs-Tanzania-Daily-2021-08-19.zip")
  
  #select indicators
  ind_sel <- c("VMMC_CIRC", 
               "HTS_INDEX",  "HTS_INDEX_NEWPOS", "HTS_TST", "HTS_TST_POS",
               "HTS_SELF", "PMTCT_STAT_D", "PMTCT_STAT", "PMTCT_STAT_POS",
               "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS")
  
  #table of preferred partner names
  df_ptnr <- tribble(
    ~mech_code,             ~partner,
        "81965",               "EpiC",
        "82164", "Police and Prisons",
        "18060",              "EGPAF",
        "18237",           "Deloitte",
        "70356",             "Baylor"
    )

  #table of age/sex disaggs
  df_disaggs <- tibble::tribble(
                          ~indicator,      ~standardizeddisaggregate,
                         "HTS_INDEX",             "4:Age/Sex/Result",
                  "HTS_INDEX_NEWPOS",               "Age/Sex/Result",
                          "HTS_SELF",          "Age/Sex/HIVSelfTest",
                           "HTS_TST",      "Modality/Age/Sex/Result",
                       "HTS_TST_POS",      "Modality/Age/Sex/Result",
                        "PMTCT_STAT",       "Age/Sex/KnownNewResult",
                      "PMTCT_STAT_D",                      "Age/Sex",
                    "PMTCT_STAT_POS",       "Age/Sex/KnownNewResult",
                           "TX_CURR",            "Age/Sex/HIVStatus",
                            "TX_NEW",            "Age/Sex/HIVStatus",
                           "TX_PVLS", "Age/Sex/Indication/HIVStatus",
                         "TX_PVLS_D", "Age/Sex/Indication/HIVStatus"
                  )

  #caption info for plotting
  source <- source_info(genie_path)
  
  #current FY and quarter
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  curr_qtr <- source_info(genie_path, return = "quarter")

  
# IMPORT ------------------------------------------------------------------
  
  df_genie <- read_msd(genie_path)   

  shp_tza <- si_path("path_vector") %>%
    file.path("tanzania_snu1.shp") %>% 
    st_read()
    

# MUNGE -------------------------------------------------------------------

  #subset to key indicators
  df_sub <- df_genie %>% 
    filter(fundingagency == "USAID",
           fiscal_year == curr_fy,
           indicator %in% ind_sel) %>% 
    clean_indicator()
  
  #limit to select partners with preferred names
  df_sub <-  inner_join(df_sub, df_ptnr)

# MUNGE - NAT/SNU ACHIEVEMENT ---------------------------------------------

  #aggregate to regional level
  df_achv <- df_sub %>% 
    bind_rows(df_sub %>% 
                mutate(snu1 = "NATIONAL",
                       snu1uid = "NATIONAL")) %>% 
    filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    group_by(fiscal_year, partner, snu1, snu1uid, indicator) %>% 
    summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
              .groups = "drop")

  #calculate achievement
  df_achv <- df_achv %>% 
    adorn_achievement(curr_qtr)

  #viz adjustments
  df_achv_viz <- df_achv %>% 
    complete(indicator, nesting(partner), fill = list(fiscal_year = curr_fy, snu1 = "NATIONAL")) %>% 
    mutate(natl_achv = case_when(snu1 == "NATIONAL" ~ achievement),
           achievement = ifelse(snu1 == "NATIONAL", NA, achievement),
           indicator = factor(indicator, ind_sel),
           baseline_pt_1 = 0,
           baseline_pt_2 = .25,
           baseline_pt_3 = .5,
           baseline_pt_4 = .75,
           baseline_pt_5 = 1,
           )
  #adjust facet label to include indicator and national values
  df_achv_viz <- df_achv_viz %>% 
    mutate(ind_w_natl_vals = case_when(snu1 == "NATIONAL" & is.na(targets) ~ 
                                         glue("**{indicator}**<br><span style = 'font-size:9pt;'>No MER reporting</span>"),
                                       snu1 == "NATIONAL" ~ 
                                         glue("**{indicator}**<br><span style = 'font-size:9pt;'>{comma(cumulative, 1)} / {comma(targets, 1)}</span>"))) %>% 
    group_by(partner, indicator) %>% 
    fill(ind_w_natl_vals, .direction = "downup") %>% 
    ungroup() %>% 
    arrange(partner, indicator) %>% 
    mutate(ind_w_natl_vals = fct_inorder(ind_w_natl_vals))
  
    

# VIZ - ACHIEVEMENT -------------------------------------------------------

  
  plot_achv <- function(ptnr, export = TRUE){
    df_achv_viz %>% 
      filter(partner == {ptnr}) %>% 
      ggplot(aes(achievement, indicator, color = achv_color)) +
      geom_blank() +
      geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
      geom_point(aes(baseline_pt_1), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_2), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_3), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_4), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_5), shape = 3, color = "#D3D3D3") +
      geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                  alpha = .4, size = 3) +
      geom_point(aes(natl_achv), size = 8, alpha = .8, na.rm = TRUE) +
      geom_text(aes(natl_achv, label = percent(natl_achv, 1)), na.rm = TRUE,
                color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(limit=c(0,1.1),oob=scales::squish) +
      scale_color_identity() +
      facet_wrap(~ind_w_natl_vals, scales = "free_y") +
      labs(x = NULL, y = NULL,
           title = glue("FY{curr_fy}Q{curr_qtr} Tanzania | {ptnr}") %>% toupper,
           subtitle = glue("Partner achievement nationally (large, labeled points) with regional reference points<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal for 75% at Q3 (snapshot indicators pegged to year end target 100%)</span>"),
           caption = glue("Target achievement capped at 110%
                        Source: {source}
                        US Agency for International Development")) +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown(),
            panel.spacing.y = unit(0, "lines"))
    
    if(export == TRUE)
      si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_TZA_Partner-Achievement_{ptnr}.png"))
  }

  walk(unique(df_achv_viz$partner), plot_achv)

# MUNGE - AGE/SEX ---------------------------------------------------------

  #limit to correct age/sex disaggs
  df_agesex <- df_sub %>% 
    inner_join(df_disaggs) %>% 
    filter(ageasentered %ni% c("Unknown Age", "<10"))
    
  #aggregate
  df_agesex <- df_agesex %>% 
    group_by(fiscal_year, partner, indicator, ageasentered, sex) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
    complete(indicator, nesting(partner), fill = list(fiscal_year = curr_fy, ageasentered = "<01"))
  
  df_agesex <- df_agesex %>% 
    adorn_achievement(curr_qtr)

  df_agesex_viz <- df_agesex %>% 
    mutate(plot_targets = ifelse(sex == "Female", -targets, targets),
           plot_cumulative = ifelse(sex == "Female", -cumulative, cumulative)) %>% 
    group_by(partner, indicator) %>% 
    mutate(balance = max(targets, cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(curr_target = ifelse(indicator %in% snapshot_ind, 1 * targets, (curr_qtr/4)*targets),
           curr_target = ifelse(sex == "Female", -curr_target, curr_target),
           indicator = factor(indicator, ind_sel))

  
  plot_agesex <- function(ptnr, export = TRUE){
    v <- df_agesex_viz %>% 
      filter(partner == {ptnr}) %>% 
      ggplot(aes(y = ageasentered)) +
      geom_blank(aes(balance)) + 
      geom_blank(aes(-balance)) + 
      geom_col(aes(x = plot_targets), fill = trolley_grey_light) +
      geom_col(aes(x = plot_cumulative, fill = sex)) +
      geom_errorbar(aes(xmin = curr_target, xmax = curr_target), size = .4, color = grey90k) +
      geom_vline(aes(xintercept = 0), size = 2, color = "white") +
      facet_wrap(~indicator, scales = "free_x") +
      scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa)) +
      scale_x_continuous(labels = ~ comma(abs(.))) +
      labs(x = NULL, y = NULL, fill = NULL,
           title = glue("FY{curr_fy}Q{curr_qtr} Tanzania | {ptnr}") %>% toupper,
           subtitle = glue("Partner cumulative **<span style = 'color:{moody_blue};'> Female</span>/<span style = 'color:{genoa};'>Male</span>** results by age against **<span style = 'color:{trolley_grey};'>targets</span>**<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal (vertical bar) for 75% at Q3 (snapshot indicators pegged to year end target 100%)</span>"),
           caption = glue("No targets for HTS_SELF
                        Source: {source}
                        US Agency for International Development")) +
      si_style_xgrid() +
      theme(#panel.spacing.x = unit(1, "lines"),
            panel.spacing.y = unit(.5, "lines"),
            legend.position = "none",
            strip.text = element_text(face = "bold"),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.subtitle = element_markdown())
    
    if(export == TRUE){
      si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_TZA_Partner-AgeSex_{ptnr}.png"), v)
    } else {
      return(v)
    }
  }
  
  plot_agesex("EGPAF", FALSE)
  
  walk(unique(df_agesex$partner), plot_agesex)
  

# MUNGE - MAP -------------------------------------------------------------

  shp_tza <- shp_tza %>% 
    select(#snu1 = orgunit, 
           snu1uid = uid, geometry)
  
  #join shapefile to data
  df_map <- df_achv %>% 
    filter(snu1 != "NATIONAL") %>% 
    complete(indicator, nesting(partner), fill = list(achv_color = trolley_grey_light, achievement = 0)) %>%
    full_join(shp_tza) %>% 
    mutate(indicator = factor(indicator, ind_sel))
  

  plot_map <- function(ptnr, export = TRUE){
    v <- df_map %>%
      filter(partner == {ptnr},
             !is.na(snu1)
      ) %>%
      ggplot() +
      geom_sf(data = shp_tza, fill = trolley_grey_light, alpha = .2, size = .2) +
      geom_sf(aes(fill = achv_color, geometry = geometry), size = .2) +
      # geom_sf_text(aes(label = percent(achievement, 1), geometry = geometry),
      #              family = "Source Sans Pro", color = "#505050", size = 7/.pt) +
      scale_fill_identity() +
      facet_wrap(~indicator) +
      labs(x = NULL, y = NULL,
           title = glue("FY{curr_fy}Q{curr_qtr} Tanzania | {ptnr}") %>% toupper,
           subtitle = glue("Partner regional achievement<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal for 75% at Q3 (snapshot indicators pegged to year end target 100%)</span>"),
           caption = glue("Source: {source}
                        US Agency for International Development")) +
      si_style_map() +
      theme(panel.spacing.x = unit(.5, "lines"),
            panel.spacing.y = unit(.5, "lines"),
            plot.subtitle = element_markdown(),
            strip.text = element_text(face = "bold"))
    
    if(export == TRUE){
      si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_TZA_Partner-Map_{ptnr}.png"), v)
    } else {
      return(v)
    }
  }
  

  plot_map("EGPAF", FALSE)
  
  walk(unique(df_map$partner)[1:5], plot_map)
  
  
  
  
  
  ###
    
  #   terr <- si_path("path_raster") %>% 
  #   gisr::get_raster()
  # 
  # df_attrs <- get_attributes(country = "Tanzania")
  # 
  # file_shp <- return_latest(
  #   folderpath = si_path("path_vector"),
  #   pattern = "VcPepfarPolygons.*.shp",
  #   recursive = TRUE
  # )
  # 
  # 
  # spdf_pepfar <- file_shp %>% 
  #   sf::read_sf() %>%
  #   left_join(df_attrs, by = c("uid" = "id")) %>% 
  #   filter(!is.na(name))
  # 
  # adm0 <- get_admin0('United Republic of Tanzania')
  # 
  # adm0 %>% gview()
  # 
  # adm1 <- get_admin1('United Republic of Tanzania')
  # 
  # adm1 %>% gview()
  # 
  # adm0 <- spdf_pepfar %>% 
  #   filter(label == "country")
  # 
  # adm1 <- spdf_pepfar %>% 
  #   filter(label == "snu1")
  
  terrain_map(countries = 'United Republic of Tanzania',
              mask = TRUE,
              terr = si_path("path_raster"))
  
  # terrain_map(countries = adm0, # or "Tanzania",
  #             adm0 = adm0,
  #             adm1 = adm1,
  #             terr = terr, #si_path("path_raster"),
  #             mask = TRUE)
  
  
  
  