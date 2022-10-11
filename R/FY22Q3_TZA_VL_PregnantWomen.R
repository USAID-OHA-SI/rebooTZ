# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  PMB VL
# REF ID:   a947fa53 
# LICENSE:  MIT
# DATE:     2022-08-30
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(zoo)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "a947fa53"

  msd_source <- source_info()
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_msd()   
  
  df_psnu <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_msd() %>% 
    filter(operatingunit == "Tanzania")
  
# MUNGE -------------------------------------------------------------------

  df_vl_pmtct <- df %>% 
    filter((indicator == "PMTCT_ART" & numeratordenom == "N" & otherdisaggregate == "Life-long ART, Already") | 
             (indicator == "TX_PVLS" & numeratordenom == "D" & standardizeddisaggregate == "PregnantOrBreastfeeding/Indication/HIVStatus" & otherdisaggregate %in% c("Pregnant, Routine", "Pregnant, Targeted")) ) %>% 
    clean_indicator() %>% 
    clean_agency()
  
  df_vl_pmtct <- df_vl_pmtct %>% 
    bind_rows(df_vl_pmtct %>% 
                mutate(funding_agency = "PEPFAR")) %>% 
    group_by(fiscal_year, operatingunit, funding_agency, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    arrange(operatingunit, funding_agency, period) %>% 
    group_by(operatingunit, funding_agency) %>% 
    mutate(pmtct_art_lag4 = rollsum(pmtct_art, 4, fill = NA, align = c("right"))) %>% 
    ungroup() %>% 
    mutate(vlc = tx_pvls_d/pmtct_art_lag4) %>% 
    filter(str_detect(period, "FY20", negate = TRUE)) 
  
  pds_brks <- df_vl_pmtct %>% 
    distinct(period) %>% 
    mutate(period = str_replace_all(period, ".*(2|4)$", "")) %>% 
    pull()
  
  v1 <- df_vl_pmtct %>% 
    filter(operatingunit == "Tanzania",
           funding_agency %ni% c("PEPFAR", "DEDUP")) %>% 
    mutate(funding_agency = factor(funding_agency, c("USAID", "CDC", "DOD"))) %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = pmtct_art_lag4), fill = moody_blue_light, alpha = .8) +
    geom_col(aes(y = tx_pvls_d), fill = moody_blue, width = .5, alpha = .8) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = pds_brks) +
    labs(x = NULL, y = NULL,
         subtitle = "Trends in Pregnant Women Eligible and Recieving Viral Load Test") +
    facet_wrap(~funding_agency) +
    si_style_ygrid() +
    theme(panel.spacing.x = unit(.5, "picas"),
          strip.text.x = element_text(family = "Source Sans Pro SemiBold"))
  
  
  v2 <- df_vl_pmtct %>% 
    filter(operatingunit == "Tanzania",
           funding_agency != "DEDUP") %>%
    mutate(funding_agency = factor(funding_agency, c("USAID", "CDC", "DOD"))) %>% 
    ggplot(aes(period, vlc, group = funding_agency)) +
    geom_path(color = moody_blue_light) +
    geom_point(shape = 21, fill = "white", color = moody_blue, stroke = 1.1, size = 20/.pt) +
    geom_text(aes(label = percent(vlc, 1)),size = 10/.pt,
              family = "Source Sans Pro", color = matterhorn) +
    facet_wrap(~funding_agency) +
    scale_x_discrete(breaks = pds_brks) +
    labs(x = NULL, y = NULL,
         subtitle = "Viral Load Coverage Proxy Rate") +
    si_style_nolines() +
    coord_cartesian(clip = "off") +
    theme(axis.text = element_blank(),
          strip.text = element_blank())
    
  
  v1 / v2 +
    plot_layout(heights = c(3, 1)) +
    plot_annotation(
      title = "USAID'S VLC FOR PREGNANT WOMEN FALLS WELL OTHER AGENCIES IN TANZANIA",
      caption = glue("Note: Pregnant VLC = TX_PVLS_D.Pregnant-RoutineOrTargeted / PMTCT_ART_N.LifeTimeART-Already [sum last 4 periods]
                        Source: {msd_source} | US Agency for International Development | {ref_id}"),
      theme = si_style())

  si_save("Graphics/FY22Q3_TZA_VLC.svg")
  
  
  
  df_vl_pmtct %>% 
    filter(funding_agency == "PEPFAR",
           str_detect(operatingunit, "Region", negate = TRUE)) %>% 
    ggplot(aes(period, vlc, group = operatingunit)) +
    geom_path(color = moody_blue_light) +
    geom_point(shape = 21, fill = "white", color = moody_blue, stroke = 1.1, size = 20/.pt) +
    geom_text(aes(label = percent(vlc, 1)),size = 10/.pt,
              family = "Source Sans Pro", color = matterhorn) +
    facet_wrap(~fct_reorder2(operatingunit, period, pmtct_art_lag4)) +
    scale_x_discrete(breaks = pds_brks) +
    scale_y_continuous(breaks = seq(0, 1, .25),
                       limits = c(0, 1.25),
                       # oob = oob_squish
                       ) +
    labs(x = NULL, y = NULL,
         title = "PEPFAR VIRAL LOAD COVERAGE FOR PREGNANT WOMEN ",
         subtitle = "Viral Load Coverage Proxy Rate") +
    si_style_ygrid() +
    coord_cartesian(clip = "off") +
    theme(axis.text.y = element_blank(),
          panel.spacing = unit(1, "picas"))
  
  
  
  
  
  
  
  
  v_age <- df %>% 
    filter(operatingunit == "Tanzania",
           (indicator == "PMTCT_ART" & numeratordenom == "N" & otherdisaggregate == "Life-long ART, Already"),
           fiscal_year == 2022) %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    count(ageasentered, wt = cumulative) %>% 
    mutate(share = n/sum(n)) %>% 
    arrange(desc(share)) %>% 
    mutate(cumsum_share = cumsum(share)) %>% 
    filter(cumsum_share < .8) %>% 
    pull(ageasentered)
  
  df_vl_fem <- df %>% 
    filter(operatingunit == "Tanzania",
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex/Indication/HIVStatus"),
           sex == "Female",
           age_2019 %in% v_age) %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    group_by(fiscal_year, operatingunit, funding_agency, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    arrange(operatingunit, funding_agency, period) %>% 
    group_by(operatingunit, funding_agency) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, 2),
           vlc = tx_pvls_d / tx_curr_lag2) %>% 
    ungroup() %>% 
    filter(str_detect(period, "FY20", negate = TRUE))
  

  v3 <- df_vl_fem %>% 
    filter(operatingunit == "Tanzania",
           funding_agency != "DEDUP") %>% 
    mutate(funding_agency = factor(funding_agency, c("USAID", "CDC", "DOD"))) %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = tx_curr_lag2), fill = moody_blue_light, alpha = .8) +
    geom_col(aes(y = tx_pvls_d), fill = moody_blue, width = .5, alpha = .8) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = pds_brks) +
    labs(x = NULL, y = NULL,
         subtitle = "Trends in Women 25-39 Eligible (TX_CURR_LAG2) and Recieving Viral Load Test") +
    facet_wrap(~funding_agency) +
    si_style_ygrid() +
    theme(panel.spacing.x = unit(.5, "picas"),
          strip.text.x = element_text(family = "Source Sans Pro SemiBold"))
  
  
  v4 <- df_vl_fem %>% 
    filter(operatingunit == "Tanzania",
           funding_agency != "DEDUP") %>%
    mutate(funding_agency = factor(funding_agency, c("USAID", "CDC", "DOD"))) %>% 
    ggplot(aes(period, vlc, group = funding_agency)) +
    geom_path(color = moody_blue_light) +
    geom_point(shape = 21, fill = "white", color = moody_blue, stroke = 1.1, size = 20/.pt) +
    geom_text(aes(label = percent(vlc, 1)),size = 10/.pt,
              family = "Source Sans Pro", color = matterhorn) +
    facet_wrap(~funding_agency) +
    scale_x_discrete(breaks = pds_brks) +
    labs(x = NULL, y = NULL,
         subtitle = "Viral Load Coverage Proxy Rate") +
    si_style_nolines() +
    coord_cartesian(clip = "off") +
    theme(axis.text = element_blank(),
          strip.text = element_blank())
    
  
  v3 / v4 +
    plot_layout(heights = c(3, 1)) +
    plot_annotation(
      title = "USAID'S VLC FOR WOMEN 25-39 YO FALLS WELL OTHER AGENCIES IN TANZANIA",
      caption = glue("Note: VLC = TX_PVLS_D / TX_CURR_Lag2 | Women 25-39 yo represent 72% of PMTCT_ART.N Life-long ART, Already  
                        Source: {msd_source} | US Agency for International Development | {ref_id}"),
      theme = si_style())


  si_save("Graphics/FY22Q3_TZA_VLC_2539f.svg")

  
  
  df_vl_pmtct_region <- df_psnu %>% 
    filter(funding_agency == "USAID",
           (indicator == "PMTCT_ART" & numeratordenom == "N" & otherdisaggregate == "Life-long ART, Already") | 
             (indicator == "TX_PVLS" & numeratordenom == "D" & standardizeddisaggregate == "PregnantOrBreastfeeding/Indication/HIVStatus" & otherdisaggregate %in% c("Pregnant, Routine", "Pregnant, Targeted")) ) %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    group_by(fiscal_year, operatingunit, snu1, funding_agency, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    arrange(operatingunit, funding_agency, snu1, period) %>% 
    group_by(operatingunit, funding_agency, snu1) %>% 
    mutate(pmtct_art_lag4 = rollsum(pmtct_art, 4, fill = NA, align = c("right"))) %>% 
    ungroup() %>% 
    mutate(vlc = tx_pvls_d/pmtct_art_lag4) %>% 
    filter(str_detect(period, "FY20", negate = TRUE)) 
  
  
  df_vl_pmtct_region %>% 
    filter(pmtct_art_lag4 > 0) %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = pmtct_art_lag4), fill = denim_light, alpha = .8) +
    geom_col(aes(y = tx_pvls_d), fill = denim, width = .5, alpha = .8) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = pds_brks) +
    labs(x = NULL, y = NULL,
         title = "CONSISTENLY LOW VLC FOR PREGNANT WOMEN ACROSS USAID/TANZANIA",
         caption = glue("Note: Pregnant VLC = TX_PVLS_D.Pregnant-RoutineOrTargeted / PMTCT_ART_N.LifeTimeART-Already [sum last 4 periods]
                        Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    facet_wrap(~fct_reorder2(snu1, period, pmtct_art), scales = "free_y") +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "picas"),
          strip.text.x = element_text(family = "Source Sans Pro SemiBold"))
  
  si_save("Graphics/FY22Q3_TZA_VLC_regional.svg")
  