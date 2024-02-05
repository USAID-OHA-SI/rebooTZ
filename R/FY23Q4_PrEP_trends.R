# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COP23 Midterm Support - PrEP
# REF ID:   68360c9d 
# LICENSE:  MIT
# DATE:     2024-02-05
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "68360c9d"
  
  get_metadata()

# IMPORT ------------------------------------------------------------------
  
  df_tza <- glamr::si_path() %>% 
    glamr::return_latest("PSNU_IM.*Tanzania") %>% 
    gophr::read_psd() 
  
  
  df_tza_arch <- glamr::si_path() %>% 
    glamr::return_latest("PSNU_IM_FY15") %>% 
    gophr::read_psd() %>% 
    filter(country == "Tanzania")
  
  
  df_tza <- bind_rows(df_tza, df_tza_arch)
  
  rm(df_tza_arch)
  

# MUNGE -------------------------------------------------------------------
  
  df_prep <- df_tza %>% 
    filter(indicator == "PrEP_NEW",
           snu1 %in% c("Dar es Salaam", "Shinyanga")) %>% 
    clean_agency()

  
  df_prep %>% 
    count(fiscal_year, indicator, standardizeddisaggregate, otherdisaggregate, wt = cumulative) %>% 
    pivot_wider(names_from = fiscal_year, values_from = n)
  
  #trends
  df_trends <- df_prep %>% 
    filter(between(fiscal_year, 2019, 2023),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, funding_agency, snu1, indicator, 
             # standardizeddisaggregate, 
             # ageasentered, sex,
             # otherdisaggregate
             ) %>% 
    summarise(across(where(is.double), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    reshape_msd("quarters") %>% 
    mutate(across(where(is.double), ~ na_if(., 0)),
           results_cumulative = ifelse(is.na(results), NA, results_cumulative)) %>% 
    adorn_achievement()
  
  
  df_trends <- df_trends %>% 
    mutate(achv_pt = case_when(!is.na(achievement_qtrly) ~ 0))
  
  
  # unique(df_trends$period) %>% 
    
  
  df_trends %>% 
    filter(!is.na(results)) %>% 
    ggplot(aes(period, results, color = funding_agency, group = funding_agency)) +
    geom_line() +
    geom_point() +
    geom_point(aes(y = achv_pt, fill = achv_color), 
               shape = 21, color = "white", size = 4) +
    geom_text(aes(y = achv_pt, label = label_percent(1)(achievement_qtrly)), 
               family = "Source Sans Pro", size = 8/.pt, color = "#50505050") +
    facet_grid(snu1 ~ funding_agency, scales = "free_y", switch = "y") +
    labs(x = NULL, y = NULL,
         subtitle = "Quarterly PrEP_NEW Result and Target Achievement",
           caption = metadata$caption) +
    scale_fill_identity() +
    scale_color_manual(values = c("CDC" = hw_viking,
                                  "USAID" = hw_midnight_blue)) +
    scale_x_discrete(breaks = unique(df_trends$period)) + 
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    si_style_ygrid() +
    theme(legend.position = "none", 
          strip.placement = "outside",
          strip.text.y = element_text(hjust = .5, face = "bold"),
          strip.text.x = element_text(face = "bold"))
  
  si_preview()
  si_save("Graphics/FY23Q4_TZA_prep-trends.svg")
  
  
  
  
  
  df_agesex <- df_prep %>% 
    filter(between(fiscal_year, 2019, 2023),
           standardizeddisaggregate != "Total Numerator") %>% 
    group_by(fiscal_year, funding_agency, snu1, indicator, 
             standardizeddisaggregate,
             ageasentered, sex,
             otherdisaggregate
    ) %>% 
    summarise(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(period = ifelse(fiscal_year == metadata$curr_fy, "FY23Q3", glue("FY{str_sub(fiscal_year, start = -2)}Q4"))) %>% 
    select(-fiscal_year) %>% 
    adorn_achievement() %>% 
    mutate(type = ifelse(standardizeddisaggregate == "KeyPopAbr",
                         otherdisaggregate,
                         glue("{str_sub(sex, end = 1)} {ageasentered}"))) %>% 
    filter(str_detect(type, "Unknown", negate = TRUE)) %>% 
    arrange(sex, ageasentered, otherdisaggregate) %>% 
    mutate(type = fct_inorder(type))
    
  df_agesex %>% 
    filter(!is.na(achievement)) %>% 
    mutate(period = ifelse(period == "FY23Q3", period, str_sub(period, end = 4))) %>% 
    ggplot(aes(period, fct_rev(type), fill = achv_color)) +
    geom_tile(color = 'white') +
    geom_text(aes(label = label_percent(1)(achievement)), 
              family = "Source Sans Pro", size = 8/.pt, color = "#505050") +
    facet_grid(snu1 ~ funding_agency, switch = "y") +
    scale_fill_identity() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         subtitle = "PrEP_NEW Achievement by Age/Sex or KP Type",
         caption = metadata$caption) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          strip.text.y = element_text(hjust = .5, face = "bold"),
          strip.text.x = element_text(face = "bold"),
          axis.text.y = element_text(size = 9))

  si_preview()  
  # si_save("Graphics/FY23Q4_TZA_prep-agesex.svg")
  si_save("Images/FY23Q4_TZA_prep-agesex.png")

  
  
  df_prep %>% 
    filter(fiscal_year == 2023) %>% 
    count(ageasentered, wt = targets)
