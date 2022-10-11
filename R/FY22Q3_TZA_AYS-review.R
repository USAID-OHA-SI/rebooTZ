# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Deloitte review
# REF ID:   8d94688a 
# LICENSE:  MIT
# DATE:     2022-10-11
# UPDATED: 


# REQUEST -----------------------------------------------------------------

# From: N.Mkate -
# what we need is SNU level  trend analysis for Deloitte (AYS) in the following indicators;
# ART gap
# viral load coverage
# Net new
# POS
# HTS
# TX_CURR growth
# 
# No disag. Just showing trends for the 6 regions from FY21Q1 to FY22Q3

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  #plot referenc code
  ref_id <- "8d94688a"
  
  #period information
  msd_source <- source_info()
  curr_pd <- source_info(return = "period")
  
  #caption for plots
  v_caption <- glue("Note: Limited to Deloitte/AYS (18237|84910) | Source: {source_info()} | Ref Id: {ref_id}
                    US Agency for International Development")

# IMPORT ------------------------------------------------------------------
  
  #MSD
  df_tza <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_msd() %>% 
    filter(operatingunit == "Tanzania")
  
  #NAT/SUBNAT with PLHIV estimates
  df_subnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_msd() %>% 
    filter(operatingunit == "Tanzania")

# MUNGE -------------------------------------------------------------------

  #Limit data to Deloitte and aggregate
  df_ays <- df_tza %>% 
    filter(mech_code %in% c("84910", "18237"),
           snu1 != "Dodoma") %>% #only contains TX_PVLS_D
    clean_indicator() %>% 
    pluck_totals()

  #limt to relevant indicators
  df_ays_snu <- df_ays %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS",
                            "TX_CURR", "TX_NET_NEW",
                            "TX_PVLS", "TX_PVLS_D")) %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>%
    filter(value != 0)
  
  #calculate needed metrics
  df_ays_snu <- df_ays_snu %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(positivity = HTS_TST_POS / HTS_TST) %>% 
    group_by(snu1) %>% 
    mutate(VLC = TX_PVLS_D / lag(TX_CURR, n = 2, order_by = period),
           tx_gr = TX_NET_NEW / lag(TX_CURR, n = 1, order_by = period)) %>% 
    ungroup() %>% 
    filter(str_detect(period, "FY(21|22)")) %>% 
    group_by(snu1) %>%
    dplyr::mutate(tx_gr_rel_baseline = TX_CURR/TX_CURR[period == min(period)]) %>% 
    ungroup()

  #aggregate (annual) PLHIV point estimates for merging with MER data
  df_plhiv <- df_subnat %>% 
    filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           between(fiscal_year, 2021, 2022)) %>% 
    count(fiscal_year, snu1, indicator, wt = targets, name = "value") %>% 
    pivot_wider(names_from = indicator)
  
  #Extract FY for MER data to merge with PLHIV data
  df_ays_snu <- df_ays_snu %>% 
    mutate(fiscal_year = period %>% 
             str_sub(3, 4) %>% 
             as.numeric() + 2000,
           .after = period) %>% 
    left_join(df_plhiv, by = c("snu1", "fiscal_year"))

  #calculate appox treatment gap
  df_ays_snu <- df_ays_snu %>% 
    mutate(TX_GAP = PLHIV - TX_CURR) 
    
  #pds to display in each plot
  pd_brks <- unique(df_ays_snu$period) %>% 
    str_subset(("(1|3)$"))

# VIZ - TX GAP ------------------------------------------------------------

  df_ays_snu %>% 
    ggplot(aes(period, TX_CURR)) +
    geom_col(aes(y = PLHIV), fill = trolley_grey_light, width = .5) +
    geom_col(fill = scooter, width = .5,
             position = position_nudge(x = .25)) +
    facet_wrap(~fct_reorder2(snu1, period, PLHIV)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = pd_brks) +
    labs(x = NULL, y = NULL,
         title = "TREATMENT COVERAGE GAPS INCREASED WITH FY22 PLHIV ESTIMATES",
         subtitle = glue("Quarterly patients on treatment (<span style='color:{scooter}'>**TX_CURR**</span>) compared with annual <span style='color:{trolley_grey}'>**PLHIV**</span> point estimates"),
         caption = v_caption %>% str_replace("MSD", "MSD + NAT_SUBNAT")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "picas"),
          plot.subtitle = element_markdown())

  si_save(glue("Images/{curr_pd}_TZA_AYS_TX_Gap.png"))  


# VIZ - VLC ---------------------------------------------------------------

  df_ays_snu %>% 
    group_by(snu1) %>% 
    mutate(curr_val = case_when(period == max(period) ~ lag(TX_CURR, 2, order_by = period))) %>% 
    fill(curr_val, .direction = "up") %>% 
    ungroup() %>% 
    mutate(goal = 1,
           baseline = 0,
           snu1_label = glue("{snu1}\n{label_number(accuracy = 1, scale = 1e-3, suffix = 'K')(curr_val)}")) %>% 
    mutate(snu1_label = ifelse(snu1 == "Iringa", 
                               paste(snu1_label, "(TX_CURR 2pd prior)"), 
                               snu1_label)
           ) %>% 
    ggplot(aes(period, VLC, group = snu1)) +
    geom_ribbon(aes(ymin = VLC, ymax = goal),fill = trolley_grey_light) + 
    geom_ribbon(aes(ymin = baseline, ymax = VLC),fill = scooter, alpha = .4) + 
    geom_point(color = scooter) +
    geom_line(color = scooter) +
    facet_wrap(~fct_reorder2(snu1_label, period, PLHIV)) +
    scale_y_continuous(labels = label_percent()) +
    scale_x_discrete(breaks = pd_brks) +
    coord_cartesian(clip = "off") +
    expand_limits(y = c(0, 1)) +
    labs(x = NULL, y = NULL,
         title = glue("UPTICK IN VIRAL LOAD COVERAGE RATES IN {max(df_ays_snu$period)} FOR DELOITTE'S"),
         subtitle = glue("Quarterly <span style='color:{scooter}'>**Viral Load Coverage**</span> by region [TX_PVLS_D/TX_CURR_2pd_prior]"),
         caption = v_caption) +
    si_style_nolines() +
    theme(panel.spacing = unit(.5, "picas"),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_AYS_VLC.png"))  
  

# VIZ - TX_NET_NEW --------------------------------------------------------

  df_ays_snu %>% 
    mutate(fill_color = ifelse(TX_NET_NEW < 0, golden_sand, scooter),
           delta_lab = ifelse(abs(tx_gr) < .01, percent(tx_gr, .1), percent(tx_gr, 1)),
           lab_pt = 3e3) %>%
    ggplot(aes(period, TX_NET_NEW)) +
    geom_col(aes(fill = fill_color), width = .8) +
    geom_text(aes(y = lab_pt, label = delta_lab),
              color = trolley_grey, family = "Source Sans Pro", size = 3) +
    facet_wrap(~fct_reorder2(snu1, period, PLHIV)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = pd_brks) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         title = glue("LARGE PATIENT LOSSES IN EARLY FY22 REVERSED IN {max(df_ays_snu$period)}"),
         subtitle = glue("Quarterly new net patient <span style='color:{scooter}'>**gains**</span>/<span style='color:{golden_sand}'>**losses**</span> | Percentage depicts NN share of TX_CURR 1pd prior"),
         caption = v_caption) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "picas"),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_AYS_tx-nn.png")) 
  

# VIZ - TX GROWTH ---------------------------------------------------------

  df_ays_snu %>% 
    ggplot(aes(period, tx_gr_rel_baseline, group = snu1)) +
    geom_hline(yintercept =  1) +
    geom_point(color = scooter) +
    geom_line(color = scooter) +
    facet_wrap(~fct_reorder2(snu1, period, PLHIV)) +
    scale_x_discrete(breaks = pd_brks) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "MOROGORO REVERSED COURSE AFTER THE LAST TWO PERIODS OF PATIENT DECLINE",
         subtitle = glue("<span style='color:{scooter}'>**Growth rate**</span> relative to {min(df_ays_snu$period)} [TX_CURR current pd / TX_CURR {min_period}]"),
         caption = v_caption) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "picas"),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_AYS_tx-gr.png")) 


# VIZ - TESTING -----------------------------------------------------------

  df_ays_snu %>% 
    mutate(lab_pos = -2500) %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = HTS_TST), fill = trolley_grey_light, width = .5) +
    geom_col(aes(y = HTS_TST_POS), 
             width = .5, fill = scooter,
             position = position_nudge(x = .25)) +
    geom_text(aes(y = lab_pos, label = percent(positivity, 1)),
              color = trolley_grey, family = "Source Sans Pro", size = 3,
              position = position_nudge(x = .25)) +
    facet_wrap(~fct_reorder2(snu1, period, PLHIV)) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = pd_brks) +
    coord_cartesian(clip = "off") +
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = glue("INCREASE IN BOTH TESTING AND POSITIVITY IN {max(df_ays_snu$period)} SIMILAR TO TREND FY21"),
         subtitle = glue("Quarterly positive tests (<span style='color:{scooter}'>**HTS_TST_POS**</span>) compared with overall tests (<span style='color:{trolley_grey}'>**HTS_TST**</span>) | Percentages below axis depict testing positivity"),
         caption = v_caption) +
    theme(panel.spacing = unit(.5, "picas"),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_AYS_hts.png")) 
  