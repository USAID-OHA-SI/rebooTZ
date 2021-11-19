# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-11-17
# UPDATED: 


# META DATA ---------------------------------------------------------------

# PSNU By IM
# DATIM data as of: 11/12/2021 23:06:26 UTC
# Genie report updated: 11/13/2021 13:32:22 UTC
# 
# Current period(s): 2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4,  2021 Target,  2021 Q1,  2021 Q2,  2021 Q3,  2021 Q4,  2022 Target 
# Operating Unit: Tanzania
# Daily/Froze: Daily
# Funding Agency: USAID
# Indicators:


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
  library(ggnewscale)
  library(ggrepel)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  genie_path <- "Data/Genie-PSNUByIMs-Tanzania-Daily-2021-11-17.zip"
  msd_source <- source_info(genie_path)
  
  curr_pd <- source_info(genie_path, return = "period")
  
  df_preferred <- tibble::tribble(
                    ~mech_code,  ~preferred_name,
                        "18237",       "Deloitte",
                        "18060",          "EGPAF",
                        "16784",          "Sauti",
                        "81965",           "EpiC",
                        "81962",  "Tohara Salama",
                        "82164",           "THPS",
                        "16787",            "JSI",
                        "82169",      "TMEC/RISE",
                        "70356",         "Baylor",
                        "81966", "Hebu Tuyajenge",
                        "17409",        "Jhpiego"
                    )


# IMPORT ------------------------------------------------------------------
  
  df_genie <- read_msd(genie_path)  
  
  df_genie <- df_genie %>% 
    left_join(df_preferred, by = "mech_code")

# MUNGE TX_CURR -----------------------------------------------------------

  df_tx <- df_genie %>% 
    filter(operatingunit == "Tanzania",
           fundingagency == "USAID",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator")
  
  
  df_tx <- df_tx %>% 
    group_by(fiscal_year, snu1, preferred_name, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd("quarters") %>% 
    adorn_achievement()

  df_tx <- df_tx %>% 
    mutate(across(starts_with("ach"), ~ case_when(period == max(period) ~ .)),
           label_targets = case_when(period == max(period) ~ targets),
           label_results = case_when(period == max(period) ~ results)) %>% 
    group_by(snu1, preferred_name) %>% 
    fill(c(starts_with("ach"), starts_with("label")), .direction = "up") %>% 
    ungroup() %>% 
    filter(!is.na(achievement_qtrly))
  
  
  df_viz_tx <- df_tx %>% 
    mutate(status = case_when(achv_color == trolley_grey_light ~ "Over achieved",
                              achv_color == scooter_med ~ "Achieved",
                              TRUE ~ "Under achieved"),
           # facet_name = glue("{snu1}  <span style='font-size:14pt; color:{achv_color}'>\u25CF</span><br>FY21: {label_number_si()(label_results)}/{label_number_si()(label_targets)}"),
           facet_name = glue("{snu1}<br><span style='font-size:14pt; color:{achv_color}'>\u25CF</span> {status} ({label_number_si()(label_results)}/{label_number_si()(label_targets)})"),
           facet_name = fct_reorder2(facet_name, period, targets),
           targets_eb = case_when(str_detect(period, "Q4") ~ targets),
           fill_color = case_when(#period == "FY21Q4" ~ scooter,
                                  fiscal_year == 2021 ~ scooter, #si_palettes$scooters[4],
                                  TRUE ~ si_palettes$scooters[3]))
  

# VIZ TX_CURR -------------------------------------------------------------

  
  print_viz_tx <- function(ptnr, export = TRUE){
    
    df_ptnr <- df_viz_tx %>% 
      filter(preferred_name == ptnr)
    
    df_ptnr_agg <- df_ptnr %>% 
      filter(period == max(period)) %>% 
      group_by(period, preferred_name, indicator) %>% 
      summarise(across(c(targets, cumulative = results), sum, na.rm = TRUE), .groups = "drop") %>% 
      adorn_achievement() %>% 
      mutate(status = case_when(achv_color == trolley_grey_light ~ "Over achieved",
                                achv_color == scooter_med ~ "Achieved",
                                TRUE ~ "Under achieved") %>% toupper)
    df_ptnr <- df_pntr
    
    brks <- df_ptnr %>% 
      distinct(period) %>% 
      filter(str_detect(period, "Q(2|4)")) %>% 
      pull()
    
    v <- df_ptnr %>% 
      ggplot(aes(period, results)) +
      geom_point(aes(y = results/2, color = achv_label), size = 1) +
      geom_col(aes(y = targets), fill = grey10k, alpha = .8) +
      geom_col(aes(fill = fill_color)) +
      geom_errorbar(aes(ymin = targets, ymax = targets), 
                    na.rm = TRUE, color = grey80k, #size = .7,
                    linetype = "dotted") +
      facet_wrap(~ facet_name) +
      scale_fill_identity() +
      scale_color_manual(values = c("<75%" = old_rose_light, 
                                    "75-89%" = burnt_sienna_light,
                                    "90-110%" = scooter_med,
                                    "+110%" = trolley_grey_light)) +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = brks) +
      labs(x = NULL, y = NULL, color = "Target Achievement",
           title = glue("{toupper(ptnr)} {df_ptnr_agg$status} THEIR FY21 TX_CURR TARGET WITH {percent(df_ptnr_agg$achievement, 1)} ACHIEVEMENT"),
           caption = glue("Source: {msd_source}
                        US Agency for International Development")) +
      si_style_ygrid() +
      theme(strip.text = element_markdown(),
            panel.spacing.y = unit(.5, "lines")) +
      guides(color = guide_legend(override.aes = list(size = 5)))
    
    if(export == TRUE){
      ptnr %>% 
        tolower() %>% 
        str_remove_all(" ") %>% 
        paste0(curr_pd,"_TZA_tx_trends_", ., ".png") %>% 
        si_save(path = "Images")
    } else {
      return(v)
    }
   
  }

  df_viz_tx %>% 
    count(preferred_name, wt = targets, sort = T) %>% 
    pull(preferred_name) %>% 
    walk(print_viz_tx)
    

# MUNGE TX GAP PSNU -------------------------------------------------------


  df_tx_gap <- df_genie %>% 
    filter(operatingunit == "Tanzania",
           fundingagency == "USAID",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator")
  
  
  df_tx_gap <- df_tx_gap %>% 
    group_by(fiscal_year, snu1, psnu, preferred_name, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd("quarters") %>% 
    adorn_achievement()
    

  df_tx_prob <- df_tx %>% 
    filter(period == max(period),
           achievement_qtrly < .9) %>% 
    select(snu1, preferred_name)
  
  df_tx_psnu_prob <- df_tx_gap %>% 
    filter(period == max(period),
           achievement_qtrly < .9) %>% 
    select(psnu, preferred_name)
  
  df_tx_gap <- df_tx_gap %>% 
    semi_join(df_tx_prob) %>% 
    # semi_join(df_tx_psnu_prob)
  
  df_viz_tx_gap <- df_tx_gap %>% 
    filter(period == max(period)) %>% 
    mutate(gap = targets - results,
           lab_psnu = case_when(achievement_qtrly <= .75 ~ psnu))
    
  print_viz_tx_psnu <- function(ptnr, export = TRUE){
    
    df_psnu_distro <- df_viz_tx_gap %>% 
      filter(preferred_name == ptnr) %>% 
      mutate(status = ifelse(achievement_qtrly <= .75, "below", "above")) %>% 
      count(status) %>% 
      janitor::adorn_totals() %>% 
      spread(status, n)
    
    v <- df_viz_tx_gap %>% 
      filter(preferred_name == ptnr) %>% 
      ggplot(aes(targets, achievement_qtrly)) +
      geom_hline(yintercept = .75, linetype = "dashed", color = "#909090") +
      geom_point(aes(fill = achv_label, size = results), shape = 21, color = "gray30", alpha = .6) +
      geom_text_repel(aes(label = lab_psnu), 
                      family = "Source Sans Pro", color = "#505050",
                      na.rm = TRUE) +
      scale_fill_manual(values = c("<75%" = old_rose_light, 
                                   "75-89%" = burnt_sienna_light,
                                   "90-110%" = scooter_med,
                                   "+110%" = trolley_grey_light)) +
      facet_wrap(~snu1) +
      scale_size(label = label_number_si()) +
      scale_x_continuous(label = comma) +
      scale_y_continuous(label = percent_format(1)) +
      coord_cartesian(clip = "off") +
      labs(title = glue("{toupper(ptnr)} WAS BELOW 75% FY21 TX_CURR TARGET ACHIEVEMENT IN {df_psnu_distro$below} of IT'S {df_psnu_distro$Total} COUNCILS"),
           x = "FY21 Targets", y = "FY21 Target Achievement",
           size = "FY21 Cumulative TX_CURR", fill = "Target Achievement",
           caption =  glue("Source: {msd_source}
                        US Agency for International Development")) +
      si_style() +
      theme(panel.spacing.y = unit(.5, "lines")) +
      guides(fill = guide_legend(override.aes = list(size = 5)))
    
    if(export == TRUE){
      ptnr %>% 
        tolower() %>% 
        str_remove_all(" ") %>% 
        paste0(curr_pd,"_TZA_tx_psnu_gap_", ., ".png") %>% 
        si_save(path = "Images")
    } else {
      return(v)
    }
  }
  
  df_tx_psnu_prob %>% 
    distinct(preferred_name) %>% 
    pull() %>% 
    walk(print_viz_tx_psnu)
  
# MUNGE VL ----------------------------------------------------------------

  df_vl <- df_genie %>% 
    filter(operatingunit == "Tanzania",
           fundingagency == "USAID",
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    clean_indicator()

  df_vl <- df_vl %>% 
    group_by(fiscal_year, snu1, psnu, preferred_name, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_vl <- df_vl %>% 
    group_by(snu1, psnu, preferred_name) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, order_by = period), .after = tx_curr) %>%
    ungroup() %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_adj = tx_pvls/tx_curr_lag2)
  
  
  df_vl <- df_vl %>% 
    filter(period == max(period),
           tx_curr_lag2 > 0)
  

  df_vl_snu <- df_vl %>% 
    group_by(period, snu1,preferred_name) %>% 
    summarise(across(starts_with("tx"), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(snu_vlc = tx_pvls_d/tx_curr_lag2,
           snu_vls = tx_pvls/tx_pvls_d,
           snu_vls_adj = tx_pvls/tx_curr_lag2) %>% 
    pivot_longer(starts_with("snu_"), 
                 names_to = "type",
                 names_prefix = "snu_",
                 values_to = "value_snu")
  
  df_vl <- df_vl %>%
    pivot_longer(starts_with("vl"), 
                 names_to = "type",
                 values_to = "value_psnu") %>% 
    bind_rows(df_vl_snu) %>% 
    filter(type != "vls_adj")
    
  df_viz_vls <- df_vl %>% 
    mutate(snu_vlc = case_when(type == "vlc" ~ value_snu),
           type = case_when(type == "vlc" ~ "Viral Load Coverage",
                            type == "vls" ~ "Viral Load Suppression"))
  
  
  print_viz_vl <- function(ptnr, export = TRUE){
    
   df_viz_vls_ptnr <- df_viz_vls %>% 
      filter(type == "Viral Load Coverage",
             preferred_name == ptnr) %>% 
      group_by(period, preferred_name) %>% 
      summarise(across(starts_with("tx"), sum, na.rm = TRUE), .groups = "drop") %>% 
      mutate(vlc = tx_pvls_d/tx_curr_lag2,
             vls = tx_pvls/tx_pvls_d,
             vls_adj = tx_pvls/tx_curr_lag2)
    
    acc <- ifelse(ptnr == "Baylor", .1, 1)
    
    v <- df_viz_vls %>% 
      filter(preferred_name == ptnr) %>% 
      ggplot(aes(value_psnu, fct_reorder(snu1, snu_vlc, na.rm = TRUE))) +
      geom_vline(xintercept = .9, linetype = "dashed") +
      geom_errorbar(aes(xmin = value_snu, xmax = value_snu), size = 1, color = golden_sand) +
      geom_point(aes(size = tx_curr_lag2), alpha = .6, color = scooter,
                 position = position_jitter(width = 0, height = 0.1, seed = 42), na.rm = TRUE) +
      facet_wrap(~type) +
      scale_x_continuous(label = percent_format(1), limit=c(NA,1.1), oob=scales::squish) +
      scale_size_continuous(label = label_number_si(acc)) +
      labs(x = NULL, y = NULL,
           title = glue("OF THE {label_number_si()(df_viz_vls_ptnr$tx_curr_lag2)} ELIGIBLE ON TREATMENT, {toupper(ptnr)} HAD A VLC RATE OF {percent(df_viz_vls_ptnr$vlc, 1)} and VLS OF {percent(df_viz_vls_ptnr$vls, 1)} IN {curr_pd}"),
           # subtitle = glue("Goal of 90% for both VLC and VLS | Each point <span style='font-size:14pt; color:{scooter}'>\u25CF</span> is a council and the bar (<span style='font-size:14pt; color:{golden_sands}'>|<\span>) represents the regional rate"),
           subtitle = glue("Each point <span style='font-size:14pt; color:{scooter}'>\u25CF</span> is a council and the bar <span style='font-size:14pt; color:{golden_sand}'>**|**</span> represents the regional VL rate, with a goal of 90% for both"),
           size = "TX_CURR, FY21Q2",
           caption = glue("VLC = TX_PVLS_D / TX_CURR_2pd_prior; VLS = TX_PVLS_N/TX_PVLS_D
                        Source: {msd_source}
                        US Agency for International Development")) +
      si_style() +
      theme(plot.subtitle = element_markdown())
    
    if(export == TRUE){
      ptnr %>% 
        tolower() %>% 
        str_remove_all(" ") %>% 
        paste0(curr_pd,"_TZA_vl_rates", ., ".png") %>% 
        si_save(path = "Images")
    } else {
      return(v)
    }
  }
 
  df_viz_vls %>% 
    count(preferred_name, wt = tx_curr_lag2/2, sort = T) %>% 
    pull(preferred_name) %>% 
    walk(print_viz_vl)
 
  print_viz_vl("EGPAF", export = FALSE)
  