# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  "push" countries review - retention
# REF ID:   17c82ae0 
# LICENSE:  MIT
# DATE:     2023-10-12
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "17c82ae0" #id for adorning to plots, making it easier to find on GH
  
  get_metadata(type = "PSNU_IM") #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_psd()   
  

# MUNGE -------------------------------------------------------------------

  df_tx <- df_msd %>% 
    filter(operatingunit == "Tanzania",
           funding_agency == "USAID",
           indicator == "TX_CURR") %>% 
    pluck_totals() %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) 
    
  df_ret <- df_msd %>% 
    filter(operatingunit == "Tanzania",
           funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_ML_IIT_less_three_mo", "TX_ML_IIT_more_three_mo", "TX_ML_died", "TX_ML_refused_stopped", "TX_ML_transferred_out", "TX_NET_NEW", "TX_NEW", "TX_RTT"),
           standardizeddisaggregate == "Total Numerator") %>% 
    mutate(ind = case_when(str_detect(indicator, "IIT") ~ "Interruption",
                           str_detect(indicator, "TX_ML") ~ "Other Loss",
                           indicator == "TX_NEW" ~ "New",
                           indicator == "TX_RTT" ~ "Returned",
                           TRUE ~ indicator)) %>% 
    group_by(fiscal_year, ind) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type)  %>% 
    mutate(value = ifelse(ind %in% c("Interruption","Other Loss"), -value, value)) 
  
  df_ret <- df_ret %>% 
    pivot_wider(names_from = ind,
                values_fill = 0) %>% 
    mutate(net = (New + Returned + Interruption + `Other Loss`),
           `Unexplained Net New` = TX_NET_NEW - net,
           `Unexplained Net New` = na_if(`Unexplained Net New`, 0)) %>% 
    select(-c(net)) %>% 
    pivot_longer(c(Interruption, New, `Other Loss`, Returned, `Unexplained Net New`),
                 names_to = "ind",
                 values_drop_na = TRUE) %>% 
    mutate(share = value/TX_CURR,
           share_nn = TX_NET_NEW/TX_CURR)

  
  df_ret <- df_ret %>% 
    mutate(ind = factor(ind, c("Interruption",  "Other Loss", "New", "Returned", "Unexplained Net New")))
  
  
  df_viz_ret <- df_ret %>%
    group_by(period) %>% 
    mutate(row = dplyr::row_number()) %>% 
    ungroup()
  
    pds_brks <- df_viz_ret %>% 
      distinct(period) %>% 
      filter(str_detect(period, "Q(2|4)")) %>% 
      pull()
    
    
    v1 <- df_tx %>% 
      ggplot(aes(period, value)) + 
      geom_col() +
      geom_hline(yintercept = 0, color = "#D3D3D3") +
      geom_text(aes(label = label_number(accuracy = 1, scale_cut = cut_short_scale())(value)),
                data = df_tx %>% filter(str_detect(period, glue("Q{metadata$curr_qtr}"))),
                family = "Source Sans Pro",
                nudge_y = 1.2e4,
                size = 9/.pt) +
      scale_x_discrete(breaks = pds_brks) +
      si_style_nolines() +
      labs(x = NULL, y = NULL,
           subtitle = "Current on Treatment",
           caption = "") +
      theme(axis.text.y = element_blank())
    
    v2 <- df_viz_ret %>% 
      # filter(preferred_name == ptnr,
      #        TX_CURR > 0) %>% 
      ggplot(aes(period, share, fill = ind)) +
      geom_col(alpha = .9) +
      geom_errorbar(aes(ymin = share_nn, ymax = share_nn), linewidth = 1.2, na.rm = TRUE) +
      geom_hline(yintercept = 0) +
      # facet_wrap(~fct_reorder2(snu1, period, TX_CURR)) +
      scale_y_continuous(label = percent_format(1), limits = c(-.1, .1)) +
      scale_x_discrete(breaks = pds_brks) +
      scale_fill_manual(values = c("Interruption" = si_palettes$burnt_siennas[5],
                                   "Other Loss" = si_palettes$burnt_siennas[4],
                                   "New" = si_palettes$scooters[5], 
                                   "Returned" = si_palettes$scooters[4], 
                                   "Unexplained Net New" = trolley_grey)) +
      labs(x = NULL, y = NULL, fill = NULL,
           # title = glue("OVERALL RETENTION REMAINS POSITIVE FOR USAID WITH LIMITED TX_CURR LOSES EACH QUARTER"),
           subtitle = "Share of Current on Treatment | Overall Net New ( **\u2015**)",
           caption =  glue("Calculated from TX_CURR, TX_NEW, TX_ML, TX_RTT
                        Source: {metadata$caption}")) +
      si_style_ygrid() +
      theme(plot.subtitle = element_markdown())
    
    v1 + v2 +
      plot_layout(widths = c(1, 2)) +
      plot_annotation(#title = glue("OVERALL RETENTION REMAINS POSITIVE FOR USAID WITH LIMITED TX_CURR LOSES EACH QUARTER"),
                      theme = si_style_ygrid())
    
    si_preview()
    si_save(glue("Images/{metadata$curr_pd}_TZA_push_retention.png"))
  