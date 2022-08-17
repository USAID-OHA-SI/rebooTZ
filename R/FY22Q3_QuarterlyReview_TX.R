# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  partner Q3 review
# REF ID:   1d42aef5 
# LICENSE:  MIT
# DATE:     2022-08-17
# UPDATED: 


# DATIM REPORT PARAMETERS -------------------------------------------------

# PSNU By IM
# DATIM data as of: 08/12/2022 22:02:39 UTC
# Genie report updated: 08/17/2022 01:37:44 UTC
# 
# Current period(s): 2021 Target,  2021 Q1,  2021 Q2,  2021 Q3,  2021 Q4,  2022 Target,  2022 Q1,  2022 Q2,  2022 Q3,  2023 Target 
# 
# Daily/Frozen: Daily
# Operating Unit: Tanzania,
# Funding Agency: USAID,
# Indicator: TX_CURR,TX_ML,TX_NET_NEW,TX_NET_NEW_SHIFT,TX_NEW,TX_PVLS,TX_RTT,
# Fiscal Year: 2023,2022,2021,

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
  
  ref_id <- "1d42aef5"
  
  genie_path <- si_path() %>% 
    return_latest("Genie-PSNUByIMs-Tanzania-Daily")
    
  msd_source <- source_info(genie_path)
  curr_pd <- source_info(genie_path, return = "period")
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  curr_qtr <- source_info(genie_path, return = "quarter")

# IMPORT ------------------------------------------------------------------
  
  df <- read_msd(genie_path)   
  

# GROWTH RATE -------------------------------------------------------------

  df_gr <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, snu1, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    arrange(snu1, period)
  
  df_gr <- df_gr %>% 
    mutate(growth_rate_req = case_when(period == curr_pd ~ ((targets/results)^(1/(4-curr_qtr))) -1)) %>% 
    group_by(snu1) %>% 
    fill(growth_rate_req, .direction = "updown") %>% 
    mutate(growth_rate = (results - lag(results, order_by = period))/lag(results, order_by = period)) %>% 
    ungroup() %>% 
    mutate(grr_lab = case_when(growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget already achieved"), 
                               growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
                               is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"),
                               TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
           gr_label_position = 0,
           disp_targets = case_when(fiscal_year == curr_fy ~ targets))
  
  df_gr %>%
    ggplot(aes(period, results, fill = as.character(fiscal_year))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .3) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(aes(label = percent(growth_rate, 1), y = gr_label_position),
              family = "Source Sans Pro", color = trolley_grey_light, size = 9/.pt, 
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(grr_lab, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(df_gr$period)[grep("Q(1|3)", unique(df_gr$period))]) +
    scale_fill_si("moody_blues", discrete = TRUE) +
    labs(x = NULL, y = NULL,
      title = glue("What growth rate is needed to reach the {str_replace(curr_fy, '20', 'FY')} treatment targets for USAID?") %>% toupper(),
      subtitle = "Current on treatment by region and growth rate needed in Q4 to reach target",
      caption = glue("Note: quarterly growth rate needed calculated as a compound annual growth rate
                         Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          panel.spacing = unit(.5, "picas"))
    

  si_save(paste0(curr_pd,"_TZA_tx-curr-growth_regional.png"),
          path = "Images",
          scale = 1.5)

# ACHIEVEMENT -------------------------------------------------------------

  #aggregate to regional level
  df_achv <- df %>%
    bind_rows(df %>% 
                mutate(psnu = snu1)) %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           fiscal_year == curr_fy) %>% 
    pluck_totals() %>% 
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    mutate(region = psnu == snu1)
  
  #calculate achievement
  df_achv <- df_achv %>% 
    adorn_achievement(curr_qtr)
  
  
  df_achv_lab <- df_achv %>%
    filter(region == TRUE) %>% 
    select(snu1, indicator, targets) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_from = targets) %>%
    mutate(snu1_lab = glue("{format(snu1, justify = 'right')}  {format(comma(tx_curr), justify = 'right')} | {format(comma(tx_new), justify = 'right')}")) %>% 
    select(snu1, snu1_lab, tx_curr)

  #viz adjustments
  df_achv_viz <- df_achv %>%
    mutate(snu1_achv = case_when(region == TRUE ~ achievement),
           baseline_pt_1 = 0,
           baseline_pt_2 = .25,
           baseline_pt_3 = .5,
           baseline_pt_4 = .75,
           baseline_pt_5 = 1) %>% 
    left_join(df_achv_lab, by = 'snu1') %>% 
    mutate(snu1_lab = fct_reorder(snu1_lab, tx_curr, max, na.rm = TRUE, .desc = TRUE))
  
    # unique(df_achv_viz$snu1_lab)
    
  df_achv_viz %>% 
    ggplot(aes(achievement, snu1_lab, color = achv_color)) +
    geom_blank() +
    geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
    geom_point(aes(baseline_pt_1), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_2), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_3), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_4), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_5), shape = 3, color = "#D3D3D3") +
    geom_jitter(data = filter(df_achv_viz, region == FALSE),
                position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                alpha = .4, size = 3) +
    geom_point(data = filter(df_achv_viz, region == TRUE),
               size = 8, alpha = .8, na.rm = TRUE) +
    geom_text(data = filter(df_achv_viz, region == TRUE),
              aes(label = percent(achievement, 1)), na.rm = TRUE,
              color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limit=c(0,1.1),oob=scales::squish) +
    scale_color_identity() + 
    facet_grid(snu1_lab ~ indicator, scales = "free_y") +
    labs(x = NULL, y = NULL,
         title = glue("FY{str_sub(curr_fy, -2)}Q{curr_qtr} Tanzania | USAID") %>% toupper,
         subtitle = glue("Regional achievement (large, labeled points) with council reference points<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal for {percent(.25*curr_qtr)} at Q{curr_qtr} (snapshot indicators pegged to year end target 100%)</span>"),
         caption = glue("Target achievement capped at 110%
                        Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(family = "Consolas"),
          strip.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(0, "lines"))
  
  
  si_save(paste0(curr_pd,"_TZA_tx-achv_regional.png"),
          path = "Images")
  