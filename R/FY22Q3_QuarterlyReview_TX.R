# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  partner Q3 review
# REF ID:   1d42aef5 
# LICENSE:  MIT
# DATE:     2022-08-17
# UPDATED:  2022-08-23


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
  library(lubridate)
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
  
  peds <- c("<01", "01-04", "05-09", "10-14", "<15")

# IMPORT ------------------------------------------------------------------
  
  df <- read_msd(genie_path)

  # df <- df %>% 
  #   filter(operatingunit == "Tanzania",
  #          funding_agency == "USAID",
  #          fiscal_year >= 2021,
  #          indicator %in% c("TX_CURR", "TX_ML", "TX_NET_NEW", "TX_NET_NEW_SHIFT", 
  #                           "TX_NEW", "TX_PVLS", "TX_RTT"))
  
# PERIODS -----------------------------------------------------------------
  
  curr_pd <- source_info(genie_path, return = "period")
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  curr_qtr <- source_info(genie_path, return = "quarter")
  
  full_pds <- (min(df$fiscal_year) - 1) %>% 
    paste0("-10-01") %>% 
    as_date() %>% 
    seq.Date(convert_qtr_to_date(curr_pd), by = "3 months") %>% 
    convert_date_to_qtr()
  
  pd_brks <- str_replace(full_pds, "FY.*(2|4)$", "")
  

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
    geom_text(data = df_gr %>% filter(fiscal_year == curr_fy),
              aes(label = percent(growth_rate, 1), y = gr_label_position),
              family = "Source Sans Pro", color = trolley_grey_light, size = 9/.pt, 
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(grr_lab, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(df_gr$period)[grep("Q(1|3)", unique(df_gr$period))]) +
    scale_fill_manual(values = c(scooter_light, scooter)) +
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

# GROWTH RATE - PEDS ------------------------------------------------------
  
  df_gr_peds <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% peds) %>%
    group_by(fiscal_year, snu1, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    arrange(snu1, period)
  
  df_gr_peds <- df_gr_peds %>% 
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
  
  df_gr_peds %>%
    ggplot(aes(period, results, fill = as.character(fiscal_year))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .3) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(data = df_gr_peds %>% filter(fiscal_year == curr_fy),
              aes(label = percent(growth_rate, 1), y = gr_label_position),
              family = "Source Sans Pro", color = matterhorn, size = 9/.pt, 
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(grr_lab, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(df_gr_peds$period)[grep("Q(1|3)", unique(df_gr_peds$period))]) +
    scale_fill_manual(values = c(golden_sand_light, golden_sand)) +
    labs(x = NULL, y = NULL,
         title = glue("What growth rate is needed to reach the {str_replace(curr_fy, '20', 'FY')} PEDS treatment targets for USAID?") %>% toupper(),
         subtitle = "Current on treatment by region and growth rate needed in Q4 to reach target",
         caption = glue("Note: quarterly growth rate needed calculated as a compound annual growth rate
                         Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          panel.spacing = unit(.5, "picas"))
  
  
  si_save(paste0(curr_pd,"_TZA-peds_tx-curr-growth_regional.png"),
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
  

# ACHIEVEMENT - PEDS ------------------------------------------------------

  #aggregate to regional level
  df_achv_peds <- df %>%
    bind_rows(df %>% 
                mutate(psnu = snu1)) %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% peds,
           fiscal_year == curr_fy) %>% 
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    mutate(region = psnu == snu1)
  
  #calculate achievement
  df_achv_peds <- df_achv_peds %>% 
    adorn_achievement(curr_qtr)
  
  
  df_achv_peds_lab <- df_achv_peds %>%
    filter(region == TRUE) %>% 
    select(snu1, indicator, targets) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_from = targets) %>%
    mutate(snu1_lab = glue("{format(snu1, justify = 'right')}  {format(comma(tx_curr), justify = 'right')} | {format(comma(tx_new), justify = 'right')}")) %>% 
    select(snu1, snu1_lab, tx_curr)
  
  #viz adjustments
  df_achv_peds_viz <- df_achv_peds %>%
    mutate(snu1_achv = case_when(region == TRUE ~ achievement),
           baseline_pt_1 = 0,
           baseline_pt_2 = .25,
           baseline_pt_3 = .5,
           baseline_pt_4 = .75,
           baseline_pt_5 = 1) %>% 
    left_join(df_achv_peds_lab, by = 'snu1') %>% 
    mutate(snu1_lab = fct_reorder(snu1_lab, tx_curr, max, na.rm = TRUE, .desc = TRUE))
  
  df_achv_peds_viz %>% 
    ggplot(aes(achievement, snu1_lab, color = achv_color)) +
    geom_blank() +
    geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
    geom_point(aes(baseline_pt_1), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_2), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_3), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_4), shape = 3, color = "#D3D3D3") +
    geom_point(aes(baseline_pt_5), shape = 3, color = "#D3D3D3") +
    geom_jitter(data = filter(df_achv_peds_viz, region == FALSE),
                position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                alpha = .4, size = 3) +
    geom_point(data = filter(df_achv_peds_viz, region == TRUE),
               size = 8, alpha = .8, na.rm = TRUE) +
    geom_text(data = filter(df_achv_peds_viz, region == TRUE),
              aes(label = percent(achievement, 1)), na.rm = TRUE,
              color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limit=c(0,1.1),oob=scales::squish) +
    scale_color_identity() + 
    facet_grid(snu1_lab ~ indicator, scales = "free_y") +
    labs(x = NULL, y = NULL,
         title = glue("FY{str_sub(curr_fy, -2)}Q{curr_qtr} Tanzania | PEDS | USAID") %>% toupper,
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
  
  
  si_save(paste0(curr_pd,"_TZA-peds_tx-achv_regional.png"),
          path = "Images")  

# NEW V NET_NEW -----------------------------------------------------------

  df_nn <- df %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW")) %>%
    pluck_totals() %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    pivot_longer(c(tx_net_new, tx_new), 
                 names_to = "indicator") %>% 
    mutate(fill_color = ifelse(indicator == "tx_net_new", old_rose, denim),
           indicator = glue("{toupper(indicator)}"),
           share = value / tx_curr)
  
  df_nn %>%
    filter(period != min(period),
           tx_curr != 0) %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~fct_reorder2(snu1, period, tx_curr),
               scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(label = pd_brks[2:length(pd_brks)]) +
    scale_fill_manual(values = c("TX_NEW" = scooter,
                                 "TX_NET_NEW" = scooter_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("IMPROVED <span style='color:{scooter_light}'>NET_NEW</span> RESULTS IN LATE FY22, BUT STILL LAGGING BEHIND <span style='color:{scooter}'>TX_NEW</span>"),
         caption = glue("Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_region_tx-new-nn.png"),
          scale = 1.1)  
  

# NEW V NET_NEW - PEDS ----------------------------------------------------

  df_nn_peds <- df %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% peds) %>%
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    pivot_longer(c(tx_net_new, tx_new), 
                 names_to = "indicator") %>% 
    mutate(fill_color = ifelse(indicator == "tx_net_new", old_rose, denim),
           indicator = glue("{toupper(indicator)}"),
           share = value / tx_curr)
  
  df_nn_peds %>%
    filter(period != min(period),
           tx_curr != 0) %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~fct_reorder2(snu1, period, tx_curr),
               scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(label = pd_brks[2:length(pd_brks)]) +
    scale_fill_manual(values = c("TX_NEW" = golden_sand,
                                 "TX_NET_NEW" = golden_sand_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("LARGELY NEGATIVE PEDS <span style='color:{golden_sand_light}'>NET_NEW</span> RESULTS THROUGH FY22 OVERSHADOWING <span style='color:{golden_sand}'>TX_NEW</span> RESULTS"),
         caption = glue("Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA-peds_region_tx-new-nn.png"),
          scale = 1.1) 
# MMD ---------------------------------------------------------------------

  df_mmd <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    ))   
  
  df_mmd <- df_mmd %>%
    group_by(fiscal_year, snu1, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd <- df_mmd %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-c(indicator, `Less than 3 months`, `3 to 5 months`)) %>% 
    pivot_longer(-c(period, snu1, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total)
  
  df_mmd <- df_mmd %>% 
    arrange(snu1, otherdisaggregate, period) %>% 
    filter(!is.na(tx_mmd)) %>% 
    mutate(share = tx_mmd/tx_curr)
  
  df_mmd <- df_mmd %>% 
    mutate(max_tx = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_curr, 0),
           max_mmd = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_mmd, 0)) %>%
    group_by(snu1, otherdisaggregate) %>% 
    mutate(endpoints = case_when(period %in% c(max(period), min(period))~share)) %>% 
    group_by(snu1) %>% 
    mutate(max_tx = max(max_tx),
           max_mmd = max(max_mmd)) %>% 
    ungroup() %>% 
    mutate(snu1_lab = case_when(max_tx == max(max_tx) ~ 
                                     glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                   TRUE ~ glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)}</span>")),
           snu1_lab = str_replace(snu1_lab, "NA", "0")) %>% 
    filter(max_tx > 0)
  
  
  df_mmd <- df_mmd %>% 
    mutate(fill_color = case_when(otherdisaggregate == "o6mmd" ~ scooter,
                                  TRUE ~ scooter_light))
  
  df_mmd %>%
    ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .4, size = .9, position = "identity") +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    facet_wrap(~fct_reorder2(snu1_lab, period, tx_curr, .desc = TRUE)) +
    scale_y_continuous(label = percent, 
                       breaks = seq(0, 1, .5)) +
    scale_x_discrete(label = pd_brks[1:length(pd_brks)]) +
    scale_color_identity(aesthetics = c("color","fill")) +
    expand_limits(y = 1) +
    labs(x = NULL, y = NULL,
         title = glue("USAID HAS MADE LARGE GAINS IN FY22 TOWARDS GETTING TREATMENT PATIENTS ON <span style='color:{scooter}'>+6 MONTHS</span> OF MMD"),
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more",  
                        "\nSource: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(plot.title = element_markdown(),
          panel.spacing.y = unit(.5, "line"),
          panel.spacing.x = unit(.5, "line"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown())
  
  
  si_save(glue("Images/{curr_pd}_TZA_region_mmd.png"),
          scale = 1.1)

# MMD - PEDS --------------------------------------------------------------

  df_mmd_peds <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus",
           ageasentered %in% peds)
  
  df_mmd_peds <- df_mmd_peds %>% 
    bind_rows(df_mmd_peds %>% 
                mutate(standardizeddisaggregate = "Total Numerator",
                       otherdisaggregate = NA)) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    ))   
  
  df_mmd_peds <- df_mmd_peds %>%
    group_by(fiscal_year, snu1, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd_peds <- df_mmd_peds %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-c(indicator, `Less than 3 months`, `3 to 5 months`)) %>% 
    pivot_longer(-c(period, snu1, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total)
  
  df_mmd_peds <- df_mmd_peds %>% 
    arrange(snu1, otherdisaggregate, period) %>% 
    filter(!is.na(tx_mmd)) %>% 
    mutate(share = tx_mmd/tx_curr)
  
  df_mmd_peds <- df_mmd_peds %>% 
    mutate(max_tx = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_curr, 0),
           max_mmd = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_mmd, 0)) %>%
    group_by(snu1, otherdisaggregate) %>% 
    mutate(endpoints = case_when(period %in% c(max(period), min(period))~share)) %>% 
    group_by(snu1) %>% 
    mutate(max_tx = max(max_tx),
           max_mmd = max(max_mmd)) %>% 
    ungroup() %>% 
    mutate(snu1_lab = case_when(max_tx == max(max_tx) ~ 
                                  glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                TRUE ~ glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)}</span>")),
           snu1_lab = str_replace(snu1_lab, "NA", "0")) %>% 
    filter(max_tx > 0)
  
  
  df_mmd_peds <- df_mmd_peds %>% 
    mutate(fill_color = case_when(otherdisaggregate == "o6mmd" ~ golden_sand,
                                  TRUE ~ golden_sand_light))
  
  df_mmd_peds %>%
    ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .4, size = .9, position = "identity") +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    facet_wrap(~fct_reorder2(snu1_lab, period, tx_curr, .desc = TRUE)) +
    scale_y_continuous(label = percent, 
                       breaks = seq(0, 1, .5)) +
    scale_x_discrete(label = pd_brks[1:length(pd_brks)]) +
    scale_color_identity(aesthetics = c("color","fill")) +
    expand_limits(y = 1) +
    labs(x = NULL, y = NULL,
         title = glue("USAID HAS MADE LARGE GAINS IN FY22 TOWARDS GETTING PEDS ON TREATMENT ON <span style='color:{golden_sand}'>+6 MONTHS</span> OF MMD"),
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more",  
                        "\nSource: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(plot.title = element_markdown(),
          panel.spacing.y = unit(.5, "line"),
          panel.spacing.x = unit(.5, "line"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown())
  
    si_save(glue("Images/{curr_pd}_TZA-peds_region_mmd.png"),
          scale = 1.1)
# IIT ---------------------------------------------------------------------

  df_iit <- df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW")) %>%
    pluck_totals() %>%
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit <- df_iit %>%
    group_by(psnu) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_snu_lab <- df_iit %>% 
    filter(period == max(period)) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    mutate(snu1_lab = glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)}")) %>% 
    # mutate(snu1_lab = ifelse(n == max(n), glue("{snu1} - {comma(n, 1)} (TX_CURR {curr_pd})"),
    #                          glue("{snu1} - {comma(n, 1)}"))) %>% 
    select(-n)
  
  # pd_brks <- df_iit %>% 
  #   distinct(period) %>% 
  #   filter(period != min(period)) %>% 
  #   mutate(period = ifelse(period == max(period), period, "")) %>% 
  #   # mutate(period = str_replace(period, "FY.*(1|3)$", "")) %>% 
  #   pull()
  
  snu_tx_order <- df_iit %>% 
    filter(period == max(period)) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    pull(snu1)
  
  
  df_iit %>%
    left_join(df_snu_lab, by = "snu1") %>% 
    mutate(snu1_lab = factor(snu1_lab, df_snu_lab$snu1_lab),
           fiscal_year = str_sub(period, end = 4)) %>% 
    # mutate(snu1 = factor(snu1, snu_tx_order)) %>% 
    filter(tx_curr_lag1 != 0) %>% 
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = scooter,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = snu1_lab),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5, color = golden_sand) +
    facet_wrap(~snu1_lab) +
    scale_size(label = comma, guide = NULL) +
    scale_x_discrete(labels = pd_brks[2:length(pd_brks)]) +
    scale_y_continuous(limits = c(0,.25),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Sizable IIT outliers in large treatment regions like Morogoro and Ruvuma in late FY22") %>% toupper,
         subtitle = "",
         caption = glue("Note: IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_region_iit_lim.png"),
          scale = 1.1)  
  
  

# IIT - PEDS --------------------------------------------------------------

  df_iit_peds <- df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"),
           ageasentered %in% peds) %>%
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_peds <- df_iit_peds %>%
    group_by(psnu) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_snu_lab_peds <- df_iit_peds %>% 
    filter(period == max(period)) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    mutate(snu1_lab = glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)}")) %>% 
    # mutate(snu1_lab = ifelse(n == max(n), glue("{snu1} - {comma(n, 1)} (TX_CURR {curr_pd})"),
    #                          glue("{snu1} - {comma(n, 1)}"))) %>% 
    select(-n)
  
  snu_tx_order_peds <- df_iit_peds %>% 
    filter(period == max(period)) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    pull(snu1)
  
  
  df_iit_peds %>%
    left_join(df_snu_lab_peds, by = "snu1") %>% 
    mutate(snu1_lab = factor(snu1_lab, df_snu_lab_peds$snu1_lab),
           fiscal_year = str_sub(period, end = 4)) %>% 
    # mutate(snu1 = factor(snu1, snu_tx_order)) %>% 
    filter(tx_curr_lag1 != 0) %>% 
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = golden_sand,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = snu1_lab),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5, color = scooter) +
    facet_wrap(~snu1_lab) +
    scale_size(label = comma, guide = NULL) +
    scale_x_discrete(labels = pd_brks[2:length(pd_brks)]) +
    scale_y_continuous(limits = c(0,.25),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Sizable IIT outliers for peds in large treatment regions like Morogoro and Ruvuma in late FY22") %>% toupper,
         subtitle = "",
         caption = glue("Note: IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source} | US Agency for International Development | {ref_id}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA-peds_region_iit_lim.png"),
          scale = 1.1)    
  