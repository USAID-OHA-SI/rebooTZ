# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  quarterly partner review
# REF ID:   c9751ba1 
# LICENSE:  MIT
# DATE:     2024-05-06
# UPDATED:  2024-05-08
# NOTE:     Adapted from FY24Q1_QuarterlyReview_TX.R


# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(googledrive)
  library(readxl)
  library(janitor, warn.conflicts = FALSE)
  library(lubridate)
  library(ggrepel)
  
  source("R/pdap_pull.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "c9751ba1"
  
  peds <- c("<01", "01-04", "01-09", "05-09", "10-14", "<15")
  
  over50 <- c("50-54","55-59", "60-64", "65+")
  
  curr_limiter <- 75
  
  #focal regions
  snu_sel <- c("Arusha","Dodoma", "Kilimanjaro", "Singida","Manyara", "Iringa",
               "Njombe", "Mtwara", "Lindi", "Ruvuma", "Morogoro")

  load_secrets()
  

# PDAP WAVE API -----------------------------------------------------------
  
  # #get TZA UID
  # cntry_uid <- pepfar_country_list %>% 
  #   filter(country == "Tanzania") %>% 
  #   pull(country_uid)
  # 
  # #establish parameters to pass into POST API
  # post_body <- list(
  #   daily_frozen='daily',
  #   fiscal_year=list(2023, 2024),
  #   funding_agency = list("USAID"),
  #   indicator=list("TX_CURR","TX_ML","TX_CURR_LAG2", "TX_NET_NEW","TX_NEW",
  #                  "TX_RTT","PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART"),
  #   uid_hierarchy_list=list(stringr::str_glue('-|-|{cntry_uid}')))
  # 
  # #run API
  # pdap_pull(post_body)
  
  genie_path <- return_latest("Data", "Genie")
  
  meta <- get_metadata(genie_path, caption_note = "USAID")

# IMPORT MER --------------------------------------------------------------

  #read genie
  df <- read_psd(genie_path)

  #read msd
  df <- df %>%
    filter(operatingunit == "Tanzania",
           funding_agency == "USAID",
           fiscal_year >= 2021,
           indicator %in% c("TX_CURR", "TX_ML", "TX_NET_NEW", "TX_NET_NEW_SHIFT",
                            "TX_NEW", "TX_PVLS", "TX_RTT", "PMTCT_ART", "PMTCT_STAT_POS"))

  
# IMPORT CTC --------------------------------------------------------------

  # ctc_path_local <- tempfile(fileext = ".xlsx")
  # 
  # drive_download(ctc_path, ctc_path_local)
  # 
  # df_ctc <- read_excel(ctc_path_local, sheet = "Output", col_types = "text",
  #                      .name_repair = make_clean_names) %>% 
  #   filter(agency == "USAID")
  
# PERIODS -----------------------------------------------------------------
  
  full_pds <- (min(df$fiscal_year) - 1) %>% 
    paste0("-10-01") %>% 
    as_date() %>% 
    seq.Date(convert_qtr_to_date(meta$curr_pd), by = "3 months") %>% 
    convert_date_to_qtr()
  
  pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")
  

# GROWTH RATE -------------------------------------------------------------

  df_gr <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           snu1 %in% snu_sel) %>% 
    group_by(fiscal_year, snu1, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), \(x) sum(x, na.rm = TRUE)), 
              .groups = "drop") %>%
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    arrange(snu1, period)
  
  df_gr <- df_gr %>% 
    mutate(targets_curr_fy = case_when(fiscal_year == meta$curr_fy ~ targets)) %>% 
    group_by(snu1) %>% 
    fill(targets_curr_fy, .direction = "updown") %>% 
    ungroup() %>% 
    filter(!is.na(targets_curr_fy))
  
  df_gr <- df_gr %>% 
    mutate(growth_rate_req = case_when(period == meta$curr_pd ~ ((targets/results)^(1/(4-meta$curr_qtr))) -1)) %>% 
    group_by(snu1) %>% 
    fill(growth_rate_req, .direction = "updown") %>% 
    mutate(growth_rate = (results - lag(results, order_by = period))/lag(results, order_by = period)) %>% 
    ungroup() %>% 
    mutate(grr_lab = case_when(targets == 0 ~ glue("{toupper(snu1)}\nNo target set"),
                               growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget already achieved"), 
                               growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
                               is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"),
                               TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
           gr_label_position = 0,
           growth_rate = ifelse(is.infinite(growth_rate) | is.nan(growth_rate), NA_real_, growth_rate),
           targets_plot = ifelse(targets == 0, NA, targets),
           disp_targets = case_when(fiscal_year == meta$curr_fy ~ targets))
  
  df_gr %>%
    ggplot(aes(period, results, fill = as.character(fiscal_year))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .3) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets_plot, ymax = targets_plot), linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(data = df_gr %>% filter(fiscal_year == meta$curr_fy),
              aes(label = percent(growth_rate, 1), y = gr_label_position),
              family = "Source Sans Pro", color = trolley_grey_light, size = 9/.pt, 
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(grr_lab, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(df_gr$period)[grep("Q(2|4)", unique(df_gr$period))]) +
    scale_fill_manual(values = c(scooter_light, scooter)) +
    labs(x = NULL, y = NULL,
      title = glue("What growth rate is needed to reach the {str_replace(meta$curr_fy, '20', 'FY')} treatment targets for USAID?") %>% toupper(),
      subtitle = "Current on treatment by region and growth rate needed in Q4 to reach target",
      caption = glue("Note: quarterly growth rate needed calculated as a compound annual growth rate
                      {meta$caption}")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          panel.spacing = unit(.5, "picas"))
    

  si_save(glue("{meta$curr_pd}_TZA_tx-curr-growth_regional.png"),
          path = "Images",
          scale = 1.5)

# GROWTH RATE - PEDS ------------------------------------------------------
  
  df_gr_peds <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/CD4/HIVStatus"),
           snu1 %in% snu_sel,
           trendscoarse %in% "<15") %>%
    group_by(fiscal_year, snu1, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), \(x) sum(x, na.rm = TRUE)), 
              .groups = "drop") %>% 
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    arrange(snu1, period)
  
  df_gr_peds <- df_gr_peds %>% 
    mutate(targets_curr_fy = case_when(fiscal_year == meta$curr_fy ~ targets)) %>% 
    group_by(snu1) %>% 
    fill(targets_curr_fy, .direction = "updown") %>% 
    ungroup() %>% 
    filter(!is.na(targets_curr_fy))
  
  df_gr_peds <- df_gr_peds %>% 
    mutate(growth_rate_req = case_when(period == meta$curr_pd ~ ((targets/results)^(1/(4-meta$curr_qtr))) -1)) %>% 
    group_by(snu1) %>% 
    fill(growth_rate_req, .direction = "updown") %>% 
    mutate(growth_rate = (results - lag(results, order_by = period))/lag(results, order_by = period)) %>% 
    ungroup() %>% 
    mutate(grr_lab = case_when(targets == 0 ~ glue("{toupper(snu1)}\nNo target set"),
                               growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget already achieved"), 
                               growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
                               is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"),
                               TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
           gr_label_position = 0,
           growth_rate = ifelse(is.infinite(growth_rate) | is.nan(growth_rate), NA_real_, growth_rate),
           targets_plot = ifelse(targets == 0, NA, targets),
           disp_targets = case_when(fiscal_year == meta$curr_fy ~ targets))
  
  df_gr_peds %>%
    ggplot(aes(period, results, fill = as.character(fiscal_year))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .3) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets_plot, ymax = targets_plot), linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(data = df_gr_peds %>% filter(fiscal_year == meta$curr_fy),
              aes(label = percent(growth_rate, 1), y = gr_label_position),
              family = "Source Sans Pro", color = matterhorn, size = 9/.pt, 
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(grr_lab, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_si(""))) +
    scale_x_discrete(breaks = unique(df_gr_peds$period)[grep("Q(2|4)", unique(df_gr_peds$period))]) +
    scale_fill_manual(values = c(golden_sand_light, golden_sand)) +
    labs(x = NULL, y = NULL,
         title = glue("What growth rate is needed to reach the {meta$curr_fy_lab} PEDS treatment targets for USAID?") %>% toupper(),
         subtitle = "Current on treatment by region and growth rate needed in Q4 to reach target",
         caption = glue("Note: quarterly growth rate needed calculated as a compound annual growth rate
                         {meta$caption}")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          panel.spacing = unit(.5, "picas"))
  
  
  si_save(glue("{meta$curr_pd}_TZA-peds_tx-curr-growth_regional.png"),
          path = "Images",
          scale = 1.5)
  

# GROWTH RATE - USAID -----------------------------------------------------

  df_gr_usaid <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, funding_agency, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), \(x) sum(x, na.rm = TRUE)), 
              .groups = "drop") %>%
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    arrange(funding_agency, period)
  
  df_gr_usaid <- df_gr_usaid %>% 
    mutate(targets_curr_fy = case_when(fiscal_year == meta$curr_fy ~ targets)) %>% 
    group_by(funding_agency) %>% 
    fill(targets_curr_fy, .direction = "updown") %>% 
    ungroup() %>% 
    filter(!is.na(targets_curr_fy))
  
  df_gr_usaid <- df_gr_usaid %>% 
    mutate(growth_rate_req = case_when(period == meta$curr_pd ~ ((targets/results)^(1/(4-meta$curr_qtr))) -1)) %>% 
    group_by(funding_agency) %>% 
    fill(growth_rate_req, .direction = "updown") %>% 
    mutate(growth_rate = (results - lag(results, order_by = period))/lag(results, order_by = period)) %>% 
    ungroup() %>% 
    mutate(grr_lab = case_when(targets == 0 ~ glue("{toupper(funding_agency)}\nNo target set"),
                               growth_rate_req < 0 ~ glue("{toupper(funding_agency)}\nTarget already achieved"), 
                               growth_rate_req < .1 ~ glue("{toupper(funding_agency)}\n{percent(growth_rate_req, 1)}"),
                               is.infinite(growth_rate_req) ~ glue("{toupper(funding_agency)}"),
                               TRUE ~ glue("{toupper(funding_agency)}\n{percent(growth_rate_req, 1)}")),
           gr_label_position = 0,
           growth_rate = ifelse(is.infinite(growth_rate) | is.nan(growth_rate), NA_real_, growth_rate),
           targets_plot = ifelse(targets == 0, NA, targets),
           disp_targets = case_when(fiscal_year == meta$curr_fy ~ targets))
  
  df_gr_usaid %>%
    ggplot(aes(period, results, fill = as.character(fiscal_year))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .3) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets_plot, ymax = targets_plot), linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(data = df_gr_usaid %>% filter(fiscal_year == meta$curr_fy),
              aes(label = percent(growth_rate, 1), y = gr_label_position),
              family = "Source Sans Pro", color = trolley_grey_light, size = 13/.pt, 
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(grr_lab, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    # scale_x_discrete(breaks = unique(df_gr$period)[grep("Q(2|4)", unique(df_gr$period))]) +
    scale_fill_manual(values = c(moody_blue_light, moody_blue)) +
    labs(x = NULL, y = NULL,
         title = glue("What growth rate is needed to reach the {str_replace(meta$curr_fy, '20', 'FY')} treatment targets for USAID?") %>% toupper(),
         subtitle = "Current on treatment and growth rate needed in Q4 to reach target",
         caption = glue("Note: quarterly growth rate needed calculated as a compound annual growth rate
                      {meta$caption}")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          panel.spacing = unit(.5, "picas"))
  
  
  si_save(glue("{meta$curr_pd}_TZA_tx-curr-growth_regional_usaid.png"),
          path = "Images",
          scale = 1.5)
  
# ACHIEVEMENT -------------------------------------------------------------

  #aggregate to regional level
  df_achv <- df %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           fiscal_year == meta$curr_fy,
           snu1 %in% snu_sel) %>% 
    pluck_totals() %>% 
    group_by(fiscal_year, snu1, indicator) %>% 
    summarize(across(c(targets, cumulative),\(x) sum(x, na.rm = TRUE)), 
              .groups = "drop") 
  
  #calculate achievement
  df_achv <- df_achv %>% 
    adorn_achievement(meta$curr_qtr)
  
  
  df_achv_lab <- df_achv %>%
    select(snu1, indicator, targets) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_from = targets) %>%
    mutate(snu1_lab = glue("{format(snu1, justify = 'right')}  {format(comma(tx_curr), justify = 'right')} | {format(comma(tx_new), justify = 'right')}")) %>% 
    select(snu1, snu1_lab, tx_curr)

  #viz adjustments
  df_achv_viz <- df_achv %>%
    mutate(snu1_achv = achievement,
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
    geom_point(size = 8, alpha = .8, na.rm = TRUE) +
    geom_text(aes(label = percent(achievement, 1)), na.rm = TRUE,
              color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limit=c(0,1.1),oob=scales::squish) +
    scale_color_identity() + 
    facet_grid(snu1_lab ~ indicator, scales = "free_y") +
    labs(x = NULL, y = NULL,
         title = glue("{meta$curr_pd} Tanzania | USAID") %>% toupper,
         subtitle = glue("Regional target achievement<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal for {percent(.25*meta$curr_qtr)} at Q{meta$curr_qtr} (snapshot indicators pegged to year end target 100%)</span>"),
         caption = glue("Note: Target achievement capped at 110%
                        {meta$caption}")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(family = "Consolas"),
          strip.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(0, "lines"))
  
  
  si_save(glue("{meta$curr_pd}_TZA_tx-achv_regional.png"),
          path = "Images")
  

# ACHIEVEMENT - PEDS ------------------------------------------------------

  #aggregate to regional level
  df_achv_peds <- df %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/CD4/HIVStatus"),
           snu1 %in% snu_sel,
           trendscoarse %in% "<15",
           fiscal_year == meta$curr_fy) %>% 
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)), 
              .groups = "drop")
  
  #calculate achievement
  df_achv_peds <- df_achv_peds %>% 
    adorn_achievement(meta$curr_qtr)
  
  
  df_achv_peds_lab <- df_achv_peds %>%
    select(snu1, indicator, targets) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_from = targets) %>%
    mutate(snu1_lab = glue("{format(snu1, justify = 'right')}  {format(comma(tx_curr), justify = 'right')} | {format(comma(tx_new), justify = 'right')}")) %>% 
    select(snu1, snu1_lab, tx_curr)
  
  #viz adjustments
  df_achv_peds_viz <- df_achv_peds %>%
    mutate(snu1_achv = achievement,
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
    geom_point(size = 8, alpha = .8, na.rm = TRUE) +
    geom_text(aes(label = percent(achievement, 1)), na.rm = TRUE,
              color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limit=c(0,1.1),oob=scales::squish) +
    scale_color_identity() + 
    facet_grid(snu1_lab ~ indicator, scales = "free_y") +
    labs(x = NULL, y = NULL,
         title = glue("{meta$curr_pd} Tanzania | PEDS | USAID") %>% toupper,
         subtitle = glue("Regional target achievement<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal for {percent(.25*meta$curr_qtr)} at Q{meta$curr_qtr} (snapshot indicators pegged to year end target 100%)</span>"),
         caption = glue("Note: Target achievement capped at 110%
                         {meta$caption}")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(family = "Consolas"),
          strip.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(0, "lines"))
  
  
  si_save(glue("{meta$curr_pd}_TZA-peds_tx-achv_regional.png"),
          path = "Images")  

# NEW V NET_NEW -----------------------------------------------------------

  df_nn <- df %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           snu1 %in% snu_sel) %>%
    pluck_totals() %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
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
         title = glue("DECLINING <span style='color:{scooter}'>TX_NEW</span> TREND COUPLED WITH INCREASING GAP WITH <span style='color:{scooter_light}'>NET_NEW</span> ACROSS REGIONS"),
         caption = glue("{meta$caption}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("{meta$curr_pd}_TZA_region_tx-new-nn.png"),
          path = "Images",
          scale = 1.1)  
  

# NEW V NET_NEW - PEDS ----------------------------------------------------

  df_nn_peds <- df %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/CD4/HIVStatus"),
           snu1 %in% snu_sel,
           trendscoarse %in% "<15") %>%
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    pivot_longer(c(tx_net_new, tx_new), 
                 names_to = "indicator") %>% 
    mutate(max_tx = ifelse(period == max(period), tx_curr, 0)) %>% 
    group_by(snu1) %>% 
    mutate(max_tx = max(max_tx)) %>% 
    ungroup() %>% 
    mutate(fill_color = ifelse(indicator == "tx_net_new", old_rose, denim),
           indicator = glue("{toupper(indicator)}"),
           share = value / tx_curr)
  
  df_nn_peds %>%
    filter(period != min(period),
           max_tx > curr_limiter) %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .75, na.rm = TRUE,
             position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~fct_reorder2(snu1, period, tx_curr),
               scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(label = pd_brks[2:length(pd_brks)]) +
    scale_fill_manual(values = c("TX_NEW" = golden_sand,
                                 "TX_NET_NEW" = golden_sand_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("DECLINING PEDS <span style='color:{golden_sand}'>TX_NEW</span> TREND COUPLED WITH INCREASING GAP WITH <span style='color:{golden_sand_light}'>NET_NEW</span> ACROSS REGIONS"),
         subtitle = glue("Limited to regions with more than {curr_limiter} TX_CURR <15"),
         caption = glue("{meta$caption}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("{meta$curr_pd}_TZA-peds_region_tx-new-nn.png"),
          path = "Images",
          scale = 1.1) 
  

# NEW V NET_NEW - USAID ---------------------------------------------------


  df_nn_usaid <- df %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW")) %>%
    pluck_totals() %>% 
    group_by(funding_agency, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    pivot_longer(c(tx_net_new, tx_new), 
                 names_to = "indicator") %>% 
    mutate(fill_color = ifelse(indicator == "tx_net_new", old_rose, denim),
           indicator = glue("{toupper(indicator)}"),
           share = value / tx_curr)
  
  df_nn_usaid %>%
    filter(period != min(period),
           tx_curr != 0) %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .4)) +
    geom_text(aes(label = label_number(accuracy = .1, scale_cut = cut_si(""))(value)),
              position = position_dodge(width = .4), vjust = -.5, na.rm = TRUE,
              family = "Source Sans Pro", color = matterhorn) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = c("TX_NEW" = moody_blue,
                                 "TX_NET_NEW" = moody_blue_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("INCREASING GAP BETWEEN  <span style='color:{moody_blue}'>TX_NEW</span> AND <span style='color:{moody_blue_light}'>NET_NEW</span> RESULTS SINCE FY23"),
         caption = glue("{meta$caption}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("{meta$curr_pd}_TZA_region_tx-new-nn_usaid.png"),
          path = "Images",
          scale = 1.1)  
  
    
# MMD ---------------------------------------------------------------------

  df_mmd <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus"),
           snu1 %in% snu_sel) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    ))   
  
  df_mmd <- df_mmd %>%
    group_by(fiscal_year, snu1, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
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
    # mutate(snu1_lab = case_when(max_tx == max(max_tx) ~ 
    #                               glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
    #                             TRUE ~ glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)}</span>")),
    #        snu1_lab = str_replace(snu1_lab, "NA", "0")) %>%
    mutate(snu1_lab = case_when(max_tx == max(max_tx) ~ 
                                     glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_si(''))(max_mmd)} / {label_number(1, scale_cut = cut_si(''))(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                   TRUE ~ glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_si(''))(max_mmd)} / {label_number(1, scale_cut = cut_si(''))(max_tx)}</span>")),
           snu1_lab = str_replace(snu1_lab, "NA", "0")) %>% 
    filter(max_tx > 0)
  
  
  df_mmd <- df_mmd %>% 
    mutate(fill_color = case_when(otherdisaggregate == "o6mmd" ~ scooter,
                                  TRUE ~ scooter_light))
  
  df_mmd %>%
    filter(otherdisaggregate == "o6mmd") %>% 
    ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .4, linewidth = .9, position = "identity") +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    facet_wrap(~fct_reorder2(snu1_lab, period, tx_curr, .desc = TRUE)) +
    scale_y_continuous(label = percent,
                       limit=c(0,1.1), oob=scales::squish,
                       breaks = seq(0, 1, .5)) +
    scale_x_discrete(label = pd_brks[1:length(pd_brks)]) +
    scale_color_identity(aesthetics = c("color","fill")) +
    expand_limits(y = 1) +
    labs(x = NULL, y = NULL,
         title = glue("USAID HAS SEEN A PLATEAU IN MOVING TREATMENT PATIENTS TO <span style='color:{scooter}'>+6 MONTHS</span> OF MMD"),
         caption = meta$caption) +
    si_style_ygrid() +
    theme(plot.title = element_markdown(),
          panel.spacing.y = unit(.5, "line"),
          panel.spacing.x = unit(.5, "line"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown())
  
  
  si_save(glue("{meta$curr_pd}_TZA_region_mmd.png"),
          path = "Images",
          scale = 1.1)

# MMD - PEDS --------------------------------------------------------------

  df_mmd_peds <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus",
           snu1 %in% snu_sel,
           trendscoarse %in% "<15")
  
  df_mmd_peds <- df_mmd_peds %>% 
    bind_rows(df_mmd_peds %>% 
                mutate(standardizeddisaggregate = "Total Numerator",
                       otherdisaggregate = NA)) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    ))   
  
  df_mmd_peds <- df_mmd_peds %>%
    group_by(fiscal_year, snu1, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
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
    # mutate(snu1_lab = case_when(max_tx == max(max_tx) ~
    #                               glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
    #                             TRUE ~ glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_short_scale())(max_mmd)} / {label_number(1, scale_cut = cut_short_scale())(max_tx)}</span>")),
    #        snu1_lab = str_replace(snu1_lab, "NA", "0")) %>%
    mutate(snu1_lab = case_when(max_tx == max(max_tx) ~ 
                                  glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_si(''))(max_mmd)} / {label_number(1, scale_cut = cut_si(''))(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                TRUE ~ glue("{snu1}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_si(''))(max_mmd)} / {label_number(1, scale_cut = cut_si(''))(max_tx)}</span>")),
           snu1_lab = str_replace(snu1_lab, "NA", "0")) %>% 
    filter(max_tx > 0)
  
  
  df_mmd_peds <- df_mmd_peds %>% 
    mutate(fill_color = case_when(otherdisaggregate == "o6mmd" ~ golden_sand,
                                  TRUE ~ golden_sand_light))
  
  df_mmd_peds %>%
    filter(otherdisaggregate == "o6mmd",
           max_tx > curr_limiter) %>%
    ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .4, linewidth = .9, position = "identity") +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    facet_wrap(~fct_reorder2(snu1_lab, period, tx_curr, .desc = TRUE)) +
    scale_y_continuous(label = percent, 
                       limit=c(0,1.1), oob=scales::squish,
                       breaks = seq(0, 1, .5)) +
    scale_x_discrete(label = pd_brks[1:length(pd_brks)]) +
    scale_color_identity(aesthetics = c("color","fill")) +
    expand_limits(y = 1) +
    labs(x = NULL, y = NULL,
         title = glue("GAINS TOWARDS GETTING PEDS ON <span style='color:{golden_sand}'>+6 MONTHS</span> OF MMD REMAINS LOWER THAN ADULT SHARES"),
         caption = glue("Regions limited to those with more than {curr_limiter} TX_CURR <15
         {meta$caption}")) +
    si_style_ygrid() +
    theme(plot.title = element_markdown(),
          panel.spacing.y = unit(.5, "line"),
          panel.spacing.x = unit(.5, "line"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown())
  
    si_save(glue("{meta$curr_pd}_TZA-peds_region_mmd.png"),
            path = "Images",
            scale = 1.1)
    

# MMD - USAID -------------------------------------------------------------


    df_mmd_usaid <- df %>% 
      filter(indicator == "TX_CURR",
             standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
      mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                           TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
      ))   
    
    df_mmd_usaid <- df_mmd_usaid %>%
      group_by(fiscal_year, funding_agency, indicator, otherdisaggregate) %>% 
      summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      filter(value > 0)
    
    #create group for o3mo and o6mo via reshaping for plotting
    df_mmd_usaid <- df_mmd_usaid %>% 
      pivot_wider(names_from = otherdisaggregate) %>% 
      rowwise() %>% 
      mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
        #unknown = ifelse(unknown < 0, 0, unknown),
        o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
      ) %>%
      ungroup() %>% 
      rename(o6mmd = `6 or more months`) %>% 
      select(-c(indicator, `Less than 3 months`, `3 to 5 months`)) %>% 
      pivot_longer(-c(period, funding_agency, total), 
                   names_to = "otherdisaggregate",
                   values_to = "tx_mmd") %>% 
      rename(tx_curr = total)
    
    df_mmd_usaid <- df_mmd_usaid %>% 
      arrange(funding_agency, otherdisaggregate, period) %>% 
      filter(!is.na(tx_mmd)) %>% 
      mutate(share = tx_mmd/tx_curr)
    
    df_mmd_usaid <- df_mmd_usaid %>% 
      mutate(max_tx = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_curr, 0),
             max_mmd = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_mmd, 0)) %>%
      group_by(funding_agency, otherdisaggregate) %>% 
      mutate(endpoints = case_when(period %in% c(max(period), min(period))~share)) %>% 
      group_by(funding_agency) %>% 
      mutate(max_tx = max(max_tx),
             max_mmd = max(max_mmd)) %>% 
      ungroup() %>% 
      mutate(funding_agency_lab = case_when(max_tx == max(max_tx) ~ 
                                    glue("{funding_agency}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_si(''))(max_mmd)} / {label_number(1, scale_cut = cut_si(''))(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                  TRUE ~ glue("{funding_agency}<br><span style = 'font-size:8pt'>{label_number(1, scale_cut = cut_si(''))(max_mmd)} / {label_number(1, scale_cut = cut_si(''))(max_tx)}</span>")),
             funding_agency_lab = str_replace(funding_agency_lab, "NA", "0")) %>% 
      filter(max_tx > 0)
    
    
    df_mmd_usaid <- df_mmd_usaid %>% 
      mutate(fill_color = case_when(otherdisaggregate == "o6mmd" ~ moody_blue,
                                    TRUE ~ moody_blue_light))
    
    df_mmd_usaid %>%
      filter(otherdisaggregate == "o6mmd") %>% 
      ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
      geom_area(alpha = .4, linewidth = .9, position = "identity") +
      geom_point(aes(y = endpoints), na.rm = TRUE) +
      geom_text(aes(y = endpoints, label = label_percent(accuracy = 1)(endpoints)),
                color = matterhorn, family = "Source Sans Pro", vjust = -.5) +
      facet_wrap(~fct_reorder2(funding_agency_lab, period, tx_curr, .desc = TRUE)) +
      scale_y_continuous(label = percent, 
                         limit=c(0,1.1), oob=scales::squish,
                         breaks = seq(0, 1, .5)) +
      scale_color_identity(aesthetics = c("color","fill")) +
      expand_limits(y = 1) +
      labs(x = NULL, y = NULL,
           title = glue("USAID HAS SEEN A PLATEAU IN MOVING TREATMENT PATIENTS TO <span style='color:{moody_blue}'>+6 MONTHS</span> OF MMD"),
           caption = meta$caption) +
      si_style_ygrid() +
      theme(plot.title = element_markdown(),
            panel.grid.major.y = element_line(color = "#E8E8E8"),
            panel.grid.minor.y = element_line(color = "#E8E8E8"),
            strip.text = element_markdown())
    
    
    si_save(glue("{meta$curr_pd}_TZA_region_mmd-usaid.png"),
            path = "Images",
            scale = 1.1)
        
# IIT ---------------------------------------------------------------------

  df_iit <- df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW"),
           snu1 %in% snu_sel) %>%
    pluck_totals() %>%
    group_by(fiscal_year, snu1, cop22_psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit <- df_iit %>%
    group_by(cop22_psnu) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_snu_lab <- df_iit %>% 
    filter(period == max(period)) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    mutate(snu1_lab = glue("{snu1} - {label_number(1, scale_cut = cut_si(''))(n)}")) %>% 
    # mutate(snu1_lab = ifelse(n == max(n), glue("{snu1} - {comma(n, 1)} (TX_CURR {curr_pd})"),
    #                          glue("{snu1} - {comma(n, 1)}"))) %>% 
    select(-n)
  
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
    scale_y_continuous(limits = c(0,.16),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Regional increases in IIT in {meta$curr_pd} across USAID/Tanzania") %>% toupper,
         subtitle = "Each point represents a council's IIT",
         caption = glue("Note: IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 15%
                        {meta$caption}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(glue("{meta$curr_pd}_TZA_region_iit_lim.png"),
          path = "Images",
          scale = 1.1)  
  
  

# IIT - PEDS --------------------------------------------------------------

  df_iit_peds <- df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"),
           snu1 %in% snu_sel,
           trendscoarse == "<15") %>%
    group_by(fiscal_year, snu1, cop22_psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_peds <- df_iit_peds %>%
    mutate(tx_max = ifelse(period == max(period), tx_curr, 0)) %>% 
    group_by(cop22_psnu) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period),
           tx_max = max(tx_max)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_snu_lab_peds <- df_iit_peds %>% 
    filter(period == max(period)) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    mutate(snu1_lab = glue("{snu1} - {label_number(1, scale_cut = cut_si(''))(n)}")) %>% 
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
    filter(tx_curr_lag1 != 0,
           tx_max > curr_limiter) %>% 
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
    scale_y_continuous(limits = c(0,.16),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Regional increases in Peds IIT in {meta$curr_pd} across USAID/Tanzania") %>% toupper,
         subtitle = "Each point represents a council's IIT",
         caption = glue("Note: IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 15%
                        Regions limited to more than {curr_limiter} TX_CURR <15
                        {meta$caption}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(glue("{meta$curr_pd}_TZA-peds_region_iit_lim.png"),
          path = "Images",
          scale = 1.1)    
  

# IIT - USAID -------------------------------------------------------------

  df_iit_usaid <- df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW")) %>%
    pluck_totals() %>%
    group_by(fiscal_year, funding_agency, cop22_psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_usaid <- df_iit_usaid %>%
    group_by(cop22_psnu) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_iit_usaid %>%
    filter(tx_curr_lag1 != 0) %>% 
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = moody_blue,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = funding_agency),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                linewidth = 1.1, color = burnt_sienna) +
    scale_size(label = comma, guide = NULL) +
    scale_x_discrete(labels = pd_brks[2:length(pd_brks)]) +
    scale_y_continuous(limits = c(0,.15),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Increated IIT in {meta$curr_pd} across USAID/Tanzania") %>% toupper,
         subtitle = "Each point represents a council's IIT",
         caption = glue("Note: IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 15%
                        {meta$caption}")) +
    si_style() +
    theme(plot.subtitle = element_markdown())
  
  si_save(glue("{meta$curr_pd}_TZA_region_iit_lim-usaid.png"),
          path = "Images",
          scale = 1.1)  
  
  

# ACHIEVEMENT - AGE/SEX ---------------------------------------------------

  #aggregate to regional level
  df_achv_age <- df %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/CD4/HIVStatus"),
           snu1 %in% snu_sel,
           # ageasentered != "50+",
           fiscal_year == meta$curr_fy) 
  
  #resolve 50+ age for TX_NEW
  df_achv_age <-df_achv_age %>% 
    mutate(ageasentered = ifelse(indicator == "TX_NEW" & ageasentered %in% over50, "50+", ageasentered))
  
  df_achv_age <- df_achv_age %>% 
    bind_rows(df_achv_age %>% mutate(snu1 = "USAID")) %>% 
    group_by(fiscal_year, snu1, indicator, target_age_2024, sex) %>% 
    summarize(across(c(targets, cumulative),\(x) sum(x, na.rm = TRUE)), 
              .groups = "drop") %>% 
    mutate(agency = snu1 == "USAID")
  
  #calculate achievement
  df_achv_age <- df_achv_age %>% 
    adorn_achievement(meta$curr_qtr)
  
  #breakout groups
  df_achv_age <- df_achv_age %>% 
    mutate(age_group = ifelse(target_age_2024 %in% peds, "Peds", "Adults"))

#viz
  df_achv_age %>% 
    filter(indicator == "TX_CURR",
           agency == FALSE) %>% 
    ggplot(aes(achievement, target_age_2024, color = sex)) +
    geom_blank() +
    annotate("rect",
             xmin = .9, xmax = 1.1,
             ymin = -Inf, ymax = Inf,
             fill = scooter, alpha = .1) +
    geom_jitter(
      aes(size = targets), #color = trolley_grey,
      position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
      alpha = .2) +
    geom_point(data = df_achv_age %>% 
                 filter(indicator == "TX_CURR",
                        agency == TRUE), 
               size = 6, alpha = .6, na.rm = TRUE) +
    geom_text(data = df_achv_age %>% 
                filter(indicator == "TX_CURR",
                       sex == "Female",
                       agency == TRUE), 
              aes(label = percent_format(1)(achievement)),
              vjust = -1,family = "Source Sans Pro") +
    geom_text(data = df_achv_age %>% 
                filter(indicator == "TX_CURR",
                       sex == "Male",
                       agency == TRUE), 
              aes(label = percent_format(1)(achievement)),
              vjust = 2.2,family = "Source Sans Pro") +
    facet_grid(age_group ~ ., scales = "free", space = "free") +
    scale_x_continuous(expand = c(.001, .001),
                       limit=c(0,1.1),
                       breaks = seq(0, 1, .25),
                       oob=scales::squish,
                       labels = label_percent(1),
                       position = "top") +
    scale_color_manual(values = c("Female" = moody_blue,
                                  "Male" = genoa)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "Significant gaps between Males and Females on TX_CURR achievement between 15-49" %>% toupper,
         subtitle = glue("Each smaller point represents regions achievement <span style='color:{genoa}'>Male</span> or <span style='color:{moody_blue}'>Female</span> | Larger + labeled points are USAID's achievement in that age band"),
         caption = glue("Note: Achievement capped at 110% \n{meta$caption}")) +
    si_style() +
    theme(panel.spacing = unit(1, "picas"),
          strip.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          legend.position = "none")
  
  si_save(glue("{meta$curr_pd}_TZA-usaid_age_txcurr_.png"),
          path = "Images",
          scale = 1.1)    
  
  
  
  #viz
  df_achv_age %>% 
    filter(indicator == "TX_NEW",
           agency == FALSE) %>% 
    ggplot(aes(achievement, target_age_2024, color = sex)) +
    geom_blank() +
    annotate("rect",
             xmin = meta$curr_qtr/4-.1, xmax = meta$curr_qtr/4+.1,
             ymin = -Inf, ymax = Inf,
             fill = scooter, alpha = .1) +
    geom_jitter(
      aes(size = targets), #color = trolley_grey,
      position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
      alpha = .2) +
    geom_point(data = df_achv_age %>% 
                 filter(indicator == "TX_NEW",
                        agency == TRUE), 
               size = 6, alpha = .6, na.rm = TRUE) +
    geom_text(data = df_achv_age %>% 
                filter(indicator == "TX_NEW",
                       sex == "Female",
                       agency == TRUE), 
              aes(label = percent_format(1)(achievement)),
              vjust = -1,family = "Source Sans Pro") +
    geom_text(data = df_achv_age %>% 
                filter(indicator == "TX_NEW",
                       sex == "Male",
                       agency == TRUE), 
              aes(label = percent_format(1)(achievement)),
              vjust = 2.2,family = "Source Sans Pro") +
    facet_grid(age_group ~ ., scales = "free", space = "free") +
    scale_x_continuous(expand = c(.001, .001),
                       limit=c(0,1.1),
                       breaks = seq(0, 1, .25),
                       oob=scales::squish,
                       labels = label_percent(1),
                       position = "top") +
    scale_color_manual(values = c("Female" = moody_blue,
                                  "Male" = genoa)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "Across the board under achievement in TX_NEW achievement for Males 15-49" %>% toupper,
         subtitle = glue("Each smaller point represents regions achievement <span style='color:{genoa}'>Male</span> or <span style='color:{moody_blue}'>Female</span> | Larger + labeled points are USAID's achievement in that age band"),
         caption = glue("Note: Achievement capped at 110% \n{meta$caption}")) +
    si_style() +
    theme(panel.spacing = unit(1, "picas"),
          strip.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          legend.position = "none")
  
  si_save(glue("{meta$curr_pd}_TZA-usaid_age_txnew_.png"),
          path = "Images",
          scale = 1.1)    

  

# MMD - AGE/SEX -----------------------------------------------------------

    
  df_mmd_age <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARVDispense/HIVStatus"),
           snu1 %in% snu_sel) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    ))   
  
  df_mmd_age <- df_mmd_age %>%
    group_by(fiscal_year, snu1, indicator, trendscoarse, sex, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd_age <- df_mmd_age %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-c(indicator, `Less than 3 months`, `3 to 5 months`)) %>% 
    pivot_longer(-c(period, snu1, sex, trendscoarse, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total)
  
  df_mmd_age <- df_mmd_age %>% 
    arrange(snu1, otherdisaggregate, period) %>% 
    filter(!is.na(tx_mmd)) %>% 
    mutate(share = tx_mmd/tx_curr)
  
  df_mmd_age <- df_mmd_age %>% 
    filter(period == max(period),
           otherdisaggregate == "o6mmd")
    
  df_mmd_age %>% 
    ggplot(aes(share, fct_reorder(snu1, tx_curr, sum), color = sex, group = snu1)) +
    geom_vline(xintercept = 0, color = "#505050") +
    geom_line(color = "#D3D3D3") +
    geom_point(aes(size = tx_curr), color = "white") +
    geom_point(aes(size = tx_curr), alpha = .5) +
    # geom_segment(aes(xend = share, yend = snu1)) +
    facet_wrap(.~trendscoarse)+
    coord_cartesian(clip = "off") +
    scale_color_manual(values = c("Female" = moody_blue,
                                  "Male" = genoa)) +
    scale_x_continuous(limits = c(0, 1.01), oob = squish,
                       label = percent_format(),
                       position = "top") +
    labs(x = NULL, y = NULL,
         title = "Large sex gaps in 6MMD pick up for Peds and for smaller regions" %>% toupper,
         subtitle = glue("Each point represents region's share of TX_CURR on 6mo MMD for <span style='color:{genoa}'>Males</span> and <span style='color:{moody_blue}'>Females</span>"),
         caption = glue("Note: MMD share capped at 101% \n{meta$caption}")) +
    si_style_xgrid() +
    theme(legend.position = "none",
          strip.placement = "outside",
          plot.subtitle = element_markdown())
  
  
  si_save(glue("{meta$curr_pd}_TZA-usaid_age_mmd_.png"),
          path = "Images",
          scale = 1.1)    

  
# PMTCT -------------------------------------------------------------------

    
  df_pmtct_linked <- df %>%
    filter(indicator %in% c("PMTCT_ART", "PMTCT_STAT_POS"),
           snu1 %in% snu_sel,
           fiscal_year == meta$curr_fy) %>%
    pluck_totals() %>%
    clean_indicator() %>%
    group_by(fiscal_year, snu1, cop22_psnu, indicator) %>%
    summarise(value = sum(cumulative, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    filter(pmtct_art_d > 0) %>%
    mutate(linked = pmtct_art / pmtct_stat_pos)



  v_p_linked <- df_pmtct_linked %>%
    ggplot(aes(linked, fct_reorder(snu1, pmtct_art_d, sum))) +
    geom_jitter(aes(size = pmtct_art_d),
                color = scooter,
                position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                alpha = .4) +
    # geom_text_repel(data = df_pmtct_linked %>% filter(linked <.9 | linked > 1.1),
    #                 aes(label = cop22_psnu),
    #                 family = "Source San Pro", color = matterhorn) +
    scale_x_continuous(label = percent_format(), position = "top",
                       limits = c(.25,1.25),
                       oob = oob_squish) +
    scale_size(guide = "none") +
    labs(x = NULL, y = NULL,
         # title = "PMTCT ART Linkage hovers around 100% with a few outliers" %>% toupper,
         subtitle = glue("Each point represents council's Proxy PMTCT Linked to ART"),
         # caption = glue("Note: Proxy Linked = PMTCT_ART / PMTCT_STAT_POS \n{meta$caption}")
         ) +
    theme(legend.position = "none") +
    si_style()


  v_p_artd <- df_pmtct_linked %>%
    count(snu1, wt = pmtct_art_d, name = "pmtct_art_d") %>%
    ggplot(aes(pmtct_art_d, fct_reorder(snu1, pmtct_art_d, sum))) +
    geom_col(fill = scooter) +
    scale_x_continuous(label = comma_format(),
                       position = "top") +
    labs(x = NULL, y = NULL,
         subtitle = "PMTCT_ART_D") +
    theme(legend.position = "none") +
    si_style()


  v_p_artd + v_p_linked +
    plot_annotation(title = "Many Councils' PMTCT ART Linkage dipped below 100%" %>% toupper,
                    caption = glue("Note: Proxy Linked = PMTCT_ART / PMTCT_STAT_POS \n{meta$caption}"),
                    theme = si_style())



  si_save(glue("{meta$curr_pd}_TZA-usaid_pmtct_linked_.png"),
          path = "Images",
          scale = 1.1)

  df_pmct_new <- df %>%
    filter(indicator == "PMTCT_ART",
           standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus",
           snu1 %in% snu_sel,
           fiscal_year == meta$curr_fy) %>%
    mutate(otherdisaggregate = str_remove(otherdisaggregate, "Life-long ART, ")) %>%
    group_by(fiscal_year, snu1, cop22_psnu, indicator,otherdisaggregate) %>%
    summarise(value = sum(cumulative, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_wider(names_from = otherdisaggregate,
                names_glue = "{tolower(otherdisaggregate)}",
                values_fill = 0) %>%
    mutate(total = new + already,
           share_new = new / total)




  v_p_new <- df_pmct_new %>%
    ggplot(aes(share_new, fct_reorder(snu1, total, sum))) +
    geom_jitter(aes(size = total),
                color = scooter,
                position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                alpha = .4) +
    scale_x_continuous(label = percent_format(), position = "top") +
    scale_size(guide = "none") +
    coord_cartesian(clip = "off")+
    labs(x = NULL, y = NULL,
         subtitle = glue("Each point represents council's PMTCT newly initiated"),
    ) +
    theme(legend.position = "none") +
    si_style()


  v_p_new_bar <- df_pmct_new %>%
    count(snu1, wt = total, name = "total") %>%
    ggplot(aes(total, fct_reorder(snu1, total, sum))) +
    geom_col(fill = scooter) +
    scale_x_continuous(label = comma_format(),
                       position = "top") +
    labs(x = NULL, y = NULL,
         subtitle = "PMTCT_ART (New + Already)") +
    theme(legend.position = "none") +
    si_style()


  v_p_new_bar + v_p_new +
    plot_annotation(title = "What share of HIV+ clients in ANC1 are new to Tx?" %>% toupper,
                    caption = meta$caption,
                    theme = si_style())

  si_save(glue("{meta$curr_pd}_TZA-usaid_pmtct_new.png"),
          path = "Images",
          scale = 1.1)
