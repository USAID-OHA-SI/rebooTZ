# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  prepare visuals for TZA FY22Q4 review
# REF ID:   165274ed 
# LICENSE:  MIT
# DATE:     2022-11-01
# UPDATED:  2022-11-02


# REQUEST -----------------------------------------------------------------

# We would like to ask for your assistance if that will be ok with you on the 
# following areas:
# TX_CURR growth
# IIT
# EID
# TB
# We would like to focus analysis at SNU level but PSNU level where we see an 
# issue.We will appreciate hearing from you by COB wednesday so that we finalize
# slides on Thursday.

# For Eid , for now if we could focus on 2 month would be great 
# for tb would be screening and tpt completion 


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
  library(ggrepel)

  
# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "165274ed"
  
  path_genie <- "Data/Genie-SiteByIMs-Tanzania-Daily-2022-11-01.zip"
  
  
  peds <- c("<01", "01-04", "05-09", "10-14", "<15")
  
  
# IMPORT ------------------------------------------------------------------
  
  df <- read_msd(path_genie)   
  

# LIMIT TO AYS ------------------------------------------------------------

  df <- df %>% 
    filter(str_detect(prime_partner_name, "(DELOITTE|Elizabeth)"))

# PERIODS -----------------------------------------------------------------

  get_metadata(path_genie, caption_note = "AYS Partners Only")
  
  full_pds <- (min(df$fiscal_year) - 1) %>% 
    paste0("-10-01") %>% 
    as_date() %>% 
    seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>% 
    convert_date_to_qtr()
  
  pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")
  
  pd_prior <- (convert_qtr_to_date(metadata$curr_pd) - months(3)) %>% 
    convert_date_to_qtr()
  
   
    

# TREATMENT AND GROWTH RATE -----------------------------------------------

  df_tx <- df %>% 
    filter(indicator == "TX_CURR", 
           (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
             (standardizeddisaggregate == "Total Numerator")) %>%
    mutate(type = ifelse(standardizeddisaggregate == "Total Numerator", "Total", "Peds")) %>% 
    group_by(fiscal_year, snu1, indicator, type) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    arrange(type, snu1, period)
  
  df_tx <- df_tx %>% 
    mutate(growth_rate_req = case_when(period == metadata$curr_pd ~ ((targets/results)^(1/(4-metadata$curr_qtr))) -1)) %>% 
    group_by(type, snu1) %>% 
    fill(growth_rate_req, .direction = "updown") %>% 
    mutate(growth_rate = (results/lag(results, order_by = period)) - 1,
           growth_rate = na_if(growth_rate, Inf)) %>% 
    ungroup() %>% 
    mutate(geo_gr_lab = case_when(is.infinite(growth_rate_req)  ~ glue("{toupper(snu1)}"), #metadata$curr_qtr == 4 | is.infinite(growth_rate_req) 
                                  growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget achieved"), 
                                  growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
                                  TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
           gr_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate, 1)),
           gr_label_position = 0,
           disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets))
  
  
  df_achv <- df_tx %>% 
    filter(period == metadata$curr_pd) %>% 
    count(type, results >= targets) %>% 
    filter(`results >= targets` == TRUE)
    
  df_tx %>%
    filter(type == "Total") %>% 
    ggplot(aes(period, results, fill = as.character(fiscal_year))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(aes(label = gr_lab, y = gr_label_position),
              family = "Source Sans Pro", color = "white", size = 9/.pt, 
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(geo_gr_lab, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(df_tx$period)[grep("Q(4)", unique(df_tx$period))]) +
    scale_fill_manual(values = c(scooter_light, scooter)) +
    labs(x = NULL, y = NULL,
         title = glue("ONLY {df_achv[df_achv$type == 'Total',]$n} of USAID's regions reached their {metadata$curr_fy_lab} treatment targets") %>% toupper(),
         subtitle = "Current on treatment by region and quarterly growth rate",
         # subtitle = "Current on treatment by region and growth rate needed in Q4 to reach target",
         caption = glue(#"Note: quarterly growth rate needed calculated as a compound annual growth rate
                         "{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          panel.spacing = unit(.5, "picas"),
          axis.text.x = element_text(size = 8))
  
  
  si_save(paste0(metadata$curr_pd,"_TZA-_tx-curr-growth_regional.png"),
          path = "Images")
  
  
  # df_tx %>%
  #   filter(type == "Peds") %>% 
  #   ggplot(aes(period, results, fill = as.character(fiscal_year))) +
  #   geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
  #   geom_col(na.rm = TRUE) +
  #   geom_errorbar(aes(ymin = targets, ymax = targets), linetype = "dashed", width = .95, na.rm = TRUE) +
  #   geom_text(aes(label = gr_lab, y = gr_label_position),
  #             family = "Source Sans Pro", color = "white", size = 9/.pt, 
  #             vjust = -.5, na.rm = TRUE) +
  #   facet_wrap(~ fct_reorder2(geo_gr_lab, period, targets), scales = "free_y") +
  #   scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  #   scale_x_discrete(breaks = unique(df_tx$period)[grep("Q(4)", unique(df_tx$period))]) +
  #   scale_fill_manual(values = c(scooter_light, scooter)) +
  #   labs(x = NULL, y = NULL,
  #        title = glue("ONLY {df_achv[df_achv$type == 'Peds',]$n} of USAID's regions reached their {metadata$curr_fy_lab} peds treatment targets") %>% toupper(),
  #        subtitle = "Current on pediatric treatment by region and quarterly growth rate",
  #        # subtitle = "Current on treatment by region and growth rate needed in Q4 to reach target",
  #        caption = glue(#"Note: quarterly growth rate needed calculated as a compound annual growth rate
  #          "{metadata$caption} | US Agency for International Development")) +
  #   si_style_ygrid() +
  #   theme(legend.position = "none",
  #         panel.spacing = unit(.5, "picas"),
  #         axis.text.x = element_text(size = 8))
  # 
  # si_save(paste0(metadata$curr_pd,"_TZA-peds_tx-curr-growth_regional.png"),
  #         path = "Images",
  #         scale = 1.5)
  
  
# GROWTH RATE -------------------------------------------------------------
  
  df_gr <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           str_detect(prime_partner_name, "(DELOITTE|Elizabeth)")) %>% 
    group_by(fiscal_year, snu1, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    arrange(snu1, period)
  
  df_gr <- df_gr %>% 
    filter(results > 0) %>% 
    group_by(snu1) %>% 
    mutate(tx_gr_rel_baseline = results/results[period == min(period)])
  
  
  df_gr %>% 
    ggplot(aes(period, tx_gr_rel_baseline, group = snu1)) +
    geom_hline(yintercept =  1) +
    geom_point(color = scooter) +
    geom_line(color = scooter) +
    facet_wrap(~fct_reorder2(snu1, period, targets)) +
    scale_x_discrete(breaks = pd_brks) +
    coord_cartesian(clip = "off") +
    expand_limits(y = c(.8, 1.2)) +
    labs(x = NULL, y = NULL,
         title = "MOROGORO REVERSED COURSE AFTER PATIENT DECLINE SINCE FY21Q4",
         subtitle = glue("<span style='color:{scooter}'>**Growth rate**</span> relative to {min(df_gr$period)} [TX_CURR current pd / TX_CURR {min(df_gr$period)}]"),
         caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(1, "picas"),
          plot.subtitle = element_markdown())
  
  si_save(paste0(metadata$curr_pd,"_TZA-_tx-curr-rel-growth.png"),
          path = "Images")
  
# ACHIEVEMENT -------------------------------------------------------------

  
  df_achv <- df %>% 
    filter(indicator == "TX_CURR", 
           (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
             (standardizeddisaggregate == "Total Numerator"),
           fiscal_year == metadata$curr_fy) %>%
    mutate(type = ifelse(standardizeddisaggregate == "Total Numerator", "Total", "Peds")) %>% 
    group_by(fiscal_year, snu1, psnu, indicator, type) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>%
    adorn_achievement()

  vct_colors <-  df_achv %>% 
    filter(!is.na(achievement)) %>% 
    distinct(achv_label, achv_color) %>% 
    deframe()
  
  df_underachv <- df_achv %>% 
    filter(!is.na(achievement)) %>%
    count(type, reached_target = achievement >= .9) %>% 
    group_by(type) %>% 
    mutate(share = n/sum(n)) %>% 
    ungroup() %>% 
    filter(reached_target == FALSE) %>% 
    select(-share) %>% 
    pivot_wider(names_from = type, values_from = n)
  
  
  df_achv %>% 
    filter(type == "Total",
           !is.na(achievement)) %>% 
    mutate(label_psnu = case_when(achievement < .9 ~ psnu)) %>% 
    ggplot(aes(targets, achievement, fill = achv_label, size = targets)) +
    geom_point(shape = 21, alpha = .6) +
    geom_text_repel(aes(label = label_psnu), size = 8/.pt, na.rm = TRUE,
                    color = matterhorn, family = "Source Sans Pro") +
    facet_wrap(~fct_reorder(snu1, targets, sum, na.rm = TRUE, .desc = TRUE)) +
    scale_size(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_manual(values = vct_colors) +
    scale_x_log10(label = comma) +
    scale_y_continuous(limits = c(0,1.1),
                       label = percent_format(1),
                       oob = oob_squish,
                       breaks = seq(0, 1, .25)) +
    coord_cartesian(clip = "off") +
    labs(x = glue("{metadata$curr_fy} Targets (log scale)"), y = "Target Achievement",
         fill = "Target Achievement", size = glue("{metadata$curr_fy_lab} Targets"),
         title = glue("{df_underachv$Total} of USAID councils failed to reach the 90% of their {metadata$curr_fy_lab} treatment targets") %>% toupper(),
         subtitle = "Councils under the achivement threshold are labeled",
         caption = glue("Note: Achievement capped at 110% 
                        {metadata$caption} | US Agency for International Development")) +
    si_style() +
    theme(panel.spacing = unit(1, "picas"),
          legend.position = "none") 
  
  
  si_save(paste0(metadata$curr_pd,"_TZA-_tx-curr-psnu-achv.png"),
          path = "Images")
 
  
# IIT ---------------------------------------------------------------------
  
  df_iit <- df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW")) %>%
    pluck_totals() %>%
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit <- df_iit %>%
    # group_by(psnu) %>% 
    # mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period)) %>% 
    # ungroup() %>% 
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_snu_lab <- df_iit %>% 
    filter(period == metadata$curr_pd) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    # mutate(snu1_lab = glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)}")) %>% 
    mutate(snu1_lab = ifelse(n == max(n), glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)} [TX_CURR {pd_prior}]"),
                             glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)}"))) %>%
    select(-n)
  
  snu_tx_order <- df_iit %>% 
    filter(period == metadata$curr_pd) %>% 
    count(snu1, wt = tx_curr, sort = TRUE) %>% 
    pull(snu1)
  
  
  vct_itt_cntry <- df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW"),) %>%
    pluck_totals() %>%
    group_by(fiscal_year, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    filter(period == metadata$curr_pd) %>% 
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pull() %>% 
    percent()
  
  df_iit %>%
    left_join(df_snu_lab, by = "snu1") %>% 
    mutate(snu1_lab = factor(snu1_lab, df_snu_lab$snu1_lab),
           fiscal_year = str_sub(period, end = 4)) %>% 
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
    scale_x_discrete(labels = pd_brks) +
    scale_y_continuous(limits = c(0,.25),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("USAID ENDED THE YEAR WITH IIT of {vct_itt_cntry} with most regions trending down") %>% toupper,
         subtitle = "Council IIT rates within each region",
         caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 25%
                        {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{metadata$curr_pd}_TZA_region_iit.png"))  
  
  
# EID COVERAGE ------------------------------------------------------------

  df_eid <- df %>% 
    clean_indicator() %>% 
    filter(indicator %in% c("PMTCT_EID_D", "PMTCT_EID_Less_Equal_Two_Months")) %>% 
    group_by(fiscal_year, snu1, psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    filter(pmtct_eid_d > 0) %>% 
    mutate(eid_coverage = pmtct_eid_less_equal_two_months / pmtct_eid_d)
  
  df_eid_snu <- df_eid %>% 
    group_by(period, snu1) %>% 
    summarise(across(c(pmtct_eid_less_equal_two_months, pmtct_eid_d), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(eid_coverage_snu = pmtct_eid_less_equal_two_months / pmtct_eid_d,
           snu_value = ifelse(eid_coverage_snu == max(eid_coverage_snu, na.rm = TRUE),
                              glue("{snu1} (PMTCT_EID_D: {comma(pmtct_eid_d)})"),
                              glue("{snu1} ({comma(pmtct_eid_d)})")))
  
  
  v_eid_snu <- df_eid %>% 
    bind_rows(df_eid_snu %>% select(-snu_value)) %>% 
    left_join(df_eid_snu %>% 
                filter(period == metadata$curr_pd) %>% 
                select(snu1, snu_value)) %>% 
    filter(period == metadata$curr_pd) %>% 
    ggplot(aes(eid_coverage, fct_reorder(snu_value, eid_coverage_snu, na.rm = TRUE))) +
    geom_point(aes(color = eid_coverage < .9),
               alpha = .4, na.rm = TRUE, size = 2,
               position = position_jitter(height = .2, seed = 42)) +
    geom_segment(
      aes(x = .9, xend = eid_coverage_snu,
          y = snu_value, yend = snu_value,
          color = eid_coverage_snu < .9),
      size = 0.8, na.rm = TRUE
    ) +
    geom_vline(xintercept = .9) +
    geom_point(aes(eid_coverage_snu, color = eid_coverage_snu < .9), 
               size = 10,  na.rm = TRUE) +
    geom_text(data = . %>% filter(eid_coverage_snu < .9), 
              aes(eid_coverage_snu, label = percent(eid_coverage_snu, 1)),
               family = "Source Sans Pro", #color = "white",
               size = 8/.pt, na.rm = TRUE) +
    geom_text(data = . %>% filter(eid_coverage_snu >= .9), 
              aes(eid_coverage_snu, label = percent(eid_coverage_snu, 1)),
              family = "Source Sans Pro", color = "white",
              size = 8/.pt, na.rm = TRUE) +
    scale_x_continuous(limits = c(0,1),
                       label = percent_format(1),
                       oob = oob_squish) +
    scale_color_manual(values = c(scooter, golden_sand)) +
    # scale_color_manual(values = c(TRUE = scooter_med, FALSE = golden_sand)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    si_style() +
    theme(legend.position = "none",
          axis.text.x = element_blank())
  
  
  df_eid_cntry <- df_eid %>% 
    group_by(period) %>% 
    summarise(across(c(pmtct_eid_less_equal_two_months, pmtct_eid_d), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    mutate(eid_coverage = pmtct_eid_less_equal_two_months / pmtct_eid_d) 
  
  
  v_eid_cntry <- df_eid_cntry %>% 
    ggplot(aes(period, eid_coverage, group = "x")) +
    geom_hline(yintercept = .9, color = matterhorn) +
    geom_line(color = golden_sand) +
    geom_point(size = 14, color = golden_sand, alpha = .8) +
    geom_text(aes(label = percent(eid_coverage, 1)),
              family = "Source Sans Pro", #color = "white",
              size = 10/.pt, na.rm = TRUE) +
    expand_limits(y = 1) +
    coord_cartesian(clip = "off") +
    scale_y_continuous(labels = c("", "", "90%", "")) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() 
  
  
  v_eid_cntry / v_eid_snu +
    plot_layout(heights = c(1, 3)) +
    plot_annotation(
      title = "USAID's EID coverage trending upward, but still below 90%" %>% toupper,
      subtitle = glue('Coverage trend across USAID and {metadata$curr_pd} coverage by region'),
      caption = glue("{metadata$caption} | US Agency for International Development"),
      theme = si_style()
    )
  
  
  si_save(glue("Images/{metadata$curr_pd}_TZA_region_eid.png"))  
  
  

# TB ----------------------------------------------------------------------

  
  df_tb <- df %>% 
    filter(indicator %in% c("TX_CURR","TB_PREV", "TX_TB")) %>% 
    pluck_totals() %>% 
    clean_indicator() %>%
    group_by(fiscal_year, snu1, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>%
    filter(str_detect(period, "Q(2|4)"),
           period == max(period)) %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(`% of ART patients screened for TB` = TX_TB_D / TX_CURR,
           `% TPT Completion` = TB_PREV / TB_PREV_D,
           snu1 = fct_reorder(snu1,TX_CURR, .desc = FALSE)) %>% 
    select(period, snu1, `% of ART patients screened for TB`, `% TPT Completion`) %>% 
    pivot_longer(-c(period, snu1),
                 names_to = "indicator") %>% 
    filter(value %ni% c(NA, NaN)) %>% 
    mutate(goal = case_when(indicator == "% TPT Completion" & snu1 == "Iringa" ~ .85,
                            indicator == "% of ART patients screened for TB" & snu1 == "Iringa" ~ .9))
  
  
  df_tb_cntry <- df %>% 
    filter(indicator %in% c("TX_CURR","TB_PREV", "TX_TB")) %>% 
    pluck_totals() %>% 
    clean_indicator() %>%
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>%
    filter(str_detect(period, "Q(2|4)"),
           period == max(period)) %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(`% of ART patients screened for TB` = TX_TB_D / TX_CURR,
           `% TPT Completion` = TB_PREV / TB_PREV_D) %>% 
    select(period, screened = `% of ART patients screened for TB`, tpt = `% TPT Completion`)
  
  df_tb %>% 
    ggplot(aes(value, snu1, fill = indicator)) +
    geom_col(alpha = .6) +
    geom_vline(xintercept = 0, color = "#D3D3D3") +
    geom_vline(aes(xintercept = goal), na.rm = TRUE, color = golden_sand_light) +
    geom_text(aes(label = percent(value, 1), color = indicator),
                  family = "Source Sans Pro", size = 11/.pt, nudge_x = -.03) +
    facet_wrap(~indicator) +
    scale_x_continuous(expand = c(.005, .005)) +
    scale_color_manual(values = c("white", matterhorn)) +
    scale_fill_manual(values = c(scooter, scooter_med)) +
    labs(x = NULL, y= NULL,
         title = glue("USAID had a more success time with TB screening ({percent(df_tb_cntry$screened, 1)}) than with TPT completion ({percent(df_tb_cntry$tpt, 1)})") %>% toupper,
         subtitle = "Goal of 90% screening rate and 85% TPT completion",
         caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          legend.position = "none")
  
  si_save(glue("Images/{metadata$curr_pd}_TZA_region_tb.png"))
  