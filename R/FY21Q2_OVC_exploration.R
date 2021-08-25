# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz, K. Srikanth | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-07-15
# UPDATED:  2022-08-24

# GENIE META DATA ---------------------------------------------------------

# PSNU By IM
# DATIM data as of: 08/14/2021 21:59:04 UTC
# Genie report updated: 08/24/2021 01:42:31 UTC
# 
# Current period(s): 2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4,  2021 Target,  2021 Q1,  2021 Q2,  2021 Q3 

# Operating Unit: Tanzania
# Daily/Frozen: Daily
# Indicator: OVC_SERV,OVC_HIVSTAT, OVC_HIVSTAT_POS, TX_CURR, TX_PLVS
# Fiscal Year: 2020, 2021, 2022


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
  library(ggrepel)
  library(waffle)
  library(readxl)
  

# GLOBAL VARIABLES --------------------------------------------------------

  genie_path <- "Data/Genie-PSNUByIMs-Tanzania-Daily-2021-08-24.zip"
  reviewfile_path <- "Data/OVC Targeting Review and Alignment - 08.12.2021.xlsx" 
  age_range <- c("<01", "01-04", "05-09", "10-14", "15-17", "15-19") #"18+"

  source <- source_info(genie_path) %>% str_replace("p", "i")
  source_nat <- source_info(si_path(), "NAT_SUBNAT")
  
  curr_pd <- source_info(genie_path, return = "period")
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  
# IMPORT ------------------------------------------------------------------
  
  #PSNUxIM Genie w/ FY22 targets  
  df_genie <- read_msd(genie_path)
  
  df_subnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_rds()  
  
  df_psnu_agency <- read_excel(reviewfile_path, range = "A2:C194") %>% 
    select(psnu = Council, clinical_ip_agency = `Agency\r\nby Regional Clinical IP`) %>% 
    mutate(psnu = ifelse(psnu == "Military", "_Military Tanzania", psnu))
  
# MUNGE -------------------------------------------------------------------
  
  df_tx <- df_genie %>% 
    filter(indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Age/Sex/Indication/HIVStatus"),
           ageasentered %in% age_range) %>% 
    mutate(age_grp = "all")
  
  df_tx <- df_tx %>%
    bind_rows(df_tx %>% filter(trendscoarse == "<15") %>% mutate(age_grp = "u15")) %>% 
    clean_indicator() %>% 
    group_by(fiscal_year, snu1, psnu, psnuuid, snuprioritization, indicator, age_grp) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>%
    rename(cumulative = results) %>% 
    pivot_longer(c(targets, cumulative), names_to = "targets_results") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    arrange(psnuuid, targets_results, age_grp, period) %>% 
    group_by(psnuuid, targets_results, age_grp) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2), .after = tx_curr) %>% 
    ungroup() %>% 
    group_by(fiscal_year) %>% 
    filter(period == max(period)) %>% 
    ungroup() %>% 
    select(-c(period)) %>% 
    filter_at(vars(starts_with("tx")), any_vars(.!=0))
  
  df_ovc <- df_genie %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_HIVSTAT", "OVC_HIVSTAT_POS"),
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/DREAMS", 
                                           "Age/Sex/Preventive", "Age/Sex/ProgramStatus", 
                                           "Age/Sex/ProgramStatusCaregiver"),
           ageasentered %in% c(NA, age_range))

  df_ovc <- df_ovc %>% 
    mutate(standardizeddisaggregate = str_remove(standardizeddisaggregate, "Age/Sex/"),
           indicator = case_when(!is.na(otherdisaggregate) ~ glue("{indicator}_{otherdisaggregate}"),
                                 str_detect(standardizeddisaggregate, "Total") ~ indicator,
                                 TRUE ~ glue("{indicator}_{standardizeddisaggregate}")))
  
  lst_ovc_psnus <- df_ovc %>% 
    filter(fiscal_year == 2021,
           indicator == "OVC_HIVSTAT",
           cumulative > 0) %>% 
    distinct(psnuuid) %>% 
    pull()
    
  df_ovc <- df_ovc %>% 
    filter(indicator != "OVC_SERV") %>% 
    mutate(age_grp = "all")
    
  df_ovc <- df_ovc %>%
    bind_rows(df_ovc %>% filter(ageasentered != "15-17") %>% mutate(age_grp = "u15")) %>% 
    group_by(fiscal_year, snu1, psnu, psnuuid, snuprioritization, indicator, age_grp) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_longer(c(targets, cumulative), names_to = "targets_results") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")
  
  df_ovc <- df_ovc %>% 
    rowwise() %>% 
    mutate(ovc_serv = sum(ovc_serv_active, ovc_serv_graduated, ovc_serv_preventive, ovc_serv_dreams, na.rm = TRUE),
           ovc_serv_comp = sum(ovc_serv_active, ovc_serv_graduated, na.rm = TRUE)) %>% 
    ungroup() %>% 
    relocate(ovc_serv_comp, .after = ovc_serv)
    
  df_subnat <- df_subnat %>% 
    filter(operatingunit == "Tanzania",
           indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate %in% c("Age/Sex", "Age/Sex/HIVStatus"),
           ageasentered %in% age_range) %>% 
    mutate(age_grp = "all")
  
  
  df_subnat <- df_subnat %>%
    bind_rows(df_subnat %>% filter(trendscoarse == "<15") %>% mutate(age_grp = "u15")) %>% 
    count(fiscal_year, snu1, psnu,  psnuuid, indicator, age_grp, wt = targets, name = "value") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")
  


# JOIN --------------------------------------------------------------------

  df_join <- df_subnat %>% 
    full_join(df_tx, by = c("fiscal_year", "snu1", "psnu", "psnuuid", "age_grp")) %>% 
    full_join(df_ovc, by = c("fiscal_year", "snu1", "psnu", "psnuuid", "snuprioritization", "targets_results", "age_grp")) %>% 
    left_join(df_psnu_agency)


  df_join <- df_join %>% 
    mutate(prevalence = plhiv/pop_est,
           prevalence_10k = round(prevalence * 1e4),
           vlc = tx_pvls_d/tx_curr,
           vls = tx_pvls/tx_pvls_d,
           vls_txcurr = tx_pvls/tx_curr,
           coverage_tx = ovc_hivstat_pos/tx_curr,
           coverage_plhiv = ovc_hivstat_pos/plhiv,
           snuprioritization = snuprioritization %>% 
             str_remove("^[:digit:]{1,} - ") %>% 
             str_remove(": Saturation"),
           snuprioritization = factor(snuprioritization, c("Scale-Up", "Sustained", "Attained")),
           psnu_ovc = psnuuid %in% lst_ovc_psnus)
  

# VIZ - PSNU CLASSIFICATIONS ----------------------------------------------


  df_class <- df_join %>%
    filter(!is.na(snuprioritization),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative") %>% 
    group_by(snuprioritization) %>% 
    summarise(across(c(tx_curr, plhiv), sum, na.rm = TRUE),
              n_psnu = n()) %>% 
    ungroup() 
  
  df_class <- df_class %>% 
    mutate(snuprioritization = ifelse(plhiv == max(plhiv),
                                      glue("{snuprioritization}\n # of councils = {n_psnu}"),
                                      glue("{snuprioritization}\n {n_psnu}"))) %>% 
    pivot_longer(c(tx_curr, plhiv), 
                 names_to = "indicator") %>% 
    mutate(indicator = toupper(indicator),
           indicator = recode(indicator,
                              "TX_CURR" = "TX_CURR (<20yo)",
                              "PLHIV" = "PLHIV (<20yo)"),
           indicator = fct_inorder(indicator) %>% fct_rev(),
           position = ifelse(indicator == "TX_CURR (<20yo)", 0, value),
           lab_color = ifelse(indicator == "TX_CURR (<20yo)", "white", "#505050"),
           alpha_fill = ifelse(indicator == "TX_CURR (<20yo", 1, .8))
  
  df_class %>% 
    ggplot(aes(fct_reorder(snuprioritization, value, max, .desc = TRUE), value,
               alpha = alpha_fill, fill = indicator)) +
    geom_blank(aes(y = value * 1.05)) +
    geom_col(position = "identity") +
    geom_col(data = filter(df_class, indicator == "TX_CURR (<20yo)")) +
    geom_text(aes(label = number(value, accuracy = 1, scale = 1e-3, suffix = "k"), 
                  y = position, color = lab_color), 
              vjust = -1,family = "Source Sans Pro", size = 12/.pt) +
    scale_y_continuous(label = comma) +
    scale_alpha_identity() +
    scale_color_identity() +
    scale_fill_manual(values = c("PLHIV (<20yo)" = trolley_grey_light, 
                                 "TX_CURR (<20yo)" = scooter)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "MOST A/CLHIV ARE IN COUNCILS CATEGORIZED AS SCALE-UP IN FY21",
         caption = glue("Source: {source_nat} + {source}")) +
    si_style_ygrid() +
    theme(axis.text.y = element_blank())
  
  si_save("Images/FY21Q2_PSNU_class.png")
    
  
  

# VIZ - NON OVC COUNCIL BREAKDOWN -----------------------------------------


  df_brkdwn <- df_join %>% 
    filter(!is.na(snuprioritization),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative") %>% 
    group_by(snuprioritization) %>% 
    summarise(n_psnu = n(),
              n_psnu_ovc = sum(psnu_ovc)) %>% 
    ungroup() %>% 
    mutate(n_psnu_non_ovc = n_psnu - n_psnu_ovc,
           tot_psnus = sum(n_psnu) - n_psnu,
           snuprioritization = glue("{snuprioritization} Councils ({n_psnu})"),
           snuprioritization = fct_reorder(snuprioritization, n_psnu, .desc = TRUE)) %>% 
    select(-n_psnu) %>% 
    pivot_longer(-snuprioritization, names_to = "type") %>% 
    mutate(fill_color = case_when(type == "n_psnu_non_ovc" ~ golden_sand,
                                  type == "n_psnu_ovc" ~ scooter,
                                  TRUE ~ trolley_grey_light),
           type = factor(type, c("n_psnu_non_ovc", "n_psnu_ovc", "tot_psnus")))
  
  tot_non_ovc <- df_brkdwn %>%
    filter(type == "n_psnu_non_ovc") %>% 
    count(wt = value) %>% 
    pull()
  
  tot_psnus <- df_join %>% 
    filter(!is.na(snuprioritization),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative") %>% 
    nrow()
  
  df_brkdwn %>% 
    ggplot(aes(fill = fill_color, values = value)) +
    geom_waffle(color = "white", size = 1.2) +
    scale_fill_identity() +
    facet_wrap(~snuprioritization) +
    coord_equal() +
    labs(title = glue("OF THE {tot_psnus} TOTAL COUNCILS, <span style = 'color:{golden_sand};'>{tot_non_ovc}</span> ARE <span style = 'color:{golden_sand};'>WITHOUT OVC PROGRAMMING</span>"),
         subtitle = "Most non-OVC councils are categorized as sustained",
         caption = glue("Source: {source}")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_markdown())
    
  si_save("Graphics/FY21Q2_PSNU_class_nonovc.svg")
  
  
  
  df_xtra <- df_join %>% 
    filter(!is.na(snuprioritization)) %>% 
    distinct(snuprioritization) %>% 
    pmap_dfr(~df_join %>% 
               filter(snuprioritization != ..1,
                      fiscal_year == 2021,
                      age_grp == "all",
                      targets_results == "cumulative") %>% 
               select(prevalence_10k, tx_curr, plhiv) %>% 
               mutate(snuprioritization = ..1))
  
  df_join_viz <- df_join %>%
    filter(!is.na(snuprioritization),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative") %>% 
    bind_rows(df_xtra) %>% 
    mutate(psnu_ovc = ifelse(psnu_ovc == "TRUE", "OVC Programming", "No OVC Programming"))
  
  
  df_join_viz %>% 
    ggplot(aes(plhiv, tx_curr, color = psnu_ovc, size = prevalence_10k)) +
    geom_point(data = filter(df_join_viz, is.na(psnu_ovc)), alpha = .1) +
    geom_point(data = filter(df_join_viz, !is.na(psnu_ovc)), alpha = .5) +
    facet_wrap(~snuprioritization) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    scale_color_manual(values = c(golden_sand, scooter), na.value = trolley_grey) +
    labs(x = "PLHIV (<20yo)",
         y = "TX_CURR (<20yo)",
         title = "OVC PROGRAMMING ALIGNED BUT NOT ENTERLY DETERMINED BY PRIORITIZATION, TREATMENT VOLUME, OR PLHIV",
         caption = glue("Source: {source_nat} + {source}"),
         color = NULL, size = "HIV Prevalence per 10,000 pop (<20yo)") +
    si_style() +
    theme(plot.title.position = "plot")
  
  si_save("Images/FY21Q2_OVC_plhiv_tx_curr_scatter.png")
  
  df_xtra2 <- df_join %>% 
    filter(!is.na(snuprioritization)) %>% 
    distinct(snuprioritization, clinical_ip_agency) %>% 
    pmap_dfr(~df_join %>% 
               filter(fiscal_year == 2021,
                      age_grp == "all",
                      targets_results == "cumulative") %>% 
               select(prevalence_10k, tx_curr, plhiv) %>% 
               mutate(snuprioritization = ..1,
                      clinical_ip_agency = ..2))
  
  
  df_join_viz2 <- df_join %>% 
    filter(!is.na(snuprioritization),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative") %>% 
    bind_rows(df_xtra2) %>% 
    mutate(psnu_ovc = ifelse(psnu_ovc == "TRUE", "OVC Programming", "No OVC Programming"))
  
  
  df_join_viz2 %>% 
    ggplot(aes(plhiv, tx_curr, color = psnu_ovc, size = prevalence_10k)) +
    geom_point(data = filter(df_join_viz2, is.na(psnu_ovc)), alpha = .1) +
    geom_point(data = filter(df_join_viz2, !is.na(psnu_ovc)), alpha = .5) +
    facet_grid(clinical_ip_agency~snuprioritization, switch = "y") +
    # scale_y_log10(labels = comma) +
    # scale_x_log10(labels = comma) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    scale_color_manual(values = c(golden_sand, scooter), na.value = trolley_grey) +
    labs(x = "PLHIV (<20yo)",
         y = "TX_CURR (<20yo)",
         title = "OVC PROGRAMMING ALIGNED BUT NOT ENTERLY DETERMINED BY PRIORITIZATION, TREATMENT VOLUME, OR PLHIV",
         color = NULL, size = "HIV Prevalence per 10,000 pop (<20yo)") +
    si_style() +
    theme(strip.text.y = element_text(hjust = .5),
          strip.placement = "outside",
          plot.title.position = "plot")
  
  si_save("Images/FY21Q2_OVC_plhiv_tx_curr_scatter_agency.png")
    
  

# TX_CURR RANKING ---------------------------------------------------------

  plot_tx_rank <- function(age, fy, targ_res, threshold = 500, export = TRUE){
  
    df_tx_rank <- df_join %>%
      filter(fiscal_year == fy,
             age_grp == age,
             targets_results == targ_res) %>% 
      mutate(psnu_ovc = ifelse(psnu_ovc == "TRUE", "OVC Programming", "No OVC Programming"),
             psnu_lab = case_when(tx_curr > threshold & psnu_ovc == "No OVC Programming" ~ psnu))
    
    n_psnu <- df_tx_rank %>% 
      filter(tx_curr > threshold,
             psnu_ovc == "No OVC Programming") %>% 
      count(psnu_ovc) %>% 
      pull()
    
    age <- df_tx_rank %>% 
      distinct( age_grp) %>% 
      mutate(age_grp = if_else(age_grp == "all", "<20y/o", "<15y/o")) %>% 
      pull()
    
    type <- df_tx_rank %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = str_to_title(targets_results),
             age_grp = if_else(age_grp == "all", "<20y/o", "<15y/o"),
             sub = glue("{fiscal_year} {targets_results} TX_CURR {age_grp}")) %>% 
      pull()
    
    
    v <- df_tx_rank %>% 
      ggplot(aes(tx_curr, fct_reorder(psnu, tx_curr), fill = psnu_ovc)) +
      geom_col(width =0.8) +
      geom_text_repel(aes(label = psnu_lab), na.rm = TRUE, 
                      seed = 42, force = 10, hjust = 1, nudge_x = 250,  
                      family = "Source Sans Pro", color = "#505050", size = 9/.pt, segment.color = "#909090") +
      scale_x_continuous(label = comma, position = "top") +
      scale_fill_manual(values = c("OVC Programming" = scooter, "No OVC Programming" = golden_sand)) +
      labs(title = glue("Of the largest councils where TX_CURR {age} is greater than {threshold} patients, there are {n_psnu} PSNUs without OVC programs") %>% 
             toupper() %>% str_wrap(95),
           subtitle = type,
           x = NULL, y = NULL, fill = NULL,
           caption = glue("Source: {source}")) +
      si_style_xgrid() +
      theme(axis.text.y = element_blank())
    
    if(export == TRUE){
      si_save(glue("Images/FY21Q2_OVC_tx_curr_rank_{fy}_{targ_res}_{age}.png"), v)
    } else {
      return(v)
    }
  }
  
  
  plot_tx_rank("all", 2021, 'cumulative')
  plot_tx_rank("all", 2021, 'targets')
  plot_tx_rank("all", 2022, 'targets')
  
  plot_tx_rank("u15", 2021, 'cumulative')
  plot_tx_rank("u15", 2021, 'targets')
  plot_tx_rank("u15", 2022, 'targets')
  
  
# VIZ ---------------------------------------------------------------------

  
  # nat_cov <- df_join %>% 
  #   filter(!is.na(tx_curr),
  #          fiscal_year == 2021,
  #          age_grp == "all",
  #          targets_results == "cumulative",
  #          snu1 != "_Military Tanzania") %>% 
  #   summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
  #   mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
  #   pull()
  
  #coverage
  # df_join %>% 
  #   filter(!is.na(tx_curr),
  #          fiscal_year == 2021,
  #          age_grp == "all",
  #          targets_results == "cumulative",
  #          snu1 != "_Military Tanzania") %>% 
  #   group_by(snu1) %>% 
  #   summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>%
  #   pivot_longer(c(tx_curr, ovc_hivstat_pos), 
  #                names_to = "indicator") %>%
  #   mutate(indicator = toupper(indicator),
  #          alpha_fill = ifelse(indicator == "TX_CURR", .8, 1), 
  #          indicator = ifelse(indicator == "TX_CURR", glue("{indicator} (<20yo)"),
  #                             glue("{indicator} (<=18yo)"))) %>%
  #   group_by(snu1) %>% 
  #   mutate(coverage = min(value)/max(value),
  #          coverage = case_when(value == max(value) ~ NA_real_,
  #                               coverage == 0 ~ NA_real_,
  #                               TRUE ~ coverage)) %>% 
  #   ungroup() %>% 
  #   ggplot(aes(value, fct_reorder(snu1, value, max), fill = indicator, alpha = alpha_fill)) +
  #   geom_col(position = "identity") +
  #   geom_text(aes(label = percent(coverage, 1)), na.rm = TRUE,
  #             hjust = 1.2,
  #             family = "Source Sans Pro", size = 8.5/.pt, color = "white") +
  #   scale_x_continuous(label = comma, position = "top",
  #                      expand = c(.005, .005)) +
  #   scale_fill_manual(values = c(scooter, trolley_grey_light)) +
  #   scale_alpha_identity() +
  #   labs(x = NULL, y = NULL, fill = NULL,
  #        title = glue("ACROSS TANZANIA, THERE IS A {percent(nat_cov,1)} COVERAGE RATE OF HIV POSITIVE OVC ON TREATMENT"),
  #        caption = glue("Source: {source}")) +
  #   si_style_xgrid()
  # 
  # si_save("Images/FY21Q2_OVC_coverage_tx_curr.png")
  
  
  n_psnus <- 20
  
  lrg_cov <- df_join %>% 
    filter(!is.na(tx_curr),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative",
           snu1 != "_Military Tanzania") %>% 
    slice_max(order_by = tx_curr, n = n_psnus) %>% 
    summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
    pull()
  
  df_join %>% 
    filter(!is.na(tx_curr),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative",
           snu1 != "_Military Tanzania") %>% 
    slice_max(order_by = tx_curr, n = n_psnus) %>% 
    select(psnu, tx_curr, ovc_hivstat_pos, coverage_tx) %>% 
    pivot_longer(c(tx_curr, ovc_hivstat_pos), 
                 names_to = "indicator") %>%
    mutate(indicator = toupper(indicator),
           alpha_fill = ifelse(indicator == "TX_CURR", .8, 1), 
           coverage_tx = ifelse(indicator == "TX_CURR", NA, coverage_tx),
           indicator = ifelse(indicator == "TX_CURR", glue("{indicator} (<20yo)"),
                              glue("{indicator} (<=18yo)"))) %>%
    ggplot(aes(value, fct_reorder(psnu, value, max), fill = indicator, alpha = alpha_fill)) +
    geom_col(position = "identity") +
    geom_text(aes(label = percent(coverage_tx, 1)), na.rm = TRUE,
              hjust = 1.2,
              family = "Source Sans Pro", size = 8.5/.pt, color = "white") +
    scale_x_continuous(label = comma, position = "top",
                       expand = c(.005, .005)) +
    scale_fill_manual(values = c(scooter, trolley_grey_light)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("ACROSS THE {n_psnus} LARGEST COUNCILS, THERE IS A {percent(lrg_cov,1)} COVERAGE RATE OF HIV+ OVC ON TX"),
         caption = glue("Source: {source}")) +
    si_style_xgrid() +
    theme(plot.title.position = "plot")
  
  si_save("Images/FY21Q2_OVC_coverage_psnu_tx_curr.png")
  
  df_join %>% 
    filter(!is.na(tx_curr),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative",
           snu1 != "_Military Tanzania") %>% 
    mutate(rank = dense_rank(-tx_curr),
           psnu_lab = case_when(rank <= 15 ~ psnu,
                                ovc_hivstat_pos > 1000 ~ psnu)) %>% 
    ggplot(aes(tx_curr, ovc_hivstat_pos)) +
    geom_blank(aes(ovc_hivstat_pos, tx_curr)) +
    geom_abline(slope = 1, intercept = 0, color = trolley_grey, linetype = "dashed") +
    geom_smooth(aes(weight = plhiv), method = "lm", se = FALSE, alpha = .5, color = denim_light) +
    geom_point(aes(size = plhiv, color = snuprioritization), alpha = .4) +
    geom_text_repel(aes(label = psnu_lab), na.rm = TRUE,
                    family = "Source Sans Pro", color = trolley_grey, size = 8/.pt) +
    scale_x_continuous(label = comma) +
    scale_y_continuous(label = comma) +
    scale_size(label = comma) +
    # scale_color_si("scooter", na.value = trolley_grey) +
    scale_color_manual(values = c("Scale-Up" = scooter,
                                  "Sustained" = moody_blue,
                                  "Attained" =  genoa)) +
    labs(x = "TX_CURR (<20yo)", y = "OVC_HIVSTAT_POS (<=18yo)",
         size = "PLHIV", color = NULL,
         title = "COUNCILS WITH LARGER TREATMENTS VOLUMES TEND TO HAVE LESS OVC COVERAGE",
         caption = glue("Source: {source_nat} + {source}")) +
    si_style() 
  
  
  si_save("Images/FY21Q2_OVC_coverage_tx_curr_scatter.png",
          width = 8)
    
  
  
  n_psnus <- 20
  
  lrg_cov <- df_join %>% 
    filter(!is.na(tx_curr),
           snu1 != "_Military Tanzania") %>% 
    slice_max(order_by = plhiv, n = n_psnus) %>% 
    summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
    pull()
  
  df_join %>%
    filter(!is.na(tx_curr),
           !is.na(ovc_hivstat_pos),
           snu1 != "_Military Tanzania") %>%
    slice_min(order_by = vls_txcurr, n = n_psnus) %>%
    select(psnu, plhiv, vls_txcurr, tx_curr, tx_pvls, coverage_tx) %>%
    pivot_longer(c(tx_curr, tx_pvls),
                 names_to = "indicator") %>%
    mutate(indicator = toupper(indicator),
           alpha_fill = ifelse(indicator == "TX_CURR", .8, 1),
           coverage_tx = ifelse(indicator == "TX_CURR", NA, coverage_tx),
           vls_txcurr = ifelse(indicator == "TX_CURR", NA, vls_txcurr),
           indicator = glue("{indicator} (<20yo)"),
           cov_pt = 20) %>%
    ggplot(aes(value, fct_reorder(psnu, value, max), fill = indicator, alpha = alpha_fill)) +
    geom_col(position = "identity") +
    geom_text(aes(label = percent(vls_txcurr, 1)), na.rm = TRUE,
              hjust = 1.2,
              family = "Source Sans Pro", size = 7/.pt, color = "white") +
    geom_point(aes(cov_pt), size = 5, color = burnt_sienna_light, show.legend = FALSE) +
    geom_text(aes(cov_pt, label = percent(coverage_tx, 1)), na.rm = TRUE,
              family = "Source Sans Pro", size = 6/.pt, color = "#202020") +
    scale_x_continuous(label = comma, position = "top",
                       expand = c(.005, .005)) +
    scale_fill_manual(values = c(trolley_grey_light, scooter)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("ACROSS THE {n_psnus} LARGEST COUNCILS, THERE IS A {percent(lrg_cov,1)} COVERAGE RATE OF HIV POSITIVE OVC ON TX"),
         caption = "Source: FY21Q2c MSD
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style_xgrid()
  
  df_join %>% 
    filter(!is.na(tx_curr),
           snu1 != "_Military Tanzania") %>% 
    slice_max(order_by = plhiv, n = n_psnus) %>% 
    select(psnu, plhiv, ovc_hivstat_pos, coverage_tx) %>% 
    pivot_longer(c(plhiv, ovc_hivstat_pos), 
                 names_to = "indicator") %>%
    mutate(indicator = toupper(indicator),
           alpha_fill = ifelse(indicator == "PLHIV", .8, 1), 
           coverage_tx = ifelse(indicator == "PLHIV", NA, coverage_tx),
           indicator = ifelse(indicator == "PLHIV", glue("{indicator} (<20yo)"),
                              glue("{indicator} (<=18yo)")),
           cov_pt = 50) %>%
    ggplot(aes(value, fct_reorder(psnu, value, max), fill = indicator, alpha = alpha_fill)) +
    geom_col(position = "identity") +
    # geom_text(aes(label = percent(coverage_tx, 1)), na.rm = TRUE,
    #           hjust = 1.2,
    #           family = "Source Sans Pro", size = 8.5/.pt, color = "white") +
    geom_point(aes(cov_pt), size = 5, color = burnt_sienna_light, show.legend = FALSE) +
    geom_text(aes(cov_pt, label = percent(coverage_tx, 1)), na.rm = TRUE,
              family = "Source Sans Pro", size = 6/.pt, color = "#202020") +
    scale_x_continuous(label = comma, position = "top",
                       expand = c(.005, .005)) +
    scale_fill_manual(values = c(scooter, trolley_grey_light)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("ACROSS THE {n_psnus} LARGEST COUNCILS, THERE IS A {percent(lrg_cov,1)} COVERAGE RATE OF HIV+ OVC ON TX"),
         caption = "Source: FY21Q2c MSD
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style_xgrid()
  
  si_save("Images/FY21Q2_OVC_coverage_plhiv.png")
  
  df_join %>% 
    filter(!is.na(tx_curr),
           snu1 != "_Military Tanzania") %>% 
    mutate(rank = dense_rank(-tx_curr),
           psnu_lab = case_when(rank <= 15 ~ psnu,
                                coverage_tx > 1.2 ~ psnu,
                                prevalence_10k > 150 ~ psnu)) %>%
    ggplot(aes(prevalence_10k, coverage_tx)) +
    geom_hline(yintercept = 1, color = trolley_grey) +
    geom_smooth(aes(weight = plhiv), method = "lm", se = FALSE, alpha = .5,
                color = denim_light) +
    geom_point(aes(size = plhiv, color = snuprioritization), alpha = .4) +
    geom_text_repel(aes(label = psnu_lab), na.rm = TRUE,
                    family = "Source Sans Pro", color = trolley_grey, size = 8/.pt,) +
    expand_limits(y = 0) +
    scale_x_continuous(label = comma) +
    scale_y_continuous(label = percent) +
    scale_size(label = comma) +
    scale_color_si("scooter", na.value = trolley_grey) +
    labs(x = "HIV per 10,000 pop (<20yo)", y = "OVC_HIVSTAT_POS (<=18yo) share of TX_CURR (<20yo)",
         size = "PLHIV", color = NULL,
         title = "COUNCILS WITH LARGER PREVALENCE TEND TO HAVE LOWER OVC TX COVERAGE",
         caption = "Source: FY21Q2c MSD + NAT_SUBAT
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style()
  
  si_save("Images/FY21Q2_OVC_coverage_plhiv_scatter.png",
          width = 8)
  
  
  n_psnus <- 20
  
  low_cov <- df_join %>% 
    filter(!is.na(tx_curr),
           !is.na(ovc_hivstat_pos),
           snu1 != "_Military Tanzania") %>% 
    slice_min(order_by = vls_txcurr, n = n_psnus) %>% 
    summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
    pull()
  
  df_join %>% 
    filter(!is.na(tx_curr),
           !is.na(ovc_hivstat_pos),
           snu1 != "_Military Tanzania") %>% 
    slice_min(order_by = vls_txcurr, n = n_psnus) %>% 
    select(psnu, plhiv, vls_txcurr, tx_curr, tx_pvls, coverage_tx) %>% 
    pivot_longer(c(tx_curr, tx_pvls), 
                 names_to = "indicator") %>%
    mutate(indicator = toupper(indicator),
           alpha_fill = ifelse(indicator == "TX_CURR", .8, 1), 
           coverage_tx = ifelse(indicator == "TX_CURR", NA, coverage_tx),
           vls_txcurr = ifelse(indicator == "TX_CURR", NA, vls_txcurr),
           indicator = glue("{indicator} (<20yo)"), 
           cov_pt = 20) %>%
    ggplot(aes(value, fct_reorder(psnu, value, max), fill = indicator, alpha = alpha_fill)) +
    geom_col(position = "identity") +
    geom_text(aes(label = percent(vls_txcurr, 1)), na.rm = TRUE,
              hjust = 1.2,
              family = "Source Sans Pro", size = 7/.pt, color = "white") +
    geom_point(aes(cov_pt), size = 5, color = burnt_sienna_light, show.legend = FALSE) +
    geom_text(aes(cov_pt, label = percent(coverage_tx, 1)), na.rm = TRUE,
              family = "Source Sans Pro", size = 6/.pt, color = "#202020") +
    scale_x_continuous(label = comma, position = "top",
                       expand = c(.005, .005)) +
    scale_fill_manual(values = c(trolley_grey_light, scooter)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("ACROSS THE {n_psnus} COUNCILS WITH THE LOWEST VLS, THERE IS A {percent(low_cov,1)} COVERAGE RATE OF HIV+ OVC ON TX"),
         caption = "Note: VLC = TX_PVLS/TX_CURR; Source: FY21Q2c MSD
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style_xgrid()
  
  si_save("Images/FY21Q2_OVC_coverage_vls.png")
  
  df_join %>% 
    filter(!is.na(tx_curr),
           snu1 != "_Military Tanzania") %>% 
    mutate(rank = dense_rank(-tx_curr),
           psnu_lab = case_when(rank <= 15 ~ psnu,
                                coverage_tx > 1.2 ~ psnu)) %>%
    ggplot(aes(vls_txcurr, coverage_tx)) +
    geom_hline(yintercept = 1, color = trolley_grey) +
    geom_vline(xintercept = .95^3, color = trolley_grey, linetype = "dashed") +
    geom_smooth(aes(weight = plhiv), method = "lm", se = FALSE, alpha = .5,
                color = denim_light) +
    geom_point(aes(size = plhiv, color = snuprioritization), alpha = .4) +
    geom_text_repel(aes(label = psnu_lab), na.rm = TRUE,
                    family = "Source Sans Pro", color = trolley_grey, size = 8/.pt,) +
    expand_limits(y = 0, x = 0) +
    scale_x_continuous(label = percent) +
    scale_y_continuous(label = percent) +
    scale_size(label = comma) +
    scale_color_si("scooter", na.value = trolley_grey) +
    labs(x = "VL Suppression Rate of those on Treatment (<20yo)", y = "OVC_HIVSTAT_POS (<=18yo) share of TX_CURR (<20yo)",
         size = "PLHIV", color = NULL,
         title = "NO STRONG RELATIONSHIP BETWEEN VLS AND LOWER OVC TX COVERAGE",
         caption = "Note: VLC = TX_PVLS/TX_CURR;  Source: FY21Q2c MSD + NAT_SUBAT
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style()
  
  si_save("Images/FY21Q2_OVC_coverage_vls_scatter.png",
          width = 8)
  