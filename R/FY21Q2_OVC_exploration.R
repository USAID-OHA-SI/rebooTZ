# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz, K. Srikanth | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-07-15
# UPDATED:  2022-08-31

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
  library(tameDP)
  

# GLOBAL VARIABLES --------------------------------------------------------

  genie_path <- "Data/Genie-PSNUByIMs-Tanzania-Daily-2021-08-24.zip"
  reviewfile_path <- "Data/OVC Targeting Review and Alignment - 08.12.2021.xlsx" 
  age_range <- c("<01", "01-04", "05-09", "10-14", "15-17", "15-19")

  source <- source_info(genie_path) %>% str_replace("p", "i")
  source_nat <- source_info(si_path(), "NAT_SUBNAT")
  source_dp <- "COP21 Data Pack [PEPFAR TZ DataPack 052121 final.xlsx]"
  
  curr_pd <- source_info(genie_path, return = "period")
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  
  max_pd_tx <- "FY21Q2"
  remove_pd <- "FY21Q3"
  
# IMPORT ------------------------------------------------------------------
  
  #PSNUxIM Genie w/ FY22 targets  
  df_genie <- read_msd(genie_path)
  
  df_subnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_rds()  
  
  df_psnu_agency <- read_excel(reviewfile_path, range = "A2:C194") %>% 
    select(psnu = Council, clinical_ip_agency = `Agency\r\nby Regional Clinical IP`) %>% 
    mutate(psnu = ifelse(psnu == "Military", "_Military Tanzania", psnu))
  
  df_plhiv22 <- import_plhiv("../badboys/Data/PEPFAR TZ DataPack 052121 final.xlsx")
  
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
    filter(period != remove_pd) %>% 
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
  
  # lst_ovc_psnus <- df_ovc %>% 
  #   filter(fiscal_year == 2021,
  #          indicator == "OVC_HIVSTAT",
  #          cumulative > 0) %>% 
  #   distinct(psnuuid) %>% 
  #   pull()
  
  df_ovc_psnus <- df_ovc %>%
    filter(fiscal_year %in% c(2021:2022),
           indicator == "OVC_SERV",
           targets > 0) %>%
    mutate(n = 1) %>% 
    group_by(fiscal_year, psnu) %>% 
    summarise(n = max(n, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = fiscal_year,
                names_glue = "fy{fiscal_year}",
                values_from = n) %>% 
    mutate(psnu_ovc_status_22 = ifelse(is.na(fy2021), "OVC Programming (New FY22)", "OVC Programming")) %>% 
    select(-starts_with("fy"))
  
    
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
    relocate(ovc_serv_comp, .after = ovc_serv) %>% 
    mutate(ovc_serv = ifelse(ovc_serv == 0, NA, ovc_serv))
    
  df_plhiv22 <- df_plhiv22 %>% 
    rename(operatingunit = countryname,
           ageasentered = age) %>% 
    mutate(standardizeddisaggregate = "Age/Sex")
  
  df_subnat <- df_subnat %>% 
    bind_rows(df_plhiv22) %>% 
    filter(operatingunit == "Tanzania",
           indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate %in% c("Age/Sex", "Age/Sex/HIVStatus"),
           ageasentered %in% age_range) %>% 
    mutate(age_grp = "all")
  
  
  df_subnat <- df_subnat %>%
    bind_rows(df_subnat %>% filter(ageasentered != "15-19") %>% mutate(age_grp = "u15")) %>% 
    count(fiscal_year, snu1, psnu,  psnuuid, indicator, age_grp, wt = targets, name = "value") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")
  


# JOIN --------------------------------------------------------------------

  df_join <- df_subnat %>% 
    full_join(df_tx, by = c("fiscal_year", "snu1", "psnu", "psnuuid", "age_grp")) %>% 
    full_join(df_ovc, by = c("fiscal_year", "snu1", "psnu", "psnuuid", "snuprioritization", "targets_results", "age_grp")) %>% 
    left_join(df_psnu_agency, by = c("psnu"))


  df_join <- df_join %>% 
    mutate(prevalence = plhiv/pop_est,
           prevalence_10k = round(prevalence * 1e4),
           vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_txcurr = tx_pvls/tx_curr_lag2,
           coverage_tx = ovc_hivstat_pos/tx_curr,
           coverage_plhiv = ovc_hivstat_pos/plhiv,
           snuprioritization = snuprioritization %>% 
             str_remove("^[:digit:]{1,} - ") %>% 
             str_remove(": Saturation"),
           snuprioritization = factor(snuprioritization, c("Scale-Up", "Sustained", "Attained")),
           # psnu_ovc = psnuuid %in% lst_ovc_psnus)
           psnu_ovc = !is.na(ovc_serv)) %>% 
    left_join(df_ovc_psnus, by = c("psnu")) %>% 
    mutate(psnu_ovc_status_22 = ifelse(is.na(psnu_ovc_status_22), "No OVC Programming", psnu_ovc_status_22))
  
  write_csv(df_join, glue("Dataout/{max_pd_tx}_TZA_OVC_TDY_data.csv", na = ""))


# READ IN FROM HERE -------------------------------------------------------

  df_join <- read_csv(glue("Dataout/{max_pd_tx}_TZA_OVC_TDY_data.csv"))
  
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
  
  

# VIZ - PLHIV v TX_CURR ---------------------------------------------------

  plot_plhiv_tx <- function(age, fy, targ_res, export = TRUE){
    
    df_join_viz <- df_join %>%
      filter(!is.na(snuprioritization),
             fiscal_year == {fy},
             age_grp == {age},
             targets_results == {targ_res})
    
    df_xtra <- df_join_viz %>% 
      filter(!is.na(snuprioritization)) %>% 
      distinct(snuprioritization) %>% 
      pmap_dfr(~df_join_viz %>% 
                 filter(snuprioritization != ..1) %>% 
                 select(prevalence_10k, tx_curr, plhiv) %>% 
                 mutate(snuprioritization = ..1))
    
    df_join_viz <- df_join_viz %>% 
      bind_rows(df_xtra) %>% 
      mutate(psnu_ovc = ifelse(psnu_ovc == "TRUE", "OVC Programming", "No OVC Programming"))
    
    type <- df_join_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = ifelse(targets_results == "targets", "Targets", "Q2 Results"),
             age_grp = if_else(age_grp == "all", "<20yo", "<15yo"),
             sub = glue("{fiscal_year} {targets_results} {age_grp}")) %>% 
      pull()
    
    v <- df_join_viz %>% 
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
           subtitle = type,
           caption = glue("A council is defined as having 'OVC programming' if it has OVC_SERV {ifelse(targ_res == 'targets', 'targets', 'results')} in FY{str_sub(fy, 3)}
                          Source: {source_nat} + {source}"),
           color = NULL, size = "HIV Prevalence per 10,000 pop (<20yo)") +
      si_style() +
      theme(plot.title.position = "plot")
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_plhiv_txcurr_scatter_{fy}_{targ_res}_{str_replace(age,'<','u')}.png")
      print(out_file)
      si_save(out_file, path = "Images")
    } else {
      return(v)
    }

  }
    
 plot_plhiv_tx("all", 2021, "cumulative")
 plot_plhiv_tx("all", 2021, "targets")
 plot_plhiv_tx("all", 2022, "targets")
  
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
    
  

# VIZ - TX/PLHIV DISTRO ---------------------------------------------------

  df_hist_viz <- df_join %>% 
    filter(fiscal_year >= 2021, 
           !is.na(targets_results)) %>% 
    select(fiscal_year, age_grp, targets_results, plhiv, tx_curr) %>% 
    pivot_longer(c(plhiv, tx_curr), names_to = "indicator") %>% 
    mutate(indicator = toupper(indicator),
           age_grp = ifelse(age_grp =="all", "<20", "<15"),
           targets_results = ifelse(targets_results == "targets", "Targets", "Q2 Results"),
           fiscal_year = glue("FY{str_sub(fiscal_year, 3)}")) %>% 
    unite(yr_type, c(fiscal_year, targets_results), sep = " ") %>% 
    filter(!is.na(value),
           !(indicator == "PLHIV" & yr_type == "FY21 Q2 Results"))
  
  df_median <- df_hist_viz %>% 
    group_by(indicator, yr_type, age_grp) %>% 
    summarise(median =  median(value, na.rm = TRUE), .groups = "drop") %>% 
    filter(!is.na(median))
  
  
  df_hist_viz %>% 
    ggplot(aes(value, fct_rev(age_grp))) +
    geom_boxplot(fill = NA, outlier.shape = NA, color = trolley_grey) +
    geom_point(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
               alpha = .1, size = 3, color = genoa_light) +
    geom_label(data = df_median, 
               aes(median, fct_rev(age_grp), label = comma(median, 1)),
               family = "Source Sans Pro", color = "#505050", size = 10/.pt,
               vjust = 2.5, label.size = 0) +
    facet_grid(indicator~yr_type, switch = "y") +
    scale_x_continuous(label = comma) +
    labs(x = NULL, y = NULL,
         title = "COUNCIL DISTRIBUTION TO DETERMINE THRESHOLDS",
         subtitle = "Median council values for PLHIV and TX_CURR results/targets displayed",
         caption = glue("Source: {source_nat} + {source_dp} + {source}")) +
    si_style_xgrid() +
    theme(strip.placement = "outside",
          strip.text.y = element_text(hjust = .5))
  
  si_save("Images/FY21Q2_OVC_tx_plhiv_council_distro.png")
  
  
# TX_CURR RANKING ---------------------------------------------------------

  plot_tx_rank <- function(age, fy, targ_res, threshold = 500, export = TRUE){
  
    df_tx_rank <- df_join %>%
      filter(fiscal_year == fy,
             age_grp == age,
             targets_results == targ_res) %>% 
      mutate(psnu_ovc = psnu_ovc_status_22,
             # psnu_ovc = ifelse(psnu_ovc == "TRUE", "OVC Programming", "No OVC Programming"),
             psnu_lab = case_when(tx_curr > threshold & psnu_ovc == "No OVC Programming" ~ psnu))
    
    n_psnu <- df_tx_rank %>% 
      filter(tx_curr > threshold,
             psnu_ovc == "No OVC Programming") %>% 
      count(psnu_ovc) %>% 
      pull()
    
    if(length(n_psnu) == 0)
      n_psnu <- "no"
    
    age <- df_tx_rank %>% 
      distinct( age_grp) %>% 
      mutate(age_grp = if_else(age_grp == "all", "<20yo", "<15yo")) %>% 
      pull()
    
    type <- df_tx_rank %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = ifelse(targets_results == "targets", "Targets", "Q2 Results"),
             age_grp = if_else(age_grp == "all", "<20yo", "<15yo"),
             sub = glue("{fiscal_year} {targets_results} TX_CURR {age_grp}")) %>% 
      pull()
    
    
    v <- df_tx_rank %>% 
      ggplot(aes(tx_curr, fct_reorder(psnu, tx_curr), fill = psnu_ovc)) +
      geom_col(width =0.8) +
      geom_text_repel(aes(label = psnu_lab), na.rm = TRUE, max.overlaps = 20,
                      seed = 42, force = 10, hjust = 1, nudge_x = 250,  
                      family = "Source Sans Pro", color = "#505050", size = 9/.pt, segment.color = "#909090") +
      scale_x_continuous(label = comma, position = "top") +
      scale_fill_manual(values = c("OVC Programming" = scooter, "OVC Programming (New FY22)" = "#084658", "No OVC Programming" = golden_sand)) +
      labs(title = glue("Of the largest councils in FY{str_sub(fy, 3)} where TX_CURR {age} {ifelse(targ_res == 'targets', 'targets', 'results')} are greater than {threshold} patients, there are {n_psnu} councils without FY22 OVC programs") %>% 
             toupper() %>% str_wrap(95),
           subtitle = type,
           x = NULL, y = NULL, fill = NULL,
           caption = glue("A council is defined as having 'OVC programming' if it has FY22 OVC_SERV targets
                          Source: {source}")) +
      si_style_xgrid() +
      theme(axis.text.y = element_blank())
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_tx_curr_rank_{fy}_{targ_res}_{str_replace(age,'<','u')}.png")
      print(out_file)
      si_save(out_file, path = "Images")
    } else {
      return(v)
    }
  }
  
  
  # plot_tx_rank("all", 2021, 'cumulative')
  plot_tx_rank("all", 2021, 'targets')
  plot_tx_rank("all", 2022, 'targets')
  
  # plot_tx_rank("u15", 2021, 'cumulative')
  plot_tx_rank("u15", 2021, 'targets')
  plot_tx_rank("u15", 2022, 'targets')
  

  #list of psnus
  df_join %>%
    filter(fiscal_year == 2021,
           age_grp == "all",
           targets_results == "targets",
           tx_curr > 500, #threshold
           psnu_ovc_status_22 == "No OVC Programming") %>% 
    arrange(desc(tx_curr)) %>% 
    pull(psnu) %>% 
    paste(collapse = ", ") %>% 
    paste("Largest councils without FY22 OVC_SERV targets: ", .)
  
# VIZ - COVERAGE ----------------------------------------------------------

  
  
  plot_cov_rate <- function(age, fy, targ_res, n_psnus = NULL, export = TRUE){
    df_cov_viz <- df_join %>% 
      select(snu1, psnu, snuprioritization, psnu_ovc, fiscal_year, age_grp, targets_results, ovc_hivstat_pos, tx_curr) %>% 
      pivot_longer(c(ovc_hivstat_pos, tx_curr), names_to = "indicator", values_drop_na = TRUE) %>% 
      filter(fiscal_year == {fy},
             (indicator == "tx_curr" & age_grp == {age}) | (indicator == "ovc_hivstat_pos" & age_grp == "all"),
             targets_results == {targ_res},
             snu1 != "_Military Tanzania") %>% 
      mutate(age_grp = {age}) %>% 
      pivot_wider(names_from = "indicator") %>% 
      mutate(coverage_tx = ovc_hivstat_pos/tx_curr)
    
    if(!is.null(n_psnus))
      df_cov_viz <- slice_max(df_cov_viz, order_by = tx_curr, n = n_psnus)
    
    cov_rate <- df_cov_viz %>% 
      filter(psnu_ovc == TRUE) %>% 
      summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
      mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
      pull()
    
    
    cov_title_seg <- ifelse(!is.null(n_psnus),
                            glue("THE {n_psnus} LARGEST COUNCILS"),
                            glue("COUNCILS WITH OVC PROGRAMMING"))
    cov_title <- glue("ACROSS {cov_title_seg}, THERE IS A {percent(cov_rate,1)} COVERAGE RATE OF HIV+ OVC ON TX")
    
    type <- df_cov_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = str_to_title(targets_results),
             sub = glue("{fiscal_year} {targets_results}")) %>% 
      pull()
    
    df_cov_viz <- df_cov_viz %>% 
      select(psnu, tx_curr, age_grp, ovc_hivstat_pos, coverage_tx) %>%
      mutate(tx_order = tx_curr) %>% 
      pivot_longer(c(tx_curr, ovc_hivstat_pos), 
                   names_to = "indicator") %>%
      mutate(indicator = toupper(indicator),
             alpha_fill = ifelse(indicator == "TX_CURR", .8, 1), 
             coverage_tx = ifelse(indicator == "TX_CURR", NA, coverage_tx),
             age_grp = case_when(indicator == "OVC_HIVSTAT_POS" ~ "<18",
                                 age_grp == "u15" ~ "<15",
                                 TRUE ~ "<20"),
             indicator = glue("{indicator} ({age_grp})"))
    
    v <- df_cov_viz %>%
      ggplot(aes(value, fct_reorder(psnu, tx_order, max), fill = indicator, alpha = alpha_fill), na.rm = TRUE) +
      geom_col(position = "identity", width = ifelse(!is.null(n_psnu), 0.9, 0.8), na.rm = TRUE) +
      geom_errorbar(#data = filter(df_cov_viz, indicator == "TX_CURR"),
                    aes(xmin = value, xmax = value), color = si_palettes$trolley_greys[1]) +
      scale_fill_manual(values = c(scooter, trolley_grey_light)) +
      scale_alpha_identity() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = cov_title,
           subtitle = type,
           caption = glue("Source: {source}")) +
      si_style_xgrid() 
    
    if(!is.null(n_psnus)){
      v <- v +
        geom_text(aes(label = percent(coverage_tx, 1)), na.rm = TRUE,
                  hjust = 1.2, family = "Source Sans Pro", size = 8.5/.pt, color = "white") +
        scale_x_continuous(label = comma, position = "top", expand = c(.005, .005)) +
        theme(plot.title.position = "plot")
    } else {
      v <- v +
        scale_x_continuous(label = comma, position = "top") +
        theme(axis.text.y = element_blank(), 
              plot.title.position = "plot")
    }
    
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_tx_coverage_{fy}_{targ_res}_{str_replace(age,'<','u')}_{ifelse(is.null(n_psnus), 'natl', 'top')}.png")
      print(out_file)
      si_save(out_file, path = "Images")
    } else {
      return(v)
    }  
  }
  
  
  plot_cov_rate('all', '2021', 'cumulative', 20)
  plot_cov_rate('all', '2021', 'cumulative')
  
  plot_cov_rate('u15', '2021', 'cumulative', 20)
  plot_cov_rate('u15', '2021', 'cumulative')
  
  

# VIZ - TX VS HIVSTAT_POS -------------------------------------------------

  
  plot_tx_ovc <- function(age, fy, targ_res, export = TRUE){
    
    df_scat_viz <- df_join %>% 
      select(snu1, psnu, snuprioritization, fiscal_year, age_grp, targets_results, plhiv, ovc_hivstat_pos, tx_curr) %>% 
      pivot_longer(c(ovc_hivstat_pos, tx_curr, plhiv), names_to = "indicator", values_drop_na = TRUE) %>% 
      filter(fiscal_year == {fy},
             (indicator %in% c("tx_curr", "plhiv") & age_grp == {age}) | (indicator == "ovc_hivstat_pos" & age_grp == "all"),
             targets_results == {targ_res},
             snu1 != "_Military Tanzania") %>% 
      mutate(age_grp = {age}) %>% 
      pivot_wider(names_from = "indicator")
    
    
    df_scat_viz <- df_scat_viz %>% 
      mutate(rank = dense_rank(-tx_curr),
             psnu_lab = case_when(rank <= 15 ~ psnu,
                                  #ovc_hivstat_pos > 1000 ~ psnu,
                                  ovc_hivstat_pos > tx_curr ~ psnu)) 
    
    type <- df_scat_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = str_to_title(targets_results),
             sub = glue("{fiscal_year} {targets_results}")) %>% 
      pull()
    
    plot_title <- ifelse(age == 'all',
                         "COUNCILS WITH LARGER TREATMENTS VOLUMES TEND TO HAVE LESS OVC COVERAGE",
                         "STRONG ALIGNMENT BETWEEN OVC_HIVSTAT_POS AND PEDS TX_CURR"
                         )
    v <- df_scat_viz %>% 
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
      scale_color_manual(values = c("Scale-Up" = scooter,
                                    "Sustained" = moody_blue,
                                    "Attained" =  genoa)) +
      labs(x = glue("TX_CURR ({ifelse({age} == 'all', '<20', '<15')})"), 
           y = "OVC_HIVSTAT_POS (<18 yo)",
           size = glue("PLHIV ({ifelse({age} == 'all', '<20', '<15')})"),
           color = NULL,
           title = plot_title,
           subtitle = type,
           caption = glue("Source: {source_nat} + {source}")) +
      si_style() 
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_coverage_TX_scatter_{fy}_{targ_res}_{str_replace(age,'<','u')}.png")
      print(out_file)
      si_save(out_file, path = "Images", width = 8)
    } else {
      return(v)
    }  
  }
  
  
  plot_tx_ovc('all', 2021, 'cumulative')
  plot_tx_ovc('u15', 2021, 'cumulative')
    
  
# VIZ - TX VS OVC_SERV -------------------------------------------------
  
  
  plot_tx_ovcserv <- function(age, fy, targ_res, export = TRUE){
    
    df_scat_viz <- df_join %>% 
      filter(fiscal_year == {fy},
             age_grp == {age},
             targets_results == {targ_res},
             snu1 != "_Military Tanzania")
    
    
    df_scat_viz <- df_scat_viz %>% 
      mutate(rank = dense_rank(-tx_curr),
             psnu_lab = case_when(rank <= 15 ~ psnu))
                                  #ovc_serv > 1000 ~ psnu,
                                  #ovc_serv > tx_curr ~ psnu)) 
    
    type <- df_scat_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = ifelse(targets_results == "targets", "Targets", "Q2 Results"),
             sub = glue("{fiscal_year} {targets_results}")) %>% 
      pull()
    
    plot_title <- ifelse(targ_res == "targets",
                         "STRONG ALIGNMENT BETWEEN OVC_SERV AND TX_CURR TARGETS",
                         "STRONG ALIGNMENT BETWEEN OVC_SERV AND TX_CURR Q2 RESULTS")
    
    v <- df_scat_viz %>% 
      ggplot(aes(tx_curr, ovc_serv)) +
      # geom_blank(aes(ovc_serv, tx_curr)) +
      # geom_abline(slope = 1, intercept = 0, color = trolley_grey, linetype = "dashed") +
      geom_smooth(aes(weight = plhiv), method = "lm", se = FALSE, alpha = .5, color = denim_light) +
      geom_point(aes(size = plhiv, color = snuprioritization), alpha = .4) +
      geom_text_repel(aes(label = psnu_lab), na.rm = TRUE,
                      family = "Source Sans Pro", color = trolley_grey, size = 8/.pt) +
      scale_x_continuous(label = comma) +
      scale_y_continuous(label = comma) +
      scale_size(label = comma) +
      scale_color_manual(values = c("Scale-Up" = scooter,
                                    "Sustained" = moody_blue,
                                    "Attained" =  genoa)) +
      labs(x = glue("TX_CURR ({ifelse({age} == 'all', '<20', '<15')})"), 
           y = glue("OVC_SERV ({ifelse({age} == 'all', '<18', '<15')})"),
           size = glue("PLHIV ({ifelse({age} == 'all', '<20', '<15')})"),
           color = NULL,
           title = plot_title,
           subtitle = type,
           caption = glue("Source: {source_nat} + {source}")) +
      si_style() +
      theme(plot.title.position = "plot")
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_ovcserv_TX_scatter_{fy}_{targ_res}_{str_replace(age,'<','u')}.png")
      print(out_file)
      si_save(out_file, path = "Images", width = 8)
    } else {
      return(v)
    }  
  }
  
  
  plot_tx_ovcserv('all', 2021, 'cumulative')
  plot_tx_ovcserv('all', 2021, 'targets')
  plot_tx_ovcserv('all', 2022, 'targets')
  
  

# VIZ - PLHIV BURDEN ------------------------------------------------------

  plot_plhiv_rank <- function(age, fy, threshold = 1000, export = TRUE){
    
    df_plhiv_rank <- df_join %>%
      filter(fiscal_year == fy,
             age_grp == age,
             targets_results == "targets") %>% 
      mutate(psnu_ovc = psnu_ovc_status_22,
             #psnu_ovc = ifelse(psnu_ovc == "TRUE", "OVC Programming", "No OVC Programming"),
             psnu_lab = case_when(plhiv > threshold & psnu_ovc == "No OVC Programming" ~ psnu))
    
    n_psnu <- df_plhiv_rank %>% 
      filter(plhiv > threshold,
             psnu_ovc == "No OVC Programming") %>% 
      count(psnu_ovc) %>% 
      pull()
    
    age <- df_plhiv_rank %>% 
      distinct( age_grp) %>% 
      mutate(age_grp = if_else(age_grp == "all", "<20yo", "<15yo")) %>% 
      pull()
    
    type <- df_plhiv_rank %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             age_grp = if_else(age_grp == "all", "<20yo", "<15yo"),
             sub = glue("{fiscal_year} PLHIV {age_grp}")) %>% 
      pull()
    
    src <- glue("A council is defined as having 'OVC programming' if it has FY22 OVC_SERV targets")
    src <- ifelse(fy == 2021, glue("{src}\nSource: {source_nat} + {source}"),
                  glue("{src}\nSource: {source_nat} + {source_dp} + {source}"))
      
    v <- df_plhiv_rank %>% 
      ggplot(aes(plhiv, fct_reorder(psnu, plhiv), fill = psnu_ovc)) +
      geom_col(width =0.8) +
      geom_text_repel(aes(label = psnu_lab), na.rm = TRUE, max.overlaps = 20,
                      seed = 42, force = 10, hjust = 1, nudge_x = 250,  
                      family = "Source Sans Pro", color = "#505050", size = 9/.pt, segment.color = "#909090") +
      scale_x_continuous(label = comma, position = "top") +
      scale_fill_manual(values = c("OVC Programming" = scooter, "OVC Programming (New FY22)" = "#084658", "No OVC Programming" = golden_sand)) +
      labs(title = glue("Of the largest councils where C/ALHIV {age} is greater than {comma(threshold, 1)}, there are {n_psnu} councils without FY22 OVC programs") %>% 
             toupper() %>% str_wrap(95),
           subtitle = type,
           x = NULL, y = NULL, fill = NULL,
           caption = src) +
      si_style_xgrid() +
      theme(axis.text.y = element_blank())
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_plhiv_rank_{fy}_{str_replace(age,'<','u')}.png")
      print(out_file)
      si_save(out_file, path = "Images")
    } else {
      return(v)
    }
  }
  
  
  plot_plhiv_rank("all", 2021)
  plot_plhiv_rank("all", 2022)
  
  plot_plhiv_rank("u15", 2021, threshold = 500)
  plot_plhiv_rank("u15", 2022, threshold = 500)
  
 
  #list of psnus
  df_join %>%
    filter(fiscal_year == 2022,
           age_grp == "u15",
           targets_results == "targets",
           plhiv > 500, #threshold
           psnu_ovc_status_22 == "No OVC Programming") %>% 
    arrange(desc(plhiv)) %>% 
    pull(psnu) %>% 
    paste(collapse = ", ") %>% 
    paste("Largest councils without FY22 OVC_SERV targets: ", .)
   
###########
  
  # n_psnus <- 20
  # 
  # lrg_cov <- df_join %>% 
  #   filter(!is.na(tx_curr),
  #          snu1 != "_Military Tanzania") %>% 
  #   slice_max(order_by = plhiv, n = n_psnus) %>% 
  #   summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
  #   mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
  #   pull()
  # 
  # df_join %>%
  #   filter(!is.na(tx_curr),
  #          !is.na(ovc_hivstat_pos),
  #          snu1 != "_Military Tanzania") %>%
  #   slice_min(order_by = vls_txcurr, n = n_psnus) %>%
  #   select(psnu, plhiv, vls_txcurr, tx_curr, tx_pvls, coverage_tx) %>%
  #   pivot_longer(c(tx_curr, tx_pvls),
  #                names_to = "indicator") %>%
  #   mutate(indicator = toupper(indicator),
  #          alpha_fill = ifelse(indicator == "TX_CURR", .8, 1),
  #          coverage_tx = ifelse(indicator == "TX_CURR", NA, coverage_tx),
  #          vls_txcurr = ifelse(indicator == "TX_CURR", NA, vls_txcurr),
  #          indicator = glue("{indicator} (<20yo)"),
  #          cov_pt = 20) %>%
  #   ggplot(aes(value, fct_reorder(psnu, value, max), fill = indicator, alpha = alpha_fill)) +
  #   geom_col(position = "identity") +
  #   geom_text(aes(label = percent(vls_txcurr, 1)), na.rm = TRUE,
  #             hjust = 1.2,
  #             family = "Source Sans Pro", size = 7/.pt, color = "white") +
  #   geom_point(aes(cov_pt), size = 5, color = burnt_sienna_light, show.legend = FALSE) +
  #   geom_text(aes(cov_pt, label = percent(coverage_tx, 1)), na.rm = TRUE,
  #             family = "Source Sans Pro", size = 6/.pt, color = "#202020") +
  #   scale_x_continuous(label = comma, position = "top",
  #                      expand = c(.005, .005)) +
  #   scale_fill_manual(values = c(trolley_grey_light, scooter)) +
  #   scale_alpha_identity() +
  #   labs(x = NULL, y = NULL, fill = NULL,
  #        title = glue("ACROSS THE {n_psnus} LARGEST COUNCILS, THERE IS A {percent(lrg_cov,1)} COVERAGE RATE OF HIV POSITIVE OVC ON TX"),
  #        caption = "Source: FY21Q2c MSD
  #        SI Analytics: Aaron Chafetz
  #        US Agency for International Development") +
  #   si_style_xgrid()

# VIZ - PLHIV + COVERAGE --------------------------------------------------

  
  plot_cov_plhiv_rate <- function(age, fy, targ_res, n_psnus = NULL, export = TRUE){
    df_cov_plhiv_viz <- df_join %>% 
      select(fiscal_year, age_grp, targets_results,
             snu1, psnu, psnu_ovc, plhiv, ovc_hivstat_pos, tx_curr) %>% 
      pivot_longer(c(plhiv, ovc_hivstat_pos, tx_curr), names_to = "indicator", values_drop_na = TRUE) %>% 
      filter(fiscal_year == {fy},
             (indicator %in% c("tx_curr", "plhiv") & age_grp == {age}) | (indicator == "ovc_hivstat_pos" & age_grp == "all"),
             targets_results == {targ_res},
             snu1 != "_Military Tanzania") %>% 
      mutate(age_grp = {age}) %>% 
      pivot_wider(names_from = "indicator") %>% 
      mutate(coverage_tx = ovc_hivstat_pos/tx_curr,
             plhiv_order = plhiv)
    
    if(!is.null(n_psnus))
      df_cov_plhiv_viz <- slice_max(df_cov_plhiv_viz, order_by = tx_curr, n = n_psnus)
    
    cov_rate <- df_cov_plhiv_viz %>% 
      filter(psnu_ovc == TRUE) %>% 
      summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
      mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
      pull()
    
    
    cov_title_seg <- ifelse(!is.null(n_psnus),
                            glue("THE {n_psnus} LARGEST COUNCILS"),
                            glue("COUNCILS WITH OVC PROGRAMMING"))
    cov_title <- glue("ACROSS {cov_title_seg}, THERE IS A {percent(cov_rate,1)} COVERAGE RATE OF HIV+ OVC ON TX")
    
    type <- df_cov_plhiv_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = str_to_title(targets_results),
             age_grp = if_else(age_grp == "all", "<20yo", "<15yo"),
             sub = glue("{fiscal_year} {targets_results} {age_grp}")) %>% 
      pull()
    
    src <- ifelse(fy == 2021, glue("Source: {source_nat} + {source}"),
                  glue("Source: {source_nat} + {source_dp} + {source}"))
    
    df_cov_plhiv_viz <- df_cov_plhiv_viz %>%
      select(psnu, plhiv, ovc_hivstat_pos, coverage_tx, age_grp, plhiv_order) %>% 
      pivot_longer(c(plhiv, ovc_hivstat_pos), 
                   names_to = "indicator") %>%
      mutate(indicator = toupper(indicator),
             alpha_fill = ifelse(indicator == "PLHIV", .8, 1), 
             coverage_tx = ifelse(indicator == "PLHIV", NA, coverage_tx),
             age_grp = case_when(indicator == "OVC_HIVSTAT_POS" ~ "<18",
                                 age_grp == "u15" ~ "<15",
                                 TRUE ~ "<20"),
             indicator = glue("{indicator} ({age_grp})"),
             cov_pt = 50)
    
    v <- df_cov_plhiv_viz %>% 
      ggplot(aes(value, fct_reorder(psnu, plhiv_order, max), fill = indicator, alpha = alpha_fill)) +
      geom_col(position = "identity", width = ifelse(!is.null(n_psnu), 0.9, 0.8), na.rm = TRUE) +
      scale_fill_manual(values = c(scooter, trolley_grey_light)) +
      scale_alpha_identity() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = cov_title,
           subtitle = type,
           caption = src) +
      si_style_xgrid()
    
    if(!is.null(n_psnus)){
      v <- v +
        geom_point(aes(cov_pt), size = 5, color = burnt_sienna_light, show.legend = FALSE) +
        geom_text(aes(cov_pt, label = percent(coverage_tx, 1)), na.rm = TRUE,
                  family = "Source Sans Pro", size = 6/.pt, color = "#202020") +
        scale_x_continuous(label = comma, position = "top", expand = c(.005, .005)) +
        theme(plot.title.position = "plot")
    } else {
      v <- v +
        scale_x_continuous(label = comma, position = "top") +
        theme(axis.text.y = element_blank(), 
              plot.title.position = "plot")
    }
    
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_tx_coverage_plhiv_{fy}_{targ_res}_{str_replace(age,'<','u')}_{ifelse(is.null(n_psnus), 'natl', 'top')}.png")
      print(out_file)
      si_save(out_file, path = "Images")
    } else {
      return(v)
    } 
  }
  
  
  plot_cov_plhiv_rate('all', '2021', 'cumulative', 20)
  plot_cov_plhiv_rate('all', '2021', 'cumulative')
  
  plot_cov_plhiv_rate('u15', '2021', 'cumulative', 20)
  plot_cov_plhiv_rate('u15', '2021', 'cumulative')
  
  

# VIZ - PREVALENCE V COVERAGE ---------------------------------------------

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
  
  
  
  plot_plhiv_ovc <- function(age, fy, targ_res, export = TRUE){
    
    df_scat_viz <- df_join %>% 
      select(fiscal_year, age_grp, targets_results,
             snu1, psnu, snuprioritization, 
             plhiv, pop_est, ovc_hivstat_pos, tx_curr) %>% 
      pivot_longer(c(plhiv, pop_est, ovc_hivstat_pos, tx_curr), names_to = "indicator", values_drop_na = TRUE) %>% 
      filter(fiscal_year == {fy},
             (indicator %in% c("tx_curr", "plhiv", "pop_est") & age_grp == {age}) | (indicator == "ovc_hivstat_pos" & age_grp == "all"),
             targets_results == {targ_res},
             snu1 != "_Military Tanzania") %>% 
      mutate(age_grp = {age}) %>% 
      pivot_wider(names_from = "indicator")
    
    
    df_scat_viz <- df_scat_viz %>% 
      mutate(prevalence = plhiv/pop_est,
             prevalence_10k = round(prevalence * 1e4),
             coverage_tx = ovc_hivstat_pos/tx_curr,
             rank = dense_rank(-tx_curr),
             psnu_lab = case_when(rank <= 15 ~ psnu,
                                  coverage_tx > 1.2 ~ psnu,
                                  prevalence_10k > 150 ~ psnu)) 
    
    type <- df_scat_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = str_to_title(targets_results),
             age_grp = if_else(age_grp == "all", "<20yo", "<15yo"),
             sub = glue("{fiscal_year} {targets_results} {age_grp}")) %>% 
      pull()
    
    plot_title <- ifelse(age == 'all',
                         "COUNCILS WITH LARGER PREVALENCE TEND TO HAVE LOWER TX COVERAGE OF OVC HIV+",
                         "COUNCILS WITH LARGER PREVALENCE TEND TO HAVE LOWER TX COVERAGE OF OVC HIV+"
    )
    
    age_txt <- if_else(age == "all", "<20", "<15")
    src <- ifelse(fy == 2021, glue("Source: {source_nat} + {source}"),
                  glue("Source: {source_nat} + {source_dp} + {source}"))
    
    v <- df_scat_viz %>% 
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
      scale_color_manual(values = c("Scale-Up" = scooter,
                                    "Sustained" = moody_blue,
                                    "Attained" =  genoa)) +
      labs(x = glue("PLHIV per 10,000 pop ({age_txt})"), 
           y = glue("OVC Treatment Coverage Rate\nOVC_HIVSTAT_POS (<18) share of TX_CURR ({age_txt})"),
           size = glue("PLHIV ({age_txt})"), 
           color = NULL,
           title = str_wrap(plot_title),
           subtitle = type,
           caption = src) +
      si_style() +
      theme(plot.title.position = "plot")
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_cov_plhiv_scatter_{fy}_{targ_res}_{str_replace(age,'<','u')}.png")
      print(out_file)
      si_save(out_file, path = "Images", width = 8)
    } else {
      return(v)
    }  
  }
  
  
  
  plot_plhiv_ovc('all', 2021, 'cumulative')
  plot_plhiv_ovc('u15', 2021, 'cumulative')
  
  
  
 
# VIZ - COVERAGE + VLC ----------------------------------------------------

  plot_cov_vlc_rate <- function(age, fy, targ_res, n_psnus = NULL, export = TRUE){
    df_cov_vl_viz <- df_join %>% 
      filter(!is.na(tx_curr_lag2),
             !is.na(ovc_hivstat_pos)) %>% 
      select(fiscal_year, age_grp, targets_results,
             snu1, psnu, psnu_ovc, 
             plhiv, tx_pvls_d, tx_pvls, tx_curr_lag2, tx_curr, ovc_hivstat_pos) %>% 
      pivot_longer(c(plhiv, tx_pvls_d, tx_pvls, tx_curr_lag2, tx_curr, ovc_hivstat_pos), names_to = "indicator", values_drop_na = TRUE) %>% 
      filter(fiscal_year == {fy},
             (indicator %in% c("plhiv", "tx_pvls_d", "tx_pvls", "tx_curr_lag2", "tx_curr") & age_grp == {age}) | (indicator == "ovc_hivstat_pos" & age_grp == "all"),
             targets_results == {targ_res},
             snu1 != "_Military Tanzania") %>% 
      mutate(age_grp = {age}) %>% 
      pivot_wider(names_from = "indicator") %>% 
      mutate(coverage_tx = ovc_hivstat_pos/tx_curr,
             vlc = tx_pvls_d/tx_curr_lag2,
             vls = tx_pvls/tx_pvls_d,
             vls_txcurr = tx_pvls/tx_curr_lag2,
             plot_order = vlc)
    
    if(!is.null(n_psnus))
      df_cov_vl_viz <- slice_min(df_cov_vl_viz, order_by = vlc, n = n_psnus)
    
    # if(!is.null(n_psnus))
    #   df_cov_vl_viz <- slice_min(df_cov_vl_viz, order_by = vls_txcurr, n = n_psnus)
    
    cov_rate <- df_cov_vl_viz %>% 
      filter(psnu_ovc == TRUE) %>% 
      summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
      mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
      pull()
    
    
    cov_title_seg <- ifelse(!is.null(n_psnus),
                            glue("THE {n_psnus} LOWEST VLC COUNCILS"),
                            glue("THE LOWEST VLC COUNCILS WITH OVC PROGRAMMING"))
    cov_title <- glue("ACROSS {cov_title_seg}, THERE IS A {percent(cov_rate,1)} COVERAGE RATE OF HIV+ OVC ON TX")
    
    type <- df_cov_vl_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = str_to_title(targets_results),
             age_grp = if_else(age_grp == "all", "<20yo", "<15yo"),
             sub = glue("{fiscal_year} {targets_results} {age_grp}")) %>% 
      pull()
    
    df_cov_vl_viz <- df_cov_vl_viz %>%
      select(psnu, plhiv, coverage_tx, age_grp, plot_order,
             vlc, #vls_txcurr
             tx_pvls_d, tx_curr_lag2) %>% 
      pivot_longer(c(tx_curr_lag2, tx_pvls_d), 
                   names_to = "indicator") %>%
      mutate(indicator = toupper(indicator),
             alpha_fill = ifelse(indicator == "TX_CURR_LAG2", .8, 1), 
             coverage_tx = ifelse(indicator == "TX_CURR_LAG2", NA, coverage_tx),
             vlc = ifelse(indicator == "TX_CURR_LAG2", NA, vlc),
             # vls_txcurr = ifelse(indicator == "TX_CURR_LAG2", NA, vls_txcurr),
             indicator = recode(indicator, "TX_CURR_LAG2" = "TX_CURR 2Qs prior"),
             age_grp = case_when(#indicator == "OVC_HIVSTAT_POS" ~ "<18",
               age_grp == "u15" ~ "<15",
               TRUE ~ "<20"),
             indicator = glue("{indicator} ({age_grp})"),
             cov_pt = 20)
    
    v <- df_cov_vl_viz %>% 
      ggplot(aes(value, fct_reorder(psnu, plot_order, max, .desc = TRUE), fill = indicator, alpha = alpha_fill)) +
      geom_col(position = "identity", width = ifelse(!is.null(n_psnu), 0.9, 0.8), na.rm = TRUE) +
      scale_fill_manual(values = c(trolley_grey_light, scooter)) +
      scale_alpha_identity() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = cov_title,
           subtitle = type,
           caption = glue("Source: {source}")) +
      si_style_xgrid()
    
    if(!is.null(n_psnus)){
      v <- v +
        geom_text(aes(label = percent(vlc, 1)), na.rm = TRUE, hjust = 1.2,
                  family = "Source Sans Pro", size = 7/.pt, color = "white") +
        geom_point(aes(cov_pt), size = 5, color = burnt_sienna_light, show.legend = FALSE) +
        geom_text(aes(cov_pt, label = percent(coverage_tx, 1)), na.rm = TRUE,
                  family = "Source Sans Pro", size = 6/.pt, color = "#202020") +
        scale_x_continuous(label = comma, position = "top", expand = c(.005, .005)) +
        theme(plot.title.position = "plot")
    } else {
      v <- v +
        scale_x_continuous(label = comma, position = "top") +
        theme(axis.text.y = element_blank(), 
              plot.title.position = "plot")
    }
    
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_coverage_vlc_{fy}_{targ_res}_{str_replace(age,'<','u')}_{ifelse(is.null(n_psnus), 'natl', 'top')}.png")
      print(out_file)
      si_save(out_file, path = "Images")
    } else {
      return(v)
    } 
  } 
  
 
  plot_cov_vlc_rate('all', 2021, 'cumulative', n_psnus = 20)
  # plot_cov_vlc_rate('u15', 2021, 'cumulative', n_psnus = 20)
  

# VIZ - COVERAGE + VLS ----------------------------------------------------

  plot_cov_vls_rate <- function(age, fy, targ_res, n_psnus = NULL, export = TRUE){
    df_cov_vl_viz <- df_join %>% 
      filter(!is.na(tx_curr_lag2),
             !is.na(ovc_hivstat_pos)) %>% 
      select(fiscal_year, age_grp, targets_results,
             snu1, psnu, psnu_ovc, 
             plhiv, tx_pvls_d, tx_pvls, tx_curr_lag2, tx_curr, ovc_hivstat_pos) %>% 
      pivot_longer(c(plhiv, tx_pvls_d, tx_pvls, tx_curr_lag2, tx_curr, ovc_hivstat_pos), names_to = "indicator", values_drop_na = TRUE) %>% 
      filter(fiscal_year == {fy},
             (indicator %in% c("plhiv", "tx_pvls_d", "tx_pvls", "tx_curr_lag2", "tx_curr") & age_grp == {age}) | (indicator == "ovc_hivstat_pos" & age_grp == "all"),
             targets_results == {targ_res},
             snu1 != "_Military Tanzania") %>% 
      mutate(age_grp = {age}) %>% 
      pivot_wider(names_from = "indicator") %>% 
      mutate(coverage_tx = ovc_hivstat_pos/tx_curr,
             vlc = tx_pvls_d/tx_curr_lag2,
             vls = tx_pvls/tx_pvls_d,
             vls_txcurr = tx_pvls/tx_curr_lag2,
             plot_order = vls_txcurr)
    
    if(!is.null(n_psnus))
      df_cov_vl_viz <- slice_min(df_cov_vl_viz, order_by = vls_txcurr, n = n_psnus)
    
    cov_rate <- df_cov_vl_viz %>% 
      filter(psnu_ovc == TRUE) %>% 
      summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
      mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
      pull()
    
    
    cov_title_seg <- ifelse(!is.null(n_psnus),
                            glue("THE {n_psnus} LOWEST VLS COUNCILS"),
                            glue("THE LOWEST VLS COUNCILS WITH OVC PROGRAMMING"))
    cov_title <- glue("ACROSS {cov_title_seg}, THERE IS A {percent(cov_rate,1)} COVERAGE RATE OF HIV+ OVC ON TX")
    
    type <- df_cov_vl_viz %>% 
      distinct(fiscal_year, targets_results, age_grp) %>% 
      mutate(fiscal_year = glue("FY{str_sub(fiscal_year, 3)}"),
             targets_results = str_to_title(targets_results),
             age_grp = if_else(age_grp == "all", "<20yo", "<15yo"),
             sub = glue("{fiscal_year} {targets_results} {age_grp}")) %>% 
      pull()
    
    df_cov_vl_viz <- df_cov_vl_viz %>%
      select(psnu, plhiv, coverage_tx, age_grp, plot_order,
             vls_txcurr, #vlc
             tx_pvls, tx_curr_lag2, vls_txcurr) %>% 
      pivot_longer(c(tx_curr_lag2, tx_pvls), 
                   names_to = "indicator") %>%
      mutate(indicator = toupper(indicator),
             alpha_fill = ifelse(indicator == "TX_CURR_LAG2", .8, 1), 
             coverage_tx = ifelse(indicator == "TX_CURR_LAG2", NA, coverage_tx),
             # vlc = ifelse(indicator == "TX_CURR_LAG2", NA, vlc),
             vls_txcurr = ifelse(indicator == "TX_CURR_LAG2", NA, vls_txcurr),
             indicator = recode(indicator, "TX_CURR_LAG2" = "TX_CURR 2Qs prior"),
             age_grp = case_when(#indicator == "OVC_HIVSTAT_POS" ~ "<18",
               age_grp == "u15" ~ "<15",
               TRUE ~ "<20"),
             indicator = glue("{indicator} ({age_grp})"),
             cov_pt = 20)
    
    v <- df_cov_vl_viz %>% 
      ggplot(aes(value, fct_reorder(psnu, plot_order, max, .desc = TRUE), fill = indicator, alpha = alpha_fill)) +
      geom_col(position = "identity", width = ifelse(!is.null(n_psnu), 0.9, 0.8), na.rm = TRUE) +
      scale_fill_manual(values = c(trolley_grey_light, scooter)) +
      scale_alpha_identity() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = cov_title,
           subtitle = type,
           caption = glue("Source: {source}")) +
      si_style_xgrid()
    
    if(!is.null(n_psnus)){
      v <- v +
        geom_text(aes(label = percent(vls_txcurr, 1)), na.rm = TRUE, hjust = 1.2,
                  family = "Source Sans Pro", size = 7/.pt, color = "white") +
        geom_point(aes(cov_pt), size = 5, color = burnt_sienna_light, show.legend = FALSE) +
        geom_text(aes(cov_pt, label = percent(coverage_tx, 1)), na.rm = TRUE,
                  family = "Source Sans Pro", size = 6/.pt, color = "#202020") +
        scale_x_continuous(label = comma, position = "top", expand = c(.005, .005)) +
        theme(plot.title.position = "plot")
    } else {
      v <- v +
        scale_x_continuous(label = comma, position = "top") +
        theme(axis.text.y = element_blank(), 
              plot.title.position = "plot")
    }
    
    
    if(export == TRUE){
      out_file <- glue("FY21Q2_OVC_coverage_vls_{fy}_{targ_res}_{str_replace(age,'<','u')}_{ifelse(is.null(n_psnus), 'natl', 'top')}.png")
      print(out_file)
      si_save(out_file, path = "Images")
    } else {
      return(v)
    } 
  }  
  
  
  plot_cov_vls_rate('all', 2021, 'cumulative', n_psnus = 20)
  # plot_cov_vls_rate('u15', 2021, 'cumulative', n_psnus = 20)
  
  
  
  
  
  df_join %>% 
    filter(!is.na(tx_curr),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative",
           snu1 != "_Military Tanzania") %>% 
    mutate(rank = dense_rank(-tx_curr),
           psnu_lab = case_when(rank <= 5 ~ psnu,
                                vlc < .72 ~ psnu,
                                coverage_tx > 1.2 ~ psnu)) %>%
    ggplot(aes(vlc, coverage_tx)) +
    # geom_blank(aes(coverage_tx, vlc)) +
    geom_hline(yintercept = 1, color = trolley_grey) +
    geom_vline(xintercept = 1, color = trolley_grey) +
    geom_smooth(aes(weight = plhiv), method = "lm", se = FALSE, alpha = .5,
                color = denim_light) +
    geom_point(aes(size = plhiv, color = snuprioritization), alpha = .4) +
    geom_text_repel(aes(label = psnu_lab), na.rm = TRUE,
                    family = "Source Sans Pro", color = trolley_grey, size = 8/.pt,) +
    expand_limits(y = 0, x = 0) +
    scale_x_continuous(label = percent, breaks = seq(0, 2, .25)) +
    scale_y_continuous(label = percent, breaks = seq(0, 2, .25)) +
    scale_size(label = comma) +
    scale_color_manual(values = c("Scale-Up" = scooter,
                                  "Sustained" = moody_blue,
                                  "Attained" =  genoa)) +
    labs(x = "% Treatment 2 Quarters Prior with a VL Test (<20)", 
         y = glue("OVC Treatment Coverage Rate\nOVC_HIVSTAT_POS (<18) share of TX_CURR (<20)"),
         size = "PLHIV (<20)", color = NULL,
         title = "NO STRONG RELATIONSHIP BETWEEN VLC AND LOWER OVC TX COVERAGE",
         subtitle = "FY21 Cumulative <20yo",
         caption = glue("Source: {source}")) +
    si_style()
  
  si_save("Images/FY21Q2_OVC_coverage_vlc_scatter.png",
          width = 8)
  
  
  df_join %>% 
    filter(!is.na(tx_curr),
           fiscal_year == 2021,
           age_grp == "all",
           targets_results == "cumulative",
           snu1 != "_Military Tanzania") %>% 
    mutate(rank = dense_rank(-tx_curr),
           psnu_lab = case_when(rank <= 5 ~ psnu,
                                vls_txcurr < .6 ~ psnu,
                                coverage_tx > 1.2 ~ psnu)) %>%
    ggplot(aes(vls_txcurr, coverage_tx)) +
    # geom_blank(aes(coverage_tx, vls_txcurr)) +
    geom_hline(yintercept = 1, color = trolley_grey) +
    geom_vline(xintercept = .95^3, color = trolley_grey, linetype = "dashed") +
    geom_smooth(aes(weight = plhiv), method = "lm", se = FALSE, alpha = .5,
                color = denim_light) +
    geom_point(aes(size = plhiv, color = snuprioritization), alpha = .4) +
    geom_text_repel(aes(label = psnu_lab), na.rm = TRUE,
                    family = "Source Sans Pro", color = trolley_grey, size = 8/.pt,) +
    expand_limits(y = 0, x = 0) +
    scale_x_continuous(label = percent, breaks = c(0, .25, .5, .75, .95^3, 1, 1.25, 1.5, 1.75, 2)) +
    scale_y_continuous(label = percent, breaks = seq(0, 2, .25)) +
    scale_size(label = comma) +
    scale_color_manual(values = c("Scale-Up" = scooter,
                                  "Sustained" = moody_blue,
                                  "Attained" =  genoa)) +
    labs(x = "% VL Suppressed of those on Treatment 2 Quarters Prior (<20)", 
         y = glue("OVC Treatment Coverage Rate\nOVC_HIVSTAT_POS (<18) share of TX_CURR (<20)"),
         size = "PLHIV (<20)", color = NULL,
         title = "NO STRONG RELATIONSHIP BETWEEN VLS AND LOWER OVC TX COVERAGE",
         subtitle = "FY21 Cumulative <20 yo",
         caption = glue("Source: {source}")) +
    si_style()
  
  si_save("Images/FY21Q2_OVC_coverage_vls_scatter.png",
          width = 8)
  

# TABLE -------------------------------------------------------------------

  
  df_tbl <- df_join %>% 
    filter(fiscal_year >= 2021,
           psnu != "_Military Tanzania") %>% 
    select(fiscal_year, psnu, age_grp, targets_results,
           psnu_ovc_status_22,
           plhiv, tx_curr) %>% 
    mutate(targets_results = replace_na(targets_results, "targets"),
           age_grp = ifelse(age_grp == "all", "u20", "u15")) %>% 
    pivot_longer(c(plhiv, tx_curr),
                 names_to = "indicator") %>% 
    filter(!(indicator == "tx_curr" & targets_results == "targets") &
           !(indicator == "plhiv" & targets_results == "cumulative"),
           value > 0) %>% 
    select(-targets_results) %>% 
    pivot_wider(names_from = c(indicator, age_grp, fiscal_year),
                names_sep = ".",
                values_from = value,
                values_fill = 0) %>% 
    select(starts_with("psnu"), contains("u20"), everything()) %>% 
    arrange(desc(tx_curr.u20.2021))
  


  thres_tx_u20 <- 400
  thres_tx_u15 <- 250
  thres_plhiv_u20 <- 700
  thres_plhiv_u15 <- 500
  
  df_tbl <- df_tbl %>% 
    mutate(abv_thres_tx_u20 = tx_curr.u20.2021 > thres_tx_u20,
           abv_thres_tx_u15 = tx_curr.u15.2021 > thres_tx_u15,
           abv_thres_plhiv_u20 = plhiv.u20.2022 > thres_plhiv_u20,
           abv_thres_plhiv_u15 = plhiv.u15.2022 > thres_plhiv_u15,
           abv_thres_cnt = case_when(psnu_ovc_status_22 != "OVC Programming" ~
                                       abv_thres_tx_u20 + abv_thres_tx_u15 + abv_thres_plhiv_u20 + abv_thres_plhiv_u15
           ))

  df_tbl %>% 
    filter(abv_thres_cnt > 0,
           abv_thres_plhiv_u15 == TRUE) %>% 
    distinct(psnu, plhiv.u15.2022)
  
  
  df_tbl <- df_tbl %>%
    pivot_longer(matches("^(tx|plhiv)"),
                 names_to = c("indicator", "age_grp", "fiscal_year"),
                 names_sep = "\\.") %>% 
    pivot_wider(names_from = c("indicator", "fiscal_year"), 
                names_sep = ".")
  
  df_tbl <- df_tbl %>% 
    mutate(psnu_new = case_when(psnu_ovc_status_22 != "OVC Programming" ~ psnu))
  
  
  p1 <- df_tbl %>% 
    filter(psnu_ovc_status_22 == "OVC Programming") %>% 
    mutate(group = "COP20 Councils") %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  p2 <- df_tbl %>% 
    filter(psnu_ovc_status_22 != "No OVC Programming") %>% 
    mutate(group = "COP21 Councils") %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  p3 <- df_tbl %>% 
    filter(psnu_ovc_status_22 == "OVC Programming" | 
             abv_thres_cnt > 0) %>% 
    mutate(group = "Option 1: Above any threshold") %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  p4 <- df_tbl %>% 
    filter(psnu_ovc_status_22 == "OVC Programming" | 
             abv_thres_tx_u20 == TRUE) %>% 
    mutate(group = glue("Option 2: TX_CURR <20 above {thres_tx_u20}")) %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  p5 <- df_tbl %>% 
    filter(psnu_ovc_status_22 == "OVC Programming" | 
             abv_thres_tx_u15 == TRUE) %>% 
    mutate(group = glue("Option 3: TX_CURR <15 above {thres_tx_u15}")) %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  
  p6 <- df_tbl %>% 
    filter(psnu_ovc_status_22 == "OVC Programming" | 
             abv_thres_plhiv_u20 == TRUE) %>% 
    mutate(group = glue("Option 4: PLHIV <20 above {thres_plhiv_u20}")) %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  p7 <- df_tbl %>% 
    filter(psnu_ovc_status_22 == "OVC Programming" | 
             abv_thres_plhiv_u15 == TRUE) %>% 
    mutate(group = glue("Option 5: PLHIV <15 above {thres_plhiv_u15}")) %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  df_options <- bind_rows(p1, p2, p3, p4, p5, p6, p7)

  df_options <- df_options %>% 
    mutate(group = fct_inorder(group),
           psnus = psnus %>% str_remove_all("(NA,|NA)") %>% str_squish,
           age_grp = ifelse(age_grp == "u20", "<20", "<15"),
           ovc_pred = round(.9*tx_curr.2021),
           fill_color = case_when(group == "COP20 Councils" ~ trolley_grey,
                                  group == "COP21 Councils" ~ scooter,
                                  TRUE ~ scooter_med))

  df_options <- df_options %>% 
    mutate(grp_name = glue("{group} ({psnu_n})"))
           # grp_name = glue("{group}({psnu_n})\n{psnus}") %>% str_wrap(20))
           # grp_name = glue("{group}<br><span style = 'font-size:7pt;color:#909090'>{psnus}</span>"))
    
  df_options %>% 
    ggplot(aes(y = fct_rev(grp_name))) +
    geom_col(aes(tx_curr.2021, fill = fill_color), alpha = .6) +
    geom_col(aes(ovc_pred, fill = fill_color), alpha = .8) +
    geom_col(aes(plhiv.2022), color = trolley_grey, fill = NA) +
    geom_text(aes(x = 7500, label = comma(ovc_pred, 1)),
              family = "Source Sans Pro SemiBold", color = "white") +
    facet_wrap(~fct_rev(age_grp)) +
    scale_fill_identity() +
    scale_x_continuous(labels = comma) +
    labs(x = NULL, y  = NULL,
         title = "evaluating different thresholds' effects on estimated ovc HIV+ necessary to cover" %>% toupper,
         subtitle = "Assuming 90% of TX_CURR are OVC HIV+",
         caption = glue("Source: {source_nat} + {source_dp} + {source}")) +
    si_style_xgrid() +
    theme(#axis.text.y = element_text(size = 7),
          plot.title.position = "plot")

  si_save("Graphics/FY21Q2_OVC_size_options.svg", height = 4.625)  
  
 
  df_options %>% 
    filter(str_detect(group, "Option 2"),
           age_grp == "<20")
  
  
  
  df_tbl %>% 
    filter(psnu_ovc_status_22 != "OVC Programming", 
             abv_thres_tx_u20 == TRUE,
           age_grp == "u20") %>% 
    mutate(group = glue("Option 2: TX_CURR <20 above {thres_tx_u20}")) %>% 
    group_by(group, age_grp) %>% 
    summarise(across(c(tx_curr.2021, plhiv.2022), sum, na.rm = TRUE),
              psnus = paste(psnu_new, collapse = ", "),
              psnu_n = n(),
              .groups = "drop")
  
  
  df_options %>% 
    filter(age_grp == "<20") %>% 
    select(group, psnus)
  
  
  df_tbl %>% 
    filter(psnu_ovc_status_22 != "OVC Programming", 
             abv_thres_tx_u15 == TRUE,
           age_grp == "u20") %>% 
    mutate(group = glue("Option 3: TX_CURR <15 above {thres_tx_u15}"))
  
  df_tbl %>% 
    select(-psnu_new) %>% 
    select(psnu, psnu_ovc_status_22, age_grp, tx_curr.2021, plhiv.2021, plhiv.2022,
           everything()) %>%
  write_csv("Dataout/COP21_OVC-Council-Thresholds_2021-09-03.csv", na = "")
  