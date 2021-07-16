# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-07-15
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(ggrepel)
  

# GLOBAL VARIABLES --------------------------------------------------------

  age_range <- c("<01", "01-04", "05-09", "10-14", "15-17",  "15-19"
                 #"18+"
                 )  

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds()   
  
  df_subnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_rds()  
  
# MUNGE -------------------------------------------------------------------
  
  curr_pd <- identifypd(df_msd)
  curr_fy <- identifypd(df_msd, "year")
  
  df_msd <- df_msd %>% 
    filter(operatingunit == "Tanzania")
  
  df_msd %>% 
    filter(fiscal_year == 2021,
           indicator %in% c("OVC_SERV","OVC_HIVSTAT", "OVC_HIVSTAT_POS")) %>% 
    count(indicator, standardizeddisaggregate, otherdisaggregate, wt = cumulative)
    
  df_tx <- df_msd %>% 
    filter(fiscal_year >= curr_fy - 1,
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Age/Sex/Indication/HIVStatus"),
           ageasentered %in% age_range) %>%
    clean_indicator() %>% 
    group_by(fiscal_year, snu1, psnu, psnuuid, snuprioritization, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    arrange(psnuuid, period) %>% 
    group_by(psnuuid) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n =2, by = period), .after = tx_curr) %>% 
    mutate(fiscal_year = curr_fy, .before =1 ) %>% 
    filter(period == max(period)) %>% 
    select(-c(period_type, period)) 
  
  df_ovc <- df_msd %>% 
    filter(fiscal_year == 2021,
           indicator %in% c("OVC_SERV", "OVC_HIVSTAT", "OVC_HIVSTAT_POS"),
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/DREAMS", 
                                           "Age/Sex/Preventive", "Age/Sex/ProgramStatus", 
                                           "Age/Sex/ProgramStatusCaregiver"),
           ageasentered %in% c(NA, age_range))

  df_ovc <- df_ovc %>% 
    mutate(standardizeddisaggregate = str_remove(standardizeddisaggregate, "Age/Sex/"),
           indicator = case_when(!is.na(otherdisaggregate) ~ glue("{indicator}_{otherdisaggregate}"),
                                 str_detect(standardizeddisaggregate, "Total") ~ indicator,
                                 TRUE ~ glue("{indicator}_{standardizeddisaggregate}")))
  
  df_ovc <- df_ovc %>% 
    group_by(fiscal_year, snu1, psnu, psnuuid, snuprioritization, indicator) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_from = cumulative)
  
  df_ovc <- df_ovc %>% 
    rowwise() %>% 
    mutate(ovc_serv = sum(ovc_serv_active, ovc_serv_graduated, ovc_serv_preventive, ovc_serv_dreams, na.rm = TRUE),
           ovc_serv_comp = sum(ovc_serv_active, ovc_serv_graduated, na.rm = TRUE)) %>% 
    ungroup() %>% 
    relocate(ovc_serv_comp, .after = ovc_serv)
    

  df_subnat <- df_subnat %>% 
    filter(operatingunit == "Tanzania",
           fiscal_year == 2021,
           indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate %in% c("Age/Sex", "Age/Sex/HIVStatus"),
           ageasentered %in% age_range) %>% 
    count(fiscal_year, snu1, psnu,  psnuuid, indicator, wt = targets, name = "value") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")
  


# JOIN --------------------------------------------------------------------

  df_join <- df_subnat %>% 
    full_join(df_tx, by = c("fiscal_year", "snu1", "psnu", "psnuuid")) %>% 
    left_join(df_ovc, by = c("fiscal_year", "snu1", "psnu", "psnuuid", "snuprioritization"))


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
           snuprioritization = factor(snuprioritization, c("Scale-Up", "Sustained", "Attained")))
  
  
# VIZ ---------------------------------------------------------------------

  
  nat_cov <- df_join %>% 
    filter(!is.na(tx_curr),
           snu1 != "_Military Tanzania") %>% 
    summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
    pull()
  
  #coverage
  df_join %>% 
    filter(!is.na(tx_curr),
           snu1 != "_Military Tanzania") %>% 
    group_by(snu1) %>% 
    summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(c(tx_curr, ovc_hivstat_pos), 
                 names_to = "indicator") %>%
    mutate(indicator = toupper(indicator),
           alpha_fill = ifelse(indicator == "TX_CURR", .8, 1), 
           indicator = ifelse(indicator == "TX_CURR", glue("{indicator} (<20yo)"),
                              glue("{indicator} (<=18yo)"))) %>%
    group_by(snu1) %>% 
    mutate(coverage = min(value)/max(value),
           coverage = case_when(value == max(value) ~ NA_real_,
                                coverage == 0 ~ NA_real_,
                                TRUE ~ coverage)) %>% 
    ungroup() %>% 
    ggplot(aes(value, fct_reorder(snu1, value, max), fill = indicator, alpha = alpha_fill)) +
    geom_col(position = "identity") +
    geom_text(aes(label = percent(coverage, 1)), na.rm = TRUE,
              hjust = 1.2,
              family = "Source Sans Pro", size = 8.5/.pt, color = "white") +
    scale_x_continuous(label = comma, position = "top",
                       expand = c(.005, .005)) +
    scale_fill_manual(values = c(scooter, trolley_grey_light)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("ACROSS TANZANIA, THERE IS A {percent(nat_cov,1)} COVERAGE RATE OF HIV POSITIVE OVC ON TREATMENT"),
         caption = "Source: FY21Q2c MSD
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style_xgrid()
  
  si_save("Images/FY21Q2_OVC_coverage_tx_curr.png")
  
  
  n_psnus <- 20
  
  lrg_cov <- df_join %>% 
    filter(!is.na(tx_curr),
           snu1 != "_Military Tanzania") %>% 
    slice_max(order_by = tx_curr, n = n_psnus) %>% 
    summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(coverage = ovc_hivstat_pos/tx_curr) %>% 
    pull()
  
  df_join %>% 
    filter(!is.na(tx_curr),
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
         caption = "Source: FY21Q2c MSD
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style_xgrid()
  
  si_save("Images/FY21Q2_OVC_coverage_psnu_tx_curr.png")
  
  df_join %>% 
    filter(!is.na(tx_curr),
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
                    family = "Source Sans Pro", color = trolley_grey, size = 8/.pt,) +
    scale_x_continuous(label = comma) +
    scale_y_continuous(label = comma) +
    scale_size(label = comma) +
    scale_color_si("scooter", na.value = trolley_grey) +
    labs(x = "TX_CURR (<20yo)", y = "OVC_HIVSTAT_POS (<=18yo)",
         size = "PLHIV", color = NULL,
         title = "COUNCILS WITH LARGER TREATMENTS VOLUMES TEND TO HAVE LESS OVC COVERAGE",
         caption = "Source: FY21Q2c MSD + NAT_SUBAT
         SI Analytics: Aaron Chafetz
         US Agency for International Development") +
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
  