# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  develop visuals for FY22Q2 review
# LICENSE:  MIT
# DATE:     2022-05-06
# UPDATED:  2022-05-11

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
  library(gt)
  library(selfdestructin5)
  library(fontawesome)
  library(lubridate)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  genie_path <- file.path(si_path(), "Genie-PSNUByIMs-Tanzania-Daily-2022-05-11.zip")
  
  msd_source <- source_info(genie_path)
  curr_pd <- source_info(genie_path, return = "period")
  curr_fy <- source_info(genie_path, return = "fiscal_year")
  curr_qtr <- source_info(genie_path, return = "quarter")

  #indicators used
  # c("HTS_TST_POS", "OVC_SERV", "PMTCT_EID", "TB_PREV", "TB_STAT", "TB_STAT_POS", "TX_CURR", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW", "TX_NEW", "TX_PVLS", "TX_PVLS", "TX_PVLS_D")

# IMPORT ------------------------------------------------------------------
  
  # df <- si_path() %>% 
  #   return_latest("PSNU_IM") %>% 
  #   read_msd() %>% 
  #   filter(operatingunit == "Tanzania")
  
  df <- read_msd(genie_path)

# PERIODS -----------------------------------------------------------------

  full_pds <- (min(df$fiscal_year) - 1) %>% 
    paste0("-10-01") %>% 
    as_date() %>% 
    seq.Date(convert_qtr_to_date(curr_pd), by = "3 months") %>% 
    convert_date_to_qtr()
  
  pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")
    
# TARGET ACHIEVEMENT ------------------------------------------------------


  # Main Table
  # Create the long mdb_df of the main summary indicators 
  # This will remove mechs with known issues by default. If you want to keep all mechs set `resolve_issues == FALSE`
  mdb_df   <- make_mdb_df(df)
  
  # Create the reshaped df that is gt() ready
  mdb_tbl  <- reshape_mdb_df(mdb_df, curr_pd)
  
  mdb_tbl %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Tanzania", type = "main", curr_pd, msd_source) %>% 
    gtsave(path = "Images", filename = glue::glue("Tanzania_{curr_pd}_mdb_main.png"))
  
  mdb_tbl_tza <- df %>%
    mutate(funding_agency = "USAID") %>% 
    make_mdb_df() %>% 
    reshape_mdb_df(curr_pd) %>% 
    filter(#indicator != "GEND_GBV",
           operatingunit == "Tanzania") %>% 
    mutate(agency = "PEPFAR")
  
  mdb_tbl_tza %>% 
    gt(groupname_col = "agency") %>%
    mdb_main_theme(curr_pd, msd_source) %>% 
    tab_header(
      title = "TANZANIA PERFORMANCE SUMMARY") %>% 
    gtsave(path = "Images", filename = glue::glue("Tanzania_PEPFAR_{curr_pd}_mdb_main.png"))
  

# Target Achievement ------------------------------------------------------

    
  viz_achv_age <- function(ind, disagg, status, top_n, export = TRUE){
    
    
    df_ach <- df %>% 
      filter(funding_agency == "USAID",
             indicator %in% ind,
             standardizeddisaggregate == disagg,
             fiscal_year == curr_fy) %>%
      mutate(ageasentered = case_when(indicator == "OVC_SERV" ~ ageasentered,
                                      trendscoarse == "<15" ~ "<15",
                                      TRUE ~ ageasentered)) 
    
    if(!missing(status))
      df_ach <- filter(df_ach, statushiv == status)
    
    df_ach_usaid <- df_ach %>% 
      group_by(fiscal_year, indicator) %>% 
      summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
      adorn_achievement(curr_qtr)
    
    v_snu <- df_ach %>% 
      count(snu1, wt = targets, sort = T) %>%
      slice_head(n = top_n) %>% 
      pull(snu1)
    
    df_ach <- df_ach %>%
      filter(snu1 %in% v_snu) %>% 
      group_by(snu1, indicator, fiscal_year, ageasentered) %>% 
      summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
      adorn_achievement(curr_qtr) %>% 
      mutate(snu1 = factor(snu1, v_snu)) %>% 
      arrange(snu1)
    
    if(ind %in% snapshot_ind)
      curr_qtr <- 4
    
    df_ach <- df_ach %>% 
      group_by(snu1) %>% 
      mutate(snu1_achv = sum(cumulative, na.rm = TRUE)/sum(targets, na.rm = TRUE),
             disp_name = glue("{snu1}<br>{percent(snu1_achv, 1)} ({comma(sum(cumulative, na.rm = TRUE))}/{comma(sum(targets, na.rm = TRUE))})"),
             disp_name = ifelse(snu1_achv <=((curr_qtr*.25)-.1), glue("**{disp_name}**"), disp_name)) %>% 
      ungroup()
    
    if(ind == "TB_STAT")
      ind <- "TB_STAT_POS"
    
    v <- df_ach %>% 
      filter(ageasentered != "Unknown Age") %>% 
      mutate(targets_rng_min = targets * ((.25*curr_qtr) - .1),
             targets_rng_max = targets * ((.25*curr_qtr) + .1)) %>% 
      ggplot(aes(cumulative, ageasentered)) +
      # geom_errorbar(aes(xmin = targets*.25, xmax = targets*.25), color = suva_grey, width = .25) +
      geom_linerange(aes(xmin = targets_rng_min, xmax = targets_rng_max), color = suva_grey) +
      geom_point(aes(fill = achv_color), size = 4,
                 shape = 21, color = "white",
                 na.rm = TRUE) +
      facet_wrap(~fct_reorder(disp_name, targets, sum, na.rm = TRUE, .desc = TRUE)) +
      scale_x_continuous(labels = comma, expand = c(.005, .005)) +
      expand_limits(x = 0) +
      scale_fill_identity() +
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL,
           title = glue("IN {curr_pd}, USAID REACHED {percent(df_ach_usaid$achievement)} OF ITS {ind} TARGET"),
           subtitle = glue("Regions ordered by target volume | Underaching regions bolded | Lines represent goal range at {curr_pd}"),
           caption = glue("Source: {msd_source}")) +
      si_style_xgrid() +
      theme(panel.spacing = unit(.5, "line"),
            strip.text = element_markdown())
    
    if(export == TRUE){
      si_save(glue("Images/{curr_pd}_TZA_achv_{ind}.png"))
    } else {
      v
    }
  }
  
  df <- clean_indicator(df)
  
  df <- df %>% 
    mutate(standardizeddisaggregate = ifelse(indicator == "OVC_SERV" & standardizeddisaggregate != "Total Numerator", "Age/Sex/All", standardizeddisaggregate),
           standardizeddisaggregate = ifelse(indicator == "TB_PREV" & standardizeddisaggregate == "Age/NewExistingArt/HIVStatus", "Age/Sex/NewExistingArt/HIVStatus", standardizeddisaggregate))

  df %>%
    filter(indicator == "TB_PREV",
           fiscal_year == curr_fy) %>%
    count(standardizeddisaggregate, #otherdisaggregate, statushiv,
          wt = cumulative)
  count(snu1, wt = targets, sort = T)

  df %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Age/Sex/All",
           fiscal_year == curr_fy,
           funding_agency == "USAID") %>%
    count(snu1, wt = targets, sort = T) %>%
    prinf()
    
  viz_achv_age("HTS_TST_POS", "Modality/Age/Sex/Result", top_n = 12)
  viz_achv_age("TX_NEW", "Age/Sex/HIVStatus", top_n = 11)
  viz_achv_age("TX_PVLS_D", "Age/Sex/Indication/HIVStatus", top_n = 11)
  viz_achv_age("TX_PVLS", "Age/Sex/Indication/HIVStatus", top_n = 11)
  viz_achv_age("TB_STAT", "Age/Sex/KnownNewPosNeg", status = "Positive", top_n = 9)
  viz_achv_age("TB_PREV_D", "Age/NewExistingArt/HIVStatus", top_n = 12)
  viz_achv_age("TB_PREV", "Age/Sex/NewExistingArt/HIVStatus", top_n = 12)
  viz_achv_age("OVC_SERV", "Age/Sex/All", top_n = 12) #25
  

# TX_CURR TRENDS ----------------------------------------------------------


  df_tx <- df %>% 
    filter(funding_agency == "USAID",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd("quarters") %>% 
    select(-results_cumulative)

  df_tx <- df_tx %>% 
    group_by(snu1) %>%
    mutate(decline = results < lag(results, 1),
           decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
           fill_color = case_when(fiscal_year < curr_fy ~ trolley_grey,
                                 decline == TRUE ~ golden_sand,
                                 TRUE ~ scooter),
           fill_alpha = ifelse(fiscal_year < curr_fy, .6, .9),
           results_latest = case_when(period == max(period) ~ results),
           decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
    fill(results_latest,decline_latest, .direction = "up") %>% 
    mutate(disp_name = glue("{snu1} {decline_latest}")) %>% 
    ungroup() 
    
  v_tx_lrg <- df_tx %>% 
    filter(period == max(period)) %>% 
    arrange(desc(results)) %>% 
    mutate(cumsum = cumsum(results)/sum(results)) %>% 
    slice_head(n = 11) %>% 
    pull(snu1)
  
  df_tx %>%
    filter(snu1 %in% v_tx_lrg) %>% 
    ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
    geom_col() +
    geom_text(data = . %>% filter(period == max(period)), 
              aes(label = label_number_si()(results_latest)), 
              vjust = -.7, color = matterhorn,
              family = "Source Sans Pro") +
    facet_wrap(~fct_reorder2(disp_name, period, results)) +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_y_continuous(label = label_number_si()) +
    scale_x_discrete(labels = pd_brks) +
    coord_cartesian(expand = T, clip = "off") +
    labs(x = NULL, y = NULL, 
         title = "DECLINES IN TX_CURR REVERSED COURSE IN FY22Q2 IN MANY REGIONS",
         subtitle = glue("TX_CURR trends in largest {length(v_tx_lrg)} regions | FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
         caption = glue("Source: {msd_source}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_region_txcurr.png"))
  
# NET NEW -----------------------------------------------------------------
  
  df_nn <- df %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           snu1 %in% v_tx_lrg) %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    pivot_longer(c(tx_net_new, tx_new), 
                 names_to = "indicator") %>% 
    mutate(fill_color = ifelse(indicator == "tx_net_new", old_rose, denim),
           indicator = glue("{toupper(indicator)}"),
           # indicator = glue("{toupper(indicator)} share of TX_CURR"),
           share = value / tx_curr)
  
  # df_nn %>%
  #   filter(period != min(period)) %>%
  #   ggplot(aes(period, share, fill = fct_rev(indicator))) +
  #   geom_col(alpha = .75,
  #            position = position_dodge(width = .4)) +
  #   geom_hline(yintercept = 0) +
  #   facet_wrap(~fct_reorder2(snu1, period, tx_curr)) +
  #   scale_x_discrete(label = pd_brks[2:length(pd_brks)]) +
  #   scale_y_continuous(label = percent_format(1),
  #                      breaks = seq(-.1, .2, .05)) +
  #   scale_fill_manual(values = c("TX_NEW share of TX_CURR" = scooter,
  #                                "TX_NET_NEW share of TX_CURR" = scooter_light)) +
  #   labs(x = NULL, y = NULL, fill = NULL,
  #        title = "Lower TX_NEW (as a share of TX_CURR) in FY22 not making up for NET_NEW losses" %>% toupper,
  #        caption = glue("Source: {msd_source}")) +
  #   si_style_ygrid() +
  #   theme(panel.spacing = unit(.5, "line"),
  #         strip.text = element_markdown())
  
  df_nn %>%
    filter(period != min(period)) %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .4)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~fct_reorder2(snu1, period, tx_curr)) +
    scale_y_continuous(label = comma) +
    scale_x_discrete(label = pd_brks[2:length(pd_brks)]) +
    scale_fill_manual(values = c("TX_NEW" = scooter,
                                 "TX_NET_NEW" = scooter_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "Lower TX_NEW in FY22 not making up for NET_NEW losses" %>% toupper,
         caption = glue("Source: {msd_source}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          strip.text = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_region_tx-new-nn.png"))
  
# VLC/S -------------------------------------------------------------------

  df_vl <- df %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
           ageasentered != "Unknown Age") %>% 
    mutate(ageasentered = case_when(trendscoarse == "<15" ~ trendscoarse,
                                    ageasentered %in% c("50-54","55-59", "60-64", "65+") ~ "50+",
                                    TRUE ~ ageasentered)) %>% 
    clean_indicator() %>% 
    group_by(snu1, indicator, ageasentered, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")
  
  df_vl <- df_vl %>% 
    arrange(snu1, ageasentered, period) %>% 
    group_by(snu1, ageasentered) %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls / tx_pvls_d,
           vls_adj = tx_pvls /tx_curr_lag2) %>% 
    ungroup() %>% 
    filter(period == max(period))
  
  df_usaid_vl <- df_vl %>% 
    summarise(vlc = sum(tx_pvls_d, na.rm = TRUE)/sum(tx_curr_lag2, na.rm = TRUE),
              vls = sum(tx_pvls, na.rm = TRUE) / sum(tx_pvls_d, na.rm = TRUE),
              vls_adj = sum(tx_pvls, na.rm = TRUE) /sum(tx_curr_lag2, na.rm = TRUE))
        
        
  df_vl %>% 
    filter(snu1 %in% v_tx_lrg) %>% 
    mutate(tx_curr_pct = 1) %>% 
    ggplot(aes(tx_curr_pct, ageasentered)) +
    geom_col(fill = "#E5E5E5") +
    geom_col(aes(vlc), fill = scooter_light) +
    geom_col(aes(vls_adj), fill = scooter) +
    # geom_richtext(aes(label = glue("<span style='color:#2166ac'>{percent(vls_adj, 1)}</span> | <span style='color:#67a9cf'>{percent(vlc, 1)}</span>")), 
    #               label.color = NA,
    #           nudge_x = .2,
    #           family = "Source Sans Pro") +
    geom_richtext(aes(vls_adj, label = glue("<span style='color:white'>{percent(vls_adj, 1)}</span>")), 
                  label.color = NA, fill = NA,
                  nudge_x = -.08, size = 3,
                  family = "Source Sans Pro") +
    geom_richtext(aes(vlc, label = glue("<span style='color:#505050'>{percent(vlc, 1)}</span>")), 
                  label.color = NA, fill = NA,
                  nudge_x = .08, size = 3,
                  family = "Source Sans Pro") +
    facet_wrap(~fct_reorder(snu1, tx_curr_lag2, sum, na.rm = TRUE,.desc = TRUE)) +
    coord_cartesian(expand = T, clip = "off") +
    scale_x_continuous(label = percent) +
    labs(x = NULL, y = NULL, 
         title = glue("While VLS was {percent(df_usaid_vl$vls, 1)} in {curr_pd}, VLC remains low only at {percent(df_usaid_vl$vlc, 1)}") %>% toupper,
         subtitle = glue("<span style='color:{scooter_light}'>VLC</span> and <span style='color:{scooter}'>VLS</span> rates out of patients on TX_CURR 2 periods prior | Largest {length(v_tx_lrg)} TX_CURR regions"),
         caption = glue("Source: {msd_source}")) +
    si_style_nolines() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          axis.text.x = element_blank())
  
  si_save(glue("Images/{curr_pd}_TZA_region_vl.png"))

# EID COVERAGE ------------------------------------------------------------

  df_eid <- df %>% 
    filter(funding_agency == "USAID",
           indicator == "PMTCT_EID",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    clean_indicator() %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(coverage = pmtct_eid / pmtct_eid_d,
           low_pnt = case_when(coverage < .95 ~ coverage),
           fill_color = ifelse(coverage < .95, old_rose, "#505050"))
  
  
  v_eid_lrg <- df_eid %>% 
    filter(period == max(period)) %>% 
    arrange(desc(pmtct_eid_d)) %>% 
    mutate(cumsum = cumsum(pmtct_eid_d)/sum(pmtct_eid_d)) %>% 
    slice_head(n = 11) %>% 
    pull(snu1)
  
  df_eid %>% 
    filter(snu1 %in% v_eid_lrg) %>% 
    ggplot(aes(period, coverage, group = snu1)) +
    geom_hline(yintercept = .95, linetype = "dashed") +
    geom_line(na.rm = TRUE) +
    geom_point(aes(y = low_pnt), shape = 21, size = 4, 
               fill = "white", color = old_rose,
               na.rm = TRUE) +
    geom_point(aes(color = fill_color), na.rm = TRUE) +
    facet_wrap(~fct_reorder(snu1, pmtct_eid_d, sum, na.rm = TRUE, .desc = TRUE)) +
    scale_y_continuous(label = percent) +
    scale_x_discrete(labels = pd_brks) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("Most regions have falled below the goal coverage rate of 95% in most of the larger regions") %>% toupper,
         subtitle = glue("PMTCT_EID coverage in the largest {length(v_eid_lrg)} PMTCT_EID regions"),
         caption = glue("Source: {msd_source}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"))
  
  si_save(glue("Images/{curr_pd}_TZA_region_eid.png"))
  
# MMD ---------------------------------------------------------------------

  #keep just TX_CURR/MMD
  df_mmd <- df %>% 
    filter(funding_agency == "USAID",
           indicator == "TX_CURR",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    )) 
  
  #add in agency agg and reshape
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
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(-c(period, snu1,indicator, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total)
  
  
  #country trends for just o3mo
  df_mmd <- df_mmd %>% 
    arrange(snu1, otherdisaggregate, period) 
  
  #create share on +3mo
  df_mmd <- df_mmd %>% 
    mutate(share = tx_mmd/tx_curr) 
  
  #data points for plotting
  df_mmd <- df_mmd %>% 
    mutate(max_tx = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_curr, 0),
           max_mmd = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_mmd, 0)) %>% 
    group_by(snu1) %>% 
    mutate(endpoints = case_when(period %in% c(max(period), min(period))~share),
           max_tx = max(max_tx),
           max_mmd = max(max_mmd)) %>% 
    ungroup() %>% 
    mutate(country_lab = case_when(max_tx == max(max_tx) ~ 
                                     glue("{snu1}<br><span style = 'font-size:8pt'>{label_number_si()(max_mmd)} / {label_number_si()(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                   TRUE ~ glue("{snu1}<br><span style = 'font-size:8pt'>{label_number_si()(max_mmd)} / {label_number_si()(max_tx)}</span>")),
           country_lab = str_replace(country_lab, "NA", "0")) %>% 
    filter(max_tx > 0)
  
  
  df_mmd <- df_mmd %>% 
    mutate(fill_color = case_when(country == "USAID" & otherdisaggregate == "o6mmd" ~ "#0f4453",
                                  country == "USAID" ~ "#78b7c9",
                                  otherdisaggregate == "o6mmd" ~ scooter,
                                  TRUE ~ scooter_light),
           lab_share = case_when(country == "USAID" & period == max(period) ~ share))
  

# TB_PREV -----------------------------------------------------------------

  df_tb_prev <- df %>% 
    filter(funding_agency == "USAID",
           indicator == "TB_PREV",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    clean_indicator() %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(across(c(tb_prev, tb_prev_d), ~ na_if(., 0)),
           coverage = tb_prev / tb_prev_d,
           low_pnt = case_when(coverage < .85 ~ coverage),
           fill_color = ifelse(coverage < .85, old_rose, "#505050"))
  
  
  v_tb_prev_lrg <- df_tb_prev %>% 
    filter(period == max(period)) %>% 
    arrange(desc(tb_prev_d)) %>% 
    mutate(cumsum = cumsum(tb_prev_d)/sum(tb_prev_d)) %>% 
    slice_head(n = 11) %>% 
    pull(snu1)

  df_tb_prev %>% 
    filter(snu1 %in% v_tb_prev_lrg,
           !is.na(tb_prev_d)) %>% 
    ggplot(aes(period, coverage, group = snu1)) +
    geom_hline(yintercept = .85, linetype = "dashed") +
    geom_step(na.rm = TRUE) +
    geom_point(aes(y = low_pnt), shape = 21, size = 4, 
               fill = "white", color = old_rose,
               na.rm = TRUE) +
    geom_point(aes(color = fill_color), na.rm = TRUE) +
    facet_wrap(~fct_reorder(snu1, tb_prev_d, sum, na.rm = TRUE, .desc = TRUE)) +
    scale_y_continuous(label = percent_format(1)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL, 
         title = glue("USAID saw signficant drps in TB_PREV coverage in {curr_pd}"),
         subtitle = glue("Largest {length(v_tb_prev_lrg)} regions for TB_PREV_D"),
         caption = glue("Source: {msd_source}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"))

  si_save(glue("Images/{curr_pd}_TZA_region_tb-prev.png"))
  
# TB_STAT -----------------------------------------------------------------

  
  df_tb_stat <- df %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TB_STAT", "TB_STAT_POS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    clean_indicator() %>% 
    group_by(snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    mutate(coverage = tb_stat/tb_stat_d,
           positivity = tb_stat_pos / tb_stat,
           lab_pos = 190)
    # mutate(fill_color = ifelse(indicator == "tx_net_new",old_rose, denim),
    #        indicator = glue("{toupper(indicator)} share of TX_CURR"),
    #        share = value / tx_curr)
  
  
  v_tb_stat_lrg <- df_tb_stat %>% 
    filter(period == max(period)) %>% 
    arrange(desc(tb_stat)) %>% 
    mutate(cumsum = cumsum(tb_stat)/sum(tb_stat)) %>%
    # prinf()
    slice_head(n = 10) %>% 
    pull(snu1)
  
  df_tb_stat %>%
    filter(snu1 %in% v_tb_stat_lrg) %>% 
    ggplot(aes(period, tb_stat_pos)) +
    geom_col(fill = scooter, alpha = .8) +
    geom_label(aes(y = lab_pos, label = percent(positivity, 1)), 
               color = matterhorn, label.size = NA,
               na.rm = TRUE) +
    facet_wrap(~fct_reorder2(snu1, period, tb_stat_pos)) +
    scale_x_discrete(label = pd_brks) +
    scale_y_continuous(label = comma) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "Stable TB_STAT positivity in FY22",
         subtitle = glue("Largest {length(v_tb_stat_lrg)} regions for <span style='color:{scooter}'>TB_STAT</span>"),
         caption = glue("Source: {msd_source}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_region_tb-stat.png"))

# IIT ---------------------------------------------------------------------

  genie_path_site <- file.path(si_path(), "Genie-SiteByIMs-Tanzania-Daily-2022-05-11.zip")
  df_site <- read_msd(genie_path_site)
  
  df_iit <- df_site %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_CURR_Lag1", "TX_RTT"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, snu1, facility, facilityuid, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_iit %>% 
    filter(snu1 %in% v_tx_lrg,
           tx_curr_lag1 != 0) %>% 
    mutate(snu1 = factor(snu1, v_tx_lrg)) %>% 
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = scooter,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = snu1),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5, color = golden_sand) +
    facet_wrap(~snu1) +
    scale_size(label = comma) +
    scale_x_discrete(labels = pd_brks) +
    scale_y_continuous(limits = c(0,.25),
                       label = percent_format(1),
                       oob = oob_squish) +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Sizable IIT continue into {curr_pd}") %>% toupper,
         subtitle = glue("IIT calculated in the largest {length(v_tx_lrg)} TX_CURR regions"),
         caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown())
  
    si_save(glue("Images/{curr_pd}_TZA_region_iit.png"))
  
  
  
  
  
  
  
  
  
  
  