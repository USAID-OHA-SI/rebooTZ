# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  partner visuals for JPPM
# LICENSE:  MIT
# DATE:     2022-02-22
# UPDATED:  2022-02-28

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
  library(selfdestructin5)
  library(gt)
  library(ggrepel)

# GLOBAL VARIABLES --------------------------------------------------------

  #caption info for plotting
  msd_source <- source_info()
  
  #current FY and quarter
  curr_pd <- source_info(return = "period")
  curr_fy <- source_info(return = "fiscal_year")
  curr_qtr <- source_info(return = "quarter")

  ptnrs <- c("THPS", "EGPAF")
  ptnrs_mech <- c("82164", "18060", "84911")
  
  #select indicators
  ind_sel <- c("HTS_INDEX",  "HTS_INDEX_NEWPOS", "HTS_TST", "HTS_TST_POS",
               "HTS_SELF", "PMTCT_STAT_D", "PMTCT_STAT", "PMTCT_STAT_POS",
               "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS")
  
# IMPORT ------------------------------------------------------------------
  
  df_psnu_im <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")


# MUNGE -------------------------------------------------------------------

  df_mech_sel <- df_psnu_im %>% 
    mutate(primepartner = case_when(primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                    primepartner == "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" ~ "THPS")) %>% 
    filter(!is.na(primepartner))
  
  rm(df_psnu_im)
  
# OVERALL PERFORMANCE -----------------------------------------------------

  #function - create MDB table
  print_ptnr_mdb <- function(ptnr_sel){
    mdb_tbl_ptnr <- df_mech_sel %>%
      filter(primepartner == ptnr_sel) %>% 
      make_mdb_df() %>% 
      reshape_mdb_df(curr_pd) %>% 
      filter(indicator != "GEND_GBV",
             operatingunit == "Tanzania") %>% 
      mutate(agency = ptnr_sel)
    
    mdb_tbl_ptnr %>% 
      gt(groupname_col = "agency") %>%
      mdb_main_theme(curr_pd, msd_source) %>% 
      tab_header(
        title = glue("{ptnr_sel} PERFORMANCE SUMMARY")) %>% 
      gtsave(path = "Images", filename = glue::glue("Tanzania_{ptnr_sel}_{curr_pd}_mdb_main.png"))
  }
  
  walk(ptnrs, print_ptnr_mdb)
 

# MUNGE - NAT/SNU ACHIEVEMENT ---------------------------------------------
  
  #subset to key indicators
  df_sub <- df_mech_sel %>% 
    filter(fiscal_year == curr_fy,
           indicator %in% ind_sel) %>% 
    clean_indicator()
  
  #aggregate to regional level
  df_achv <- df_sub %>% 
    bind_rows(df_sub %>% 
                mutate(snu1 = "NATIONAL",
                       snu1uid = "NATIONAL")) %>% 
    filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    group_by(fiscal_year, primepartner, snu1, snu1uid, indicator) %>% 
    summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
              .groups = "drop")
  
  #calculate achievement
  df_achv <- df_achv %>% 
    adorn_achievement(curr_qtr)
  
  #viz adjustments
  df_achv_viz <- df_achv %>% 
    complete(indicator, nesting(primepartner), fill = list(fiscal_year = curr_fy, snu1 = "NATIONAL")) %>% 
    mutate(natl_achv = case_when(snu1 == "NATIONAL" ~ achievement),
           achievement = ifelse(snu1 == "NATIONAL", NA, achievement),
           indicator = factor(indicator, ind_sel),
           baseline_pt_1 = 0,
           baseline_pt_2 = .25,
           baseline_pt_3 = .5,
           baseline_pt_4 = .75,
           baseline_pt_5 = 1,
    )
  #adjust facet label to include indicator and national values
  df_achv_viz <- df_achv_viz %>% 
    mutate(ind_w_natl_vals = case_when(snu1 == "NATIONAL" & is.na(targets) ~ 
                                         glue("**{indicator}**<br><span style = 'font-size:9pt;'>No MER reporting</span>"),
                                       snu1 == "NATIONAL" ~ 
                                         glue("**{indicator}**<br><span style = 'font-size:9pt;'>{comma(cumulative, 1)} / {comma(targets, 1)}</span>"))) %>% 
    group_by(primepartner, indicator) %>% 
    fill(ind_w_natl_vals, .direction = "downup") %>% 
    ungroup() %>% 
    arrange(primepartner, indicator) %>% 
    mutate(ind_w_natl_vals = fct_inorder(ind_w_natl_vals))

  
  plot_achv <- function(ptnr, export = TRUE){
    v <- df_achv_viz %>% 
      filter(primepartner == {ptnr}) %>% 
      ggplot(aes(achievement, indicator, color = achv_color)) +
      geom_blank() +
      geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
      geom_point(aes(baseline_pt_1), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_2), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_3), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_4), shape = 3, color = "#D3D3D3") +
      geom_point(aes(baseline_pt_5), shape = 3, color = "#D3D3D3") +
      geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                  alpha = .4, size = 3) +
      geom_point(aes(natl_achv), size = 8, alpha = .8, na.rm = TRUE) +
      geom_text(aes(natl_achv, label = percent(natl_achv, 1)), na.rm = TRUE,
                color = "#202020", family = "Source Sans Pro", size = 9/.pt) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(limit=c(0,1.1),oob=scales::squish) +
      scale_color_identity() +
      facet_wrap(~ind_w_natl_vals, scales = "free_y") +
      labs(x = NULL, y = NULL,
           title = glue("FY{str_sub(curr_fy, -2)}Q{curr_qtr} Tanzania | {ptnr}") %>% toupper,
           subtitle = glue("Partner achievement nationally (large, labeled points) with regional reference points<br>
                         <span style = 'font-size:11pt;color:{color_caption};'>Goal for {percent(.25*curr_qtr)} at Q{curr_qtr} (snapshot indicators pegged to year end target 100%)</span>"),
           caption = glue("Target achievement capped at 110%
                        Source: {msd_source}
                        US Agency for International Development")) +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown(),
            panel.spacing.y = unit(0, "lines"))
    
    if(export == TRUE){
      si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_TZA_Partner-Achievement_{ptnr}.png"))
    } else {
      return(v)
    }
    
  }
  
  walk(ptnrs, plot_achv)  

  rm(df_sub, df_achv, df_achv_viz, plot_achv)  

# POS ACHIEVEMENT ---------------------------------------------------------


  df_hts <- df_mech_sel %>% 
    filter(indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == curr_fy) %>% 
    group_by(fiscal_year, primepartner, psnu, indicator) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
    adorn_achievement(curr_qtr) %>% 
    arrange(desc(targets)) %>% 
    group_by(primepartner) %>% 
    mutate(target_share = targets/sum(targets, na.rm = TRUE),
           target_share_cum = cumsum(target_share)) %>% 
    ungroup()

  
  df_hts_viz <- df_hts %>% 
    rowwise() %>% 
    mutate(achievment_cap = min(1.1, achievement)) %>% 
    ungroup() %>% 
    mutate(fill_color = ifelse(target_share_cum < .51, denim, scooter),
           fill_alpha = ifelse(fill_color == denim, 1, .3),
           label_psnu = case_when(fill_color == denim ~ psnu))
    
  
  plot_achv_hts <- function(ptnr, export = TRUE){
    v <- df_hts_viz %>% 
      filter(primepartner == ptnr) %>% 
      ggplot(aes(psnu, achievment_cap, width = targets,
                 fill = fill_color, alpha = fill_alpha)) +
      geom_col(color = matterhorn) +
      geom_text_repel(aes(label = label_psnu), na.rm = TRUE,
                      family = "Source Sans Pro", color = nero,
                      point.padding = 0.02,
                      # nudge_x = .15,
                      nudge_y = .15,
                      segment.curvature = -1e-20,
                      force = 10,
                      arrow = arrow(length = unit(0.015, "npc")),
                      min.segment.length = 0, seed = 42) +
      facet_grid(~fct_reorder(psnu, achievement), scales = "free_x", space = "free_x") +
      scale_y_continuous(labels = percent,
                         breaks = seq(0, 1.25, by = .25)) +
      scale_fill_identity() +
      scale_alpha_identity() +
      coord_cartesian(clip = "off") +
      labs(x = "Council's share of HTS_POS targets", y = "Council Target Achievement",
           title = glue("FY{str_sub(curr_fy, -2)}Q{curr_qtr} Tanzania | {ptnr}") %>% toupper,
           subtitle = "HIV+ target achievement and share of total target distribution",
           caption = glue("Target achievement capped at 110%; Largest ouncils making up 50% of partner's HTS_POS targets filled dark blue
                        Source: {msd_source}
                        US Agency for International Development")) +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
            #axis.title.x = element_blank(),
            strip.text.x = element_blank(),
            panel.spacing.x = unit(.1, "line"))
    
    if(export == TRUE){
      si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_TZA_HTS-POS-Achievement_{ptnr}.png"))
    } else {
      return(v)
    }
  }

  walk(ptnrs, plot_achv_hts)  
  
  rm(df_hts, df_hts_viz, plot_achv_hts)  
  

# RETENTION ---------------------------------------------------------------

  df_ret <- df_mech_sel %>% 
    filter(indicator %in% c("TX_CURR", "TX_ML_IIT_less_three_mo", 
                            "TX_ML_IIT_more_three_mo", "TX_ML_died", 
                            "TX_ML_refused_stopped", "TX_ML_transferred_out", 
                            "TX_NET_NEW", "TX_NEW", "TX_RTT"),
           standardizeddisaggregate == "Total Numerator") %>% 
    mutate(ind = case_when(str_detect(indicator, "IIT") ~ "Interruption",
                           str_detect(indicator, "TX_ML") ~ "Other Loss",
                           indicator == "TX_NEW" ~ "New",
                           indicator == "TX_RTT" ~ "Returned",
                           TRUE ~ indicator)) %>% 
    group_by(fiscal_year, snu1, primepartner, ind) %>% 
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
    mutate(keep = case_when(period == max(period) ~ TRUE)) %>% 
    group_by(primepartner, snu1) %>% 
    fill(keep, .direction = "updown") %>% 
    ungroup() %>% 
    filter(keep == TRUE)
  
  df_ret <- df_ret %>% 
    mutate(ind = factor(ind, c("Interruption",  "Other Loss", "New", "Returned", "Unexplained Net New")))
  
  
  df_viz_ret <- df_ret %>%
    group_by(period, snu1, primepartner) %>% 
    mutate(row = dplyr::row_number()) %>% 
    ungroup() %>% 
    mutate(across(c(TX_NET_NEW, share_nn), ~ case_when(row == 1 ~ .)),
           share = case_when(primepartner == "Deloitte" & snu1 == "Ruvuma" & period == "FY20Q1" & ind == "Unexplained Net New" ~ NA_real_,
                             primepartner == "THPS" & period == "FY21Q2" & ind == "Unexplained Net New" ~ NA_real_,
                             TRUE ~ share),
           share_nn = case_when(primepartner == "Deloitte" & snu1 == "Ruvuma" & period == "FY20Q1" ~ NA_real_,
                                primepartner == "THPS" & period == "FY21Q2" ~ NA_real_,
                                TRUE ~ share_nn))
  
  
  
  print_viz_ret <- function(ptnr, export = TRUE){
    pds_brks <- df_viz_ret %>% 
      distinct(period) %>% 
      filter(str_detect(period, "Q(1|3)")) %>% 
      pull()
    
    v <- df_viz_ret %>% 
      filter(primepartner == ptnr,
             TX_CURR > 0) %>% 
      ggplot(aes(period, share, fill = ind)) +
      geom_col(alpha = .9) +
      geom_errorbar(aes(ymin = share_nn, ymax = share_nn), size = 1.2, na.rm = TRUE) +
      geom_hline(yintercept = 0) +
      facet_wrap(~fct_reorder2(snu1, period, TX_CURR)) +
      scale_y_continuous(label = percent_format(1)) +
      scale_x_discrete(breaks = pds_brks) +
      scale_fill_manual(values = c("Interruption" = si_palettes$burnt_siennas[5],
                                   "Other Loss" = si_palettes$burnt_siennas[4],
                                   "New" = si_palettes$scooters[5], 
                                   "Returned" = si_palettes$scooters[4], 
                                   "Unexplained Net New" = trolley_grey)) +
      labs(x = NULL, y = NULL, fill = NULL,
           title = glue("DROP IN RETENTION FOR {toupper(ptnr)} GOING INTO {curr_pd}"),
           subtitle = "Share of Current on Treatment | Overall Net New ( **\u2015**)",
           caption =  glue("Calculated from TX_CURR, TX_NEW, TX_ML, TX_RTT
                        Source: {msd_source}
                        US Agency for International Development")) +
      si_style_ygrid() +
      theme(panel.spacing.y = unit(.5, "line"),
            plot.subtitle = element_markdown())
    
    if(export == TRUE){
      ptnr %>% 
        tolower() %>% 
        str_remove_all(" ") %>% 
        paste0(curr_pd,"_TZA_retention_", ., ".png") %>% 
        si_save(path = "Images")
    } else {
      return(v)
    }
  }  
 
  
  # print_viz_ret("EGPAF", FALSE)
  walk(ptnrs, print_viz_ret)
  
  rm(df_ret, df_viz_ret, print_viz_ret)
  

# TX_GROWTH ---------------------------------------------------------------

  df_tx <- df_mech_sel %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, primepartner, snu1, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>%
    # mutate(across(starts_with("qtr"), ~ ifelse(fiscal_year == curr_fy & . == 0, -1, .))) %>% 
    reshape_msd("quarters") %>% 
    select(-results_cumulative) %>% 
    # mutate(results = ifelse(results == -1, NA_real_, results)) %>% 
    arrange(primepartner, snu1, period)

  df_tx_viz <- df_tx %>% 
    mutate(growth_rate_req = case_when(period == curr_pd ~ ((targets/results)^(1/(4-curr_qtr))) -1)) %>% 
    group_by(primepartner, snu1) %>% 
    fill(growth_rate_req, .direction = "updown") %>% 
    mutate(growth_rate = (results - lag(results, order_by = period))/lag(results, order_by = period)) %>% 
    ungroup() %>% 
    mutate(grr_lab = case_when(growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget already achieved"), 
                               # growth_rate_req < .1 ~ glue("{toupper(snu1)}\nQuarterly growth needed for remainder of {str_replace(curr_fy, '20', 'FY')}: {percent(growth_rate_req, .1)}"),
                               growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, .1)}"),
                               TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, .1)}")),
           gr_label_position = 0,
           disp_targets = case_when(fiscal_year == curr_fy ~ targets))
  
  print_viz_tx_gr <- function(ptnr, export = TRUE){
    v <- df_tx_viz %>% 
      filter(primepartner == ptnr) %>% 
      ggplot(aes(period, results, fill = as.character(fiscal_year))) +
      geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .3) +
      geom_col() +
      geom_text(aes(label = percent(growth_rate, .1), y = gr_label_position),
                family = "Source Sans Pro", color = matterhorn, size = 9/.pt, 
                vjust = 1.3, na.rm = TRUE) +
      geom_errorbar(aes(ymin = targets, ymax = targets), linetype = "dashed", width = .95) +
      facet_wrap(~fct_reorder2(grr_lab, period, targets)) +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = unique(df_tx$period)[grep("Q(1|3)", unique(df_tx$period))]) +
      scale_fill_si("moody_blues",discrete = TRUE) +
      labs(x = NULL, y = NULL,
           title = glue("What growth rate is needed to reach the {str_replace(curr_fy, '20', 'FY')} treatment targets for {ptnr}?") %>% toupper,
           subtitle = "TX_CURR by region | quarterly growth rates (below)",
           caption =  glue("Note: quarterly growth rate needed calculated as a compound annual growth rate
                         Source: {msd_source}
                        US Agency for International Development")) +
      si_style_ygrid() +
      theme(legend.position = "none")
    
    if(export == TRUE){
      ptnr %>% 
        tolower() %>% 
        str_remove_all(" ") %>% 
        paste0(curr_pd,"_TZA_tx-curr-growth_", ., ".png") %>% 
        si_save(path = "Images")
    } else {
      return(v)
    }
  }

  walk(ptnrs, print_viz_tx_gr)  
  
  rm(df_tx, df_tx_viz, print_viz_tx_gr)


# MMD TRENDS --------------------------------------------------------------

  #keep just TX_CURR/MMD
  df_mmd <- df_mech_sel %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    )) 
  
  #add in agency agg and reshape
  df_mmd <- df_mmd %>%
    group_by(fiscal_year, countryname, primepartner, psnu, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd <- df_mmd %>% 
    pivot_wider(names_from = otherdisaggregate,
                values_fill = 0) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(c(o3mmd, o6mmd),
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total) %>% 
    relocate(otherdisaggregate, .after = psnu) %>% 
    mutate(mmd_share = tx_mmd/tx_curr,
           date = convert_qtr_to_date(period),
           fiscal_year = str_sub(period, end = 4),
           disagg_label = ifelse(otherdisaggregate == "o3mmd", "3 or more months", "6 or more months"))
  
  df_mmd_nat <- df_mmd %>%
    group_by(fiscal_year, period, date, countryname, primepartner, otherdisaggregate, disagg_label) %>% 
    summarise(across(c(tx_curr, tx_mmd), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(mmd_share = tx_mmd/tx_curr,
           label_share = case_when(str_detect(period, glue("{curr_qtr}$")) ~ percent(mmd_share, 1))) %>% 
    arrange(primepartner, otherdisaggregate, period)
  

  
  print_viz_mmd <- function(ptnr, export = TRUE){
    df_mmd %>%
      filter(primepartner == ptnr) %>% 
      ggplot(aes(period, mmd_share, size = tx_curr)) +
      geom_hline(yintercept = 0) +
      geom_point(position = position_jitter(width = .1, height = 0, seed = 42),
                 color = genoa_light, alpha = .4, na.rm = TRUE) +
      # geom_smooth(aes(weight = tx_curr),
      #            se = FALSE, na.rm = TRUE, size = .9,
      #            method='lm', formula= y~x, color = genoa) +
      geom_point(data = df_mmd_nat %>% filter(primepartner == ptnr),
                 shape = 23, fill = genoa, color = "white", stroke = 1,
                 size = 5) +
      geom_text(data = df_mmd_nat %>% filter(primepartner == ptnr),
                aes(label = label_share),
                size = 10/.pt, hjust = -.8, color = nero,
                family = "Source Sans Pro", na.rm = TRUE) +
      facet_grid(disagg_label ~ fiscal_year, switch = "y", scales = "free_x", space = "free") +
      scale_y_continuous(label = percent) +
      scale_size(label = label_number_si()) +
      labs(x = NULL, y = "Share of treatment patients on MMD",
           title = glue("Large increases in 6 month dispensing (MMD) in FY22Q1 for {ptnr}") %>% toupper,
           subtitle = "Share of MMD by council (lighter points) and nationally (darker diamond)",
           size = "Patients on treatment (TX_CURR) in each council",
           caption =  glue("Source: {msd_source}
                        US Agency for International Development")) +
      si_style_ygrid() +
      theme(strip.placement = "outside",
            strip.text.y = element_text(hjust = .5, family = "Source Sans Pro SemiBold"))
    
    if(export == TRUE){
      ptnr %>% 
        tolower() %>% 
        str_remove_all(" ") %>% 
        paste0(curr_pd,"_TZA_mmd-share_", ., ".png") %>% 
        si_save(path = "Images")
    } else {
      return(v)
    }
    
  }
    
  walk(ptnrs, print_viz_mmd)  
    
  rm(df_mmd, df_mmd_nat, df_mmd_viz, print_viz_mmd)
  

# VIRAL LOAD --------------------------------------------------------------

  #access from https://pepfar.sharepoint.com/:f:/r/sites/ICPI/Products/ICPI%20Data%20Store/MER/FY22%20Q1%20Beta%20Datasets%20-%20Calculations%20Across%20Time/FY22%20Q1%20Initial%20(Feb%2018)?csf=1&web=1&e=b20E8z
  
  df_vl  <- si_path() %>% 
    return_latest("CalcAcrTime_flags") %>% 
    vroom::vroom(delim = "\t", col_types = c(.default = "c"))  %>% 
    filter(indicator %in% c("TX_CURR_Lag2", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
           mech_code %in% ptnrs_mech,
           period_code == max(period_code)) %>% 
    clean_indicator() %>% 
    mutate(period = period_code %>% 
             lubridate::yq() %>% 
             lubridate::quarter(fiscal_start = 10, with_year = TRUE) %>% 
             as.character() %>% 
             str_replace("20", "FY") %>% 
             str_replace("\\.", "Q"),
           value = quarterly_result %>% as.numeric %>% round,
           fiscal_year = as.numeric(fiscal_year)) 
  
  df_vl <- df_vl %>% 
    rename_official() %>% 
    mutate(primepartner = case_when(primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                    primepartner == "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" ~ "THPS"))
  
  df_tza_orgs <- Wavelength::pull_hierarchy("mdXu6iCbn2G",
                                            datim_user(), datim_pwd())
  
  df_vl  <- left_join(df_vl , df_tza_orgs) 
  
  rm(df_tza_orgs)
  
  df_vl <- df_vl %>% 
    select(snu1, psnu, orgunituid, mech_code, indicator, indicatortype,
           period, value, lag2_flag_safe_comparison,
           lag2_mech_change_type, lag2_change_desc) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") 
           
  df_vl <- df_vl %>% 
    filter(lag2_mech_change_type %in% c("OO", "OOC")) %>% 
    group_by(snu1, psnu, primepartner) %>% 
    summarise(across(c(tx_curr_lag2, tx_pvls_d, tx_pvls), sum, na.rm = TRUE),
              .groups = "drop")
    
  df_vl <- df_vl %>% 
    mutate(vlc = tx_pvls_d / tx_curr_lag2,
           vls = tx_pvls / tx_curr_lag2,
           not_covered = case_when(vlc < 1 ~ 1-vlc),
           psnu_lab = glue("{psnu} ({comma(tx_curr_lag2, 1)})"))
  
  print_viz_vl <- function(ptnr, export = TRUE){
    
    v <- df_vl %>% 
      filter(primepartner == ptnr) %>% 
      ggplot(aes(y = fct_reorder(psnu_lab, vlc, .desc = TRUE))) +
      geom_col(aes(x = 1), fill = "gray90") +
      geom_col(aes(x = vlc), fill = denim_light) +
      geom_col(aes(x = vls), fill = denim) +
      geom_text(aes(x = .98, label = percent(not_covered, 1), 
                    color = ifelse(not_covered >=.39, old_rose, "gray30")), 
                size = 2.5, family = "Source Sans Pro", na.rm = TRUE) +
      geom_vline(xintercept = c(.25, .5, .75), linetype = "dashed", color = "gray90") +
      scale_x_continuous(#breaks = seq(0, 1, .25),
        label = percent,expand = c(0.005, 0.005), position = "top") +
      scale_color_identity() +
      labs(x = NULL, y = NULL,
           title = glue("{curr_pd} VL <span style='color:{denim_light}'>COVERAGE</span> AND <span style='color:{denim}'>SUPPRESSION</span> ACROSS {ptnr} COUNCILS"),
           subtitle = "Coverage gap in gray and identified on right hand side",
           caption = "VLC = TX_PVLS_D / TX_CURR_Lag2; VLS = TX_PVLS / TX_CURR_Lag2
       Source: FY22Q1i MSD [CalcAcrTime_flags]
       US Agency for International Development") +
      si_style_nolines() +
      theme(axis.text.y = element_text(size = 10),
            plot.title = element_markdown())
    
    if(export == TRUE){
      ptnr %>% 
        tolower() %>% 
        str_remove_all(" ") %>% 
        paste0(curr_pd,"_TZA_vlcs_", ., ".png") %>% 
        si_save(path = "Images")
    } else {
      return(v)
    }
    
  }
  
  walk(ptnrs, print_viz_vl)  
  
  rm(df_vl, print_viz_vl)
  
  
  
    

# SITE NET_NEW ------------------------------------------------------------

  
  #access from https://pepfar.sharepoint.com/:f:/r/sites/ICPI/Products/ICPI%20Data%20Store/MER/FY22%20Q1%20Beta%20Datasets%20-%20Calculations%20Across%20Time/FY22%20Q1%20Initial%20(Feb%2018)?csf=1&web=1&e=b20E8z
  
  df_nn  <- si_path() %>% 
    return_latest("CalcAcrTime_flags.*txt") %>% 
    vroom::vroom(delim = "\t", col_types = c(.default = "c"))  %>% 
    filter(indicator %in% c("TX_CURR", "TX_NET_NEW_SHIFT"),
           standardizeddisaggregate == "Total Numerator",
           mech_code %in% ptnrs_mech,
           period_code == max(period_code)) %>% 
    clean_indicator() %>% 
    mutate(period = period_code %>% 
             lubridate::yq() %>% 
             lubridate::quarter(fiscal_start = 10, with_year = TRUE) %>% 
             as.character() %>% 
             str_replace("20", "FY") %>% 
             str_replace("\\.", "Q"),
           value = quarterly_result %>% as.numeric %>% round,
           fiscal_year = as.numeric(fiscal_year)) 
  
  df_nn <- df_nn %>% 
    rename_official() %>% 
    mutate(primepartner = case_when(primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                    primepartner == "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" ~ "THPS"))
    
  df_tza_orgs <- Wavelength::pull_hierarchy("mdXu6iCbn2G",
                                            datim_user(), datim_pwd())
  
  df_nn  <- left_join(df_nn , df_tza_orgs) 
  
  rm(df_tza_orgs)
  
  
  df_nn_share <- df_nn %>% 
    select(snu1, psnu, sitename, orgunituid, primepartner, indicator, indicatortype,
           period, value) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(share_nn = tx_net_new_shift/tx_curr,
           fill_color = ifelse(share_nn <= 0, burnt_sienna, scooter),
           lab_site = case_when(share_nn <= 0 ~ sitename))
  

  
  df_nn_share %>% 
    filter(primepartner == "THPS") %>% 
    ggplot(aes(share_nn, fct_reorder(snu1, tx_curr, sum), size = tx_curr, color = fill_color)) +
    geom_vline(xintercept = 0) +
    geom_point(position = position_jitter(width = 0, height = .3, seed = 42),
               alpha = .4, na.rm = TRUE) +
    geom_text(aes(label = lab_site), na.rm = TRUE, size = 11/.pt,
               family = "Source Sans Pro", color = "#505050") +
    scale_x_continuous(labels = percent_format(1), 
                       breaks = seq(-.3, 1, .1)) +
    scale_size(label = label_number_si()) +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "MINIMAL NET NEW GROWTH FOR THPS WITH A NUMBER OF SITES WITH LOSSES",
         subtitle = "TX_NET_NEW (Adjusted) share of TX_CURR by facility",
         size = "Patients on treatment (TX_CURR) in each council",
         caption = "TX_NET_NEW_SHIFT used
         Source: FY22Q1i MSD [CalcAcrTime_flags]
       US Agency for International Development") +
    si_style()
  
  
  df_nn_share %>% 
    filter(primepartner == "THPS") %>% 
    count(wt = tx_net_new_shift)
  
    si_save("Graphics/FY22Q1_TZA_nn_thps.svg")

        
    df_nn_share %>% 
      filter(primepartner == "THPS",
             snu1 == "Kilimanjaro") 
    