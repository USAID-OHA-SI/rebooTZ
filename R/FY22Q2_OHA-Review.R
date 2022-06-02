# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  FY22Q2 review visuals
# LICENSE:  MIT
# DATE:     2022-06-02
# UPDATED: 

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
  library(Wavelength)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ind_prev <- c("GEND_GBV", "KP_PREV","OVC_SERV_UNDER_18", 
                "PP_PREV", "PrEP_NEW", #"TB_PREV_D", 
                "TB_PREV", 
                #"TB_STAT_D", 
                "TB_STAT", "VMMC_CIRC")
  ind_test <- c("HTS_TST",  "HTS_TST_POS", "HTS_SELF")
  ind_treat <- c("TX_CURR", "TX_NEW")
  ind_vl <- c("TX_PVLS_D", "TX_PVLS")
  
  ind_all <- c(ind_prev, ind_test, ind_treat, ind_vl)
  
  msd_source <- source_info()
  curr_pd <- source_info(return = "period")
  curr_fy <- source_info(return = "fiscal_year")
  curr_qtr <- source_info(return = "quarter")
  
  hfr_path <- "../Wavelength/out/joint/HFR_Tableau_SQLview.csv"
  
  hfr_source <- file.info(hfr_path)$mtime %>% format("%Y-%m-%d")
  
  genie_path_site <- file.path(si_path(), "Genie-SiteByIMs-Tanzania-Daily-2022-05-11.zip")
  
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_msd()   
  

  df_hfr <- hfr_read(hfr_path) %>% 
    filter(operatingunit == "Tanzania",
           indicator == "TX_CURR")
  
  df_site <- read_msd(genie_path_site)
  
# MUNGE -------------------------------------------------------------------

  df_achv <- df %>% 
    clean_indicator() %>% 
    filter(operatingunit == "Tanzania",
           funding_agency == "USAID",
           fiscal_year == curr_fy, 
           indicator %in% ind_all) %>% 
    pluck_totals()

  df_achv <- df_achv %>% 
    group_by(funding_agency, fiscal_year, indicator) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>%
    adorn_achievement(curr_qtr)

  df_tbl <- df_achv %>% 
    mutate(category = case_when(indicator %in% ind_prev ~ "Prevention",
                                indicator %in% ind_test ~ "Test",
                                indicator %in% ind_treat ~ "Treat",
                                indicator %in% ind_vl ~ "Viral Load"),
           # flag = glue('<span style=\"font-size:30pt;color:{achv_color}\">&#8226;</span>')) %>% 
           flag = case_when(achv_color %in% c("#ff939a", "#ffcaa2") ~ 
                              glue('<span style=\"color:{achv_color}\">&#9650;</span>')),
           # indicator = factor(indicator, ind_all)
           ) %>% 
    select(category, indicator, targets, cumulative, achievement, flag)
    
  df_tbl %>% 
    # group_by(category) %>% 
    gt(groupname_col = "category") %>% 
    tab_header(glue("{curr_pd} USAID/Tanzania")) %>% 
    tab_source_note(glue("Source: {msd_source}")) %>% 
    fmt_number(columns = c(targets, cumulative),
               decimals = 0) %>% 
    fmt_percent(columns = achievement, 
                decimals = 0) %>% 
    fmt_markdown(columns = flag) %>%
    fmt_missing(columns = flag, missing_text = "") %>%
    cols_label(
      indicator = "",
      targets = "FY Targets",
      cumulative = glue("{curr_pd} Cum."),
      achievement = "Achv.",
      flag = ""
    ) %>% 
    tab_style(style = cell_text(indent = "12px"),
              locations = cells_body(indicator)) %>% 
    opt_table_font(font = 
      google_font(name = "Source Sans Pro")
    )  %>% 
    gtsave(glue("Images/{curr_pd}_TZA_Achv-tbl.png"))

# TREATMENT ---------------------------------------------------------------

  df_tx <- df %>% 
    filter(operatingunit == "Tanzania",
           funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           fiscal_year != 2020) %>% 
    pluck_totals() %>% 
    group_by(funding_agency, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE)
  
  
  df_tx <- df_tx %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")

  si_palettes$scooters %>% show_col()
  
  df_tx %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = tx_curr), fill = si_palettes$scooters[4], alpha = .8) +
    geom_col(aes(y = tx_new), fill = si_palettes$scooters[7]) +
    geom_errorbar(aes(ymin = tx_net_new, ymax = tx_net_new), color = golden_sand, size = 1.4) +
    scale_y_continuous(labels = label_number_si()) +
    labs(x = NULL, y = NULL,
         subtitle = "USAID/Tanzania\nTX_CURR | TX_NEW | TX_NET_NEW",
         caption = glue("Source: {msd_source}")) +
    si_style_ygrid()
  
  si_save(glue("Graphics/{curr_pd}_TZA_tx.svg"), width = 5)
  
  

# HFR TREATMENT -----------------------------------------------------------

  df_hfr_tx <- df_hfr %>% 
    mutate(primepartner = case_when(primepartner == "DELOITTE CONSULTING LIMITED" ~ "Deloitte",
                                    primepartner == "ELIZABETH GLASER PEDIATRIC AIDS FOUNDATION" ~ "EGPAF")) %>% 
    filter(!is.na(primepartner)) %>% 
    count(date, primepartner, indicator, wt = val, name = "value")

  df_hfr_tx %>% 
    ggplot(aes(date, value)) +
    geom_area(color = si_palettes$scooters[4], fill = si_palettes$scooters[4], alpha = .8) +
    geom_vline(xintercept = as.Date("2021-10-01"), 
               color = "white", linetype = "dashed") +
    geom_point(shape = 21, fill = si_palettes$scooters[4], color = "white") +
    facet_wrap(~primepartner) +
    # facet_wrap(~primepartner, scales = "free") +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(label = label_number_si()) +
    labs(x = NULL, y = NULL,
         subtitle = "USAID/Tanzania HFR | TX_CURR",
         caption = glue("Source: HFR [{hfr_source}]")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "lines"))
  
  si_save(glue("Images/{curr_pd}_TZA_tx.png"), width = 5, height = 2)

# IIT ---------------------------------------------------------------------


  df_iit <- df_site %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_ML", "TX_CURR", "TX_NEW"),
           facility != "Data reported above Facility level") %>%
    pluck_totals() %>%
    group_by(fiscal_year, snu1, facility, facilityuid, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit <- df_iit %>%
    group_by(facilityuid) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  
  v_lrg <- df_iit %>% 
    filter(period == max(period)) %>% 
    count(snu1, wt = tx_ml) %>% 
    slice_max(order_by = n, n = 5) %>% 
    pull(snu1)
  

  
  pd_brks <- df_iit %>% 
    distinct(period) %>% 
    filter(period != min(period)) %>% 
    mutate(period = str_replace(period, "FY.*(1|3)$", "")) %>% 
    pull()
  
  df_iit %>% 
    filter(snu1 %in% v_lrg,
      tx_curr_lag1 != 0) %>% 
    mutate(snu1 = factor(snu1, v_lrg)) %>%
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = scooter,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = snu1),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5, color = golden_sand) +
    facet_grid(~snu1) +
    scale_size(label = comma, guide = NULL) +
    scale_x_discrete(labels = pd_brks) +
    scale_y_continuous(limits = c(0,.25),
                       label = percent_format(1),
                       oob = oob_squish) +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         # title = glue("Sizable IIT continue into {curr_pd}") %>% toupper,
         subtitle = glue("Site IIT calculated in the {length(v_lrg)} largest TX_ML regions, FY21Q2-FY22Q2"),
         caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {msd_source}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{curr_pd}_TZA_region_iit_lim.png"), width = 5, height = 2)
  
  