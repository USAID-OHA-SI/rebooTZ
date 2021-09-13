# PROJECT:  rebooTZ 
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-09-13
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  df_archv <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_rds() 
  
  
  
  df_arch_tza <- df_archv %>% 
    filter(operatingunit == "Tanzania",
           indicator %in% c("OVC_HIVSTAT"),
           standardizeddisaggregate == "StatusPosART")
  
  df_tza <- df %>% 
    filter(operatingunit == "Tanzania",
           indicator %in% c("OVC_HIVSTAT"),
           statushiv == "Positive"))
  
  
  df_viz <- df_tza %>%
    bind_rows(df_arch_tza) %>%
    group_by(countryname, fiscal_year, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(qtr3 = ifelse(fiscal_year == 2021 & otherdisaggregate == "Not Receiving ART", 50100, qtr3)) %>% 
    reshape_msd() %>% 
    group_by(period) %>%
    mutate(ovc_hivstat_pos = sum(value)) %>% 
    filter(otherdisaggregate == "Receiving ART") %>% 
    select(-otherdisaggregate) %>% 
    rename(receiving_art = value) %>% 
    mutate(across(where(is.double), ~na_if(., 0)),
           coverage = receiving_art/ovc_hivstat_pos)
    
  brk_pts <- df_viz %>% 
    filter(str_detect(period, "(2|4)$|(FY21Q3)")) %>% 
    pull(period)
  
  v1 <- df_viz %>% 
    filter(period != "FY17Q1") %>% 
    ggplot(aes(period, ovc_hivstat_pos, group = countryname)) +
    geom_area(alpha = .2, fill = genoa, color = genoa, size = .9) +
    geom_point(fill = genoa, color = genoa, size = 4) +
    scale_y_continuous(labels = comma, position = "right") +
    scale_x_discrete(breaks = brk_pts, position = "top") +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "HIV+ OVC") +
    si_style_ygrid()
  
  v2 <- df_viz %>% 
    filter(!is.na(coverage)) %>% 
    ggplot(aes(period, coverage, group = countryname)) +
    # geom_path(data = filter(df_viz, !is.na(coverage)), na.rm = TRUE) +
    geom_path(na.rm = TRUE, color = moody_blue) +
    geom_point(shape = 21, fill = "white", size = 10, stroke = 2, 
               na.rm = TRUE, color = moody_blue) +
    geom_text(aes(label = percent(coverage, 1)), na.rm = TRUE,
              family = "Source Sans Pro", color = "#505050") +
    scale_y_continuous(labels = percent, position = "right") +
    expand_limits(y = .75) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "ART coverage") +
    si_style_nolines() +
    theme(axis.text = element_blank())

  v2 / v1  +
    plot_layout(heights = c(1, 4)) &
    theme(plot.background = element_rect(fill = "white", color = NA))

  si_save("Graphics/FY21Q2_OVC_hivpos_cov.svg")
  
  
  max_pd_tx <- "FY21Q2"
  df_join <- read_csv(glue("Dataout/{max_pd_tx}_TZA_OVC_TDY_data.csv"))
  
  glimpse(df_join)
  
  
  df_join %>% 
    filter(fiscal_year == 2021,
           targets_results == "cumulative",
           # psnu != "_Military Tanzania",
           psnu_ovc_status_22 == "OVC Programming") %>% 
    group_by(age_grp) %>% 
    summarise(across(c(tx_curr, ovc_hivstat_pos), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(cov = ovc_hivstat_pos/tx_curr)
  
  
  df_psnu <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")
  
  df_psnu %>% 
    filter(fiscal_year == 2021,
           operatingunit == "Tanzania",
           indicator == "OVC_HIVSTAT_POS") %>% 
    count(standardizeddisaggregate, wt = cumulative)
  
  lst_ovc_psnu <- df_psnu %>%
    filter(fiscal_year == 2021,
           indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator",
           targets > 0) %>% 
    count(psnu, wt = targets) %>% 
    pull(psnu)
    
  ovcpos <- df_psnu %>% 
    filter(fiscal_year == 2021,
           operatingunit == "Tanzania",
           indicator == "OVC_HIVSTAT_POS") %>% 
    count(psnu, wt = cumulative, name = "fy21q2_ovchivstatpos")
  
  ovctarg <- df_psnu %>%
    filter(fiscal_year == 2021,
           indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator",
           targets > 0) %>% 
    count(psnu, wt = targets, name = "fy21targets_ovcserv")
  
  u20 <- df_psnu %>% 
    filter(fiscal_year == 2021,
           operatingunit == "Tanzania",
           psnu %in% c(lst_ovc_psnu, "Ifakara TC"),
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% c("<01", "01-04", "05-09", "10-14", "15-19")
           ) %>% 
    count(psnu, wt = qtr2, name = "fy21q2_txcurr_u20")
  
  u15 <- df_psnu %>% 
    filter(fiscal_year == 2021,
           operatingunit == "Tanzania",
           psnu %in% c(lst_ovc_psnu, "Ifakara TC"),
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% c("<01", "01-04", "05-09", "10-14")
    ) %>% 
    count(psnu, wt = qtr2, name = "fy21q2_txcurr_u15")

  full_join(u20, u15) %>% 
    full_join(ovcpos) %>% 
    full_join(ovctarg) %>% 
    View()
  
  
  df_psnu %>% 
    filter(fiscal_year == 2021,
           operatingunit == "Tanzania",
           indicator == "OVC_HIVSTAT_POS") %>% 
    count(wt = cumulative, name = "fy21q2_ovchivstatpos")
  
  
  

  