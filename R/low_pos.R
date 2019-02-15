##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  which sites have low HTS positivity
##  DATE:     2019-02-13
##  UPDATED:  2019-02-14

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse) 
  library(ICPIutilities)
  library(scales)
  library(extrafont)

  #import theme
  source("R/plot_theme.R")


# IMPORT ------------------------------------------------------------------

  #GENIE PULL 
  #  - Indicators: HTS_TST, HTS_TST_POS, TX_NEW, TX_CURR (Total Numerator)
  #  - Date: 2019-02-14
  
  #site data
    df_genie_site <- match_msd("~/GitHub/rebooTZ/data/PEPFAR-Data-Genie-SiteByIMs-2019-02-14.zip",
                               save_rds = FALSE)


# MUNGE -------------------------------------------------------------------

  #keep just USAID HTS
    df_genie_hts <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             indicator %in% c("HTS_TST", "HTS_TST_POS")) 
    
  #aggregate to FY18APR and FY19Q1 (ensure 1 obs per site) for positivity creation and comparison
    df_genie_hts <- df_genie_hts %>% 
      group_by(orgunituid, sitename, psnu, indicator) %>% 
      summarise_at(vars(fy2018apr, fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(indicator, val) %>% 
      filter(HTS_TST != 0) 
    
  #create positivity
    df_genie_hts <- df_genie_hts %>% 
      mutate(positivity = HTS_TST_POS/HTS_TST)

  #summary
    df_genie_hts %>% 
      select(-c(HTS_TST, HTS_TST_POS)) %>% 
      spread(pd, positivity) %>% 
      summary()
  #n
    df_genie_hts %>%
      filter(HTS_TST!=0, !is.na(HTS_TST)) %>% 
      count(pd)

      
  #compare distribution
    df_genie_hts %>% 
      mutate(pd = toupper(pd)) %>% 
      ggplot(aes(positivity)) + 
      geom_histogram() +
      scale_x_continuous(labels = percent) +
      facet_grid(pd ~ .) +
      plot_theme()
    
    
  #define low posivity
    lowpos <- .02
    
  #How many sites were below lowpos positivity in FY19Q1?
    df_genie_hts %>% 
      filter(pd == "fy2019q1",
             positivity < lowpos) %>%
      nrow()
    
    df_genie_hts %>% 
      filter(pd == "fy2019q1",
             HTS_TST !=0,
             !is.na(HTS_TST)) %>%
      distinct(orgunituid) %>% 
      nrow()
  
  #How many sites were below lowpos positivity both FY18 and FY19Q1?
    df_genie_hts %>% 
      select(-c(HTS_TST, HTS_TST_POS)) %>% 
      spread(pd, positivity) %>% 
      filter(fy2018apr < lowpos & fy2019q1 < lowpos) %>%
      nrow()
    
    df_genie_hts %>% 
      filter(HTS_TST !=0,
             !is.na(HTS_TST)) %>% 
      distinct(orgunituid) %>% 
      nrow()  
    

  #scatter plot
    df_plot <- df_genie_hts %>% 
      select(-c(HTS_TST, HTS_TST_POS)) %>%
      mutate(pd = toupper(pd)) %>% 
      spread(pd, positivity) %>%
      mutate(low = ifelse(FY2018APR < lowpos & FY2019Q1 < lowpos, "low", "okay")) 
    
    df_plot %>% 
      ggplot(aes(FY2019Q1, FY2018APR, color = low)) +
      geom_point(alpha = .5, na.rm = TRUE) +
      geom_hline(yintercept = lowpos) +
      geom_vline(xintercept = lowpos) +
      scale_y_continuous(label = percent) +
      scale_x_continuous(label = percent) +
      scale_color_manual(values = c("#CC5234", "#7f8c8d", "#7f8c8d")) +
      theme_light() +
      plot_theme()
    
    ggsave("TZA_lowpos_scatter.png", 
           path = "Output",
           dpi = 300, 
           height = 5.5, width = 5.5, units = "in")
    
    df_plot %>% 
      rename(low_in_both = low) %>% 
      mutate(low_in_both = case_when(low_in_both == "low" ~ "low")) %>% 
      arrange(low_in_both, FY2019Q1) %>% 
      write_csv("Output/TZA_lowpos_sites.csv", na = "")
    
    
  #aggregate trend for positivity creation and comparison
    df_genie_trend <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
      group_by(indicator) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(indicator, val) %>% 
      filter(HTS_TST != 0)
    
  #create positivity
    df_genie_trend <- df_genie_trend %>% 
      mutate(positivity = HTS_TST_POS/HTS_TST,
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania")
    
  #plot trend
    df_genie_trend %>% 
      ggplot(aes(pd, positivity, group = operatingunit, color = operatingunit)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point( size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#335B8E") +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "", y = "positivity") +
      plot_theme() 
    
    ggsave("TZA_lowpos_trend.png", 
           path = "Output",
           dpi = 300, 
           height = 5.5, width = 7, units = "in")
      
