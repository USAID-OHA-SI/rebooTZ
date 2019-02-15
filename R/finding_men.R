##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  finding men
##  DATE:     2019-02-14
##  UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse) 
  library(scales)
  library(extrafont)
  library(ICPIutilities)
  
  #import theme
  source("R/plot_theme.R")


# IMPORT ------------------------------------------------------------------

  #GENIE PULL 
  #  - Indicators: HTS_TST, TX_NEW (All disaggs)
  #  - Date: 2019-02-14
  
  #site data
    df_genie_site <- match_msd("~/GitHub/rebooTZ/data/PEPFAR-Data-Genie-SiteByIMs-2019-02-14 HTS_TX.zip",
                               save_rds = FALSE)
    

# MUNGE -------------------------------------------------------------------

  #look at USAID testing
    df_mods_o15 <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             indicator == "HTS_TST",
             agecoarse == "15+",
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", 
                                             "Modality/Age/Sex/Result"))
  #trend in finding men  
    df_male_trend <- df_mods_o15 %>% 
      filter(resultstatus == "Positive") %>% 
      group_by(sex) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(sex, val) %>% 
      mutate(share_m = Male / (Female + Male),
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania")
    
  #plot tend
    df_male_trend %>% 
      ggplot(aes(pd, share_m, color = operatingunit, group = operatingunit)) +
      geom_hline(yintercept = 0, color = "#595959") +
      geom_line(size = 1) +
      geom_point(size = 5) +
      scale_y_continuous(labels = percent_format(1)) +
      scale_color_manual(values = "#335B8E")+
      labs(x = "", y = "male share of positive tests") +
      plot_theme()
    
    ggsave("TZA_male_trend.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
    
  #male share by site
    df_male_share_sites <- df_mods_o15 %>% 
      filter(resultstatus == "Positive") %>% 
      group_by(orgunituid, sex) %>% 
      summarise_at(vars(fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(sex, val) %>% 
      mutate(share_m = Male / (Female + Male),
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania") %>% 
      filter(!is.nan(share_m)) %>% 
      arrange(share_m) %>% 
      mutate(orgunituid = as_factor(orgunituid),
             rownum = 1:n())
    
    # rows <- df_male_share_sites %>% 
    #   nrow()
    # 
    # namedrows <- c(.25, .5, .75) *rows
    # 
    # names <- df_male_share_sites %>% 
    #   mutate(site_share = case_when(rownum %in% namedrows~ paste0((rownum/rows)*100, "%"))) %>% 
    #   pull(site_share)

    #plot male share col
    # df_male_share_sites %>% 
    #   ggplot(aes(orgunituid, share_m)) +
    #   geom_col(na.rm = TRUE) +
    #   scale_y_continuous(label = percent) +
    #   scale_x_discrete(labels = names) +
    #   labs(y = "site's male share of positives", x = "") +
    #   plot_theme() +
    #   theme(axis.text.x = element_blank(),
    #         panel.grid.major.x = element_blank(),
    #         panel.grid.minor.x = element_blank())
    
    df_male_share_sites %>% 
      ggplot(aes(share_m)) +
      geom_histogram(bins = 20, color = "white", 
                     fill = c(rep("#595959", 10), rep("#CC5234", 10))) +
      scale_x_continuous(label = percent) +
      labs(x = "site's male share of positives", y = "sites") +
      plot_theme() 
    
    ggsave("TZA_male_bysite.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
    
    
    df_male_share_sites %>% 
      mutate(below = share_m <.5) %>% 
      count(below) %>% 
      spread(below, n) %>% 
      mutate(sites_more_men = `FALSE`/ (`FALSE` + `TRUE`))
    
  #where are we finding adult men?