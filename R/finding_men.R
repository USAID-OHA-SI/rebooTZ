##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  finding men
##  DATE:     2019-02-14
##  UPDATED:  2019-02-19


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
  
    
    #trend in finding men in priority sites
    df_male_trend_priority <- df_mods_o15 %>% 
      filter(resultstatus == "Positive",
             orgunituid %in% sites_hts) %>% 
      group_by(sex, orgunituid, sitename) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(sex, val) %>% 
      mutate(share_m = Male / (Female + Male),
             share_m = ifelse(is.nan(share_m), NA, share_m),
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania",
             orgunituid = factor(orgunituid, sites_hts),
             sitename = str_remove(sitename, " - District Hospital|- Regional Referral Hospital"),
             sitename = str_replace(sitename, "Dispensary", "Disp."),
             sitename = str_replace(sitename, "Health Cent(er|re)", "HC")) %>% 
      arrange(orgunituid) %>% 
      mutate(sitename = as_factor(sitename))
    
    
    #plot tend, priority
    df_male_trend_priority %>% 
      ggplot(aes(pd, share_m, color = operatingunit, group = sitename)) +
      geom_hline(yintercept = 0, color = "#595959") +
      geom_hline(yintercept = .5, color = "#595959", linetype = "dashed", size = .75) +
      geom_line(size = 1, na.rm = TRUE) +
      geom_point(size = 5, na.rm = TRUE) +
      scale_y_continuous(labels = percent_format(1)) +
      scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
      scale_color_manual(values = "#6CA18F")+
      labs(x = "", y = "male share of positive tests") +
      facet_wrap(. ~ sitename, nrow = 2) +
      plot_theme()
    
    ggsave("TZA_male_trend_priority.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 11, units = "in") 
      
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
    df_male_mod <- df_mods_o15 %>% 
      filter(resultstatus == "Positive") %>% 
      group_by(indicator, modality, sex, resultstatus) %>% 
      summarise_at(vars(fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      filter(fy2019q1 != 0) %>% 
      spread(sex, fy2019q1, fill = 0) %>% 
      mutate(share_m = Male / (Male + Female)) %>% 
      arrange(share_m) %>% 
      mutate(modality = as_factor(modality))
    
    
    df_male_mod %>% 
      ggplot(aes(modality, share_m)) +
      geom_segment(aes(x = modality, xend = modality, y = 0, yend = 1),
                   linetype = "dashed", size = .4, color = "#bfbfbf")+
      geom_point(color = ifelse(df_male_mod$share_m > .5, "#CC5234", "#7f8c8d"),
                 size = 13) +
      geom_text(aes(label = percent(share_m)),
                family = "Gill Sans MT",
                color = "white") +
      coord_flip() +
      plot_theme() +
      labs(x = "", y = "share of modality male") +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 14))

    ggsave("TZA_male_mod.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
    