##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  which sites have low HTS positivity
##  DATE:     2019-02-13
##  UPDATED:  2019-02-19


# MUNGE -------------------------------------------------------------------

  #keep just USAID HTS
    df_genie_hts <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             indicator == "HTS_TST",
             (standardizeddisaggregate == "Total Numerator") | 
               (standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                                "Modality/Age/Sex/Result") & 
                  resultstatus == "Positive")) %>% 
      mutate(indicator = ifelse(is.na(resultstatus), "HTS_TST", "HTS_TST_POS"))
    
  #flag priority sites
    df_genie_hts <- df_genie_hts %>% 
      mutate(site_type = ifelse(orgunituid %in% sites_hts, "Priority", "Other"),
             site_type = factor(site_type, c("Priority", "Other")))
    
  #aggregate to FY18APR and FY19Q1 (ensure 1 obs per site) for positivity creation and comparison
    df_genie_hts <- df_genie_hts %>% 
      group_by(orgunituid, sitename, psnu, indicator, site_type) %>% 
      summarise_at(vars(fy2018apr, fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(indicator, val) %>% 
      filter(HTS_TST != 0) 
  
  #create positivity
    df_genie_hts <- df_genie_hts %>% 
      mutate(positivity = HTS_TST_POS/HTS_TST)
    
  #define low posivity
    lowpos <- .02
    
  #How many sites were below lowpos positivity in FY19Q1?
    df_genie_hts %>% 
      filter(pd == "fy2019q1") %>%
      count(positivity < lowpos)
  
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
      geom_hline(yintercept = lowpos, linetype = "dashed", size = .75, color = "#595959") +
      geom_vline(xintercept = lowpos, linetype = "dashed", size = .75, color = "#595959") +
      scale_y_continuous(limits = c(0, .25), label = percent_format(1)) +
      scale_x_continuous(limits = c(0, .25), label = percent_format(1)) +
      scale_color_manual(values = c("#CC5234", "#7f8c8d", "#7f8c8d")) +
      theme_light() +
      plot_theme()
    
    ggsave("TZA_lowpos_scatter.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 5, units = "in")
    
    df_plot %>% 
      count(FY2018APR > .25 | FY2019Q1 > .25)
    
    df_plot %>% 
      rename(low_in_both = low) %>% 
      mutate(low_in_both = case_when(low_in_both == "low" ~ "low")) %>% 
      arrange(low_in_both, FY2019Q1) %>% 
      write_csv("Output/TZA_lowpos_sites.csv", na = "")
    
  #scatter plot with priority sites
    df_plot_priority <- df_genie_hts %>% 
      filter(site_type == "Priority") %>% 
      select(-c(HTS_TST, HTS_TST_POS)) %>%
      mutate(pd = toupper(pd)) %>% 
      spread(pd, positivity) %>%
      mutate(low = ifelse(FY2018APR < lowpos & FY2019Q1 < lowpos, "low", "okay")) 
    
    df_plot %>% 
      ggplot(aes(FY2019Q1, FY2018APR)) +
      geom_point(color = "#7f8c8d", alpha = .5, na.rm = TRUE) +
      geom_point(data = df_plot_priority, aes(FY2019Q1, FY2018APR), 
                 color = "#CC5234", na.rm = TRUE) +
      geom_hline(yintercept = lowpos, linetype = "dashed", size = .75, color = "#595959") +
      geom_vline(xintercept = lowpos, linetype = "dashed", size = .75, color = "#595959") +
      scale_y_continuous(limits = c(0, .25), label = percent_format(1)) +
      scale_x_continuous(limits = c(0, .25), label = percent_format(1)) +
      plot_theme()
    
    ggsave("TZA_lowpos_scatter_priority.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 5, units = "in")
    
    df_plot_priority %>% 
      count(low)
    
  #aggregate trend for positivity creation and comparison
    df_genie_trend <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             indicator == "HTS_TST",
             (standardizeddisaggregate == "Total Numerator") | 
               (standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                                "Modality/Age/Sex/Result") & 
                  resultstatus == "Positive")) %>% 
      mutate(indicator = ifelse(is.na(resultstatus), "HTS_TST", "HTS_TST_POS")) %>% 
      group_by(indicator) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(indicator, val) %>% 
      filter(HTS_TST != 0)
    
  #create positivity
    df_genie_trend <- df_genie_trend %>% 
      mutate(`Positivity (%)` = HTS_TST_POS/HTS_TST *100) %>% 
      rename(`Total Tests` = HTS_TST) %>% 
      select(-HTS_TST_POS) %>% 
      gather(ind, val, -pd) %>% 
      mutate(ind = factor(ind, c("Total Tests", "Positivity (%)")),
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania")
    
  #plot trend
    df_genie_trend %>% 
      ggplot(aes(pd, val, group = operatingunit, color = ind)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point( size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = c("#335B8E", "#CC5234")) +
      scale_y_continuous(labels = comma) +
      labs(x = "", y = "") +
      facet_wrap(ind ~ ., ncol = 1, scales = "free_y")+
      plot_theme() 
    
    ggsave("TZA_lowpos_trend.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
      

  #aggregate trend for positivity creation and comparison
    df_genie_trend_priority <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             orgunituid %in% sites_hts,
             indicator == "HTS_TST",
             (standardizeddisaggregate == "Total Numerator") | 
               (standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                                "Modality/Age/Sex/Result") & 
                  resultstatus == "Positive")) %>% 
      mutate(indicator = ifelse(is.na(resultstatus), "HTS_TST", "HTS_TST_POS")) %>% 
      group_by(orgunituid, sitename, indicator) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(indicator, val) %>% 
      filter(HTS_TST != 0)
    
  #create positivity
    df_genie_trend_priority <- df_genie_trend_priority %>% 
      mutate(positivity = HTS_TST_POS/HTS_TST,
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania",
             orgunituid = factor(orgunituid, sites_hts),
             sitename = str_remove(sitename, " - District Hospital| - Regional Referral Hospital| - Designated District Hospital"),
             sitename = str_replace(sitename, "Dispensary", "Disp."),
             sitename = str_replace(sitename, "Health Cent(er|re)", "HC")) %>% 
      arrange(orgunituid) %>% 
      mutate(sitename = as_factor(sitename))
  
  #plot trend
    df_genie_trend_priority %>% 
      ggplot(aes(pd, positivity, group = sitename, color = operatingunit)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point( size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#6CA18F") +
      scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "", y = "positivity") +
      facet_wrap(. ~ sitename, nrow = 3) +
      plot_theme() 
    
    ggsave("TZA_lowpos_trend_priority.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 11, units = "in")
  
  #plot trend for
    df_genie_trend_priority %>% 
      #mutate(HTS_TST = HTS_TST / 1000) %>% 
      ggplot(aes(pd, HTS_TST, group = sitename, color = operatingunit)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point( size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#595959") +
      scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
      scale_y_continuous(labels = ks) +
      #scale_y_continuous(labels = comma) +
      labs(x = "", y = "total tests") +
      facet_wrap(. ~ sitename, nrow = 3) +
      plot_theme()
    
    ggsave("TZA_lowpos_trend_priority_hts.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 11, units = "in")
  
    
    
  #community priority sites
    
  #aggregate trend for positivity creation and comparison, comm
    df_genie_trend_priority <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             orgunituid %in% comm_hts,
             indicator == "HTS_TST",
             (standardizeddisaggregate == "Total Numerator") | 
               (standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                                "Modality/Age/Sex/Result") & 
                  resultstatus == "Positive")) %>% 
      mutate(indicator = ifelse(is.na(resultstatus), "HTS_TST", "HTS_TST_POS")) %>% 
      group_by(orgunituid, sitename, indicator) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(indicator, val) %>% 
      filter(HTS_TST != 0)
    
    #create positivity, comm
    df_genie_trend_priority <- df_genie_trend_priority %>% 
      mutate(positivity = HTS_TST_POS/HTS_TST,
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania",
             orgunituid = factor(orgunituid, comm_hts),
             sitename = str_remove(sitename, " - District Hospital| - Regional Referral Hospital| - Designated District Hospital"),
             sitename = str_replace(sitename, "Dispensary", "Disp."),
             sitename = str_replace(sitename, "Health Cent(er|re)", "HC")) %>% 
      arrange(orgunituid) %>% 
      mutate(sitename = as_factor(sitename))
    
    #plot trend, comm
    df_genie_trend_priority %>% 
      ggplot(aes(pd, positivity, group = sitename, color = operatingunit)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point( size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#6CA18F") +
      scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "", y = "positivity") +
      facet_wrap(. ~ sitename, nrow = 3) +
      plot_theme() 
    
    ggsave("TZA_lowpos_trend_priority_comm.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 11, units = "in")
    
    
    #plot trend for
    df_genie_trend_priority %>% 
      ggplot(aes(pd, HTS_TST, group = sitename, color = operatingunit)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point( size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#595959") +
      scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
      scale_y_continuous(labels = ks) +
      #scale_y_continuous(labels = comma) +
      labs(x = "", y = "total tests") +
      facet_wrap(. ~ sitename, nrow = 3) +
      plot_theme()
    
    ggsave("TZA_lowpos_trend_priority_comm_hts.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 11, units = "in")
    

# CLEAN -------------------------------------------------------------------

rm(df_genie_hts, df_genie_trend, df_genie_trend_priority, 
   df_plot, df_plot_priority, lowpos)  
    