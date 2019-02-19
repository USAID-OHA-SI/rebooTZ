##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  finding men
##  DATE:     2019-02-14
##  UPDATED:  


# MUNGE -------------------------------------------------------------------

  df_proxyret_age <- df_genie_site %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("TX_NEW", "TX_CURR"),
           standardizeddisaggregate %in% c("Age Aggregated/Sex/HIVStatus", 
                                           "Age/Sex/HIVStatus")) %>% 
    mutate(sex = ifelse(agecoarse == "<15", "Peds", sex),
           site_type = ifelse(orgunituid %in% sites_tx, "Priority", "Other"),
           site_type = factor(site_type, c("Priority", "Other"))) %>% 
    group_by(orgunituid, sitename, site_type, indicator, sex, agecoarse) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, starts_with("fy")) %>% 
    filter(val != 0) %>% 
    spread(indicator, val, fill = 0) %>% 
    group_by(orgunituid, sex, agecoarse) %>% 
    mutate(proxyret = (TX_CURR - TX_NEW) / lag(TX_CURR))
  

  df_proxyret_overall <- df_proxyret_age %>% 
    group_by(pd) %>% 
    summarise_at(vars(TX_NEW, TX_CURR, proxyret), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(proxyret = (TX_CURR - TX_NEW) / lag(TX_CURR),
           pd = str_remove(pd, "20") %>% toupper(.),
           operatingunit =  "Tanzania") %>% 
    filter(pd != "FY18Q1")
  

  df_proxyret_overall %>% 
    ggplot(aes(pd, proxyret, group = operatingunit, color = operatingunit)) +
    geom_line(size = 1) +
    geom_point(size = 5) +
    geom_text(aes(y = proxyret, label = percent(proxyret, accuracy = 1)),
              vjust = -1.7) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_color_manual(values = "#335B8E") +
    expand_limits(y = c(0, 1.05)) +
    labs(x = "", y = "proxy retention") +
    plot_theme()
  
  ggsave("TZA_proxyret_trend.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in") 
  
  df_proxyret_priority <- df_proxyret_age %>% 
    filter(site_type == "Priority") %>% 
    group_by(orgunituid, sitename, pd) %>% 
    summarise_at(vars(TX_NEW, TX_CURR, proxyret), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(proxyret = (TX_CURR - TX_NEW) / lag(TX_CURR),
           pd = str_remove(pd, "20") %>% toupper(.),
           operatingunit =  "Tanzania",
           orgunituid = factor(orgunituid, sites_tx),
           sitename = str_remove(sitename, " - District Hospital| - Regional Referral Hospital| - Designated District Hospital"),
           sitename = str_replace(sitename, "Dispensary", "Disp."),
           sitename = str_replace(sitename, "Health Cent(er|re)", "HC")) %>% 
    arrange(orgunituid) %>% 
    mutate(sitename = as_factor(sitename)) %>% 
    filter(pd != "FY18Q1")
  
  df_proxyret_priority %>% 
    ggplot(aes(pd, proxyret, group = operatingunit, color = operatingunit)) +
    geom_line(size = 1) +
    geom_point(size = 5) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY18Q2", "", "", "FY19Q1")) +
    scale_color_manual(values = "#6CA18F") +
    expand_limits(y = 0) +
    labs(x = "", y = "proxy retention") +
    facet_wrap(. ~ sitename, nrow = 2) +
    plot_theme()

  ggsave("TZA_proxyret_trend_priority.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 11, units = "in") 
  
  #sex
  df_proxyret_sex <- df_proxyret_age %>% 
    group_by(pd, sex) %>% 
    summarise_at(vars(TX_NEW, TX_CURR), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(sex, pd) %>% 
    group_by(sex) %>% 
    mutate(proxyret = (TX_CURR - TX_NEW) / lag(TX_CURR)) %>% 
    ungroup() %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           sex = factor(sex, c("Male", "Peds", "Female")),
           operatingunit =  "Tanzania") %>% 
    filter(pd != "FY18Q1")
  
  
  df_proxyret_sex %>% 
    ggplot(aes(pd, proxyret, group = sex, color = sex)) +
    geom_line(size = 1) +
    geom_point(size = 5) +
    geom_text(aes(y = proxyret, label = percent(proxyret, accuracy = 1)),
              vjust = -1.7) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY18Q2", rep("", 2), "FY19Q1")) + 
    scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
    expand_limits(y = c(0, 1.05)) +
    labs(x = "", y = "proxy retention") +
    facet_wrap(. ~ sex) +
    plot_theme()
  
  ggsave("TZA_proxyret_trend_sex.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in") 
  