##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  proxy linkage
##  DATE:     2019-02-15
##  UPDATED:  2019-02-19


# MUNGE -------------------------------------------------------------------

  df_genie_link <- df_genie_site %>% 
    filter(fundingagency == "USAID",
           (indicator == "HTS_TST" & 
              standardizeddisaggregate %in% c("Modality/Age/Sex/Result", 
                                              "Modality/Age Aggregated/Sex/Result") &
              resultstatus == "Positive") | 
           (indicator == "TX_NEW" & standardizeddisaggregate %in% c("Age/Sex/HIVStatus", 
                                                                    "Age Aggregated/Sex/HIVStatus"))) %>% 
    mutate(indicator = ifelse(indicator == "HTS_TST", "HTS_TST_POS", indicator),
           sex = ifelse(agecoarse == "<15", "Peds", sex))
    
  
  df_link <- df_genie_link %>% 
    group_by(orgunituid, sitename, indicator) %>% 
    summarise_at(vars(fy2019q1), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(fy2019q1 != 0) %>% 
    spread(indicator, fy2019q1, fill = 0) %>% 
    mutate(proxylink = TX_NEW / HTS_TST_POS,
           proxylink = ifelse(is.infinite(proxylink), NA, proxylink),
           operatingunit = "Tanzania")

  #summary stats
  summary(df_link$proxylink)

  df_link %>% 
    filter(orgunituid %in% sites_hts) %>% 
    skimr::skim(proxylink)
  
  #trend
  df_link <- df_genie_link %>% 
    group_by(indicator) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>%
    gather(pd, val, -indicator) %>% 
    spread(indicator, val, fill = 0) %>% 
    mutate(proxylink = TX_NEW / HTS_TST_POS,
           proxylink = ifelse(is.infinite(proxylink), NA, proxylink),
           operatingunit = "Tanzania",
           pd = str_remove(pd, "20") %>% toupper(.))

  df_link %>% 
    ggplot(aes(pd, proxylink, group = operatingunit, color = operatingunit)) +
    geom_line(size = 1) +
    geom_point(size = 5) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_color_manual(values = "#335B8E") +
    expand_limits(y = 0) +
    labs(x = "", y = "proxy linkage") +
    plot_theme()
  
  ggsave("TZA_linkage_trend.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in")    

  
  #trend, priority
  df_link_priority <- df_genie_link %>% 
    filter(orgunituid %in% sites_hts) %>% 
    group_by(orgunituid, sitename, indicator) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>%
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val, fill = 0) %>% 
    mutate(proxylink = TX_NEW / HTS_TST_POS,
           proxylink = ifelse(is.infinite(proxylink) | is.nan(proxylink), NA, proxylink),
           operatingunit = "Tanzania",
           pd = str_remove(pd, "20") %>% toupper(.),
           orgunituid = factor(orgunituid, sites_hts),
           sitename = str_remove(sitename, " - District Hospital|- Regional Referral Hospital"),
           sitename = str_replace(sitename, "Dispensary", "Disp."),
           sitename = str_replace(sitename, "Health Cent(er|re)", "HC")) %>% 
    arrange(orgunituid) %>% 
    mutate(sitename = as_factor(sitename))
  
  
  df_link_priority %>% 
    ggplot(aes(pd, proxylink, group = operatingunit, color = operatingunit)) +
    geom_line(size = 1) +
    geom_point(size = 5, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
    scale_color_manual(values = "#6CA18F") +
    expand_limits(y = 0) +
    facet_wrap(. ~ sitename, nrow = 3) +
    labs(x = "", y = "proxy linkage") +
    plot_theme()
  
  ggsave("TZA_linkage_trend_priority.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 11, units = "in")   
  

  #sex
  df_link_age <- df_genie_link %>% 
    filter(agecoarse != "Unknown Age") %>% 
    #mutate(agesex = paste(sex, agecoarse)) %>% 
    group_by(indicator, sex, agecoarse) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>%
    gather(pd, val, -indicator, -sex, -agecoarse) %>% 
    spread(indicator, val, fill = 0) %>% 
    mutate(proxylink = TX_NEW / HTS_TST_POS,
           proxylink = ifelse(is.infinite(proxylink), NA, proxylink),
           operatingunit = "Tanzania",
           pd = str_remove(pd, "20") %>% toupper(.),
           sex = factor(sex, c("Male", "Peds", "Female")),
           lab = case_when(str_detect(pd, "Q1") ~ percent(proxylink, accuracy = 1)))
  
  df_link_age %>% 
    ggplot(aes(pd, proxylink, group = sex, color = sex)) +
    geom_hline(yintercept = 0, color = "#bfbfbf") +
    geom_line(size = 1) +
    geom_point(size = 5) +
    geom_text(aes(label = lab), vjust = -1,
              family = "Gill Sans MT",
              na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) + 
    scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
    #scale_color_manual(values = c("#CC5234", "#335B8E", "#6CA18F")) +
    expand_limits(y = c(0, .8)) +
    facet_wrap(. ~ sex) +
    labs(x = "", y = "proxy linkage") +
    plot_theme()
  
  ggsave("TZA_linkage_trend_sex.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in")
  