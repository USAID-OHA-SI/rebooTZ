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
      mutate(share_m = Male / (Female + Male) *100,
             pd = str_remove(pd, "20") %>% toupper(.),
             operatingunit = "Tanzania") %>% 
      select(pd, `Total Male Pos.` = Male, 
             `Male Share of Pos. (%)` = share_m, 
             operatingunit) %>% 
      gather(ind, val, `Total Male Pos.`, `Male Share of Pos. (%)`) %>% 
      mutate(max = ifelse(ind == "Total Male Pos.", 13000, 50),
             ind = factor(ind, c("Total Male Pos.", "Male Share of Pos. (%)")))
    
  #plot tend
    df_male_trend %>% 
      ggplot(aes(pd, val, color = ind, group = operatingunit)) +
      geom_point(aes(y = max), color = "white", size = 0) + 
      geom_hline(yintercept = 0, color = "#595959") +
      geom_line(size = 1) +
      geom_point(size = 5) +
      geom_text(aes(label = comma(val)),
                family = "Gill Sans MT",
                vjust = -1) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values = c("#335B8E", "#CC5234"))+
      labs(x = "", y = "") +
      facet_grid(ind ~ ., scales = "free_y") +
      plot_theme() +
      theme(axis.text.y = element_blank())
    
    ggsave("TZA_male_trend.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
  
  
  full_hts <- c(sites_hts, comm_hts)
  
  #trend in finding men in priority sites
    df_male_trend_priority <- df_mods_o15 %>% 
      filter(resultstatus == "Positive",
             orgunituid %in% full_hts) %>% 
      group_by(sex, orgunituid, sitename) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>% 
      
      mutate(sex = factor(sex, c("Male", "Female"))) %>% 
      group_by(orgunituid, pd) %>% 
      mutate(tot = sum(val),
             share = val / sum(val),
             tot = case_when(sex == "Male" ~ tot),
             share = case_when(sex == "Male" ~ share)) %>% 
      ungroup() %>% 
      filter(!is.nan(share)) %>% 
      mutate(pd = str_remove(pd, "20") %>% toupper(.),
             orgunituid = factor(orgunituid, full_hts),
             sitename = str_remove(sitename, " - District Hospital| - Regional Referral Hospital| - Designated District Hospital"),
             sitename = str_replace(sitename, "Dispensary", "Disp."),
             sitename = str_replace(sitename, "Health Cent(er|re)", "HC")) %>% 
      arrange(orgunituid, pd, sex)%>% 
      mutate(sitename = as_factor(sitename))
    
    
    #plot tend, priority
    df_male_trend_priority %>% 
      filter(orgunituid %in% sites_hts) %>% 
      ggplot(aes(pd, val, fill = sex, group = sitename)) +
      geom_hline(yintercept = 0, color = "#595959") +
      geom_col(na.rm = TRUE) +
      geom_text(aes(y = tot, label = percent(share, accuracy = 1)),
                color = "#595959",
                family = "Gill Sans MT",
                vjust = -1,
                na.rm = TRUE) +
      scale_y_continuous(labels = comma) +
      scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
      scale_fill_manual(name = "", values = c("#6CA18F", "#bfbfbf")) +
      expand_limits(y = 250) +
      labs(x = "", y = "positive tests (male share)") +
      facet_wrap(. ~ sitename, nrow = 3) +
      plot_theme() +
      theme(legend.position = "bottom")
    
    ggsave("TZA_male_trend_priority.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 11, units = "in") 
    #comm
    df_male_trend_priority %>% 
      filter(orgunituid %in% comm_hts) %>% 
      ggplot(aes(pd, val, fill = sex, group = sitename)) +
      geom_hline(yintercept = 0, color = "#595959") +
      geom_col(na.rm = TRUE) +
      geom_text(aes(y = tot, label = percent(share, accuracy = 1)),
                color = "#595959",
                family = "Gill Sans MT",
                vjust = -1,
                na.rm = TRUE) +
      scale_y_continuous(labels = comma) +
      scale_x_discrete(labels = c("FY18Q1", rep("", 3), "FY19Q1")) +
      scale_fill_manual(name = "", values = c("#6CA18F", "#bfbfbf")) +
      expand_limits(y = 250) +
      labs(x = "", y = "positive tests (male share)") +
      facet_wrap(. ~ sitename, nrow = 3) +
      plot_theme() +
      theme(legend.position = "bottom")
    
    ggsave("TZA_male_trend_priority_comm.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 11, units = "in")
    
    df_male_trend_priority %>% 
      filter(sex == "Male",
             pd == "FY19Q1") %>% 
      summarise_at(vars(val, tot), sum, na.rm = TRUE) %>% 
      mutate(share_m = val/ tot)
    
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

# CLEAN -------------------------------------------------------------------

rm(df_male_mod, df_male_share_sites, df_male_trend, df_male_trend_priority,
   df_mod_pos_priority, df_mods_o15, full_hts)  
    