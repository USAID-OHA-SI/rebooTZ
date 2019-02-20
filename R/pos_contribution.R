##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  positive contribution
##  DATE:     2019-02-14
##  UPDATED:  2019-02-19


# MUNGE -------------------------------------------------------------------

  #look at USAID testing
    df_mods <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             indicator == "HTS_TST",
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", 
                                             "Modality/Age/Sex/Result"))

  #site share of positives
    df_site_pos <- df_mods %>% 
      group_by(sitename, orgunituid, indicator, resultstatus) %>% 
      summarise_at(vars(fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(resultstatus, fy2019q1, fill = 0) %>% 
      mutate(Total = Negative + Positive,
             positivity = Positive / Total) %>% 
      filter(Total != 0)
    
    df_site_pos %>% 
      ggplot(aes(positivity)) +
      geom_histogram(bins = 50, color = "white", 
                   fill = c(rep("#CC5234",2),  rep("#595959", 48))) +
      scale_x_continuous(label = percent) +
      labs(x = "positivity", y = "# of sites") +
      plot_theme() 
    
    ggsave("TZA_pos_bysite.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")  
    
    df_site_pos %>% 
      mutate(below = positivity <.03) %>% 
      count(below) %>% 
      spread(below, n) %>% 
      mutate(sites_low_pos = `TRUE`/ (`FALSE` + `TRUE`))
  
  #modality breakdown
    df_mod_pos <- df_mods %>% 
      group_by(indicator, modality, resultstatus) %>% 
      summarise_at(vars(fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(resultstatus, fy2019q1, fill = 0) %>% 
      mutate(Total = Negative + Positive,
             Positivity = Positive / Total) %>% 
      filter(Total != 0) %>% 
      arrange(Total) %>% 
      mutate(modality = as_factor(modality)) %>% 
      select(-Negative, -Positive) %>% 
      gather(ind, val, -indicator, -modality) %>% 
      mutate(ind = factor(ind, c("Total", "Positivity")),
             lab_tot = case_when(ind == "Total" ~ comma(val)),
             lab_pct = case_when(ind != "Total" ~ percent(val, accuracy = .1)),
             max = ifelse(ind == "Total", 660000, .2))
    
    df_mod_pos %>% 
      ggplot(aes(modality, val)) +
      geom_col(fill = ifelse(str_detect(df_mod_pos$modality, "Index"), "#CC5234", "#7f8c8d")) +
      geom_point(aes(y = max), size = 0, color = "white") +
      geom_text(aes(y = val, label = lab_tot),
                hjust = -.3, color = "#595959",
                family = "Gill Sans MT", 
                na.rm = TRUE) +
      geom_text(aes(y = val, label = lab_pct),
                hjust = -.4, color = "#595959",
                family = "Gill Sans MT", 
                na.rm = TRUE) +
      coord_flip() +
      labs(x = "", y = "") +
      facet_wrap(. ~ ind, scales = "free_x") +
      plot_theme() +
      theme(axis.text.x = element_blank())
  
  ggsave("TZA_pos_bymod.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in")  
  
  #modality breakdown for priority sites
  full_hts <- c(sites_hts, comm_hts)
  
  df_mod_pos_priority <- df_mods %>% 
      filter(orgunituid %in% full_hts) %>% 
      group_by(indicator, modality, resultstatus) %>% 
      summarise_at(vars(fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(resultstatus, fy2019q1, fill = 0) %>% 
      mutate(Total = Negative + Positive,
             Positivity = Positive / Total) %>% 
      filter(Total != 0) %>% 
      arrange(Total) %>% 
      mutate(modality = as_factor(modality)) %>% 
      select(-Negative, -Positive) %>% 
      gather(ind, val, -indicator, -modality) %>% 
      mutate(ind = factor(ind, c("Total", "Positivity")),
             lab_tot = case_when(ind == "Total" ~ comma(val)),
             lab_pct = case_when(ind != "Total" ~ percent(val, accuracy = .1)),
             max = ifelse(ind == "Total", 26000, .1)
             )
    
    df_mod_pos_priority %>% 
      ggplot(aes(modality, val)) +
      geom_col(fill = ifelse(str_detect(df_mod_pos_priority$modality, "Index|Mobile"), "#6CA18F", "#7f8c8d")) +
      geom_point(aes(y = max), size = 0, color = "white") +
      geom_text(aes(y = val, label = lab_tot),
                hjust = -.3, color = "#595959",
                family = "Gill Sans MT", 
                na.rm = TRUE) +
      geom_text(aes(y = val, label = lab_pct),
                hjust = -.4, color = "#595959",
                family = "Gill Sans MT", 
                na.rm = TRUE) +
      coord_flip() +
      labs(x = "", y = "") +
      facet_wrap(. ~ ind, scales = "free_x") +
      plot_theme() +
      theme(axis.text.x = element_blank())
    
    ggsave("TZA_pos_bymod_priority.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")  
    