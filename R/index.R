##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  index testing trend
##  DATE:     2019-02-14
##  UPDATED:  2019-02-15

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
  #  - Date: 2019-02-15
  
  #site data
  df_genie_site <- match_msd("~/GitHub/rebooTZ/data/PEPFAR-Data-Genie-SiteByIMs-2019-02-15.zip",
                             save_rds = FALSE)
  
  #priority sites
  load("data/sites_hts.rda")
  
# MUNGE -------------------------------------------------------------------

  #look at USAID testing
    df_mods <- df_genie_site %>% 
      filter(fundingagency == "USAID",
             indicator == "HTS_TST",
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", 
                                             "Modality/Age/Sex/Result"))
  #trend in index testing
    df_index <- df_mods %>% 
      filter(modality %in% c("Index", "IndexMod")) %>% 
      group_by(indicator) %>%
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, contains("q")) %>%
      mutate(pd = str_remove(pd, "20") %>% toupper(.))
     
  
  #plot index trend
    df_index %>% 
      ggplot(aes(pd, val)) +
      geom_col(fill = "#335B8E") +
      geom_text(aes(label = comma(val)),
                color = "#595959",
                family = "Gill Sans MT",
                vjust = -.5) +
      labs(x = "", y = "") +
      plot_theme() +
      theme(axis.text.y = element_blank())
    
    ggsave("TZA_index_trend.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
  
  #trend in index testing
    df_index_priority <- df_mods %>% 
      filter(modality %in% c("Index", "IndexMod"),
             orgunituid %in% sites_hts) %>% 
      group_by(indicator) %>%
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, contains("q")) %>%
      mutate(pd = str_remove(pd, "20") %>% toupper(.))
    
    
  #plot index trend
    df_index_priority %>% 
      ggplot(aes(pd, val)) +
      geom_col(fill = "#6CA18F") +
      geom_text(aes(label = comma(val)),
                color = "#595959",
                family = "Gill Sans MT",
                vjust = -.5) +
      labs(x = "", y = "") +
      plot_theme() +
      theme(axis.text.y = element_blank())
    
    ggsave("TZA_index_trend_priority.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
    
  #facility v community
    df_index_type <- df_mods %>% 
      filter(modality %in% c("Index", "IndexMod")) %>% 
      group_by(indicator, modality) %>%
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, contains("q")) %>%
      mutate(pd = str_remove(pd, "20") %>% toupper(.)) %>% 
      arrange(modality, pd) %>% 
      mutate(lab = case_when(pd == "FY18Q1" & modality == "Index"    ~ "Fac",
                             pd == "FY18Q1" & modality == "IndexMod" ~ "Comm"))

  #graph type
    df_index_type %>% 
      ggplot(aes(pd, val, group = modality, color = modality)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point(size = 5) +
      geom_text(aes(label = lab),
                na.rm = TRUE,
                family = "Gill Sans MT",
                hjust = 1.5) +
      scale_y_continuous(label = comma) +
      scale_color_manual(values = c("#CC5234", "#335B8E")) +
      labs(x = "", y = "") +
      plot_theme() 
  
    ggsave("TZA_index_type_trend_.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")

   #facility v community yield
    df_index_type_pos <- df_mods %>% 
      filter(modality %in% c("Index", "IndexMod")) %>% 
      group_by(indicator, modality, resultstatus) %>%
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, contains("q")) %>%
      mutate(pd = str_remove(pd, "20") %>% toupper(.)) %>% 
      arrange(modality, pd) %>% 
      group_by(pd, modality) %>% 
      mutate(positivity = val / sum(val)) %>% 
      ungroup %>% 
      filter(resultstatus == "Positive") %>% 
      mutate(lab = case_when(pd == "FY19Q1" & modality == "Index"    ~ "Fac",
                             pd == "FY19Q1" & modality == "IndexMod" ~ "Comm"))
      
  #graph type pos
    df_index_type_pos %>% 
      ggplot(aes(pd, positivity, group = modality, color = modality)) +
      geom_hline(yintercept = 0, color = "#bfbfbf") +
      geom_line(size = 1) +
      geom_point(size = 5) +
      geom_text(aes(label = lab),
                na.rm = TRUE,
                family = "Gill Sans MT",
                hjust = -.5) +
      scale_y_continuous(label = percent) +
      scale_color_manual(values = c("#CC5234", "#335B8E")) +
      labs(x = "", y = "") +
      plot_theme() 
    
    ggsave("TZA_index_type_trend_pos_.png", 
           path = "Output",
           dpi = 300, 
           height = 5, width = 7, units = "in")
  
  #volume v yield
    df_index_scatter <- df_mods %>% 
      filter(modality %in% c("Index", "IndexMod")) %>% 
      group_by(indicator, orgunituid, sitename, modality, resultstatus) %>%
      summarise_at(vars(fy2018apr, fy2019q1), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, starts_with("fy")) %>%
      spread(resultstatus, val) %>% 
      mutate(Total = Negative + Positive,
             positivity = Positive / Total) %>% 
      filter(Total != 0) %>% 
      arrange(modality, pd) 
    
    #plot scatter
      df_index_scatter %>% 
        filter(pd == "fy2019q1") %>% 
        ggplot(aes(Total, positivity, color = modality)) +
        geom_point(alpha = .4) +
        scale_x_continuous(label = comma) +
        scale_y_continuous(label = percent) +
        scale_color_manual(values = c("#CC5234", "#335B8E")) +
        expand_limits(x = 825) +
        labs(x = "total index testing volume", y = "positivity") +
        plot_theme()
      
      ggsave("TZA_index_scatter.png", 
             path = "Output",
             dpi = 300, 
             height = 5, width = 5, units = "in")
      
      
    #volume v yield
      df_index_scatter_priority <- df_mods %>% 
        filter(modality %in% c("Index", "IndexMod"),
               orgunituid %in% sites_hts) %>% 
        group_by(indicator, orgunituid, sitename, modality, resultstatus) %>%
        summarise_at(vars(fy2018apr, fy2019q1), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        gather(pd, val, starts_with("fy")) %>%
        spread(resultstatus, val) %>% 
        mutate(Total = Negative + Positive,
               positivity = Positive / Total) %>% 
        filter(Total != 0) %>% 
        arrange(modality, pd) 
      
    #plot scatter
      df_index_scatter_priority %>% 
        filter(pd == "fy2019q1") %>% 
        ggplot(aes(Total, positivity, color = modality)) +
        geom_point(alpha = .4) +
        scale_x_continuous(label = comma) +
        scale_y_continuous(label = percent) +
        scale_color_manual(values = c("#CC5234", "#335B8E")) +
        expand_limits(x = 825, y = 1) +
        labs(x = "total index testing volume", y = "positivity") +
        plot_theme()
      
      ggsave("TZA_index_scatter_priority.png", 
             path = "Output",
             dpi = 300, 
             height = 5, width = 5, units = "in")
      