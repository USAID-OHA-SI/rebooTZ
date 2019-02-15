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
  df_mods <- df_genie_site %>% 
    filter(fundingagency == "USAID",
           indicator == "HTS_TST",
           standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", 
                                           "Modality/Age/Sex/Result"))

  #
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
    geom_histogram(bins = 20, color = "white", 
                 fill = c("#CC5234", rep("#595959", 19))) +
    scale_x_continuous(label = percent) +
    labs(x = "site's share of positives", y = "sites") +
    plot_theme() 
  
  ggsave("TZA_pos_bysite.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in")  
  
  df_site_pos %>% 
    mutate(below = positivity <.05) %>% 
    count(below) %>% 
    spread(below, n) %>% 
    mutate(sites_more_men = `TRUE`/ (`FALSE` + `TRUE`))
  
  #modality breakdown
  df_mod_pos <- df_mods %>% 
    group_by(indicator, modality, resultstatus) %>% 
    summarise_at(vars(fy2019q1), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(resultstatus, fy2019q1, fill = 0) %>% 
    mutate(Total = Negative + Positive,
           positivity = Positive / Total) %>% 
    filter(Total != 0) %>% 
    arrange(Total) %>% 
    mutate(modality = as_factor(modality))
  
  df_mod_pos %>% 
    ggplot(aes(modality, Total)) +
    geom_col(fill = "#335B8E") +
    geom_text(aes(y = Total, label = comma(Total)),
              hjust = -.3, color = "#595959",
              family = "Gill Sans MT") +
    coord_flip() +
    expand_limits(y = 600000) +
    labs(x = "", y = "") +
    plot_theme() +
    theme(axis.text.x = element_blank())
  
  ggsave("TZA_pos_bymod.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in")  
  
  df_mod_pos <- df_mod_pos %>% 
    arrange(positivity) %>% 
    mutate(modality = as.character(modality),
           modality = as_factor(modality))
  
  df_mod_pos %>% 
    ggplot(aes(modality, positivity)) +
    geom_col(fill = "#335B8E") +
    geom_text(aes(y = positivity, label = percent(positivity)),
              hjust = -.3, color = "#595959",
              family = "Gill Sans MT") +
    coord_flip() +
    expand_limits(y = .18) +
    labs(x = "", y = "") +
    plot_theme() +
    theme(axis.text.x = element_blank())
  
  ggsave("TZA_yield_bymod.png", 
         path = "Output",
         dpi = 300, 
         height = 5, width = 7, units = "in")  
