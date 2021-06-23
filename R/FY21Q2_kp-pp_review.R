# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  KP review of FY21Q2 data
# LICENSE:  MIT
# DATE:     2021-06-21
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ind_sel <- c("KP_PREV", "PP_PREV", "PrEP_NEW", "PreP_CURR")
  source_info <- "Source: FY21Q2i MSD"

# IMPORT ------------------------------------------------------------------
    
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")



# PREV - RESULTS/TARGETS --------------------------------------------------

  curr_yr <- identifypd(df, "year")
  
  df_prev <- df %>% 
    filter(indicator %in% c("KP_PREV", "PP_PREV"),
           standardizeddisaggregate %in% c("Total Numerator", "KeyPop", "Age/Sex"),
           ageasentered %in% c(NA, "15-19", "20-24"),
           fiscal_year == curr_yr) %>% 
    mutate(otherdisaggregate = case_when(str_detect(otherdisaggregate, "PWID") ~ "PWID", 
                                         str_detect(otherdisaggregate, "MSM") ~ "MSM",
                                         TRUE ~ otherdisaggregate)) %>% 
    group_by(fiscal_year, operatingunit, indicator, standardizeddisaggregate, ageasentered, otherdisaggregate) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    calc_achievement()
  
  df_prev <- df_prev %>% 
    mutate(ind_disagg = case_when(standardizeddisaggregate == "Total Numerator" ~ "Total",
                                  standardizeddisaggregate == "KeyPop" ~ otherdisaggregate,
                                  standardizeddisaggregate == "Age/Sex" ~ ageasentered),
           ind_disagg = factor(ind_disagg, c("Total", "FSW", "MSM", "PWID", "15-19", "20-24")),
           fill_color = ifelse(ind_disagg == "Total", scooter, scooter_light))
           
  df_prev %>% 
    ggplot(aes(ind_disagg)) +
    geom_col(aes(y = targets), fill = trolley_grey_light, alpha = .6) +
    geom_col(aes(y = cumulative, fill = fill_color)) +
    geom_errorbar(aes(ymax = targets, ymin = targets), color = trolley_grey, linetype = "dashed") +
    geom_text(aes(y = cumulative, label = percent(achievement, 1)),
               family = "Source Sans Pro SemiBold", color = "#202020",
               vjust = -.4) +
    scale_y_continuous(labels = comma) +
    scale_fill_identity() +
    facet_grid(~indicator, scales = "free", space = "free") +
    labs(x = NULL, y = NULL,
         caption = source_info) +
    si_style_ygrid()
  
  si_save("Images/FY20Q2_PREV_achievement.png",
          height = 4.15, width = 9.32)
  
  df_prev %>% 
    select(fiscal_year, indicator, ind_disagg, cumulative, targets, achievement)

# PREP - RESULTS/TARGETS --------------------------------------------------


  df_prep <- df %>% 
    filter(indicator %in% c("PrEP_NEW", "PrEP_CURR"),
           standardizeddisaggregate %in% c("Total Numerator", "KeyPop",
                                           "KeyPopAbr","Age/Sex"),
           # ageasentered %in% c(NA, "15-19", "20-24"),
           fiscal_year == curr_yr) %>% 
    mutate(standardizeddisaggregate = ifelse(standardizeddisaggregate =="KeyPopAbr", "KeyPop", standardizeddisaggregate)) %>% 
    group_by(fiscal_year, operatingunit, indicator, standardizeddisaggregate, otherdisaggregate, ageasentered) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    calc_achievement()
  
  df_prep <- df_prep %>% 
    mutate(ind_disagg = case_when(standardizeddisaggregate == "Total Numerator" ~ "Total",
                                  standardizeddisaggregate == "KeyPop" ~ otherdisaggregate,
                                  standardizeddisaggregate == "Age/Sex" ~ ageasentered),
           fill_color = ifelse(ind_disagg == "Total", moody_blue, moody_blue_light),
           targets = na_if(targets, 0),
           standardizeddisaggregate = factor(standardizeddisaggregate, c("Total Numerator", "Age/Sex", "KeyPop")),
           placement = ifelse(standardizeddisaggregate == "Total Numerator", cumulative, targets)) %>% 
    filter(ind_disagg != "Unknown Age")
  
  
  df_prep %>% 
    filter(!is.na(targets)) %>% 
    ggplot(aes(ind_disagg)) +
    geom_col(aes(y = targets), fill = trolley_grey_light, alpha = .6) +
    geom_col(aes(y = cumulative, fill = fill_color)) +
    geom_errorbar(aes(ymax = targets, ymin = targets), color = trolley_grey, linetype = "dashed") +
    geom_text(aes(y = placement, label = percent(achievement, 1)),
              family = "Source Sans Pro SemiBold", color = "#202020",
              vjust = -.4) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_fill_identity() +
    facet_grid(indicator~ standardizeddisaggregate, scales = "free", space = "free") +
    labs(x = NULL, y = NULL,
         caption = source_info) +
    si_style_ygrid() +
    theme(strip.text.x = element_blank(),
          panel.spacing.y = unit(.5, "lines"),
          panel.spacing.x = unit(.5, "lines"))

  si_save("Images/FY20Q2_PrEP_achievement.png",
          height = 4.15, width = 9.32)  
  
  

# EXPORT DATA -------------------------------------------------------------

  df_prev %>% 
    bind_rows(df_prep) %>% 
    select(fiscal_year, operatingunit, indicator, ind_disagg, cumulative, targets, achievement) %>% 
    write_csv("Dataout/FY21Q2i_TZA_PREV-PrEP-Achievement.csv", na = "")
  
  
  df_prep %>% 
    select(fiscal_year, indicator, ind_disagg, cumulative, targets, achievement)
  