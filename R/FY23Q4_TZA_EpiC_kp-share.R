# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COP23 Midterm: Community v KP testing
# REF ID:   67019f02 
# LICENSE:  MIT
# DATE:     2024-02-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "67019f02"
  get_metadata("../../../Downloads/Genie-PSNUByIMs-Tanzania-Daily-2024-02-06.zip")

# IMPORT ------------------------------------------------------------------
  
  df_tza <- si_path() %>% 
    return_latest("PSNU_IM.*Tanzania") %>% 
    read_psd()   
  
  df_tza_23 <- read_psd("../../../Downloads/Genie-PSNUByIMs-Tanzania-Daily-2024-02-06.zip")
  

# MUNGE -------------------------------------------------------------------
  
  df_hts <- df_tza %>% 
    filter(fiscal_year != 2023,
           indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
    bind_rows(df_tza_23)

  df_hts_type <- df_hts %>% 
    filter(standardizeddisaggregate != "Total Numerator") %>%
    mutate(type = case_when(str_detect(modality, "Mod") ~ "Community",
                            standardizeddisaggregate == "KeyPop/Result" ~ "KP",
                            standardizeddisaggregate == "Total Numerator" ~ "Total",
                            TRUE ~ "Facility")) 
  
  df_hts_type <- df_hts_type %>% 
    filter(snu1 %in% c("Dar es Salaam", "Shinyanga"),
           mech_code == 81965) %>% 
    group_by(fiscal_year, snu1, mech_code, indicator,type) %>% 
    summarise(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)),
                  .groups = "drop") %>% 
    pivot_longer(c(targets, cumulative),
                 names_to = "value_type"
                 ) %>% 
    filter(value != 0)
  
  df_hts_type <- df_hts_type %>% 
    group_by(fiscal_year, value_type, indicator, snu1) %>% 
    mutate(total = max(value),
           total = case_when(type == "KP" ~ total),
           share = value/total)
  
  df_viz <- df_hts_type %>% 
    mutate(stroke_color = case_when(type == "Community" ~ hw_electric_indigo,
                                  type == "KP" ~ hw_orchid_bloom,
                                  type == "Facility" ~ "#626672"),
           fill_color = ifelse(value_type == "targets", "white", stroke_color),
           fill_alpa = ifelse(value_type == "targets", .6, .9),
           grp = glue("{fiscal_year}_{snu1}_{indicator}_{value_type}"))
  

# VIZ ---------------------------------------------------------------------

  df_viz %>%
    filter(fiscal_year != 2024) %>% 
    ggplot(aes(as.character(fiscal_year), value, group = fct_rev(grp), 
               color = stroke_color, fill = fill_color, alpha = fill_alpa)) +
    geom_line(position = position_dodge(.25), color = "#626672") +
    geom_point(shape = 21, fill = "white", color = "white", alpha = 1,
               size = 4, stroke = 2,
               position = position_dodge(.25)) +
    geom_point(shape = 21, 
               size = 4, stroke = 2,
               position = position_dodge(.25)) +
    geom_text(aes(label = label_percent(1)(share)),
               position = position_dodge(.7),
              color = "#626672", size = 9/.pt,
               na.rm = TRUE) +
    facet_grid(indicator ~ snu1, scales = "free", switch = "y") + 
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    coord_cartesian(clip = "off") +
    expand_limits(y = 0) +
    labs(x = NULL, y = NULL,
         title = "EPIC HAS SEEN A DECLINE IN THE SHARE OF KP TO TOTAL TEST RESULTS",
         subtitle = "EpiC (81965) Targets vs Results Total/Share KP",
         caption = metadata$caption) +
    si_style_ygrid() +
    theme(strip.placement = "outside",
          strip.text = element_text(face = "bold", hjust = .5))
  
  si_preview()

  si_save("Graphics/FY24Q4_TZA_EpiC-Testing-KP-Share.svg")    
  