# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  "push" countries review - testing coverage/peformance 
# REF ID:   9a17424e 
# LICENSE:  MIT
# DATE:     2023-10-05
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(ggrepel)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "9a17424e" #id for adorning to plots, making it easier to find on GH
  
  get_metadata() #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_psd() %>% 
    filter(country == "Tanzania")
  
  
  df_arch <- si_path() %>% 
    list.files() %>% 
    read_psd() %>% 
    filter(country == "Tanzania")
  
  df_subnat <- si_path() %>% 
    list.files("NAT_SUBNAT", full.names = TRUE) %>% 
    map_dfr(read_psd) %>% 
    filter(country == "Tanzania")

  
# MUNGE -------------------------------------------------------------------
  
  df_hts <- df_msd %>% 
    filter(indicator %in% c("HTS_TST_POS", "HTS_SELF"),
           fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    pluck_totals()

  df_hts <- df_hts %>% 
    group_by(fiscal_year, funding_agency, snu1, indicator) %>% 
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    adorn_achievement(qtr = metadata$curr_qtr)

  
  df_hts_m <- df_msd %>% 
    filter(indicator == "HTS_TST_POS",
           funding_agency == "USAID") %>% 
    count(fiscal_year, standardizeddisaggregate)
  
# VIZ ---------------------------------------------------------------------


  df_hts %>% 
    ggplot(aes(targets, cumulative, fill = achv_color, size = targets)) +
    geom_abline(slope = .65, linetype = 'dashed', color = matterhorn, alpha = .4) +
    geom_abline(slope = .85, linetype = 'dashed', color = matterhorn, alpha = .4) +
    geom_blank(aes(cumulative, targets)) +
    geom_point(shape = 21, color = "white",
               alpha = .9, na.rm = TRUE) +
    geom_text_repel(aes(label = snu1), 
                    data = df_hts %>% filter(achievement < (metadata$curr_qtr/4)-.1, targets > 2e3),
                    size = 11/.pt,
                    family = "Source Sans Pro",
                    color = matterhorn) + 
    facet_wrap(~fct_rev(indicator), scales = "free") +
    scale_fill_identity() +
    scale_x_continuous(expand = c(.005, .005), labels = label_number(scale_cut = cut_short_scale())) +
    scale_y_continuous(expand = c(.005, .005), labels = label_number(scale_cut = cut_short_scale())) +
    scale_size(guide = NULL) +
    coord_cartesian(clip = "off") +
    labs(x = glue("{metadata$curr_fy_lab} Targets"),
         y = glue("{metadata$curr_fy_lab} Results (thru Q{metadata$curr_qtr})"),
         caption = metadata$caption) +
    si_style() +
    theme(strip.text = element_text(face = "bold"))

  
  si_save(glue("Images/{metadata$curr_pd}_TZA_push_test.png"))  
