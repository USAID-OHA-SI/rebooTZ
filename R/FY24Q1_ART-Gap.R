# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Testing need to close TX Gap
# REF ID:   203864b9 
# LICENSE:  MIT
# DATE:     2024-02-20
# UPDATED:  2024-02-21


# DATIM -------------------------------------------------------------------

# PSNU By IM
# DATIM data as of: 2/15/2024, 22:57:07 UTC
# Genie report updated: 2/20/2024, 05:56:10 UTC
# Current period(s): 2023 Q1, 2023 Q2, 2023 Q3, 2023 Q4, 2023 Target, 2024 Q1, 2024 Target
# Operating Unit: Tanzania
# Daily/Frozen: Daily
# Indicators: HTS_TST, HTS_TST_POS, TX_CURR, TX_NEW
# Fiscal Year: 2024, 2023


# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(readxl)
  library(janitor)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "203864b9"
  
  path_genie <- "Data/Genie-PSNUByIMs-Tanzania-Daily-2024-02-20.zip"
  
  path_this <- "../../../Documents/2024.02 TZA/THIS PLHIV - ART Coverage Feb 5 2024.xlsx"
  
  get_metadata(path_genie)
  
  key_regions <- c("Dodoma", "Kilimanjaro", "Singida", "Ruvuma", "Morogoro")

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM.*Tanzania") %>% 
    read_psd()   
  
  df_genie <- read_psd(path_genie)
  
  df_this <- read_excel(path_this, skip = 1,
                        .name_repair = make_clean_names)
  

# MUNGE -------------------------------------------------------------------
  
  df_tza <- df_msd %>% 
    filter(indicator %in% unique(df_genie$indicator),
           fiscal_year %ni% unique(df_genie$fiscal_year)) %>% 
    bind_rows(df_genie) %>%  
    filter(use_for_age == "Y",
           snu1 %in% key_regions,
           trendscoarse == "15+") %>% 
    mutate(mod_type = case_when(str_detect(modality, 'Mod') ~ "Community",
                                indicator %in% c("HTS_TST", "HTS_TST_POS") ~ "Facility"))


# COMM V FAC SHARES -------------------------------------------------------
 
  #modality source
  df_mod <- df_tza %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) 
    
  
  #list of all the modalities by type
  v_mods <- df_mod %>% 
    filter(cumulative > 0) %>% 
    distinct(mod_type, modality) %>% 
    arrange(mod_type, modality) %>% 
    group_by(mod_type) %>% 
    summarise(all = paste0(modality, collapse = ", ")) %>% 
    unite(mods, c(mod_type, all), sep = ": ") %>% 
    pull() %>% 
    paste(collapse = "\n")
    

  df_mod_trends <- df_mod %>% 
    group_by(fiscal_year, snu1, indicator, mod_type) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  df_mod_trends <- df_mod_trends %>% 
    reshape_msd(include_type = FALSE) %>% 
    group_by(period, snu1, indicator) %>% 
    mutate(share = value/sum(value)) %>% 
    ungroup()
  
  #period
  v_pds <- df_mod_trends %>% 
    distinct(period) %>% 
    slice_max(order_by = period, n =  6) %>% 
    pull() %>% 
    sort()
  
  df_viz_trends <- df_mod_trends %>% 
    filter(period %in% v_pds) %>% 
    mutate(point_end = case_when(period == max(period) ~ share),
           point_start = case_when(period == min(period) ~ share),
           rib_min = case_when(mod_type == "Community" ~ share),
           rib_max = case_when(mod_type == "Facility" ~ share),
           order = ifelse(indicator == "HTS_TST" & period == max(period), value, 0)) %>% 
    group_by(period, snu1, indicator) %>% 
    fill(starts_with("rib"), .direction = "updown") %>% 
    ungroup()
  
  
  pd_brks <- df_viz_trends %>% 
    distinct(period) %>% 
    mutate(label = ifelse(period == max(period) | period == min(period), period, "")) %>% 
    pull()
  
  
  df_labs <- df_viz_trends %>% 
    filter(period == max(period)) %>% 
    count(snu1, indicator, period, wt = value) %>% 
    mutate(n = label_number(.1,scale_cut = cut_short_scale())(n),
           indicator = ifelse(indicator == "HTS_TST_POS", "HTS_POS", indicator)) %>% 
    unite(values, c(indicator, n), sep = ": ") %>% 
    group_by(snu1, period) %>% 
    summarise(values = paste0(values, collapse = " | "),
              .groups = "drop") %>% 
    mutate(snu1_lab = glue("{snu1}<br><span style = 'font-size:6pt'>{values} [{period}]</span>")) %>% 
    select(snu1, snu1_lab)
  
  df_viz_trends <- df_viz_trends %>% 
    left_join(df_labs, by = join_by(snu1))
  
  df_viz_trends %>%
    ggplot(aes(period, share, group = mod_type, color = mod_type, fill = mod_type,
               label = label_percent(1)(share))) +
    geom_ribbon(aes(ymin = rib_min, ymax = rib_max), alpha = .1, fill = "#909090") +
    geom_hline(aes(yintercept = 0), color = "grey92") +
    geom_line() +
    geom_point(aes(y = point_start), na.rm = TRUE, shape = 21, stroke = 1.2) +
    geom_point(aes(y = point_end), na.rm = TRUE,  shape = 21, stroke = 1.2, fill = "white") +
    geom_text(aes(y = point_start), na.rm = TRUE,
              family = "Source Sans Pro", color = matterhorn,
              size = 9/.pt, nudge_x = -.9) +
    geom_text(aes(y = point_end), na.rm = TRUE,
              family = "Source Sans Pro", color = matterhorn,
              size = 9/.pt, nudge_x = .9) +
    facet_grid(indicator~fct_reorder(snu1_lab, order, sum, .desc = TRUE), switch = "y") +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    scale_x_discrete(labels = pd_brks) +
    scale_color_manual(aesthetics = c("color", "fill"),
                       values = c("Community" = hw_viking,
                                  "Facility" = hw_hunter)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "A overwhelming majority of tests in these regions comes from facilities" %>% toupper,
         subtitle = glue("Regional quarterly shares of testing (15+) from <span style = 'color:{hw_hunter};'>facility</span> or <span style = 'color:{hw_viking};'>community</span> entry points"),
         caption = glue("{v_mods}
                        {metadata$caption}")) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          strip.text.y = element_text(face = "bold", hjust = .5),
          strip.text.x = element_markdown(),
          axis.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          legend.position = "none")
    
  
  # si_preview()
  
  si_save(glue("{metadata$curr_pd}_TZA_tst-comm-share.png"),
          path = "Images",
          scale = 1.1)

# AGE/SEX -----------------------------------------------------------------

  df_mod_age <- df_mod %>% 
    group_by(fiscal_year, snu1, indicator, mod_type, sex, target_age_2024) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") 
  
  df_mod_age <- df_mod_age %>% 
    reshape_msd(include_type = FALSE)
  
  df_mod_age_trends <- df_mod_age %>% 
    filter(period %in% v_pds) %>% 
    group_by(snu1, period, indicator, sex, target_age_2024) %>% 
    mutate(share = value/sum(value)) %>% 
    ungroup() %>% 
    mutate(point_end = case_when(period == max(period) ~ share),
           point_start = case_when(period == min(period) ~ share),
           rib_min = case_when(mod_type == "Community" ~ share),
           rib_max = case_when(mod_type == "Facility" ~ share)) %>% 
    group_by(period, snu1, indicator, sex, target_age_2024) %>% 
    fill(starts_with("rib"), .direction = "updown") %>% 
    ungroup()
  
  
  plot_agesex <- function(region, save = FALSE) {
    
    df_viz_age_trends <- df_mod_age_trends %>% 
      filter(snu1 == {region})
    
    pd_brks <- df_viz_age_trends %>% 
      distinct(period) %>% 
      mutate(label = ifelse(period == max(period) | period == min(period), period, "")) %>% 
      pull()
    
    df_viz_age_trends %>% 
      ggplot(aes(period, share, group = mod_type, color = mod_type, fill = mod_type,
                 label = label_percent(1)(share))) +
      geom_ribbon(aes(ymin = rib_min, ymax = rib_max), alpha = .1,
                  fill = "#909090") +
      geom_hline(aes(yintercept = 0), color = "grey92") +
      geom_line() +
      geom_point(aes(y = point_start), na.rm = TRUE,
                 shape = 21, stroke = 2) +
      geom_point(aes(y = point_end), na.rm = TRUE,
                 shape = 21, stroke = 2,
                 fill = "white") +
      geom_text(aes(y = point_start), na.rm = TRUE,
                family = "Source Sans Pro", color = matterhorn,
                size = 9/.pt, nudge_x = -1.2) +
      geom_text(aes(y = point_end), na.rm = TRUE,
                family = "Source Sans Pro", color = matterhorn,
                size = 9/.pt, nudge_x = 1.5) +
      facet_grid(indicator~sex + target_age_2024, switch = "y",
                 # labeller = labeller(.multi_line = FALSE)
      ) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
      scale_x_discrete(labels = pd_brks) +
      scale_color_manual(aesthetics = c("color", "fill"),
                         values = c("Community" = hw_viking,
                                    "Facility" = hw_hunter)) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL,
           title = toupper({region}),
           subtitle = glue("Age/sex quarterly shares of testing from <span style = 'color:{hw_hunter};'>facility</span> or <span style = 'color:{hw_viking};'>community</span> entry points"),
           caption = glue("{v_mods}
                        {metadata$caption}")) +
      si_style_nolines() +
      theme(strip.placement = "outside",
            strip.text.y = element_text(face = "bold", hjust = .5),
            strip.text.x = element_markdown(hjust = .5),
            axis.text.y = element_blank(),
            plot.subtitle = element_markdown(),
            legend.position = "none",
            panel.spacing = unit(2, "picas"))
    
    if(save == TRUE){
      si_save(glue("{metadata$curr_pd}_TZA_tst-share-age-{region}.png"),
              path = "Images",
              scale = 1.1)
    }
  }
  
  plot_agesex_val <- function(region, save = FALSE) {
    df_viz_as <- df_mod_age_trends %>% 
      filter(snu1 == {region},
             period == max(period)) %>% 
      unite(agesex, c(sex, target_age_2024), sep = " ") %>% 
      count(indicator, period, agesex, wt = value, name = "value") 
    
    df_viz_as %>% 
      ggplot(aes(value, fct_rev(agesex))) +
      geom_col(fill = "#626672") +
      facet_wrap(~indicator, scales = "free_x") +
      scale_x_continuous(label = label_comma()) +
      labs(x = NULL, y = NULL,
           title = toupper({region}),
           subtitle = glue("Total tests by age/sex in {unique(df_viz_as$period)}"),
           caption = glue("{metadata$caption}")) +
    si_style_xgrid()
    
    if(save == TRUE){
      si_save(glue("{metadata$curr_pd}_TZA_tst-share-age-val-{region}.png"),
              path = "Images",
              scale = 1.1)
    }
    
  }
  
  
  # si_preview()
  
  map(key_regions,
       ~plot_agesex(.x, save = TRUE))
  
  walk(key_regions,
      ~plot_agesex_val(.x, save = TRUE))
  


# GAP ---------------------------------------------------------------------

  df_tx <- df_tza %>% 
    filter(indicator == "TX_CURR",
           fiscal_year == metadata$curr_fy) %>% 
    count(snu1, wt = cumulative, name = "tx_curr")
  
  df_gap <- df_this %>% 
    select(region, art_coverage = this_2022_2023, plhiv_est = estimated_plhiv_15, plhiv_ucl = ucl_2) %>% 
    filter(region %in% key_regions) %>% 
    left_join(df_tx, by = c("region" = "snu1")) %>% 
    mutate(art_est = plhiv_est * art_coverage,
           gap = (plhiv_ucl * .95^2) - (plhiv_est * art_coverage),
           gap_mer = (plhiv_ucl * .95^2) - tx_curr) %>% 
    mutate(across(c(plhiv_est, plhiv_ucl, art_est, gap, gap_mer), \(x) round(x, 0)))

   #modality source
   df_hts <- df_tza %>% 
     filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) 
   
   df_hts_stats <- df_hts %>% 
     group_by(fiscal_year, snu1, indicator) %>% 
     summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
               .groups = "drop") 
   
   df_hts_stats <- df_hts_stats %>% 
     reshape_msd(include_type = FALSE) %>% 
     filter(period %in% v_pds) %>% 
     group_by(snu1, indicator) %>% 
     summarise(total = sum(value),
               avg = mean(value),
               .groups = "drop")

   
   #modality source
   df_cont <- df_hts %>% 
     group_by(fiscal_year, snu1, indicator, mod_type, modality) %>% 
     summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
               .groups = "drop") 
   
   df_cont <- df_cont %>% 
     reshape_msd(include_type = FALSE) %>% 
     filter(period %in% v_pds,
            value != 0) %>% 
     group_by(snu1, indicator, mod_type, modality) %>% 
     summarise(total = sum(value),
               qtrly_avg = mean(value),
               .groups = "drop") %>% 
     pivot_wider(names_from = indicator,
                 values_from = c(total, qtrly_avg),
                 values_fill = 0) %>% 
     mutate(positivity = total_HTS_TST_POS/total_HTS_TST) %>% 
     group_by(snu1) %>% 
     mutate(pos_contribution = total_HTS_TST_POS / sum(total_HTS_TST_POS)) %>% 
     ungroup()

   df_cont <- df_cont %>% 
     left_join(df_gap, by = c("snu1" = "region")) 

   df_cont <- df_cont %>% 
     mutate(gap_pos_mod = gap_mer * pos_contribution,
            gap_tst_mod = gap_pos_mod/positivity) %>% 
     mutate(gap_tst_mod = ifelse(is.nan(gap_tst_mod), 0, gap_tst_mod)) %>% 
     mutate(qtrly_gap_pos_mod = round(gap_pos_mod / 7),
            qtrly_gap_tst_mod = round(gap_tst_mod / 7)) 

   
   plot_htsneed <- function(region, save = FALSE) {
     
     df_viz_gap <- df_cont %>%
       filter(snu1 == {region})
     
     subt <- glue("With a PLHIV (UCL) of {label_comma()(unique(df_viz_gap$plhiv_ucl))} and TX_CURR of {label_comma()(unique(df_viz_gap$tx_curr))}, {region} needs to close a {label_comma()(unique(df_viz_gap$gap_mer))} ART gap in the next 7 quarters. Reallocating tests to higher positivity modalities will reduce the total number of tests needed and increase the speed to close the gap.") %>% 
       str_wrap(width = 130)
     
     v1 <-  df_viz_gap %>% 
       ggplot(aes(y = fct_reorder(modality, qtrly_gap_tst_mod), color = mod_type, fill = mod_type)) +
       geom_segment(aes(x = qtrly_gap_tst_mod, xend = qtrly_avg_HTS_TST, yend = fct_reorder(modality, qtrly_gap_tst_mod)),
                    arrow = arrow(length = unit(.5, "picas"), ends = "first", type = "closed")
       ) +
       # geom_point(aes(x = qtrly_gap_tst_mod), shape = 21, fill = "white") +
       # geom_text(aes(x = qtrly_gap_tst_mod, label = label_comma()(qtrly_gap_tst_mod)),
       geom_text(aes(x = qtrly_gap_tst_mod, label = label_number(.1, scale_cut = cut_short_scale())(qtrly_gap_tst_mod)),
                 family = "Source Sans Pro", size = 10/.pt, color = matterhorn, vjust = -1) +
       geom_point(aes(x = qtrly_avg_HTS_TST), shape = 21) +
       facet_grid(mod_type ~ ., scales = "free_y", space = "free_y", switch = "y") +
       scale_x_continuous(label = label_comma()) +
       scale_color_manual(aesthetics = c("color", "fill"),
                          values = c("Community" = hw_viking,
                                     "Facility" = hw_hunter)) +
       coord_cartesian(clip = "off") +
       labs(x = NULL, y = NULL,
            subtitle = "Estimated quarterly testing (HTS_TST) change needed to close the treatment gap by the end of FY25") +
       si_style_xgrid() +
       theme(strip.placement = "outside",
             strip.text.y = element_text(hjust = .5),
             legend.position = "none",
             panel.spacing = unit(2, "picas"))
     
     
     v2 <- df_viz_gap %>% 
       ggplot(aes(positivity, fct_reorder(modality, qtrly_gap_tst_mod), color = mod_type, fill = mod_type)) +
       geom_vline(aes(xintercept = 0), color = "grey92") +
       geom_segment(aes(x = 0, xend = positivity, yend = fct_reorder(modality, qtrly_gap_tst_mod))) +
       geom_point() +
       geom_text(aes(label = label_percent(1)(positivity)),
                 family = "Source Sans Pro", color = matterhorn, size = 9/.pt,
                 hjust =  -.4) +
       facet_grid(mod_type ~ ., scales = "free_y", space = "free_y") +
       scale_x_continuous(labels = label_percent()) +
       coord_cartesian(clip = "off") +
       scale_color_manual(aesthetics = c("color", "fill"),
                          values = c("Community" = hw_viking,
                                     "Facility" = hw_hunter)) +
       labs(x = NULL, y = NULL,
            subtitle = "Historic Positivity") +
       si_style_nolines() +
       theme(axis.text = element_blank(),
             strip.placement = "outside",
             strip.text.y = element_blank(),
             legend.position = "none",
             panel.spacing = unit(2, "picas"))
     
     v_x <- v1 + v2 + plot_layout(widths = c(3, 1))  +
       plot_annotation(title = toupper({region}),
                       subtitle = subt,
                       caption = glue("Assumes 100% linkage, no LTFU, stagnant PLHIV, no change in positivity, or modality contribution | 15+ Only | Historic period covered: {v_pds[1]} - {v_pds[length(v_pds)]}
                                    {metadata$caption}"),
                       theme = si_style())
     
     if(save == TRUE){
       si_save(glue("{metadata$curr_pd}_TZA_tst-needed-{region}.png"),
               path = "Images",
               scale = 1.1)
     } else {
       return(v_x)
     }
   }
   
  
   # plot_htsneed(key_regions[4])
   # si_preview()
   
   walk(key_regions,
        ~plot_htsneed(.x, save = TRUE))
   
   # ART Gap =  PLHIV [UCL] * .95^2 - TX_CURR
   # Modality Contribution = Modality positive total over total positives (total over last 6 quarters)
   # Positivity = HTS_TST_POS / HTS_TST (total over last 6 quarters)
   # Total tests needed to close the gap = ART Gap * Historic Modality Contribution to Total Positives / Positivity 
   # Quarterly Testing Needed = Total tests needed to close the gap / 7 quarters remaining
  