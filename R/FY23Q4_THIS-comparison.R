# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  investigate new THIS results
# REF ID:   8b93f398 
# LICENSE:  MIT
# DATE:     2023-12-05
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
  library(ggrepel)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "8b93f398" #id for adorning to plots, making it easier to find on GH
  
  get_metadata(type = "SUBNAT") #list of MSD metadata elements
  
  load_secrets('email')
  
  gs_id <- as_sheets_id('1hY2TNcOXYXU7qlxaIoTEMbR1KTr12vZFtBiyOf5Q3B4') 

# IMPORT ------------------------------------------------------------------
  
  #THIS data
  df_this <- read_sheet(gs_id)  

  #NAT_SUBNAT
  df_subnat <- si_path() %>% 
    return_latest("SUBNAT") %>% 
    read_psd()   
  
  #MSD
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_psd() %>% 
    filter(country == "Tanzania",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator")
  
  
# MUNGE -------------------------------------------------------------------

  # df_subnat %>%
  #   filter(operatingunit == "Tanzania",
  #          indicator %in% c("POP_EST","PLHIV","HIV_PREV", "TX_CURR_SUBNAT"),
  #          standardizeddisaggregate %ni% c('Total Numerator', 'Age Aggregated/Sex/HIVStatus')) %>% 
  #   count(fiscal_year, indicator, standardizeddisaggregate, wt = targets) %>%
  #   pivot_wider(names_from = "fiscal_year",
  #               values_from = 'n') %>% 
  #   arrange(indicator, standardizeddisaggregate)
  
  
  df_subnat <- df_subnat %>%
    filter(operatingunit == "Tanzania",
           indicator %in% c("POP_EST","PLHIV",#"HIV_PREV", 
                            "TX_CURR_SUBNAT"),
           standardizeddisaggregate %ni% c('Total Numerator', 'Age Aggregated/Sex/HIVStatus'))
  

  df_agencies <- df_msd %>% 
    filter(indicator == "TX_CURR",
           psnu != "_Military Tanzania",
           fiscal_year == metadata$curr_fy) %>% 
    pluck_totals() %>% 
    clean_agency() %>% 
    count(psnu, psnuuid, funding_agency, wt = targets, sort = TRUE) %>% 
    group_by(psnu, psnuuid,) %>% 
    mutate(share = n / sum(n)) %>% 
    filter(share == max(share)) %>% 
    ungroup() %>% 
    select(psnu, psnuuid, funding_agency) %>% 
    mutate(funding_agency = fct_inorder(funding_agency))
  
  df_tx_curr <- df_msd %>% 
    filter(indicator == "TX_CURR",
           psnu != "_Military Tanzania",
           fiscal_year == metadata$curr_fy) %>% 
    pluck_totals() %>% 
    count(psnu, psnuuid, wt = cumulative, name = "tx_curr") 
  
# CHANGE IN INCIDENCE -----------------------------------------------------

  dodge <- position_dodge(width = .5)
  
  df_this %>% 
    filter(indicator == "Incidence",
           age == "15-49") %>%
    mutate(yr_lab = case_when(sex == "Total" ~ year),
           fill_alpha = ifelse(sex == "Total", 1, .6)) %>% 
    ggplot(aes(fct_rev(sex), value, color = year)) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  width = 0.2,
                  position = dodge) +
    geom_point(size = 15, 
               position = dodge) +
    geom_label(aes(label = label_percent()(value)),
              position = dodge,
              family = "Source Sans Pro") +
    geom_text(aes(y = ci_high, label = yr_lab), na.rm = TRUE,
               position = dodge, vjust = -1,
               family = "Source Sans Pro") +
    scale_y_continuous(labels = label_percent()) +
    scale_color_manual(values = c("#606060", scooter_med)) +
    scale_x_discrete(position = 'top') +
    scale_alpha_identity() +
    labs(title = "Modest decline in Incidence Rate in 5 years" %>% toupper(),
         subtitle = "Difference of about 72k new infections in 2017 compared with 60k in 2023",
         x = NULL, y = NULL,
         caption = glue("Source: THIS 2016-2017 and 2022-2023 Summary Sheets | Ref Id: {ref_id}")) +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text.x = element_text(face = "bold", size = 12))
  
  # si_preview()
  si_save("Images/FY23Q4_TZA-THIS-incidence.png")
  

# PLHIV COMPARISON --------------------------------------------------------

  
  df_prev <- df_subnat %>% 
    filter(trendscoarse == "15+") %>%
    count(fiscal_year, indicator, #ageasentered, sex, 
          wt = targets, name = "value") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(prev = plhiv/pop_est)
  
  pop23 <- df_prev %>% 
    filter(fiscal_year == 2023) %>% 
    select(pop_est) %>% 
    pull()
  
  this_prev_bands <- df_this %>% 
    filter(year == "2022-2023",
           indicator == "Prevalence",
           region == "Tanzania",
           age == "15+",
           sex == "Total") %>% 
    select(prev_this = value, starts_with("ci_")) 
  
  df_this_plhiv_est <- this_prev_bands %>% 
    mutate(fiscal_year = 2023, 
           pop_est = pop23,
           .before = 1) %>% 
    mutate(plhiv_est = pop_est * prev_this,
           plhiv_est_low = pop_est * ci_low,
           plhiv_est_high = pop_est * ci_high)
  
  df_prev_est <- df_prev %>% 
    left_join(df_this_plhiv_est) %>% 
    mutate(across(starts_with("plhiv_"), ~ ifelse(fiscal_year == 2022, plhiv, .)))
  
  pt_labels <- df_prev_est %>% 
    filter(fiscal_year %in% c(2022:2023)) %>%
    mutate(across(starts_with('plhiv_'), ~ ifelse(fiscal_year == 2022, NA, .))) %>% 
    select(fiscal_year, starts_with("plhiv")) %>% 
    pivot_longer(-fiscal_year, 
                 names_to = "type",
                 values_drop_na = TRUE)
  
  df_prev_est %>% 
    ggplot(aes(fiscal_year, plhiv, group = "x")) +
    # geom_blank(aes(y = 0)) +
    geom_ribbon(aes(ymin = plhiv_est_low, ymax = plhiv_est_high),
                fill = scooter_light,
                # fill = "gray30",
                alpha = .2) +
    geom_line(color = "#606060", linewidth = 1) +
    geom_line(aes(y = plhiv_est), linewidth = 1,
              linetype = "dotted", color = scooter_med,
              na.rm = TRUE) +
    geom_point(aes(y = plhiv_est), shape = 21,  size = 4,
               fill = "white", color = scooter_med,
               stroke = 2, na.rm = TRUE) +
    geom_point(shape = 21, stroke = 2, size = 4,
               color = "#606060", fill= "#606060") +
    geom_label_repel(data = pt_labels,
                    aes(y = value, label = label_number(.01, scale_cut = cut_short_scale())(value)),
                    family = "Source Sans Pro", color = matterhorn,
                    force = 15) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    labs(x = NULL, y = NULL,
         title = "THIS SHOWS DECLINE IN PLHIV POINT ESTIMATE from PEPFAR PLANNING ESTIMATE" %>% toupper(),
         subtitle = glue("PEPFAR point estimate still falls within THIS confidence interval; THIS prevalence reported at {percent_format(.1)(this_prev_bands$prev_this)} [{percent_format(.1)(this_prev_bands$ci_low)} - {percent_format(.1)(this_prev_bands$ci_high)}]"),
         caption = glue("Note: THIS PLHIV Estimate = PEPFAR FY23 POP_EST (15+) * 2022-3 THIS Prevalence/CI
                        Source: {metadata$source} + THIS 2016-2017 and 2022-2023 Summary Sheets | Ref Id: {ref_id}")) +
    si_style()
  

  # si_preview()
  si_save("Images/FY23Q4_TZA-THIS-plhiv.png")
  
# REGIONAL PLHIV COMPARISON -----------------------------------------------

  
  df_prev_reg <- df_subnat %>% 
    filter(fiscal_year < 2024, 
           trendscoarse == "15+") %>%
    count(fiscal_year, psnu, indicator,
          wt = targets, name = "value") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(prev = plhiv/pop_est)
  
  this_prev_bands_reg <- df_this %>% 
    filter(year == "2022-2023",
           indicator == "Prevalence",
           region != "Tanzania",
           age == "15+",
           sex == "Total") %>% 
    select(psnu = region, prev_this = value, starts_with("ci_")) %>% 
    mutate(fiscal_year = 2023, .before = 1)
  
  
  df_plhiv_est_reg <- df_prev_reg %>% 
    filter(fiscal_year == 2023) %>% 
    tidylog::left_join(this_prev_bands_reg)
  
  
  df_plhiv_est_reg <- df_plhiv_est_reg %>% 
    mutate(plhiv_est = pop_est * prev_this,
           plhiv_est_low = pop_est * ci_low,
           plhiv_est_high = pop_est * ci_high)
  
  df_plhiv_est_reg <- df_plhiv_est_reg %>% 
    tidylog::left_join(df_agencies) %>% 
    tidylog::left_join(df_tx_curr)
  
  df_plhiv_est_reg <- df_plhiv_est_reg %>% 
    mutate(fill_color = ifelse(plhiv > plhiv_est_high, burnt_sienna, "#606060"))
  
  n_over <- df_plhiv_est_reg %>% count(plhiv > plhiv_est_high) %>% filter(`plhiv > plhiv_est_high` == TRUE) %>% pull()
  
  df_plhiv_est_reg %>% 
    filter(!is.na(funding_agency)) %>%
    mutate(funding_agency = factor(funding_agency, c("USAID", "CDC", "DOD"))) %>% 
    ggplot(aes(plhiv, reorder_within(psnu, plhiv, funding_agency))) +
    geom_errorbar(aes(xmin = plhiv_est_low, xmax = plhiv_est_high), width = .5,
                  color = scooter_med, alpha = .4) +
    geom_point(aes(plhiv_est), color = scooter_med, size = 3) +
    geom_point(aes(color = fill_color), shape = 4, size = 3) +
    facet_grid(funding_agency~., scales = "free_y", space = "free", switch = "y") +
    scale_y_reordered() +
    scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL,
         title = glue("{n_over} REGIONS HAVE FY23 PLHIV ESTIMATES (<span style='color:{burnt_sienna}'>x</span>/<span style='color:#606060'>x</span>) OVER <span style='color:{scooter_med}'>NEW THIS</span> CONFIDENCE INTERVALS"),
         subtitle = "The largest two regions to have over estimated PLHIV are Dar es Salaam and Mwanza",
         caption = glue("Note: THIS PLHIV Estimate = PEPFAR FY23 POP_EST (15+) * 2022-3 THIS Prevalence/CI
                        Source: {str_replace(metadata$source, 'NAT_SUBNAT', 'MSD & NAT_SUBNAT')} + THIS 2022-2023 Summary Sheets | Ref Id: {ref_id}")) +
    si_style_xgrid() +
    theme(panel.spacing = unit(".5", 'picas'),
          plot.title = element_markdown(),
          strip.text = element_text(hjust = .5, face = "bold"),
          strip.placement = "outside")

  si_preview()
  si_save("Images/FY23Q4_TZA-THIS-plhiv-reg.png")  
  
  