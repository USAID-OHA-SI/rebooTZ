# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Monthly data review - May
# LICENSE:  MIT
# DATE:     2021-06-21
# UPDATED:  2021-06-28

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
  library(readxl)
  library(janitor)
  library(lubridate)
  library(waffle) #devtools::install_github("hrbrmstr/waffle")

# GLOBAL VARIABLES --------------------------------------------------------
  
  md_path <- "Data/D 5 USAID Monthly _all  Priority Levels_ALL_May 2021.xlsx"  

# IMPORT ------------------------------------------------------------------

  df_md <- read_excel(md_path, sheet = "Facility", col_types = "text")
  

# MUNGE -------------------------------------------------------------------

  #reshape long
  df_md <- df_md %>% 
    pivot_longer(-(Month_Num:SITE_ID),
                 names_to = "indicator",
                 values_to = 'value',
                 values_drop_na = TRUE)

  #rename
  df_md <- df_md %>% 
    clean_names() %>% 
    rename(priority_tier = priority_tier_1_2_3_4)


  #convert values
  df_md <- df_md %>% 
    select(-month_num) %>% 
    mutate(month = my(month),
           priority_tier = as.integer(priority_tier),
           value = as.numeric(value)) %>% 
    filter(value > 0)


# CURRENT MONTH -----------------------------------------------------------

  curr_mo <- max(df_md$month)
  
  

# OVERALL TESTING ---------------------------------------------------------

  lst_hts_reg <- df_md %>% 
    filter(indicator %in% c("HTS_POS_ALL"),
           partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_hts <- df_md %>% 
    filter(indicator %in% c("HTS_TST_ALL", "HTS_POS_ALL"),
           month >= curr_mo - years(1),
           region %in% lst_hts_reg,
           region != "Tabora")
  
  df_hts <- df_hts %>% 
    bind_rows(df_hts %>% mutate(region = "USAID")) %>% 
    group_by(month, region, indicator) %>% 
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") 
    
  df_hts <- df_hts %>% 
    mutate(latest = ifelse(month == curr_mo, hts_pos_all, 0)) %>% 
    group_by(region) %>% 
    mutate(latest = max(latest, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(positivity = hts_pos_all/hts_tst_all,
           fill_color = ifelse(region == "USAID", golden_sand, scooter),
           start_cap = case_when(month == min(month) ~ positivity),
           end_cap = case_when(month == max(month) ~ positivity),
           region_val = ifelse(latest == max(latest),
                               glue("{region}<span style = 'font-size:8pt'><br>All HIV Tests May: {comma(latest, 1)}"),
                               glue("{region}<span style = 'font-size:8pt'><br>{comma(latest, 1)}")))
  
  
  df_hts %>% 
    ggplot(aes(month, positivity, group = region_val, fill = fill_color, color = fill_color)) +
    geom_path(size = .9) +
    geom_point(aes(y = end_cap), size = 3, na.rm = TRUE) +
    geom_point(aes(y = start_cap), shape = 21, fill = "white", stroke = 1,
               size = 2,  na.rm = TRUE) +
    facet_wrap(~fct_reorder(region_val, latest, max, na.rm = TRUE, .desc = TRUE)) +
    scale_y_continuous(label = percent_format(1)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_x_date(date_labels = "%b %y") +
    labs(x = NULL, y = NULL,
         title = "POSITIVITY (ALL AGES)",
         caption = "Positivity = Positive HIV Tests / All HIV Tests
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style() +
    theme(strip.text.x = element_markdown(),
          panel.spacing.x = unit(1.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Images/TZA_MonthlyReview_HTS-ALL-region.png")

  
  
# OPTIMIZED PITC ----------------------------------------------------------

  sum_opd <- function(df, fct_grp = agency){
    curr_mo <- max(df$month)
    
    df_opd <- df %>% 
      filter(str_detect(indicator, "^SCREEN|OPD"),
             month >= "2020-01-01") %>% 
      mutate(facet_group = {{fct_grp}}) %>% 
      group_by(facet_group, month, indicator) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")
    
    df_opd_wd <- df_opd %>% 
      pivot_wider(names_from = indicator,
                  names_glue = "{tolower(indicator)}") %>% 
      mutate(latest_elig = ifelse(month == curr_mo, screen_elig_a, 0),
             elig_positivity = screen_hts_pos_a/screen_elig_a,
             positivity = screen_hts_pos_a/screen_hts_a,
             flag_mo = month == curr_mo | month == curr_mo - months(6) | month == curr_mo - months(12),
             lab_elig = case_when(flag_mo == TRUE ~ number(screen_elig_a, accuracy = 1, scale = 1e-3, suffix = "k")),
             lab_elig_pos = case_when(flag_mo == TRUE ~ percent(elig_positivity, .1)))
    return(df_opd_wd)
  }
  
  viz_opd <- function(df){
    
    v1 <- df %>% 
      ggplot(aes(month, elig_positivity)) +
      geom_blank(aes(y = elig_positivity*1.1)) +
      geom_blank(aes(y = elig_positivity*.75)) +
      geom_path(size = .9, color = golden_sand) +
      # geom_point(shape = 21, size = 3, fill = "white", color = golden_sand, stroke = 2) +
      geom_text(aes(label = lab_elig_pos), na.rm = TRUE, vjust = -1,
                family = "Source Sans Pro", color = "#909090") +
      facet_grid(~fct_reorder(facet_group, latest_elig, max, .desc = TRUE)) +
      scale_y_continuous(label = percent) +
      scale_x_date(date_breaks = "6 months",
                   date_labels = "%b %y") +
      labs(x = NULL, y = "Positivity") +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    v2 <- df %>% 
      ggplot(aes(month, screen_elig_a)) +
      geom_col(fill = scooter) +
      geom_text(aes(label = lab_elig), na.rm = TRUE, vjust = -.4,
                family = "Source Sans Pro", color = "#909090") +
      facet_grid(~fct_reorder(facet_group, latest_elig, max, .desc = TRUE)) +
      scale_y_continuous(label = comma) +
      scale_x_date(date_breaks = "6 months",
                   date_labels = "%b %y") +
      labs(x = NULL, y = "Screened-Eligible Adults") +
      si_style_ygrid() +
      theme(strip.text.x = element_blank())
    
    v1 / v2 + plot_layout(heights = c(1, 4)) + plot_annotation(
      title = str_wrap("OPTIMIZED PITC"),
      caption = "Positivity = Positive Tests/Screened Eligible Adults
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") & theme(
                         plot.title = element_text(family = "Source Sans Pro", size = 14, face = "bold",
                                                   color =  "#202020", hjust = 0),
                         plot.caption = element_text(family = "Source Sans Pro",  size = 9,
                                                     color = "#909090", hjust = 1, vjust = 1))
  }

  df_md %>% 
    sum_opd() %>% 
    viz_opd()
  si_save("Images/TZA_MonthlyReview_OPD-USAID.png")
  
  df_md %>% 
    filter(partner != "John Snow, Inc.") %>% 
    sum_opd(partner) %>% 
    viz_opd()
  si_save("Images/TZA_MonthlyReview_OPD-partner.png")                                  


# OPTIMIZED PITC - SITE ---------------------------------------------------

  df_md %>% 
    filter(partner %in% c("Deloitte", "EGPAF")) %>%
    sum_opd(region) %>%
    ggplot(aes(month, positivity)) +
    geom_area(alpha = .4, fill = genoa) +
    facet_wrap(~fct_reorder(facet_group, screen_hts_a, max, .desc = TRUE)) +
    scale_y_continuous(label = percent_format(1)) +
    labs(x = NULL, y = NULL) +
    si_style_ygrid()
  
  df_opd_site <- df_md %>% 
    filter(month >= curr_mo - months(5)) %>% 
    mutate(site = glue("{region}/{site}/{site_id}")) %>% 
    sum_opd(site)

  df_opd_site %>% 
    filter(month == curr_mo,
           screen_elig_a > 1) %>% 
    mutate(fill_clr = case_when(positivity < .03 ~ old_rose,
                                positivity < .05 ~ burnt_sienna,
                                TRUE ~ scooter)) %>% 
    ggplot(aes(screen_elig_a, positivity, color = fill_clr)) +
    geom_point(aes(size = screen_hts_pos_a)) +
    scale_x_log10() +
    scale_color_identity()
  
  lst_low_pos <- df_opd_site %>% 
    filter(month == curr_mo,
           positivity < .03) %>% 
    slice_max(order_by =screen_elig_a, n = 20) %>% 
    pull(facet_group)
  
  df_opd_site <- df_opd_site %>% 
    filter(facet_group %in% lst_low_pos) %>%
    group_by(facet_group) %>% 
    mutate(latest_elig = max(latest_elig)) %>% 
    ungroup() %>% 
    separate(facet_group, c("region", "site", "site_id"), sep = "/") %>% 
    mutate(site_nm = ifelse(latest_elig == max(latest_elig),
                            glue("{region}/{site}<span style = 'font-size:6pt'><br>Screened-eligible Adults May - {comma(latest_elig, 1)}</span>"),
                            glue("{region}/{site}<span style = 'font-size:6pt'><br>{comma(latest_elig, 1)}</span>")),
           fill_clr = case_when(is.na(positivity) ~ NA_character_,
                                positivity < .03 ~ old_rose,
                                positivity < .05 ~ burnt_sienna,
                                # TRUE ~ scooter),
                                TRUE ~ NA_character_),
           pd = format.Date(month, "%b %y"),
           pd = fct_inorder(pd)) 
  
  df_opd_site %>% 
    ggplot(aes(pd, fct_reorder(site_nm, latest_elig, max), 
               fill = fill_clr)) +
    geom_tile(color = "white", size = 1, alpha = .4) +
    geom_text(aes(label = percent(positivity, .1)), na.rm = TRUE,
              family = "Source Sans Pro SemiBold") +
    scale_fill_identity() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         title = "LARGEST OPD SITES WITH LOW POSITIVITY (<3%) IN MAY 2021",
         caption = "Positivity = Positive Tests/Tested Eligible Adults
         Source: Tanzania Monthly Data (thru May 2021)
         SI analytics: Aaron Chafetz
         US Agency for International Development") + 
    si_style_nolines() +
    theme(axis.text.y = element_markdown(size = 9))
  
  si_save("Images/TZA_MonthlyReview_OPD-site.png")  
  
# INDEX -------------------------------------------------------------------


  df_index <- df_md %>% 
    filter(indicator %in% c("Contacts Tested_All", "C_POS_All"),
           month >= "2020-10-01",
           partner != "THPS") 
  
  df_index <- df_index %>%
    bind_rows(df_index %>% mutate(partner = "USAID")) %>% 
    mutate(facet_group = partner) %>% 
    group_by(facet_group, month, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")

  df_index <- df_index %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator) %>% str_replace(' ', '_')}") %>% 
    mutate(positivity = c_pos_all/contacts_tested_all) %>% 
    pivot_longer(c(c_pos_all, contacts_tested_all), names_to = "indicator") 
  
  
  df_index <- df_index %>% 
    mutate(ind_fill = case_when(indicator == "c_pos_all" & facet_group == "USAID" ~ golden_sand,
                                indicator == "c_pos_all" ~ scooter,
                                TRUE ~ trolley_grey_light),
           ind_alpha = ifelse(indicator == "c_pos_all", 1, .6),
           ind_names = ifelse(indicator == "c_pos_all", "Positives", "Contacts Tested"),
           latest = ifelse(month == curr_mo, value, 0))
  
  
  df_index %>% 
    ggplot(aes(month, value, fill = ind_names, alpha = ind_alpha)) +
    geom_col(data = filter(df_index, indicator == "contacts_tested_all")) +
    geom_col(data = filter(df_index, indicator == "c_pos_all")) +
    geom_text(data = filter(df_index, indicator == "c_pos_all"),
              aes(label = percent(positivity, 1)), 
              hjust = -.4,
              family = "Source Sans Pro", color = "#909090") +
    coord_flip() +
    facet_grid(~fct_reorder(facet_group, latest, max, .desc = TRUE), scales = "free_x") +
    scale_y_continuous(label = comma_format(1)) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b %y") +
    scale_fill_manual(values = c("Positives" = scooter,
                                 "Contacts Tested" = trolley_grey_light)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "INDEX TESTING (ALL AGES)",
         caption = "Positivity = Positive Tests/All Contacts Tested
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style_xgrid()
  
  si_save("Images/TZA_MonthlyReview_Index-partner.png")  
  si_save("Graphics/TZA_MonthlyReview_Index-partner.svg")  
  

# INDEX SHARE OF TOTAL TESTS ----------------------------------------------

  lst_hts_index_reg <- df_md %>% 
    filter(indicator == "C_POS_All",
           partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_index_share <- df_md %>% 
    filter(indicator %in% c("C_POS_All", "HTS_POS_ALL"),
           month >= "2020-10-01",
           region %in% lst_hts_index_reg)
  
  df_index_share <- df_index_share %>%
    bind_rows(df_index_share %>% mutate(region = "USAID")) %>% 
    group_by(month, region, indicator) %>% 
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(index_share = c_pos_all/hts_pos_all,
           latest_pos = ifelse(month == curr_mo, hts_pos_all, 0)) %>% 
    group_by(region) %>% 
    mutate(latest_pos = max(latest_pos)) %>% 
    ungroup() %>% 
    mutate(fill_color = ifelse(region == "USAID", golden_sand, scooter),
           region_val = ifelse(latest_pos == max(latest_pos),
                               glue("{region}<span style = 'font-size:8pt'><br>Positive Tests May: {comma(latest_pos, 1)}"),
                               glue("{region}<span style = 'font-size:8pt'><br>{comma(latest_pos, 1)}")))
  

  df_index_share %>% 
    ggplot(aes(month, index_share, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .6) +
    facet_wrap(~fct_reorder(region_val, hts_pos_all, max, na.rm = TRUE, .desc = TRUE)) +
    expand_limits(y = 1) +
    scale_y_continuous(labels = percent) +
    scale_x_date(breaks = c(curr_mo - years(1), "2020-10-01",curr_mo), 
                 date_labels = "%b %y") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL,
         title = "SHARE OF POSITIVE INDEX TEST OF ALL POSITIVE TESTS (ALL AGES)",
         caption = "Positive Index Share = Positive Index Tests / All Positive Tests
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style_ygrid() +
    theme(strip.text.x = element_markdown(),
          panel.spacing.x = unit(1.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  
  
  si_save("Images/TZA_MonthlyReview_Index-Share-region.png") 
  
  
# LINKAGE -----------------------------------------------------------------

  lst_link_reg <- df_md %>% 
    filter(indicator %in% c("HTS_POS_ALL", "TX_NEW_ALL"),
           partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_link <- df_md %>% 
    filter(indicator %in% c("HTS_POS_ALL", "TX_NEW_ALL"),
           region %in% lst_link_reg,
           region != "Tabora") 
  
  df_link <- df_link %>%
    bind_rows(df_link %>% mutate(region = "USAID")) %>% 
    group_by(month, region, indicator) %>% 
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(proxy_link = TX_NEW_ALL/HTS_POS_ALL,
           latest_pos = ifelse(month == curr_mo, HTS_POS_ALL, 0),
           end_caps = case_when(month %in% c(min(month), max(month))~proxy_link)) %>% 
    group_by(region) %>% 
    mutate(latest_pos = max(latest_pos)) %>% 
    ungroup() %>% 
    mutate(fill_color = ifelse(region == "USAID", golden_sand, scooter),
           region_val = ifelse(latest_pos == max(latest_pos),
                               glue("{region}<span style = 'font-size:8pt'><br>Positive Tests May: {comma(latest_pos, 1)}"),
                               glue("{region}<span style = 'font-size:8pt'><br>{comma(latest_pos, 1)}")))

  
  df_link %>% 
    ggplot(aes(month, proxy_link, group = region_val, fill = fill_color, color = fill_color)) +
    geom_hline(yintercept = 1, linetype = "dashed", size = .8) +
    geom_path(size = .9) +
    geom_point(data = df_link %>% filter(month == max(month)),
                                        aes(y = end_caps), size = 3, na.rm = TRUE) +
    geom_point(data = df_link %>% filter(month == min(month)),
               aes(y = end_caps), shape = 21, fill = "white", stroke = 1,
               size = 2,  na.rm = TRUE) +
    facet_wrap(~fct_reorder(region_val, latest_pos, max, na.rm = TRUE, .desc = TRUE)) +
    scale_y_continuous(label = percent_format(1)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_x_date(date_labels = "%b %y") +
    labs(x = NULL, y = NULL,
         title = "PROXY LINKAGE (ALL AGES)",
         caption = "Proxy Linkage = New on Treatment / Positive Tests
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style() +
    theme(strip.text.x = element_markdown(),
          panel.spacing.x = unit(1.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))

  si_save("Images/TZA_MonthlyReview_Linkage-region.png") 
  

# SAME DAY INITIATION -----------------------------------------------------

  
  df_md %>% 
    filter(str_detect(indicator, "SDI|TX_NEW_ALL")) %>% 
    count(month, indicator, wt = value) %>% 
    spread(indicator, n, fill = 0) %>% 
    mutate(match = SDI_0_7 + SDI_8_14 + SDI_15,
           delta = match/TX_NEW_ALL) %>% 
    prinf()
    summari
    
  lst_sdi_reg <- df_md %>% 
    filter(str_detect(indicator, "SDI|TX_NEW_ALL"),
           partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_sdi <- df_md %>% 
    filter(str_detect(indicator, "SDI|TX_NEW_ALL"),
           month >= "2020-01-01",
           region %in% lst_link_reg,
           region != "Tabora") 
  
  df_sdi <- df_sdi %>%
    bind_rows(df_sdi %>% mutate(region = "USAID")) %>% 
    group_by(month, region, indicator) %>% 
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(sdi_share = sdi_0_7/tx_new_all,
           latest = ifelse(month == curr_mo, tx_new_all, 0),
           end_caps = case_when(month %in% c(min(month), max(month))~sdi_share)) %>% 
    group_by(region) %>% 
    mutate(latest = max(latest)) %>% 
    ungroup() %>% 
    mutate(fill_color = ifelse(region == "USAID", golden_sand, scooter),
           region_val = ifelse(latest == max(latest),
                               glue("{region}<span style = 'font-size:8pt'><br>New on Treatment May: {comma(latest, 1)}"),
                               glue("{region}<span style = 'font-size:8pt'><br>{comma(latest, 1)}")))
  
  df_sdi %>% 
    ggplot(aes(month, sdi_share,  color = fill_color)) +
    geom_path(size = .9) +
    geom_point(data = df_sdi %>% filter(month == max(month)),
               aes(y = end_caps), size = 3, na.rm = TRUE) +
    geom_point(data = df_sdi %>% filter(month == min(month)),
               aes(y = end_caps), shape = 21, fill = "white", stroke = 1,
               size = 2,  na.rm = TRUE) +
    facet_wrap(~fct_reorder(region_val, latest, max, .desc = TRUE)) +
    scale_y_continuous(label = percent_format(1)) +
    scale_x_date(date_labels = "%b %y") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL,
         title = "SAME DAY INITIATION (ALL AGES)",
         subtitle = "Share of newly on treatment initiated under 7 days since diagnosis",
         caption = "SDI = Patients Initiated on Treatment <7 days / New Patients Initiated on Treatment
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style() +
    theme(strip.text.x = element_markdown(),
          panel.spacing.x = unit(1.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  

  si_save("Images/TZA_MonthlyReview_SDI-region.png") 

# NEW ON TREATMENT --------------------------------------------------------

  lst_new_reg <- df_md %>% 
    filter(indicator == "TX_NEW_ALL",
           month >= "2020-10-01",
           partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_new <- df_md %>% 
    filter(indicator == "TX_NEW_ALL",
           month >= curr_mo - years(1),
           region %in% lst_new_reg) 
  
  df_new <- df_new %>% 
    bind_rows(df_new %>% mutate(region = "USAID")) %>%
    group_by(month, region, indicator) %>% 
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop")

  df_new <- df_new %>% 
    mutate(lastest_value = ifelse(month == max(month), value, 0),
           fill_color = ifelse(region == "USAID", golden_sand, scooter))
  
  df_new %>% 
    ggplot(aes(month, value, fill = fill_color)) +
    geom_blank(aes(y = 1500)) +
    geom_col() +
    facet_wrap(~fct_reorder(region, lastest_value, max, .desc = TRUE),scales = "free_y") +
    scale_y_continuous(labels = comma) +
    scale_x_date(breaks = c(curr_mo - years(1), "2020-10-01",curr_mo), 
                 date_labels = "%b %y") +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         title = "HIV+ PATIENTS NEWLY INITIATING ON TREATMENT",
         caption = "TX_NEW_ALL
         Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style_ygrid() +
    theme(panel.spacing.x = unit(.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))

  si_save("Images/TZA_MonthlyReview_TXNEW-region.png")

  

# NET NEW -----------------------------------------------------------------


  lst_nn_reg <- df_md %>% 
    filter(indicator %in% c("TX_CURR", "TX_PREV", "TX_NEW_ALL"),
           month >= "2020-10-01",
           partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_nn <- df_md %>% 
    filter(indicator %in% c("TX_CURR", "TX_PREV", "TX_NEW_ALL"),
           month >= curr_mo - years(1),
           region %in% lst_nn_reg) 
  
  df_nn_reg <- df_nn %>%
    bind_rows(df_nn %>% mutate(region = "USAID")) %>%
    group_by(month, region, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")
  
  df_nn_reg <- df_nn_reg %>%
    mutate(latest_txcurr = ifelse(month == max(month), tx_curr, 0),
           fill_color = ifelse(region == "USAID", golden_sand, scooter))
  
  df_nn <- df_nn %>%
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")
  
  df_nn <- df_nn %>% 
    mutate(net_new = tx_curr - tx_prev,
           fill_color = ifelse(net_new < 0.8*tx_new_all, golden_sand, scooter),
           large = ifelse(tx_curr > 200, "Large sites (+200 patients)", "Smaller"))
  
  
  df_nn %>% 
    filter(month == max(month)) %>% 
    ggplot(aes(tx_new_all, net_new, size = tx_curr, color = fill_color)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0) +
    geom_point(alpha = .2) +
    facet_wrap(~large, scales = "free") +
    scale_color_identity() +
    scale_x_continuous(expand = c(.005, .005)) +
    scale_size_continuous( label = comma) +
    labs(x = "TX_NEW", y = "TX_NEW_NEW", size = "TX_CURR",
         title = "ARE SITES LOSING MORE PATIENTS THAN THEIR GAINING?",
         subtitle = "Highlighting sites where TX_NET_NEW is less than 90% TX_NEW in May 2021",
         caption = "TX_NET_NEW = TX_CURR - TX_PREV
         Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style()
    
  si_save("Images/TZA_MonthlyReview_NEW-NETNEW-region.png")
  
  
  df_nn_reg %>% 
    group_by(region) %>% 
    mutate(latest_txcurr = max(latest_txcurr)) %>% 
    ungroup() %>% 
    mutate(net_new = tx_curr - tx_prev,
           fill_color = ifelse(region == "USAID", golden_sand, scooter),
           region_val = ifelse(latest_txcurr == max(latest_txcurr),
                               glue("{region}<span style = 'font-size:8pt'><br>Current on Treatment May: {comma(latest_txcurr, 1)}"),
                               glue("{region}<span style = 'font-size:8pt'><br>{comma(latest_txcurr, 1)}"))) %>% 
    ggplot(aes(month, net_new, fill = fill_color)) +
    geom_blank(aes(y = 4000)) +
    geom_blank(aes(y = -2000)) +
    geom_col() +
    facet_wrap(~fct_reorder(region_val, tx_curr, max, .desc = TRUE), scales = "free_y") +
    scale_y_continuous(labels = comma) + 
    scale_x_date(breaks = c(curr_mo - years(1), "2020-10-01",curr_mo), 
                 date_labels = "%b %y") +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         title = "USAID SEEING NEGATIVE NET NEW IN 6 OF THE LAST 7 QUARTERS",
         caption = "TX_NET_NEW = TX_CURR - TX_PREV
         Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style_ygrid() + 
    theme(strip.text = element_markdown(),
          panel.spacing.x = unit(.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Images/TZA_MonthlyReview_NETNEW-region.png")

# NET NEW SITE ------------------------------------------------------------

  lst_nn_sites <- df_nn %>%
    group_by(site_id) %>% 
    mutate(flag = net_new < 0 & lag(net_new, order_by = month) < 0) %>% 
    ungroup() %>% 
    filter(month == curr_mo,
           flag == TRUE) %>% 
    slice_max(order_by = tx_curr, n = 20) %>% 
    pull(site_id)

  df_nn_site <- df_nn %>% 
    mutate(latest = ifelse(month == curr_mo, tx_curr, 0)) %>% 
    group_by(site_id) %>% 
    mutate(latest = max(latest)) %>% 
    ungroup()
  
    df_nn_viz <- df_nn_site %>% 
      filter(site_id %in% lst_nn_sites,
             month >= curr_mo - months(5)) %>% 
      mutate(site_nm = ifelse(latest == max(latest),
                              glue("{region}/{site}<span style = 'font-size:6pt'><br>Current on Treatment  May - {comma(latest, 1)}</span>"),
                              glue("{region}/{site}<span style = 'font-size:6pt'><br>{comma(latest, 1)}</span>")),
             fill_clr = case_when(is.na(net_new) ~ NA_character_,
                                  net_new < 0 ~ old_rose,
                                  TRUE ~ NA_character_),
             pd = format.Date(month, "%b %y"),
             pd = fct_inorder(pd)) 
    
    df_nn_viz %>% 
      ggplot(aes(pd, fct_reorder(site_nm, latest, max), 
                 fill = fill_clr)) +
      geom_tile(color = "white", size = 1, alpha = .4) +
      geom_text(aes(label = comma(net_new, 1)), na.rm = TRUE,
                family = "Source Sans Pro SemiBold") +
      scale_fill_identity() +
      scale_x_discrete(position = "top") +
      labs(x = NULL, y = NULL,
           title = "LARGEST SITES WITH THE LAST 2 OR MORE MONTHS OF NEGATIVE NET NEW | MAY 2021",
           caption = "TX_NET_NEW = TX_CURR - TX_PREV
         Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") + 
      si_style_nolines() +
      theme(axis.text.y = element_markdown(size = 9))
  
    si_save("Images/TZA_MonthlyReview_NETNEW-site.png")
    
# MMS ---------------------------------------------------------------------

  lst_mms_reg <- df_md %>% 
    filter(indicator == "TX_CURR" | str_detect(indicator, "MMS"),
           partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_mms <- df_md %>% 
    filter(str_detect(indicator, "^TX_CURR$|MMS"),
           region %in% lst_mms_reg)
  
  df_mms <- df_mms %>% 
    bind_rows(df_mms %>% mutate(region = "USAID")) %>%
    group_by(month, region, indicator) %>% 
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") 
  
  df_wfl <- df_mms %>% 
    select(-mms_1, -mms_2) %>% 
    pivot_longer(starts_with("mms"),
                 names_to = "type",
                 values_to = "patients") %>% 
    mutate(share = round((patients/tx_curr) * 100),
           gap = 100-share) %>% 
    pivot_longer(c(share, gap), 
                 names_to = "status") %>% 
    mutate(fill_color = case_when(status == "gap" ~ "#EBEBEB", 
                                  region == "USAID" ~ golden_sand,
                                  TRUE ~ scooter),
           fill_alpha = ifelse(status == "gap", .1, 1),
           mmd_share = case_when(status == "share" ~ percent(patients/tx_curr, 1)),
           reg_val = ifelse(region == "USAID",
                            glue("{region}<span style = 'font-size:8pt'><br>{comma(tx_curr, 1)}<br>current on treatment May"),
                            glue("{region}<span style = 'font-size:8pt'><br>{comma(tx_curr, 1)}")))
  
  df_wfl %>% 
    filter(month == curr_mo,
           type == "mms_3") %>% 
    ggplot(aes(fill = fill_color, values = value, alpha = fill_alpha)) +
    geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE) +
    geom_text(aes(x = 5, y  = 12, label = mmd_share, color = trolley_grey),
              family = "Source Sans Pro SemiBold", size = 14/.pt, na.rm = TRUE) +
    facet_wrap(~fct_reorder(reg_val, tx_curr, max, .desc = TRUE),
               nrow = 2, strip.position = "bottom") +
    expand_limits(y = 14) +
    scale_x_discrete() +
    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    coord_equal() +
    labs(x = NULL, y = NULL,
         title = "PATIENTS ON 3 OR MORE MONTHS OF MULTI-MONTH SCRIPTING",
         caption = "MMS Share = MMS 3mo/ TX_CURR
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          strip.text.x = element_markdown(hjust = .5),
          panel.spacing = unit(1, "pt"))
  
  si_save("Images/TZA_MonthlyReview_MMS3mo-region.png")
  
  

df_mms6 <- df_wfl %>% 
  filter(type == "mms_6",
         status == "share",
         region == "USAID",
         month >= "2020-03-01") %>%
  mutate(share = patients/tx_curr) 

curr_mms6 <- df_mms6 %>% 
  filter(month == curr_mo)


sub <- glue("In {month(curr_mo, label = TRUE)}, {comma(curr_mms6$patients,1)} of USAID's {comma(curr_mms6$tx_curr,1)} treatment patients were on 6 or more months of scripting")

df_mms6 %>% 
  ggplot(aes(month, share)) +
  geom_area(color = golden_sand, fill = golden_sand_light, 
            alpha = .4, size = 1) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = "PATIENTS ON 6 OR MORE MONTHS OF MULTI-MONTH SCRIPTING",
       subtitle =sub,
       caption = "MMS Share = MMS 6mo/ TX_CURR
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development" ) +
  si_style_ygrid()
  
si_save("Images/TZA_MonthlyReview_MMS6mo-usaid.png")


# VLC ---------------------------------------------------------------------

lst_vl_reg <- df_md %>% 
  filter(indicator %in% c("VL_ELIG", "VL_D", "VL_N"),
         month >= "2020-10-01",
         partner %in% c("Deloitte", "EGPAF")) %>% 
  distinct(region) %>% 
  pull()

df_vl <- df_md %>% 
  filter(indicator %in% c("VL_ELIG", "VL_D", "VL_N"),
         month >= "2020-10-01",
         region %in% lst_vl_reg)

df_vl <- df_vl %>% 
  bind_rows(df_vl %>% mutate(region = "USAID")) %>% 
  group_by(month, region, indicator) %>% 
  summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}")

df_vl <- df_vl %>% 
  mutate(vlc = vl_d/vl_elig,
         vls = vl_n/vl_elig,
         notcov = 1-vlc,
         full = 1,
         reg_val = glue("{region} ({comma(vl_elig, 1)})"))

df_vl %>% 
  filter(month == curr_mo) %>% 
  ggplot(aes(full, fct_reorder(reg_val, vlc, .desc = TRUE))) +
  geom_col(fill = "#e5e5e5") +
  geom_col(aes(vlc), fill = "#67a9cf") +
  geom_col(aes(vls), fill = "#2166ac") +
  geom_text(aes(x = .97, label = percent(notcov, 1)),
            family = "Source Sans Pro") +
  geom_vline(xintercept = c(.25, .5, .75), color = "white", linetype = "dashed") +
  # expand_limits(x = 1) +
  scale_x_continuous(labels = percent, position = "top",
                     expand = c(.005, .005)) +
  labs(x = NULL, y = NULL,
       title = "GAP TO FULL VIRAL LOAD COVERAGE AND SUPPRESSION",
       caption = "VLC = Clients with documented viral load result/Clients eligible for viral load testing
       VLS = Clients with suppressed viral load result/Clients eligible for viral load testing
           Source: Tanzania Monthly Data (thru May 2021)
                          SI analytics: Aaron Chafetz
                       US Agency for International Development") +
  si_style_nolines()

si_save("Graphics/TZA_MonthlyReview_VLCS-region.svg")


# VLC - SITE --------------------------------------------------------------

df_vl_site <- df_md %>% 
  filter(indicator %in% c("VL_ELIG", "VL_D", "VL_N"),
         month >= curr_mo - months(5),
         partner %in% c("Deloitte", "EGPAF")
         ) %>% 
  mutate(site = glue("{region}/{site}/{site_id}")) %>% 
  group_by(month, site, indicator) %>% 
  summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>%
  mutate(vlc = vl_d/vl_elig,
         vls = vl_n/vl_d)

lst_low_vlc <- df_vl_site %>% 
  filter(month == curr_mo,
         vlc < .5) %>% 
  slice_max(order_by =vl_elig, n = 20) %>% 
  pull(site)

lst_low_vls <- df_vl_site %>% 
  filter(month == curr_mo,
         vls < .5) %>% 
  slice_max(order_by =vl_elig, n = 20) %>% 
  pull(site)

lst_med_vls <- df_vl_site %>% 
  filter(month == curr_mo,
         vls > .5,
         vls < .75) %>% 
  slice_max(order_by =vl_elig, n = 20-length(lst_low_vls)) %>% 
  pull(site)

lst_low_vls <- c(lst_low_vls, lst_med_vls)

df_vl_site <- df_vl_site %>% 
  mutate(latest_elig = ifelse(month == curr_mo, vl_elig, 0)) %>% 
  group_by(site) %>% 
  mutate(latest_elig = max(latest_elig)) %>% 
  ungroup() 

df_vl_site <- df_vl_site %>% 
  select(-vl_d, -vl_n) %>% 
  pivot_longer(c(vlc, vls), names_to = "type") %>% 
  mutate(lrg_site_low_vl = case_when(type == "vlc" & site %in% lst_low_vlc ~ TRUE,
                                     type == "vls" & site %in% lst_low_vls ~ TRUE,
                                     TRUE ~ FALSE)) %>% 
  separate(site, c("region", "site", "site_id"), sep = "/")

df_vlc_viz <- df_vl_site %>% 
  filter(type == "vlc",
         lrg_site_low_vl == TRUE) %>% 
  mutate(site_nm = ifelse(latest_elig == max(latest_elig),
                          glue("{region}/{site}<span style = 'font-size:6pt'><br>Clients eligible for viral load testing  May - {comma(latest_elig, 1)}</span>"),
                          glue("{region}/{site}<span style = 'font-size:6pt'><br>{comma(latest_elig, 1)}</span>")),
         fill_clr = case_when(is.na(value) ~ NA_character_,
                              value < .5 ~ old_rose,
                              value < .9 ~ burnt_sienna,
                              # TRUE ~ scooter),
                              TRUE ~ NA_character_),
         pd = format.Date(month, "%b %y"),
         pd = fct_inorder(pd)) 

df_vlc_viz %>% 
  ggplot(aes(pd, fct_reorder(site_nm, latest_elig, max), 
             fill = fill_clr)) +
  geom_tile(color = "white", size = 1, alpha = .4) +
  geom_text(aes(label = percent(value, .1)), na.rm = TRUE,
            family = "Source Sans Pro SemiBold") +
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL,
       title = "LARGEST SITES WITH LOW VIRAL LOAD COVERAGE (<50%) IN MAY 2021",
       caption = "VLC = Clients with documented viral load result/Clients eligible for viral load testing
         Source: Tanzania Monthly Data (thru May 2021)
         SI analytics: Aaron Chafetz
         US Agency for International Development") + 
  si_style_nolines() +
  theme(axis.text.y = element_markdown(size = 9))


si_save("Images/TZA_MonthlyReview_VLC-site.png")


df_vls_viz <- df_vl_site %>% 
  filter(type == "vls",
         lrg_site_low_vl == TRUE) %>% 
  mutate(site_nm = ifelse(latest_elig == max(latest_elig),
                          glue("{region}/{site}<span style = 'font-size:6pt'><br>Clients eligible for viral load testing May - {comma(latest_elig, 1)}</span>"),
                          glue("{region}/{site}<span style = 'font-size:6pt'><br>{comma(latest_elig, 1)}</span>")),
         fill_clr = case_when(is.na(value) ~ NA_character_,
                              value < .5 ~ old_rose,
                              value < .9 ~ burnt_sienna,
                              # TRUE ~ scooter),
                              TRUE ~ NA_character_),
         pd = format.Date(month, "%b %y"),
         pd = fct_inorder(pd)) 

df_vls_viz %>% 
  ggplot(aes(pd, fct_reorder(site_nm, latest_elig, max), 
             fill = fill_clr)) +
  geom_tile(color = "white", size = 1, alpha = .4) +
  geom_text(aes(label = percent(value, .1)), na.rm = TRUE,
            family = "Source Sans Pro SemiBold") +
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL,
       title = "LARGEST SITES WITH LOW VIRAL LOAD SUPRESSION (<75%) IN MAY 2021",
       caption = "VLS = Clients with suppressed viral load result/Clients with documented viral load result
         Source: Tanzania Monthly Data (thru May 2021)
         SI analytics: Aaron Chafetz
         US Agency for International Development") + 
  si_style_nolines() +
  theme(axis.text.y = element_markdown(size = 9))


si_save("Images/TZA_MonthlyReview_VLS-site.png")
