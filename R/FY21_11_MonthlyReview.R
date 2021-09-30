# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  support for monthly review
# LICENSE:  MIT
# DATE:     2021-09-24
# UPDATED:  2021-09-30

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(Wavelength)
  library(readxl)
  library(lubridate)
  library(janitor)

# GLOBAL VARIABLES --------------------------------------------------------
  
  hfr_path <- "../Wavelength/out/joint/HFR_Tableau_SQLview.csv"
  mnth_path <- "../../../Downloads/D 5 USAID Monthly _all  Priority Levels_ALL_Aug 2021.xlsx"
  
  hfr_source <- "HFR 2021.11 [August]"
  mnth_source <- "USAID Tanzania Monthly Data Reporting [August]"

# IMPORT ------------------------------------------------------------------
  
  df_hfr <- hfr_read(hfr_path) %>% 
    filter(operatingunit == "Tanzania")

  df_mnth <- read_excel(mnth_path, sheet = "Facility", col_types = "text")
  
# MUNGE HFR ---------------------------------------------------------------

  #filter to create MMD variables
  df_mmd <- df_hfr %>% 
    filter(indicator %in% c("TX_CURR", "TX_MMD"),
           !(indicator == "TX_MMD" & is.na(otherdisaggregate)),
           val > 0) %>% 
    mutate(otherdisaggregate = case_when(otherdisaggregate == "<3 months" ~ "u3mo",
                                         # otherdisaggregate == "3-5 months" ~ "35mo",
                                         # otherdisaggregate == "6 months or more"~ "o6mo"
                                         !is.na(otherdisaggregate) ~ "o3mo"),
           indicator = ifelse(indicator == "TX_MMD", glue("{indicator}.{otherdisaggregate}"), indicator)) 
  
  df_mmd <- df_mmd %>% 
    mutate(primepartner = recode(primepartner,
                                 "DELOITTE CONSULTING LIMITED" = "Deloitte",
                                 "ELIZABETH GLASER PEDIATRIC AIDS FOUNDATION" = "EGPAF",
                                 "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" = "THPS",
                                 "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" = "Baylor",
                                 "JSI RESEARCH AND TRAINING INSTITUTE, INC." = "JSI",
                                 "FAMILY HEALTH INTERNATIONAL" = "EpiC")) 

  #add extra df to create national
  df_mmd <- df_mmd %>% 
    bind_rows(df_mmd %>% 
                mutate(snu1 = "NATIONAL"))
  
  #add extra df to create age/sex totals
  df_mmd <- df_mmd %>% 
    bind_rows(df_mmd %>% 
                mutate(agecoarse = "Total",
                       sex = "Total"))
    
  df_mmd <- df_mmd %>% 
    group_by(date, primepartner, indicator, snu1, agecoarse, sex) %>% 
    summarise(value = sum(val, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator,
                values_fill = 0) %>% 
    mutate(TX_MMD.unk = TX_CURR - (TX_MMD.u3mo + TX_MMD.o3mo),
           TX_CURR.adj = ifelse(TX_MMD.unk < 0, TX_MMD.u3mo + TX_MMD.o3mo, TX_CURR),
           TX_MMD.unk = ifelse(TX_MMD.unk < 0, 0, TX_MMD.unk))
  
  df_mmd <- df_mmd %>% 
    pivot_longer(starts_with("TX_MMD"),
                 names_to = "otherdisaggregate",
                 names_prefix = "TX_MMD.") %>% 
    group_by(date, primepartner, snu1, agecoarse, sex) %>% 
    mutate(share = value/TX_CURR.adj) %>% 
    ungroup() %>% 
    mutate(otherdisaggregate = factor(otherdisaggregate, c("o3mo", "u3mo", "unk")),
           lab_share = case_when(otherdisaggregate == "o3mo" ~ share))
    
  

  
  plot_mmd <- function(ptnr){

    
    df_viz <- df_mmd %>% 
      filter(primepartner == ptnr,
             sex == "Total",
             date == max(date)) %>% 
      mutate(fill_alpha = ifelse(snu1 == "NATIONAL", 1, .8),
             fill_color = case_when(otherdisaggregate == "u3mo" ~ grey10k, 
                                    otherdisaggregate == "unk" ~ "white",
                                    snu1 == "NATIONAL" ~ golden_sand, 
                                    TRUE ~ scooter),
             fill_color = factor(fill_color, c(golden_sand, scooter, grey10k, "white")),
             snu1 = ifelse(snu1 == "NATIONAL",
                           glue("{snu1}: TX_CURR = {comma(TX_CURR.adj, 1)}"),
                           glue("{snu1}: {comma(TX_CURR.adj, 1)}")))
    
    
    v1 <- df_viz %>% 
      ggplot(aes(share, fct_reorder(snu1, TX_CURR.adj, max), fill = fct_rev(fill_color))) +
      geom_col(aes(alpha = fill_alpha)) +
      geom_vline(xintercept = seq(0, 1, by = .25), 
                 color = "white", alpha = .6) +
      geom_text(aes(label = percent(lab_share, 1)), na.rm = TRUE,
                family = "Source Sans Pro", hjust = -.25, color = "#505050") +
      labs(x = NULL, y = NULL,
           # title = glue("AS OF {month}, {shr} OF {ptnr} TOTAL REPORTED TREATMENT PATIENTS WERE ON <span style = 'color:{genoa};'>+3 MONTHS OF MMD</span>"),
           # caption = glue("Source: {hfr_source}")
           ) +
      scale_x_continuous(label = percent, expand = c(.005, .005), position = "top") +
      scale_fill_identity() +
      scale_alpha_identity() +
      si_style_nolines() +
      theme(legend.position = "none",
            # axis.text.x = element_blank(),
            plot.title = element_markdown(),
            plot.title.position = "plot",
            plot.background = element_rect(fill = "white", color = NA))
    
    df_viz2 <- df_mmd %>%
      mutate(snu1 = fct_reorder(snu1, TX_CURR.adj, max)) %>% 
      filter(primepartner == ptnr,
             sex != "Total",
             date == max(date),
             otherdisaggregate == "o3mo") %>% 
      mutate(fill_color = case_when(snu1 == "NATIONAL" ~ golden_sand_light, 
                                    TRUE ~ scooter_light))
    
    v2 <- df_viz2 %>% 
      ggplot(aes(agecoarse, snu1)) +
      geom_tile(aes(fill = fill_color),color = "white", size = 1, alpha = .8) +
      geom_text(aes(label = percent(lab_share, 1)), na.rm = TRUE,
                family = "Source Sans Pro", color = "#505050") + 
      facet_grid(~sex) +
      scale_x_discrete(position = "top") +
      scale_fill_identity() +
      labs(x = NULL, y = NULL) +
      si_style_nolines() +
      theme(strip.placement = "outside",
            strip.text.x = element_text(hjust = .5),
            panel.spacing.x = unit(0, "points"),
            plot.background = element_rect(fill = "white", color = NA))

    ptnr <- toupper(ptnr)
    
    month <- format(max(df_viz$date), "%B") %>% toupper()
    
    tx <- df_viz %>% 
      filter(str_detect(snu1, "NATIONAL"),
             otherdisaggregate == "o3mo") %>% 
      pull(TX_CURR.adj) %>% 
      comma(accuracy = 1, scale = 1e-3, suffix = "k")
    
    shr <- df_viz %>% 
      filter(str_detect(snu1, "NATIONAL"),
             otherdisaggregate == "o3mo") %>% 
      pull(share) %>% 
      percent(1)
    
    v1 + v2 + plot_annotation(
      title = glue("AS OF {month}, {shr} OF {ptnr} TOTAL REPORTED TREATMENT PATIENTS WERE ON <span style = 'color:{genoa};'>+3 MONTHS OF MMD</span>"),
      caption = glue("Source: {hfr_source}<br>
                     SI Analytics: Aaron Chafetz<br>
                     US Agency for International Development")
    )  & si_style_nolines() & theme(
      # axis.text.x = element_blank(),
      #legend.position = "none",
      strip.placement = "outside",
      strip.text.x = element_text(hjust = .5),
      panel.spacing.x = unit(.01, "points"),
      plot.title = element_markdown(),
      plot.title.position = "plot",
      plot.background = element_rect(fill = "white", color = NA)
    )
    

    file <- glue("FY21_Monthly-Review-{format(max(df_viz$date), '%B')}_MMD_{ptnr}.png")
    si_save(file, path = "Images")
  }
  
  
  mmd_ptnr <- df_mmd %>% 
    filter(date == max(date)) %>% 
    distinct(primepartner) %>% 
    pull()

  walk(mmd_ptnr, plot_mmd)  
  

# MUNGE MONTHLY DATA ------------------------------------------------------


df_mnth %>% 
    glimpse()
  
df_mnth <- df_mnth %>% 
  mutate(date = my(Month))

df_nn <- df_mnth %>% 
  rename_all(tolower) %>% 
  select(date, region, partner, site, site_id, tx_prev, tx_curr, ltfu = lftu, starts_with("l_")) %>%
  group_by(partner, site_id) %>% 
  mutate(tx_prev_act = lag(tx_curr, order_by = date), .after = tx_prev) %>% 
  ungroup() %>% 
  filter(date == max(date)) %>% 
  mutate(across(c(tx_prev, tx_prev_act, tx_curr, ltfu, starts_with("l_")), as.double)) %>% 
  mutate(tx_net_new = tx_curr - tx_prev_act, 
         pct_delta = tx_net_new / tx_curr,
         .after = tx_curr) %>% 
  mutate(pct_ltfu = ltfu / tx_prev_act, .after = ltfu) %>% 
  mutate(fill_color = case_when(tx_net_new < 0 ~ golden_sand, #burnt_sienna,
                               tx_net_new == 0 ~ trolley_grey,
                               TRUE ~ scooter)) %>% 
  filter(tx_curr > 0)

df_nn %>% 
  distinct(partner)

df_ltfu_share <- df_nn %>% 
  group_by(date, region, partner) %>% 
  summarise(across(c(tx_curr, tx_prev_act, tx_net_new, ltfu), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_longer(c(tx_net_new, ltfu), 
               names_to = "indicator") %>% 
  mutate(share = round(value/tx_prev_act, 2),
         fill_color = case_when(share < 0 ~ golden_sand, #burnt_sienna,
                                share == 0 ~ trolley_grey,
                                TRUE ~ scooter), #denim
         plot_pt = 1.1)

df_nn %>% 
  ggplot(aes(pct_delta, fct_reorder(region,tx_curr, sum), 
             color = fill_color,
             size = tx_curr)) +
  geom_label(data = df_ltfu_share %>% filter(indicator == "tx_net_new"),
             aes(plot_pt, label = percent(share, 1),
                 fill = fill_color, color = "white"), 
             family = "Source Sans Pro", size = 7/.pt,  alpha =  .8) +
  geom_point(alpha = .3, position = position_jitter(width = 0, height = 0.2, seed = 42)) +
  scale_x_continuous(label = percent,
                     limit=c(-1.1, 1.1),
                     oob=scales::squish) +
  coord_cartesian(clip = "off") +
  facet_grid(~fct_reorder(partner, tx_curr, sum, .desc = TRUE), scales = "free_y", space = "free_y") +
  labs(y = NULL, x = "% Growth/Loss in Treatment from Prior Month",
       title = glue("WHILE TANGA AND MARA SAW +10% GROWTH IN TX_CURR, NATIONALLY USAID ONLY GREW BY 1% IN {format(max(df_nn$date), '%B') %>% toupper}") %>% str_wrap(width = 60),
       caption = glue("Growth Rate = (TX_CURR - TX_PREV) / TX_PREV <br>
                      Source: {mnth_source}<br>
                      SI Analytics: Aaron Chafetz<br>
                      US Agency for International Development")) +
  scale_size(label = label_number_si(), name = "Site TX_CURR") +
  scale_color_identity() +
  scale_fill_identity() +
  si_style() +
  theme(plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA))
  

  file <- glue("FY21_Monthly-Review-{format(max(df_nn$date), '%B')}_NetNew.png")
  si_save(file, path = "Images")
  
  
 
  
  df_ltfu_mean <- df_nn %>% 
    group_by(region, partner) %>% 
    summarise(across(c(tx_prev_act, tx_curr, ltfu), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct_ltfu = ltfu/tx_prev_act)
  
  df_ltfu_max <- df_nn %>% 
    group_by(region) %>% 
    summarise(across(c(tx_prev_act, tx_curr, ltfu), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct_ltfu = ltfu/tx_prev_act,
           share_tx = tx_curr/sum(tx_curr)) %>% 
    slice_max(order_by = tx_curr, n= 10) %>% 
    filter(pct_ltfu == max(pct_ltfu))
  
  df_nn %>% 
    ggplot(aes(pct_ltfu, fct_reorder(region,tx_curr, sum), size = tx_curr)) + 
    geom_point(alpha = .3, position = position_jitter(width = 0, height = 0.2, seed = 42), color = scooter) +
    geom_errorbar(data = df_ltfu_mean, aes(xmin = pct_ltfu, xmax = pct_ltfu), color = golden_sand, size = .8) +
    scale_x_continuous(label = percent_format(1),
                       limit=c(0, .51),
                       oob=scales::squish) +
    scale_size(label = label_number_si(), name = "Site TX_CURR") +
    facet_grid(~fct_reorder(partner, tx_curr, sum, .desc = TRUE), scales = "free_y", space = "free_y") +
    labs(y = NULL, x = "% LTFU from Prior Month",
         title = glue("Of the 10 largest regions, {df_ltfu_max$region} has the highest LTFU rate of {percent(df_ltfu_max$pct_ltfu, 1)}") %>% toupper,
         caption = glue("LTFU Rate = LTFU / TX_PREV; capped at 50% <br>
                      Source: {mnth_source}<br>
                                            SI Analytics: Aaron Chafetz<br>
                      US Agency for International Development")) +
    si_style() +
    theme(plot.title.position = "plot",
          plot.background = element_rect(fill = "white", color = NA))
  
  
  file <- glue("FY21_Monthly-Review-{format(max(df_nn$date), '%B')}_LTFU.png")
  si_save(file, path = "Images")
  
  
  
# OVERALL TESTING ---------------------------------------------------------
  
  df_mnth <- df_mnth %>% 
    clean_names() %>% 
    mutate(month = my(month)) 
  
  lst_hts_reg <- df_mnth %>% 
    mutate(across(c(hts_tst_all, hts_pos_all), as.double)) %>% 
    filter(partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  
  curr_mo <- max(df_mnth$date)
  
  df_hts <- df_mnth %>% 
    filter(month >= curr_mo - years(1),
           region %in% lst_hts_reg,
           region != "Tabora")
  
  df_hts <- df_hts %>% 
    bind_rows(df_hts %>% mutate(region = "USAID")) %>% 
    mutate(across(c(hts_tst_all, hts_pos_all), as.double)) %>% 
    group_by(month, region) %>% 
    summarise(across(c(hts_tst_all, hts_pos_all), sum, na.rm = TRUE), .groups = "drop")
  
  
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
                               glue("{region}<span style = 'font-size:8pt'><br>All HIV Tests Aug: {comma(latest, 1)}"),
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
         caption = "Positivity = Positive HIV Tests / All HIV Tests <br>
           Source: Tanzania Monthly Data (thru August 2021) <br>
                          SI analytics: Aaron Chafetz<br>
                       US Agency for International Development") +
    si_style() +
    theme(strip.text.x = element_markdown(),
          panel.spacing.x = unit(1.5, "lines"),
          panel.spacing.y = unit(.5, "lines"),
          plot.title.position = "plot",
          plot.background = element_rect(fill = "white", color = NA))
  
  si_save("Images/TZA_MonthlyReview_HTS-ALL-region.png")

  

# INDEX SHARE OF TOTAL TESTS ----------------------------------------------
  
  
  
  lst_hts_index_reg <- df_mnth %>% 
    mutate(across(c(c_pos_all), as.double)) %>% 
    filter(partner %in% c("Deloitte", "EGPAF")) %>% 
    distinct(region) %>% 
    pull()
  
  df_index_share <- df_mnth %>% 
    filter(month >= "2020-10-01",
           region %in% lst_hts_index_reg,
           region != "Tabora")
  
  df_index_share <- df_index_share %>%
    bind_rows(df_index_share %>% mutate(region = "USAID")) %>% 
    mutate(across(c(c_pos_all, hts_pos_all), as.double)) %>% 
    group_by(month, region) %>% 
    summarise(across(c(c_pos_all, hts_pos_all), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(index_share = c_pos_all/hts_pos_all,
           latest_pos = ifelse(month == curr_mo, hts_pos_all, 0)) %>% 
    group_by(region) %>% 
    mutate(latest_pos = max(latest_pos)) %>% 
    ungroup() %>% 
    mutate(fill_color = ifelse(region == "USAID", golden_sand, scooter),
           region_val = ifelse(latest_pos == max(latest_pos),
                               glue("{region}<span style = 'font-size:8pt'><br>Positive Tests Aug: {comma(latest_pos, 1)}"),
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
         caption = "Positive Index Share = Positive Index Tests / All Positive Tests <br>
           Source: Tanzania Monthly Data (thru August 2021) <br>
                          SI analytics: Aaron Chafetz <br>
                       US Agency for International Development") +
    si_style_ygrid() +
    theme(strip.text.x = element_markdown(),
          panel.spacing.x = unit(1.5, "lines"),
          panel.spacing.y = unit(.5, "lines"),
          plot.title.position = "plot",
          plot.background = element_rect(fill = "white", color = NA))
  
  
  si_save("Images/TZA_MonthlyReview_Index-Share-region.png") 
  