# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Monthly data review (Feb) to support Q2 insights 
# LICENSE:  MIT
# DATE:     2021-03-21
# UPDATED:  

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
library(readxl)
library(janitor)
library(lubridate)
library(waffle) #remotes::install_github("hrbrmstr/waffle")

# GLOBAL VARIABLES --------------------------------------------------------

  (md_path <- return_latest("Data", "USAID Monthly"))

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

# PARTNER TX_CURR TRENDS --------------------------------------------------

df_tx_ptnr <- df_md %>% 
  filter(partner %in% c("EGPAF", "Deloitte"),
         indicator == "TX_CURR",
         month >= curr_mo - months(13)) %>% 
  count(month, partner, wt = value, name = "value") %>% 
  mutate(quarter = convert_date_to_qtr(month),
         fiscal_year = str_sub(quarter, end = 4),
         month_abbr = format(month, "%b"),
         month_abbr = fct_reorder(month_abbr, month, min),
         fill_color = case_when(quarter == "FY22Q2" ~ golden_sand,
                                quarter == "FY22Q1" ~ scooter,
                                TRUE ~ scooter_med),
         fill_alpha = case_when(quarter == "FY22Q2" ~ .9,
                                quarter == "FY22Q1" ~ .8,
                                TRUE ~ .7))

df_tx_ptnr %>% 
  ggplot(aes(month_abbr, value, fill = fill_color, alpha = fill_alpha)) +
  geom_col() +
  facet_grid(partner ~ quarter, scales = "free", space = "free") +
  scale_y_continuous(labels = comma) +
  scale_fill_identity() +
  scale_alpha_identity() +
  labs(x = NULL, y = NULL,
       subtitle = "Trend in TX_CURR") +
  si_style_ygrid() +
  theme(panel.spacing.x = unit(.5, "line"))

si_save("FY22Q2_TZA_Feb-Monthly_tx-curr-ptnr.png", 
        path = "Images")

# PARTNER X SNU1 TX_CURR --------------------------------------------------

df_tx_ptnr_snu <- df_md %>% 
  filter(partner %in% c("EGPAF", "Deloitte"),
         indicator == "TX_CURR",
         month >= curr_mo - months(13)) %>% 
  count(month, partner, region, wt = value, name = "value") %>% 
  mutate(quarter = convert_date_to_qtr(month),
         fiscal_year = str_sub(quarter, end = 4),
         month_abbr = format(month, "%b"),
         month_abbr = fct_reorder(month_abbr, month, min),
         fill_color = case_when(quarter == "FY22Q2" ~ golden_sand,
                                quarter == "FY22Q1" ~ scooter,
                                TRUE ~ scooter_med),
         fill_alpha = case_when(quarter == "FY22Q2" ~ .9,
                                quarter == "FY22Q1" ~ .8,
                                TRUE ~ .7))


plot_txcurr <- function(ptnr){
  df_tx_ptnr_snu %>%
    filter(partner == ptnr) %>% 
    ggplot(aes(month, value, fill = fill_color, alpha = fill_alpha)) +
    geom_col() +
    facet_wrap(. ~ fct_reorder2(region, quarter, value)) +
    scale_y_continuous(labels = comma) +
    scale_fill_identity() +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL,
         subtitle = glue("{ptnr} TX_CURR by Region")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"))
  
  si_save(glue("FY22Q2_TZA_Feb-Monthly_tx-curr-{tolower(ptnr)}-psnu.png"), 
          path = "Images")  
}

walk(c("EGPAF", "Deloitte"), plot_txcurr)




# LTFU --------------------------------------------------------------------


df_ltfu <- df_md %>% 
  filter(partner %in% c("EGPAF", "Deloitte"),
         indicator %in% c("LFTU", "TX_CURR"),
         month >= curr_mo - months(13)) %>% 
  count(month, partner, indicator, wt = value, name = "value") %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>%
  rename(ltfu = lftu) %>% 
  mutate(share_ltfu = ltfu/tx_curr,
         quarter = convert_date_to_qtr(month),
         fiscal_year = str_sub(quarter, end = 4),
         month_abbr = format(month, "%b"),
         month_abbr = fct_reorder(month_abbr, month, min),
         fill_color = case_when(quarter == "FY22Q2" ~ golden_sand,
                                quarter == "FY22Q1" ~ scooter,
                                TRUE ~ scooter_med),
         fill_alpha = case_when(quarter == "FY22Q2" ~ .9,
                                quarter == "FY22Q1" ~ .8,
                                TRUE ~ .7))

df_ltfu %>% 
  ggplot(aes(month, share_ltfu, group = partner)) +
  geom_area(alpha = .3, fill = scooter, color = scooter) +
  geom_area(data = filter(df_ltfu, quarter == "FY22Q2"),
            fill = golden_sand, color = scooter) +
  geom_point(color = scooter, fill = "white", shape = 21, stroke = 1) +
  facet_grid(partner ~ ., scales = "free", space = "free") +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       subtitle = "Share of TX_CURR Lost to Follow up") +
  si_style_ygrid() +
  theme(panel.spacing.x = unit(.5, "line"))


si_save("FY22Q2_TZA_Feb-Monthly_ltfu-ptnr.png", 
        path = "Images")


# PARTNER X SNU1 LTFU --------------------------------------------------

df_ltfu_snu <- df_md %>% 
  filter(partner %in% c("EGPAF", "Deloitte"),
         indicator %in% c("LFTU", "TX_CURR"),
         month >= curr_mo - months(13)) %>% 
  count(month, partner, region, indicator, wt = value, name = "value") %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>%
  rename(ltfu = lftu) %>% 
  mutate(share_ltfu = ltfu/tx_curr,
         quarter = convert_date_to_qtr(month),
         fiscal_year = str_sub(quarter, end = 4),
         month_abbr = format(month, "%b"),
         month_abbr = fct_reorder(month_abbr, month, min),
         fill_color = case_when(quarter == "FY22Q2" ~ golden_sand,
                                quarter == "FY22Q1" ~ scooter,
                                TRUE ~ scooter_med),
         fill_alpha = case_when(quarter == "FY22Q2" ~ .9,
                                quarter == "FY22Q1" ~ .8,
                                TRUE ~ .7))


plot_ltfu <- function(ptnr){
  df_ltfu_snu %>% 
    filter(partner == ptnr) %>% 
    ggplot(aes(month, share_ltfu, group = region)) +
    geom_area(alpha = .3, fill = scooter, color = scooter) +
    geom_area(data = df_ltfu_snu %>% 
                filter(partner == ptnr, 
                       quarter == "FY22Q2"),
              fill = golden_sand, color = scooter) +
    geom_point(color = scooter, fill = "white", shape = 21, stroke = 1) +
    facet_wrap(~fct_reorder2(region, month, tx_curr)) +
    scale_y_continuous(labels = percent_format(1)) +
    labs(x = NULL, y = NULL,
         subtitle = glue("{ptnr} share of TX_CURR Lost to Follow up")) +
    si_style_ygrid() +
    theme(panel.spacing.x = unit(.5, "line"))
  
  si_save(glue("FY22Q2_TZA_Feb-Monthly_ltfu-{tolower(ptnr)}-snu.png"), 
          path = "Images")  
}

walk(c("EGPAF", "Deloitte"), plot_ltfu)



# MMD ---------------------------------------------------------------------


df_mmd <- df_md %>% 
  filter(partner %in% c("EGPAF", "Deloitte"),
         indicator %in% c("MMS_3", "MMS_6", "TX_CURR"),
         month >= curr_mo - months(13)) %>% 
  count(month, partner, indicator, wt = value, name = "value") %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>%
  mutate(share_mms_3 = mms_3/tx_curr,
         share_mms_6 = mms_6/tx_curr,
         quarter = convert_date_to_qtr(month),
         fiscal_year = str_sub(quarter, end = 4),
         month_abbr = format(month, "%b"),
         month_abbr = fct_reorder(month_abbr, month, min),
         fill_color = case_when(quarter == "FY22Q2" ~ golden_sand,
                                quarter == "FY22Q1" ~ scooter,
                                TRUE ~ scooter_med),
         fill_alpha = case_when(quarter == "FY22Q2" ~ .9,
                                quarter == "FY22Q1" ~ .8,
                                TRUE ~ .7))

df_mmd %>% 
  ggplot(aes(month)) +
  geom_area(aes(y = share_mms_3), color = scooter_med, fill = scooter_med, alpha = .3) +
  geom_area(aes(y = share_mms_6), color = scooter, fill = scooter, alpha = .8) +
  geom_area(data = filter(df_mmd, quarter == "FY22Q2"),
            aes(y = share_mms_3), color = scooter, fill = golden_sand_light, alpha = .6) +
  geom_area(data = filter(df_mmd, quarter == "FY22Q2"),
            aes(y = share_mms_6), color = scooter, fill = golden_sand) +
  geom_point(aes(y = share_mms_3), color = scooter, fill = "white", shape = 21, stroke = 1) +
  geom_point(aes(y = share_mms_6), color = scooter, fill = "white", shape = 21, stroke = 1) +
  facet_grid(partner~.) +
  expand_limits(y = 1) +
  scale_y_continuous(label = percent) +
  labs(x = NULL, y = NULL,
       subtitle = glue("Share of TX_CURR on <span style='color:{scooter_med}'>+3 MMS</span> and <span style='color:{scooter}'>+6 MMS</span>")) +
  si_style() +
  theme(plot.subtitle = element_markdown(),
        panel.spacing.x = unit(.5, "line"))

si_save("FY22Q2_TZA_Feb-Monthly_mmd-ptnr.png", 
        path = "Images")



# MMD ---------------------------------------------------------------------


df_mmd_snu <- df_md %>% 
  filter(partner %in% c("EGPAF", "Deloitte"),
         indicator %in% c("MMS_3", "MMS_6", "TX_CURR"),
         month >= curr_mo - months(13)) %>% 
  count(month, partner, region, indicator, wt = value, name = "value") %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>%
  mutate(share_mms_3 = mms_3/tx_curr,
         share_mms_6 = mms_6/tx_curr,
         quarter = convert_date_to_qtr(month),
         fiscal_year = str_sub(quarter, end = 4),
         month_abbr = format(month, "%b"),
         month_abbr = fct_reorder(month_abbr, month, min),
         fill_color = case_when(quarter == "FY22Q2" ~ golden_sand,
                                quarter == "FY22Q1" ~ scooter,
                                TRUE ~ scooter_med),
         fill_alpha = case_when(quarter == "FY22Q2" ~ .9,
                                quarter == "FY22Q1" ~ .8,
                                TRUE ~ .7))


plot_mmd <- function(ptnr){
  df_mmd_snu %>% 
    filter(partner == ptnr) %>% 
    ggplot(aes(month, group = region)) +
    geom_area(aes(y = share_mms_3), color = scooter_med, fill = scooter_med, alpha = .3) +
    geom_area(aes(y = share_mms_6), color = scooter, fill = scooter, alpha = .8) +
    geom_area(data = filter(df_mmd_snu, quarter == "FY22Q2", partner == ptnr),
              aes(y = share_mms_3), color = scooter, fill = golden_sand_light, alpha = .6) +
    geom_area(data = filter(df_mmd_snu, quarter == "FY22Q2", partner == ptnr),
              aes(y = share_mms_6), color = scooter, fill = golden_sand) +
    geom_point(aes(y = share_mms_3), color = scooter, fill = "white", shape = 21, stroke = 1) +
    geom_point(aes(y = share_mms_6), color = scooter, fill = "white", shape = 21, stroke = 1) +
    facet_wrap(~ fct_reorder2(region, month, tx_curr)) +
    expand_limits(y = 1) +
    scale_y_continuous(label = percent) +
    labs(x = NULL, y = NULL,
         subtitle = glue("{ptnr} share of TX_CURR on <span style='color:{scooter_med}'>+3 MMS</span> and <span style='color:{scooter}'>+6 MMS</span>")) +
    si_style() +
    theme(plot.subtitle = element_markdown(),
          panel.spacing.x = unit(.5, "line"))
  
  si_save(glue("FY22Q2_TZA_Feb-Monthly_mmd-snu-{tolower(ptnr)}.png"), 
          path = "Images")
}

walk(c("EGPAF", "Deloitte"), plot_mmd)
