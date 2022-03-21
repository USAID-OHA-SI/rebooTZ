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
