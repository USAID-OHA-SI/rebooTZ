# PROJECT: rebootTZ
# AUTHOR: K. Srikanth | USAID
# PURPOSE: OVC Ranking
## DATE CREATED:     2021-07-20

# DEPENDENCIES -------------------------------------------------------------------
library(tidyverse)
library(glamr)
library(scales)
library(ICPIutilities)
library(lubridate)
library(RColorBrewer)
library(skimr)
library("patchwork")
library(ggrepel)
library(glitr)

# load fonts
folderpath <- "C:/Users/STAR/Downloads/font folder"
font_import(folderpath)
library(extrafont)

# IMPORT ---------------------------------------------------------------------

df_msd <- si_path() %>% 
  return_latest("PSNU_IM") %>% 
  read_msd()

df_nat <- si_path() %>% 
  return_latest("NAT") %>% 
  read_msd()

#TX_CURR<20 df
df_msd %>% 
  filter(indicator == "TX_CURR") %>% 
  count(standardizeddisaggregate, wt = cumulative)

df_tx <- df_msd %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year == 2021,
         (trendscoarse == "<15" | ageasentered == "15-19")) %>%
  group_by(psnu, psnuuid, snuprioritization, snu1, indicator) %>% 
  summarize(across(cumulative, sum, na.rm = TRUE)) %>% 
  ungroup()

#OVC_HIVSTAT df
df_msd %>% 
  filter(indicator == "OVC_HIVSTAT") %>% 
  count(standardizeddisaggregate, wt = cumulative)

df_ovc <- df_msd %>% 
  filter(indicator == "OVC_HIVSTAT",
         standardizeddisaggregate == "Total Numerator",
         fiscal_year == 2021) %>%
  group_by(psnu, psnuuid, snuprioritization, snu1, indicator) %>% 
  summarize(across(cumulative, sum, na.rm = TRUE)) %>% 
  ungroup()

#PLHIV df
df_plhiv <- df_nat %>% 
  filter(operatingunit == "Tanzania",
         indicator == "PLHIV",
         fiscal_year == 2021,
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         (trendscoarse == "<15" | ageasentered == "15-19")) %>%
  group_by(psnu, psnuuid, snuprioritization, snu1, indicator) %>% 
  summarize(across(targets, sum, na.rm = TRUE)) %>% 
  ungroup()


# JOIN --------------------------------------------------------------------------

df_join <- df_tx %>% #join the tx_curr and ovc dfs
  bind_rows(df_ovc) %>% 
  pivot_wider(names_from = indicator, values_from = cumulative, names_glue = "{tolower(indicator)}") %>% 
  mutate(ovc_psnu = !is.na(ovc_hivstat))


#joi plhiv to df_join with full_join
df_join_plhiv <- df_plhiv %>% 
  pivot_wider(names_from = indicator, values_from = targets,  names_glue = "{tolower(indicator)}") %>% 
  full_join(df_join, by = c("psnu", "psnuuid", "snuprioritization", "snu1")) %>% 
  mutate(fill_color = ifelse(ovc_psnu == TRUE, scooter, golden_sand),
         txcurr_label = case_when(tx_curr > 500 & ovc_psnu == FALSE ~ psnu),
         plhiv_label = case_when(plhiv > 500 & ovc_psnu == FALSE ~ psnu)) 

#checking the missing values from each df
plhiv_psnu <- unique(df_plhiv$psnu)
tx_psnu <- unique(df_tx$psnu)

setdiff(plhiv_psnu, tx_psnu)
setdiff(tx_psnu, plhiv_psnu)

# VIZ -----------------------------------------------------------------------

#TX_CURR 
df_join_plhiv %>% 
  ggplot(aes(x=reorder(psnu, (tx_curr)), y = tx_curr)) +
  coord_flip() +
  geom_col(aes(fill = fill_color), width =0.8, show.legend = F) +
  geom_hline(yintercept = 0, size = .5) +
  #geom_vline(xintercept = 126, linetype="dashed", size =0.5) +
  geom_text_repel(aes(label = txcurr_label), na.rm = TRUE,
            hjust = -0.35,
            family = "Source Sans Pro", size = 10/.pt) +
  scale_y_continuous(labels = comma) +
  si_style() +
  si_style_xgrid() +
  scale_fill_identity() +
  labs(title = "Of the largest councils where TX_CURR <20y/o is greater than 500 patients, there are 4 PSNUs without OVC programs", 
       subtitle = "PSNUs ordered by largest to smallest by TX_CURR <20",
       y = "TX_CURR <20y/o", 
       x = NULL,
       caption = "Source: FY21Q2c MSD
         SI Analytics: Karishma Srikanth
         US Agency for International Development") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#CLHIV
df_join_plhiv %>% 
  ggplot(aes(x=reorder(psnu, (plhiv)), y = plhiv)) +
  coord_flip() +
  geom_col(aes(fill = fill_color), width =0.8, show.legend = F) +
  geom_hline(yintercept = 0, size = .5) +
  #geom_vline(xintercept = 126, linetype="dashed", size =0.5) +
  geom_text_repel(aes(label = plhiv_label), na.rm = TRUE,
                  hjust = 0.1,
                  nudge_y = 120,
                  direction = "y",
                  family = "Source Sans Pro", size = 10/.pt) +
  scale_y_continuous(labels = comma) +
  si_style() +
  si_style_xgrid() +
  scale_fill_identity() +
  labs(title = "Of the largest councils where CLHIV <20y/o is greater than 500 patients, there are 21 PSNUs without OVC programs", 
       subtitle = "PSNUs ordered by largest to smallest by Number of CLHIV",
       y = "Number of CLHIV", 
       x = NULL,
       caption = "Source: FY21Q2c MSD + NAT_SUBNAT
         SI Analytics: Karishma Srikanth/Aaron Chafetz
         US Agency for International Development") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


