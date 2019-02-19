##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  import data
##  DATE:     2019-02-16
##  UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse) 
library(scales)
library(extrafont)
library(ICPIutilities)

#import theme
source("R/plot_theme.R")

# IMPORT ------------------------------------------------------------------

#GENIE PULL 
#  - Indicators: HTS_TST, TX_NEW, TX_CURR, TX_NEW (All disaggs)
#  - Date: 2019-02-16

#site data
df_genie_site <- match_msd("~/GitHub/rebooTZ/data/PEPFAR-Data-Genie-SiteByIMs-2019-02-16.zip",
                           save_rds = FALSE)


#priority HTS site uids list
  load("data/sites_tx.rda")

#priority HTS site uids list
  load("data/sites_tx.rda")