##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  identify the largest sites
##  DATE:     2019-02-15
##  UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse) 
library(ICPIutilities)


# IMPORT ------------------------------------------------------------------

#GENIE PULL 
#  - Indicators: HTS_TST, TX_NEW (All disaggs)
#  - Date: 2019-02-15

#site data
df_genie_site_num <- match_msd("~/GitHub/rebooTZ/data/PEPFAR-Data-Genie-SiteByIMs-2019-02-15 Total Num.zip",
                           save_rds = FALSE)


# FUNCTION ----------------------------------------------------------------


  top_sites <- function(df, ind, wt_pd){
    df %>% 
      dplyr::filter(indicator %in% ind,
             fundingagency == "USAID") %>% 
      dplyr::group_by(orgunituid, sitename, fundingagency,indicator) %>% 
      dplyr::summarise_at(dplyr::vars(!!wt_pd), sum, na.rm = TRUE) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange_at(vars(fundingagency, indicator, !!wt_pd), ~ desc(.)) %>% 
      dplyr::group_by(fundingagency, indicator) %>% 
      dplyr::top_n(10) %>%
      dplyr::ungroup() %>% 
      dplyr::pull(orgunituid)
  }



# MUNGE -------------------------------------------------------------------

  sites_hts <- top_sites(df_genie_site_num, "HTS_TST_POS", "fy2019_targets")
  
  save(sites_hts, file = "data/sites_hts.rda")
  
  rm(top_sites, sites_hts)

  