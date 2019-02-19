##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  identify the largest sites
##  DATE:     2019-02-15
##  UPDATED:  


# FUNCTION ----------------------------------------------------------------


  top_sites <- function(df, ind, type, wt_pd, n_sites = 15){
    
    wt_pd_q <- dplyr::enquo(wt_pd)
    
    if(ind == "HTS_TST_POS") {
      df <- df %>% 
        dplyr::filter(indicator == "HTS_TST",
                      standardizeddisaggregate == "Modality/Age/Sex/Result",
                      resultstatus == "Positive") %>% 
        dplyr::mutate(indicator = "HTS_TST_POS")
    } else {
      df <- df %>% 
        dplyr::filter(indicator %in% ind,
                      standardizeddisaggregate == "Total Numerator")
    }
    
    df %>% 
      dplyr::filter(sitetype == type,
                    fundingagency == "USAID") %>% 
      dplyr::group_by(orgunituid, sitename, fundingagency,indicator) %>%
      dplyr::summarise_at(dplyr::vars(!!wt_pd_q), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::arrange_at(vars(fundingagency, indicator, !!wt_pd_q), ~ desc(.)) %>%
      dplyr::group_by(fundingagency, indicator) %>%
      dplyr::top_n(n_sites) %>%
      dplyr::ungroup() %>%
      dplyr::pull(orgunituid)
  }



# MUNGE -------------------------------------------------------------------

  sites_hts <- top_sites(df_genie_site, "HTS_TST_POS", "Facility","fy2019_targets")
  save(sites_hts, file = "data/sites_hts.rda")
  
  comm_hts <- top_sites(df_genie_site, "HTS_TST_POS", "Community","fy2019_targets")
  save(comm_hts, file = "data/comm_hts.rda")
  
  sites_tx <- top_sites(df_genie_site, "TX_CURR", "Facility", "fy2019_targets")
  save(sites_tx, file = "data/sites_tx.rda")
  
  comm_tx <- top_sites(df_genie_site, "TX_CURR", "Community", "fy2019_targets")
  save(comm_tx, file = "data/comm_tx.rda")
  
  rm(top_sites, sites_hts, comm_hts, sites_tx, comm_tx)

  