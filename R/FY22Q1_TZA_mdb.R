# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  prelim Q1 Mission Director Briefer
# LICENSE:  MIT
# DATE:     2022-02-08
# UPDATED:  2022-02-09


# DATIM GENIE PULL --------------------------------------------------------

# PSNU By IM
# DATIM data as of: 02/08/2022 00:10:20 UTC
# Genie report updated: 02/08/2022 03:45:00 UTC
# Current period(s): 2021 Target,  2021 Q1,  2021 Q2,  2021 Q3,  2021 Q4,  2022 Target,  2022 Q1,  2023 Target
# Operating Unit: Tanzania,
# Fiscal Year: 2022,2021,

# DEPENDENCIES ------------------------------------------------------------
  
  library(glitr)
  library(glamr)
  library(tidyverse)
  library(gophr)
  library(gt)
  library(selfdestructin5)
  library(fontawesome)


# GLOBAL VARIABLES --------------------------------------------------------
  
  path_genie <- "Data/Genie_PSNU_IM_Tanzania_Daily_d8a6b9b0-9f02-456e-ad48-78f420d4d4b5.zip"

  mdb_out <- "Images" #Alter this path to where you'd like saved tables to go
  load_secrets()
  
  
# IMPORT ------------------------------------------------------------------
  
  df_genie <- read_msd(path_genie)   
  
  pd <- create_pd(df_genie)
  msd_source <- source_info(path_genie)
  
  
  # Main Table
  # Create the long mdb_df of the main summary indicators 
  # This will remove mechs with known issues by default. If you want to keep all mechs set `resolve_issues == FALSE`
  mdb_df   <- make_mdb_df(df_genie)
  
  # Create the reshaped df that is gt() ready
  mdb_tbl  <- reshape_mdb_df(mdb_df, pd)

  mdb_tbl %>% 
    filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Tanzania", type = "main", pd, msd_source) %>% 
    gtsave(path = mdb_out, filename = glue::glue("Tanzania_{pd}_mdb_main.png"))
  
  
  
  mdb_tbl_tza <- df_genie %>%
    mutate(fundingagency = "USAID") %>% 
    make_mdb_df() %>% 
    reshape_mdb_df(pd) %>% 
    filter(indicator != "GEND_GBV",
           operatingunit == "Tanzania") %>% 
    mutate(agency = "PEPFAR")
  
  mdb_tbl_tza %>% 
    gt(groupname_col = "agency") %>%
    mdb_main_theme(pd, msd_source) %>% 
    tab_header(
      title = "TANZANIA PERFORMANCE SUMMARY") %>% 
    gtsave(path = mdb_out, filename = glue::glue("Tanzania_PEPFAR_{pd}_mdb_main.png"))
  