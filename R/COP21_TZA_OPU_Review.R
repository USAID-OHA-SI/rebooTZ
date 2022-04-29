# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  summary output for COP21 OPU
# LICENSE:  MIT
# DATE:     2022-04-20
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
  library(tameDP)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  dp_path <- "Data/PEPFAR TZ DataPack 052121 final.xlsx"
  opu_path <- "Data/OPU Data Pack_Tanzania_20220408165545 V4.xlsx"
  
  df_partner <- tibble::tribble(
                  ~mech_code,              ~partner,
                     "160645",                "ICAP",
                     "160646",                 "MDH",
                     "160647",                "THPS",
                      "18488",               "AMREF",
                      "18627",                 "HJF",
                      "18628",                 "HJF",
                      "80095",                 "MDH",
                      "81961",                "Pact",
                      "81962", "USAID Tohara Salama",
                      "81965",                "EpiC",
                      "82169",                "RISE",
                      "85185",           "TBD 85185",
                      "85186",           "TBD 85186",
                      "17358",        "Kizazi Kipya"
                  )


# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_msd()   
  
  df_dp <- tame_dp(dp_path, "PSNUxIM")
  df_opu <- tame_dp(opu_path, "PSNUxIM")

# MUNGE -------------------------------------------------------------------


  df_dp_fltr <- df_dp %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_HIVSTAT", "VMMC_CIRC")) %>%
    get_names(datim_user = datim_user(),
              datim_password = datim_pwd())
  
  df_opu_fltr <- df_opu %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_HIVSTAT", "VMMC_CIRC")) %>%
    get_names(datim_user = datim_user(),
              datim_password = datim_pwd())

  df_join_vars <- df_opu_fltr %>% 
    distinct(operatingunit, fiscal_year, indicator, standardizeddisaggregate)
  
  df_msd_fltr <- df_msd %>% 
    semi_join(df_join_vars) %>% 
    select(names(df_opu_fltr)) %>% 
    rename(targets_msd = targets) 
  
  df_comp <- df_opu_fltr %>% 
    rename(targets_opu = targets) %>% 
    bind_rows(df_dp_fltr) %>% 
    rename(targets_dp = targets) %>% 
    bind_rows(df_msd_fltr)

  df_comp <- df_comp %>% 
    clean_agency() %>% 
    left_join(df_partner) %>% 
    mutate(fundingagency = factor(fundingagency, c("Tanzania", "USAID", "CDC", "DOD")))
  
  df_comp <- df_comp %>% 
    mutate(disagg = ifelse(standardizeddisaggregate == "Age/Sex/ProgramStatus", 
                           paste0("Age/Sex/ProgramStatus", " - ", otherdisaggregate),
                           standardizeddisaggregate))
    
  write_csv(df_comp, "Dataout/COP21_TZA_OPU-review.csv", na = "")
  
# REVIEW ------------------------------------------------------------------


  #agency
  df_comp %>% 
    bind_rows(df_comp %>%
                mutate(fundingagency = "Tanzania")) %>% 
    mutate(fundingagency = factor(fundingagency, c("Tanzania", "USAID", "CDC", "DOD"))) %>% 
    group_by(fundingagency, indicator, standardizeddisaggregate) %>% 
    summarise(across(c(targets_dp, targets_opu), sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(indicator, standardizeddisaggregate, fundingagency) %>% 
    mutate(delta = round((targets_opu / targets_dp)-1, 3)) %>% 
    prinf()
    
  
  #partner
  df_comp %>% 
    bind_rows(df_comp %>%
                mutate(partner = "Tanzania")) %>% 
    group_by(partner, indicator, standardizeddisaggregate) %>% 
    summarise(across(c(targets_dp, targets_opu), sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(indicator, standardizeddisaggregate, partner) %>% 
    mutate(delta = round((targets_opu / targets_dp)-1, 3)) %>%  
    prinf()
  
  
  #ageasentered
  df_comp %>% 
    group_by(ageasentered, indicator, standardizeddisaggregate) %>% 
    summarise(across(c(targets_dp, targets_opu), sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(indicator, standardizeddisaggregate, ageasentered) %>% 
    mutate(delta = round((targets_opu / targets_dp)-1, 3)) %>% 
    prinf()
  
  
  #council
  df_comp %>% 
    group_by(psnu, indicator, standardizeddisaggregate) %>% 
    summarise(across(c(targets_dp, targets_opu), sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(indicator, standardizeddisaggregate, psnu) %>% 
    mutate(delta = round((targets_opu / targets_dp)-1, 3)) %>%
    filter(delta != 0) %>% 
    prinf()
  