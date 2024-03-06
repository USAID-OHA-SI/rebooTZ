


# INSTALL PACKAGES --------------------------------------------------------

# if there is an error reinstall package in a clean instance (CTRL+ SHIFT + F10)
# install.packages(c('gagglr', 'tameDP', 'gophr'), repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))

# DEPENDENCIES ------------------------------------------------------------

  library(tameDP)
  library(gophr)
  library(dplyr)
  library(tidyr)
  library(readxl)


# GLOBAL VARIABLES --------------------------------------------------------

  #folder location where data are stored
  folder_path <- "Data"
  
  #data pack file path
  dp_filepath <- file.path("Data/Target Setting Tool_Tanzania__030424_clean.xlsx")
  
    file.exists(dp_filepath) #confirm file exist
    
  #msd file path
  genie_filepath <- file.path("../../Data/MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Tanzania.zip")

    file.exists(genie_filepath) #confirm file exist
    
  #SUBNAT file path
  subnat_filepath <- file.path("../../Data/MER_Structured_Datasets_NAT_SUBNAT_FY22-24_20240215_v1_1.zip")
    
    file.exists(subnat_filepath) #confirm file exist


# IMPORT ------------------------------------------------------------------

  #process the data pack to extract the targets
  df_dp <- tame_dp(dp_filepath)
    
  #process the data pack PLHIV
  df_dp_subnat <- tame_dp(dp_filepath, type = "SUBNAT")
    
  #import Cascade tab for extra fields (TX_CURR Derived and Expected)
  df_dp_extra <- read_excel(dp_filepath, "Cascade", skip = 12)
  
  #read in MSD
  df_msd <- read_psd(genie_filepath) %>% filter(country == "Tanzania")
  
  #read in SUBNAT
  df_subnat <- read_psd(subnat_filepath) %>% filter(country == "Tanzania")


# MUNGE EXTRA FIELDS TO MATCH TST/MSD -------------------------------------

  df_dp_extra <- df_dp_extra %>% 
    select(PSNU, ageasentered = Age, Sex, TX_CURR.C, TX_CURR.Expected.T_1) %>% 
    pivot_longer(c(TX_CURR.C, TX_CURR.Expected.T_1),
                 names_to = "indicator", values_to = "targets") %>% 
    rename_all(tolower) %>% 
    split_psnu() %>% 
    mutate(fiscal_year = 2024,
           operatingunit = "Tanzania",
           country = "Tanzania",
           numeratordenom = "N",
           standardizeddisaggregate = "Age/Sex/HIVStatus",
           target_age_2024 = ageasentered,
           indicator = case_match(indicator,
                                  "TX_CURR.C" ~"TX_CURR (Derived)",
                                  "TX_CURR.Expected.T_1" ~ "TX_CURR (Expected)",
                                  .default = indicator),
           source_name = unique(df_dp$source_name)) %>% 
    tameDP:::align_agecoarse() 
  
  
# JOIN TST DATA -----------------------------------------------------------

  #combine targets and SUBNAT (PLHIV) data
  df_dp <- bind_rows(df_dp, df_dp_subnat)  
    
  #Removing fiscal year 2023-24 from the Target setting tool (avoid duplicates)
  df_dp <- df_dp %>% 
    filter(fiscal_year==2025)  
  
  #combine with extra TX data (FY24)
  df_dp <- bind_rows(df_dp, df_dp_extra)  
  
  #remove fields not in MSD from TST
  df_dp <- df_dp %>% 
    select(-source_processed)
  
# JOIN MSD + SUBNAT -------------------------------------------------------

  #bind MSD + SUBNAT 
  df_msd <- bind_rows(df_msd, df_subnat) 
  
  #subset columns from MSD for binding with Genie
  df_msd_lim <- df_msd %>% 
    select(all_of(names(df_dp))) 

  
# BIND MSD + TST ----------------------------------------------------------

  #bind Data Pack data onto MSD
  df_msd_dp <- df_msd_lim %>% 
    bind_rows(df_dp) 


# EXPORT ------------------------------------------------------------------

  #today's date for file name
  todays_date <- format(Sys.Date(), "%Y_%b%d")
  
  #export file 
  export_path <- file.path(folder_path, paste0("Genie_Target", todays_date, ".csv"))
  
  #export Genie + TST dataset
  readr::write_csv(df_msd_dp, file = export_path, na = "")
