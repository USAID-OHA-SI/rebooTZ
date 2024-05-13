# PROJECT:  rebooTZ
# PURPOSE:  Combine TX + Site Attributes
# AUTHOR:   A.Chafetz | USAID
# REF ID:   a487bcd2 
# LICENSE:  MIT
# DATE:     2024-05-13
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(janitor, warn.conflicts = FALSE)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  library(grabr)  
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets("datim")

  ref_id <- "a487bcd2"  #a reference to be places in viz captions 
  
  cntry <- "Tanzania"
  
# PDAP WAVE API -----------------------------------------------------------

  # #get TZA UID
  # cntry_uid <- pepfar_country_list %>%
  #   filter(country == cntry) %>%
  #   pull(country_uid)
  # 
  # #establish parameters to pass into POST API
  # post_body <- list(
  #   daily_frozen='daily',
  #   fiscal_year=list(2024),
  #   funding_agency = list("USAID"),
  #   indicator=list("TX_CURR"),
  #   standardizeddisaggregate = list("Total Numerator"),
  #   uid_hierarchy_list=list(stringr::str_glue('-|-|{cntry_uid}')))
  # 
  # #run API
  # wave_process_query(post_body, psd_type = "site_im", request_type = "GET")
  # 
  # genie_path <- return_latest("Data", "Genie")
  # 
  # meta <- get_metadata(genie_path, caption_note = "USAID")  

# DATIM ATTRIBUTES --------------------------------------------------------
  
  # #get sql view table uid
  # sqlview_name <- datim_sqlviews() %>% 
  #   filter(str_detect(name, "Attributes")) %>% 
  #   pull(name)
  # 
  # #get iso code
  # cntry_iso <- pepfar_country_list %>%
  #   filter(country == cntry) %>%
  #   pull(country_iso)
  # 
  # # Extract Organisation Units data for a specific country - All levels with child / parent links
  # df_attr <- datim_sqlviews(view_name = {sqlview_name}, 
  #                            dataset = TRUE,
  #                            query = list(type = "variable", params = list("OU" = cntry_iso)))
  
# IMPORT ------------------------------------------------------------------
  
  # df_genie <- read_psd(genie_path) 
  

# DATIM API ---------------------------------------------------------------

  info <- get_outable() %>% 
    filter(country == cntry) %>% 
    select(country, country_uid, facility_lvl) %>% 
    as.list()
  
  
  url <- paste0("https://final.datim.org/api/analytics.json?",
                "dimension=ou:", info$country_uid, ";LEVEL-", info$facility_lvl, "&", #hierarchy
                "filter=RUkVjD3BsS1&", #top level
                "dimension=bw8KHXzxd9i&", #Funding Agency
                "dimension=CH5v24DUJO0&", #Site Attribute: Ownership Type
                "dimension=SbeNLojYo3t&", #Site Attribute: Facility Type
                "dimension=LxhLO68FcXm:MvszPTQrUhy&", #indicator: TX_CURR
                "dimension=pe:2021Oct;2022Oct;2023Oct&", #periods
                "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=false")
  
  df_datim <- datim_process_query(url)
  
  df_datim_clean <- df_datim %>% 
    rename(orgunit = `Organisation unit`,
           funding_agency = `Funding Agency`,
           ownership_type = `SA Own Type`,
           facility_type = `SA Facility Type`,
           indicator = `Technical Area`) %>% 
    rename_with(tolower) %>% 
    clean_agency() %>% 
    mutate(ownership_type = str_remove(ownership_type, "^OWN "),
           facility_type = str_remove(facility_type, "^FT ")
           ) %>% 
    mutate(fiscal_year = period %>% str_sub(-4) %>% as.numeric(), .before = value) %>% 
    arrange(fiscal_year) %>% 
    select(-period) %>% 
    pivot_wider(names_from = c(indicator, fiscal_year),
                names_glue = "{tolower(indicator)}_fy{str_sub(fiscal_year, -2)}")
  
  df_orgs <- datim_orgunits(info$country, reshape = TRUE) %>% 
    select(orgunituid, country, snu1)
  
  df_datim_clean <- df_datim_clean %>% 
    right_join(df_orgs, .) %>% 
    relocate(orgunit, 1)
  
  write_csv(df_datim_clean, "Dataout/TZA_TX_SA.csv", na  = "")
      

# MUNGE -------------------------------------------------------------------

  #aggregate to one observation by site
  # df_sites <- df_genie %>%
  #   filter(funding_agency == "USAID") %>%
  #   group_by(fiscal_year, snu1, sitetype, sitename, orgunituid, mech_code, indicator) %>%
  #   summarise(cumulative = sum(cumulative, na.rm = TRUE),
  #             .groups = "drop")

  #clean columns
  # df_attr <- df_attr %>% 
  #   clean_names() %>%
  #   select(orgunituid = datim_uid,
  #          facility_type:clinic_hours)%>%
  #   mutate(across(facility_type:clinic_hours,  \(x) na_if(x, "")))
  
  #join with attributes


# VIZ ---------------------------------------------------------------------
  
  df_datim_clean %>% 
    count(funding_agency, ownership_type) %>% 
    mutate(ownership_type = fct_reorder(ownership_type, n, sum)) %>% 
    arrange(desc(ownership_type)) %>% 
    pivot_wider(names_from = funding_agency, values_from = n)
  
  df_datim_clean %>% 
    count(funding_agency, ownership_type, wt = tx_curr_fy24) %>% 
    mutate(ownership_type = fct_reorder(ownership_type, n, sum)) %>% 
    arrange(desc(ownership_type)) %>%
    pivot_wider(names_from = funding_agency, values_from = n)
  
  
  df_datim_clean %>% 
    count(funding_agency, wt = tx_curr_fy24) 
