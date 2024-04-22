# PROJECT:  rebooTZ
# PURPOSE:  Pull distinct headers to keep in Prep Flow  
# AUTHOR:   A.Chafetz | USAID
# REF ID:   7336d5a3 
# LICENSE:  MIT
# DATE:     2024-04-22
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------

  folderpath <- "../../../Downloads/requestforsupporttableaujoin"
  
  files <- list.files(folderpath, full.names = TRUE)


# COLLECT HEADERS ---------------------------------------------------------

  #import
  headers <- files %>% 
    set_names(basename) %>% 
    map(~read_excel(.x, skip = 4,
                    n_max = 0) %>% 
          names() %>% 
          as_tibble_col("header")) %>% 
    list_rbind(names_to = "id") 
  
  #remove repeated meta data
  headers <- headers %>% 
    filter(!header %in% c("Region", "Council", "Facility", 
                          "DATIM_ID", "Tier", "Report Period"))
  

# REVIEW DUPLICATES -------------------------------------------------------

  headers %>% 
    group_by(header) %>% 
    filter(n() > 1) %>% 
    distinct(header)
  
  headers %>% 
    group_by(header) %>% 
    filter(n() > 1) %>% 
    ungroup() %>% 
    arrange(header)
  

# KEEP ONLY FIRST INSTANCE USED -------------------------------------------

  #limit to one observation each
  headers <- headers %>% 
    distinct(header, .keep_all = TRUE)

  
# EXPORT ------------------------------------------------------------------

  write_csv(headers, "../../../Downloads/distinct_headers.csv")
  