# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Populate IM alloction to updated SNUxIM file
# REF ID:   1b4e7256 
# LICENSE:  MIT
# DATE:     2024-03-06
# UPDATED: 


# INSTALL PACKAGES --------------------------------------------------------

  # install.packages(c("tidyverse", "openxlsx", "waldo", "tidylog"))

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(openxlsx)
  library(waldo)
  library(readxl)
  #install tidylog as well, but don't load it (called explicity to check join)


# GLOBAL VARIABLES --------------------------------------------------------

  #PSNUxIM file with desired IM allocation
  path_allocation <- "Data/PSNUxIM_Tanzania_20240306 335PM_ORIGINAL.xlsx"
  
  #new/regenerated PSNUxIM file with correct values but default allocation
  path_new <- "Data/PSNUxIM_Tanzania_030624 to be updated_new DP version.xlsx"
  
  #name of the new file created replacing the default allocation
  path_out <- str_replace(path_new, ".xlsx", "_alloc-applied.xlsx")
    
  
# IMPORT ------------------------------------------------------------------
  
  #read in PSNUxIM file with desired IM allocation
  df_alloc <- read_excel(path_allocation,
                         sheet = "PSNUxIM",
                         skip = 13,
                         col_types = "text")
  
  #new/regenerated PSNUxIM file with correct values but default allocation
  df_new <- read_excel(path_allocation,
                       sheet = "PSNUxIM",
                       skip =  13,
                       col_types = "text")
  
# MUNGE -------------------------------------------------------------------
  
  #keep only IM allocation columns (desired allocation file)
  df_alloc_lim <- df_alloc %>% 
    select(ID, indicator_code,
           matches("(Not PEPFAR\\.{3}\\d{1,2}|.*DSD.*\\.{3}\\d{1,2})$")) %>%
    rename_all(~str_remove(., "...[:digit:]+$")) 
  
  #keep only IM allocation columns (new file)
  df_new_lim <- df_new %>% 
    select(ID, indicator_code,
           matches("(Not PEPFAR\\.{3}\\d{1,2}|.*DSD.*\\.{3}\\d{1,2})$")) %>% 
    rename_all(~str_remove(., "...[:digit:]+$")) 
  
  #ensure no differences in the ordering of the IM columns
  compare(names(df_alloc_lim), names(df_new_lim))

# REPLACEMENT DATA FRAME --------------------------------------------------

  #limit new data frame to just the join key (for row ordering) and then join on desired allocation
  df_replace <- df_new_lim %>% 
    select(ID, indicator_code) %>% 
    tidylog::left_join(df_alloc_lim, by = c("ID", "indicator_code")) %>% 
    mutate(across(-c(ID, indicator_code), as.numeric))
  
  #output dataset (removed binding keys)
  df_out <- df_replace %>% 
    select(-c(ID, indicator_code))
  
# REPLACE DATA IN EXCEL ---------------------------------------------------

  #load the new Excel workbook
  wb <- loadWorkbook(path_new)
  
  #identify the start column to place the data in the PSNUxIM tab
  col_start <- df_new %>% 
    select(matches("Not PEPFAR\\.{3}\\d{1,2}$")) %>% 
    names() %>% 
    match(names(df_new))
  
  #overwrite the PSNUxIM tab with the new allocations
  writeData(wb, "PSNUxIM", 
            df_out,
            startCol = col_start,
            startRow = 15,
            colNames = FALSE)
  
  #test
  # openXL(wb)
  
  saveWorkbook(wb, path_out, overwrite = TRUE)
 
  