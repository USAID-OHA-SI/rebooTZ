# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  resolve HFR issue with FY22 data
# LICENSE:  MIT
# DATE:     2022-03-30
# UPDATED:  2022-05-18

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glue)
  library(googlesheets4)
  library(googledrive)
  library(glamr)
  library(janitor)
  library(readxl)
  library(openxlsx)
  library(usethis)

# GLOBAL VARIABLES --------------------------------------------------------

  #hfr submission table
  gs_submissions <- as_sheets_id("1gQvY1KnjreRO3jl2wzuVCKmKjUUgZDwByVK1c-bzpYI")

  #old mechanism to new mechanism mapping
  df_map <- tibble::tribble(
    ~old,   ~new,
    "18237", "84910",
    "18060", "84911")


# IDENTIDY TZA ORIGINAL SUBMISSIONS ---------------------------------------

  #read in file
  df_submissions <- read_sheet(gs_submissions,
                             .name_repair = make_clean_names)

  #limit to FY22 TZA files and take the last submission for each period
  df_gsubm_files <- df_submissions %>%
    filter(operating_unit_country == "Tanzania",
           email_address == "nmkate@usaid.gov",
           str_detect(hfr_fy_and_period, "FY22")) |>
    group_by(hfr_fy_and_period) |>
    filter(timestamp == max(timestamp)) |>
    ungroup() |>
    mutate(file_id = str_extract(upload_your_hfr_file_s_here, "(?<=id=).*")) |>
    select(timestamp, hfr_pd = hfr_fy_and_period, file_id) |>
    mutate(filename = map(file_id, ~drive_get(as_id(.x))$name)) |>
    unnest(filename) %>% 
    filter(timestamp == max(timestamp))


# DOWNLOAD AND IDENTIFY TABS ----------------------------------------------

  #create a temp folder to store downloaded submissions
  temp_folder()

  #download all the submissions
  df_gsubm_files |>
    select(file_id, filename) |>
    pwalk(~ drive_download(as_id(..1), path = file.path(folderpath_tmp, ..2)))

  #store file paths for looping over to read in
  df_files_tabs <- folderpath_tmp |>
    list.files(full.names = TRUE) |>
    map_dfr(~ tibble(file = .x,
                     tabs = excel_sheets(.x))) |>
    filter(str_detect(tabs, "HFR"))

# FUNCTION - RECTIFY SUBMISSION -------------------------------------------

  rectify_submission <- function(subm_file, subm_tab){

    #print status
    ui_line("reading in {ui_field(subm_tab)} from {ui_path(basename(subm_file))}")

    #read in data from submission
    df <- read_excel(subm_file, subm_tab, skip = 1, col_types = "text")

    #clean up date from import
    df <- df |>
      mutate(date = date %>%
               as.double %>%
               excel_numeric_to_date %>%
               as.character)

    #convert values to numeric
    df <- mutate(df, across(matches("^(hts|tx|vmmc|prep)"), as.numeric))

    #identify the correct date for date issues
    act_date <- tibble(tab = subm_tab) |>
      mutate(month = tab %>%
               str_extract("[:alpha:]{3}(?=_Tan)") %>%
               match(month.abb) %>%
               as.character() %>%
               str_pad(2, pad = "0"),
             year = ifelse(month %in% c("10", "11", "12"), 2021, 2022),
             date = glue("{year}-{month}-01") %>% as.character) |>
      pull()

    #check if there are any wrong dates
    df_wrong_date <- df |>
      filter(date != act_date)

    if(nrow(df_wrong_date) > 0){
      #print status
      ui_info("resolving date issue")
      #update date to correct one
      df_fixed_date <- df_wrong_date |>
        mutate(date = act_date)

      #zero out values for wrong dates (to clear out data base)
      df_zero_out_date <- df_wrong_date |>
        mutate(across(matches("^(hts|tx|vmmc|prep)"), ~ifelse(is.na(.), NA, 0)))

      #table of all the unaffected data
      df_okay_date <- df |>
        filter(date == act_date)

      #join back together
      df <- bind_rows(df_fixed_date, df_okay_date)
    }

    #check if there are any old mechanisms
    if(any(df_map$old %in% unique(df$mech_code))){
      #print status
      ui_info("updating old mech_code")
      #replace old mechanism with new one
      df_new_mech <- df |>
        inner_join(df_map, by = c("mech_code" = "old")) |>
        mutate(mech_code = new) |>
        select(-new)

      #zero out values for old mechanism (to clear out data base)
      df_zero_out_mech <- df |>
        semi_join(df_map, by = c("mech_code" = "old")) |>
        mutate(across(matches("^(hts|tx|vmmc|prep)"), ~ifelse(is.na(.), NA, 0)))

      #table of all the unaffected data
      df_okay_mech <- df |>
        anti_join(df_map, by = c("mech_code" = "old"))

      #join back together
      df <- bind_rows(df_new_mech, df_zero_out_mech, df_okay_mech)
    }


    #load workbook object
    wb <- loadWorkbook(subm_file)

    #unprotect sheet to overwrite data
    protectWorksheet(wb, sheet = subm_tab, protect = FALSE)

    #remove extra worksheets
    removeWorksheet(wb, str_subset(names(wb), glue("meta|{subm_tab}"), negate = TRUE))

    #pull header info
    sht_hdrs <- read.xlsx(wb, subm_tab, colNames = FALSE, rows = 1)
    
    #write data to tab
    # writeData(wb, subm_tab, df, startRow = 3, colNames = FALSE)
    addWorksheet(wb, "HFR")
    writeData(wb, "HFR", sht_hdrs, colNames = FALSE)
    writeData(wb, "HFR", df, startRow = 2)
    removeWorksheet(wb, subm_tab)
    renameWorksheet(wb, "HFR", subm_tab)
    
    #identify new name for saving removing user and adding site type (outputing as single tab)
    site_type <- ifelse(str_detect(subm_tab, "f$"), "fac", "comm")
    n_file <- glue('{str_remove(subm_file, " -.*")}adj_{site_type}.xlsx')

    ui_info("saving as {ui_path(basename(n_file))}")

    #export tab
    saveWorkbook(wb, n_file, overwrite = TRUE)
    
    #output zero date data
    if(nrow(df_wrong_date) > 0){
      pd <- glue::glue("FY{lubridate::quarter(df_zero_out_date$date[1], fiscal_start = 10, with_year = TRUE) %>% str_sub(3, 4)
} { lubridate::month(df_zero_out_date$date[1], TRUE)}")
      writeData(wb, "meta", pd, xy = c("C", 3), colNames = FALSE)
      addWorksheet(wb, "HFR")
      writeData(wb, "HFR", sht_hdrs, colNames = FALSE)
      writeData(wb, "HFR", df_zero_out_date, startRow = 2)
      removeWorksheet(wb, subm_tab)
      renameWorksheet(wb, "HFR", subm_tab)
      n_file <- glue('{str_remove(subm_file, " -.*")}zero_{site_type}.xlsx')
      ui_info("saving as {ui_path(basename(n_file))}")
      saveWorkbook(wb, n_file, overwrite = TRUE)
    }
      
  }


# RUN FUNCTION OVER TABS & EXPORT NEW FILES -------------------------------

  pwalk(df_files_tabs,
       ~rectify_submission(..1, ..2))

  shell.exec(folderpath_tmp)

  
  subm_file <- df_files_tabs$file[1]
  subm_tab <- df_files_tabs$tabs[1]

  
  
  library(Wavelength)
  
  readxl::read_excel(subm_file, subm_tab, skip = 1, col_types = "text") %>% 
    dplyr::mutate(date = date %>%
                    as.double %>%
                    excel_numeric_to_date %>%
                    as.character,
                  dplyr::across(matches("^(hts|tx|vmmc|prep)"), as.numeric)) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE)) %>% 
    dplyr::glimpse()
  
  df %>% 
    group_by(date) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    glimpse()
  
  
  local_files_orig <- list.files(folderpath_tmp, "Nyaso", full.names = TRUE)
  local_files_adj <- list.files(folderpath_tmp, "(fac|comm)", full.names = TRUE)
    

  check_agg_total <- function(filename,type){
    filename %>% 
      excel_sheets() %>% 
      str_subset("HFR") %>% 
      set_names() %>% 
      map_dfr(~read_excel(filename, .x, skip = 1, col_types = "text") %>% 
                mutate(date = ifelse(str_detect(date, "-"), date,
                                     date %>%
                                       as.double %>%
                                       excel_numeric_to_date %>%
                                       as.character),
                       across(matches("^(hts|tx|vmmc|prep)"), as.numeric)),
              .id = "tab") %>% 
      group_by(date) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(-c(date),
                   names_to = "indicator",
                   values_to = {type})
  } 

  df_check_orig <- map_dfr(local_files_orig, ~ check_agg_total(.x, "orig")) %>% 
    filter(!(is.na(date) & orig == 0)) %>% 
    rename(value_orig = orig)
  
  df_check_adj <- map_dfr(local_files_adj, ~ check_agg_total(.x, "adj")) %>% 
    filter(!(is.na(date) & adj == 0)) %>% 
    count(date, indicator, wt = adj, name = "value_adj")
  
  full_join(df_check_orig, df_check_adj,
            by = c("date", "indicator")) %>% 
    arrange(date, indicator) %>% 
    mutate(match = value_orig == value_adj) %>% 
    prinf()
  
  df_check_orig %>% 
    mutate(date_correct = ifelse(date == "2022-01-02", "2022-02-01", date),
           .after = date) %>% 
    rename(date_entered = date) %>% 
    arrange(date_correct, indicator) %>% 
    clipr::write_clip()
  