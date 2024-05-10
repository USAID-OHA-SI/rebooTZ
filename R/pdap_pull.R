#' Pull Genie Data from PDAP Wave
#'
#' @param post_body elements to pass into the PDAP Wave POST API
#' @param folderpath_dwnld where to download, default = "Data"
#' @param psd_type Type of PEPFAR Structured dataset: "psnu_im" (default), 
#'   "ou_im", or "site_im"
#' @param username DATIM username, if blank looks for stored credentials 
#'   (glamr::set_datim) and then prompts for credentials if not found 
#' @param password DATIM password, if blank looks for stored credentials 
#'   (glamr::set_datim) and then prompts for credentials if not found
#'
#' @return list of request and stored data in zip
#' @export
#'
#' @examples
#' \dontrun{
#'  library(tidyverse)
#'  library(glamr)
#'  
#'  #get country uid for API
#' cntry_uid <- pepfar_country_list %>% 
#'  filter(country == "Tanzania") %>% 
#'  pull(country_uid)
#'  
#'  #establish parameters to pass into POST API
#'  post_body <- list(
#'    daily_frozen='daily',
#'    fiscal_year=list(2023, 2024),
#'    funding_agency = list("USAID"),
#'    indicator=list("TX_CURR","TX_ML","TX_CURR_LAG2", "TX_NET_NEW","TX_NEW",
#'                   "TX_RTT","PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART"),
#'    uid_hierarchy_list=list(str_glue('-|-|{cntry_uid}')))
#'    
#'  #run API
#'  pdap_pull(cntry_uid, post_body)
#'
#'  #load data
#'  df_genie <- return_latest("Data") %>% 
#'     read_psd(zip_path)
#'    }
  
pdap_pull <- function(post_body,
                      folderpath_dwnld = "Data",
                      psd_type = c("psnu_im", "ou_im", "site_im"),
                      username,
                      password){
  
  #urls
  datim_hostname = 'genie.testing.datim.org'
  api_host = 'wave.test.pdap.pepfar.net'
  
  #store/prompt for credentials
  accnt <- grabr::lazy_secrets("datim", username, password)
  
  #establish DATIM session
  datim_results <- httr::GET(
    stringr::str_glue('https://{datim_hostname}/api/me'),
    httr::authenticate(accnt$username, accnt$password, type = "basic")
  )
  
  #establish DATIM API session
  api_login_result <- httr::GET(
    stringr::str_glue('https://{api_host}/api/Authenticate/datim'),
    httr::set_cookies(JSESSIONID = datim_results$cookies$value),
    query=list(datim_hostname=datim_hostname)
  )
  
  #country 
  cntry_uid <- stringr::str_sub(post_body$uid_hierarchy_list[[1]], start = -11)
  
  #country
  cntry <- glamr::pepfar_country_list %>% 
    dplyr::filter(country_uid == cntry_uid) %>% 
    dplyr::pull(country) %>% 
    stringr::str_remove_all("( |')")
  
  #ensure only one psd_type
  psd_type <- psd_type[1]
  
  #string type
  psd_type_string <- psd_type %>% 
    stringr::str_extract("^.*(?=_)") %>% 
    toupper()
  
  #output path 
  zip_path <- file.path(folderpath_dwnld, 
                        stringr::str_glue('Genie-{psd_type_string}ByIMs-{cntry}-Daily-{Sys.Date()}.zip'))
  
  #DATIM POST request
  httr::POST(
    stringr::str_glue('https://{api_host}/api/data/{psd_type}'),
    httr::set_cookies(wave_session = api_login_result$cookies$value),
    encode='json',
    body={post_body},
    httr::write_disk(zip_path, overwrite=TRUE)
  )
  
  #notification
  cli::cli_alert_info("The PDAP Wave successfully executed and the output is saved as {.file {zip_path}} ")
  
  invisible()
  
  
}
