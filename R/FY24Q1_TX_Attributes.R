#urls
datim_hostname = 'genie.testing.datim.org'
api_host = 'wave.test.pdap.pepfar.net'

#store/prompt for credentials
accnt <- grabr::lazy_secrets("datim")

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
psd_type <- "site_ims"

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

return_latest("Data", "Genie") %>% 
  read_psd() %>% 
  glimpse()
