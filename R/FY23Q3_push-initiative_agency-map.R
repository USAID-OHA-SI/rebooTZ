# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  "push" countries review - agency presence 
# REF ID:   96c239cf 
# LICENSE:  MIT
# DATE:     2023-10-04
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(sf)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "96c239cf" #id for adorning to plots, making it easier to find on GH
  
  get_metadata(type = "NAT_SUBNAT") #list of MSD metadata elements
  metadata_subnat <- metadata
  
  get_metadata(type = "PSNU_IM") #list of MSD metadata elements

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_psd() %>% 
    filter(country == "Tanzania")
  
  df_subnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_psd() %>% 
    filter(country == "Tanzania")
  
  shp_tza_snu1 <-  si_path("path_vector") %>% 
    return_latest("tanzania_snu1.shp", recursive = TRUE) %>% 
    st_read() %>% 
    select(snu1uid = uid, geometry)
    
 
  terr <- gisr::get_raster(path = si_path("path_raster"))
  
  admin1 <- gisr::get_admin1(countries = "United Republic of Tanzania") %>% 
    dplyr::select(name)
  
  admin0 <- gisr::get_admin0(countries = "United Republic of Tanzania") %>% 
    dplyr::select(name)
  
  basemap <- terrain_map(countries = admin0,
                         adm0 = admin0,
                         # adm1 = admin1,
                         mask = TRUE,
                         terr = terr)

  
  
  
# MUNGE -------------------------------------------------------------------
  
  df_plhiv <- df_subnat %>% 
    filter(indicator == "PLHIV",
           fiscal_year == max(fiscal_year)) %>% 
    pluck_totals() %>% 
    count(fiscal_year, snu1, snu1uid, wt = targets, name = "plhiv")
  
  sf_plhiv <- full_join(df_plhiv, shp_tza_snu1)
  
  df_agencies <- df_msd %>% 
    filter(indicator == "TX_CURR",
           psnu != "_Military Tanzania",
           fiscal_year == metadata$curr_fy) %>% 
    pluck_totals() %>% 
    clean_agency() %>% 
    count(snu1, snu1uid, funding_agency, wt = targets) %>% 
    group_by(snu1, snu1uid) %>% 
    mutate(share = n / sum(n)) %>% 
    filter(share == max(share)) %>% 
    ungroup() %>% 
    select(snu1, snu1uid, funding_agency)
  
  
  sf_agencies <- full_join(df_agencies, shp_tza_snu1)
  
  
  df_msd %>% 
    clean_indicator() %>% 
    filter(indicator %in% c("TX_CURR"),
           snu1 %in% filter(df_agencies, funding_agency == "USAID")$snu1,
           funding_agency == "USAID") %>% 
    pluck_totals() %>% 
    # distinct(snu1, prime_partner_name) %>% 
    distinct(snu1, prime_partner_name, mech_name) %>% 
    mutate(prime_partner_name = case_match(prime_partner_name,
                                       "DELOITTE CONSULTING LIMITED" ~ "[Deloitte]",
                                       "Elizabeth Glaser Pediatric Aids Foundation" ~ "[EGPAF]",
                                       "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)"~ "[THPS]",
                                       "Family Health International" ~ "[FHI]"),
           mech_name = case_match(mech_name,
                                  "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
                                  "USAID Tulonge Afya" ~ "Tulonge Afya",
                                  .default = mech_name)) %>% 
    unite(partner, c(mech_name, prime_partner_name), sep = " ") %>% 
    arrange(snu1, partner) %>% 
    group_by(snu1) %>% 
    summarise(partner = paste0(partner, collapse = ", "))

  
  shp_usaid <- full_join(shp_tza_snu1, df_agencies) %>% 
    st_make_valid() %>%
    summarise(geometry =  sf::st_union(geometry),
              .by = funding_agency) %>% 
    filter(funding_agency == "USAID")

# VIZ ---------------------------------------------------------------------

   
    m1 <- ggplot() +
    geom_sf(aes(fill = plhiv, geometry = geometry),
            data = sf_plhiv, alpha = .8,
            color = "gray", na.rm = TRUE) +
    geom_sf_text(aes(label = snu1, geometry = geometry),
                 data = sf_agencies %>% filter(funding_agency == "USAID"),
                 family = "Source Sans Pro", color = grey10k) +
    scale_fill_viridis_c(label = label_number(scale_cut = cut_short_scale())) +
    labs(x = "", y = "", fill = "PLHIV (2024)",
         caption = "") +
    si_style_map() +
    si_legend_fill()
  
  
  m2 <- ggplot() +
    geom_sf(aes(fill = funding_agency, geometry = geometry),
            data = sf_agencies, alpha = .6,
            color = "white", na.rm = TRUE) +
    geom_sf_text(aes(label = snu1, geometry = geometry),
            data = sf_agencies %>% filter(funding_agency == "USAID"),
            family = "Source Sans Pro", color = matterhorn) +
    scale_fill_manual(values = c("USAID" = denim,
                                 "CDC" = scooter_light,
                                 "DOD" = genoa_light),
                      na.value = NA) +
    labs(x = "", y = "", fill = "Agency",
         caption = str_replace(metadata$caption, "MSD", "MER + NAT_SUBNAT")) +
    si_style_map() +
    si_legend_color()

  m1 + m2 
  
  si_save("Images/tza_usaid_presence.png")
  