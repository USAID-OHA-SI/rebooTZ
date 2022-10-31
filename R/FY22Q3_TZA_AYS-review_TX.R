# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  show Q3 growth in TX
# REF ID:   e83bd1af 
# LICENSE:  MIT
# DATE:     2022-10-24
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "e83bd1af"
  get_metadata()

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_msd() %>% 
    filter(operatingunit == "Tanzania")
  

# MUNGE TX ----------------------------------------------------------------

  df_tx <- df %>% 
    filter(indicator == "TX_CURR",
           funding_agency == "USAID",
           str_detect(prime_partner_name, "(DELOITTE|Elizabeth)"),
           fiscal_year >= 2021) %>% 
    pluck_totals() %>% 
    mutate(partner = recode(prime_partner_name,
           "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF",
           "DELOITTE CONSULTING LIMITED" = "Deloitte")) %>% 
    group_by(fiscal_year, indicator, partner) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    mutate(value_q4 = case_when(period == "FY21Q4" ~ value),
           fy = str_sub(period, end = 4),
           fill_color = ifelse(partner == "Deloitte", burnt_sienna, moody_blue),
           fill_alpha = ifelse(fy == "FY22", 1, .7)) %>%
    arrange(partner, period) %>% 
    group_by(partner) %>% 
    fill(value_q4, .direction = "down") %>% 
    ungroup() %>% 
    mutate(value_q4 = ifelse(period == "FY21Q4" | value > value_q4, NA_real_, value_q4))


# PLOT TX -----------------------------------------------------------------

  df_tx %>% 
    ggplot(aes(period, value, fill = fill_color, alpha = fill_alpha)) +
    geom_blank() +
    annotate(geom = "rect",
             xmin = 4.5, xmax = 7.5,
             ymin = 0, ymax = Inf,
             fill = matterhorn, alpha = .1
             ) +
    geom_col(aes(y = value_q4, color = fill_color), fill = NA, na.rm = TRUE) +
    geom_col() +
    geom_text(aes(label = label_number(1, scale_cut = cut_short_scale())(value)),
              family = "Source Sans Pro", color = nero,
              vjust = -1) +
    facet_wrap(~partner) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_alpha_identity() +
    scale_y_continuous(expand = c(.005, .005)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         # title = "ACCESS TO NECESSARY FUNDING IN APRIL ALLOWED DELOITTE TO INCREASE RESULTS AFTER 2Q DECLINE",
         # subtitle = "Patients Currently on Treatment (TX_CURR)",``
         caption = metadata$caption) +
    si_style_xline() +
    theme(axis.text.y = element_blank())

  si_save("Graphics/FYQ3_TXCURR-trends.svg",
          height = 6, width = 12)  


# MUNGE MMD ---------------------------------------------------------------

  df_mmd <- df %>% 
    filter(indicator %in% "TX_CURR",
           funding_agency == "USAID",
           str_detect(prime_partner_name, "(DELOITTE|Elizabeth)"),
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus"),
           otherdisaggregate %in% c(NA, "ARV Dispensing Quantity - 6 or more months"),
           fiscal_year >= 2021) %>% 
    mutate(indicator = ifelse(is.na(otherdisaggregate), indicator, "TX_MMD"),
           partner = recode(prime_partner_name,
                            "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF",
                            "DELOITTE CONSULTING LIMITED" = "Deloitte")) %>% 
    # count(indicator, otherdisaggregate, wt = cumulative)
    group_by(fiscal_year, indicator, partner) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(fy = str_sub(period, end = 4),
           fill_color = ifelse(partner == "Deloitte", burnt_sienna, moody_blue),
           fill_alpha = ifelse(fy == "FY22", 1, .7),
           eligible = .6 * tx_curr,
           share_mmd = tx_mmd / eligible) 
  
  

# PLOT MMD ----------------------------------------------------------------

  df_mmd %>% 
    ggplot(aes(period, share_mmd, color = fill_color,
               fill = fill_color, group = partner)) +
    geom_area(alpha = .6, size = 1.2) +
    # geom_text(aes(label = label_number(1, scale_cut = cut_short_scale())(value)),
    #           family = "Source Sans Pro", color = nero,
    #           vjust = -1) +
    facet_wrap(~partner, scales = "free_y") +
    scale_color_identity() +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_y_continuous(label = percent, position = "right") +
    expand_limits(y = 1) +
    labs(x = NULL, y = NULL,
         subtitle = "Share of Eligible Clients on MMD 6 months",
         caption = glue("Note: MMD eligible estimated as 60% of current treatment cohort | {metadata$caption}")) +
    si_style_ygrid()
    
  
  si_save("Graphics/FY22Q3_MMD-eligible.svg",
          height = 6, width = 12)
    