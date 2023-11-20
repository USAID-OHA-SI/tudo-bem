# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Noami HIV prevalence map for Moz
# REF ID:   f2eb2324 
# LICENSE:  MIT
# DATE:     2023-11-20
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  library(httr)
  library(gagglr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "f2eb2324"
  
  # Set up paths
  merdata <- glamr::si_path("path_msd")
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim   <- glamr::si_path("path_datim")
  dir_terr <- glamr::si_path("path_raster")
  
  # API ----------------------------------------------------------------
  
  #example of url
  url <- "https://naomi2023.azurewebsites.net/api/v1/data?country=BWA&indicator=unaware_plhiv_num&ageGroup=Y015_049&period=2022-4&sex=both&areaLevel=3"
  
  #grab the base url
  baseurl <- "https://naomi2023.azurewebsites.net/api/v1/data?"
  
  #https://naomi2023.azurewebsites.net/api/v1/data?country=MOZ&indicator=incidence&ageGroup=Y015_049&period=2022-4&sex=both&areaLevel=1
  
  iso <- "MOZ"
  indic <- "incidence"
  age <- "Y015_049"
  period <- "2022-4" #dont change
  sex <- "both"
  area_level <- "2"
  
  url2 <- paste0(baseurl,
                 "country=", iso,
                 "&indicator=", indic,
                 "&ageGroup=", age,
                 "&period=",period,
                 "&sex=", sex,
                 "&areaLevel=", area_level)
  
  df <- httr::GET(url = url2)
  
  #take read in API and convert to comma separated string
  response_content <- df %>%
    httr::content("text")
  
  get_naomi <- function(iso, indicator, age_band = age, pd = period, gend = sex, level = area_level) {
    
    #create URL
    url2 <- paste0(baseurl,
                   "country=", iso,
                   "&indicator=", indicator,
                   "&ageGroup=", age_band,
                   "&period=",pd,
                   "&sex=", gend,
                   "&areaLevel=", area_level)
    #hit url
    df <- httr::GET(url = url2)
    
    #extract context
    response_content <- df %>%
      httr::content("text")
    
    df_final <- response_content %>%
      read_csv() %>%
      mutate(
        age_band = age,
        sex = sex,
        indicator = indicator,
        period = period,
        iso = iso,
        level = as.numeric(level),
        mean = as.numeric(mean),
        lower = as.numeric(lower),
        upper = as.numeric(upper))
    
    return(df_final)
    
  }
  
  df_incidence <- get_naomi(iso = "MOZ", indicator = "incidence") %>% 
    mutate(age = str_remove(age, "Y0")) %>%
    mutate(age = str_replace(age, "_0", "-")) %>% 
    mutate(area = case_when(area == 'Zambézia' ~ "Zambezia",
                            area == "Maputo Província" ~ "Maputo",
                            TRUE ~ area)) %>% 
    filter(level == 2)
  
  

  # shape files ----------------------------------------------------------
  
  # Load the shapefiles to grab boundaries from below
  spdf_pepfar <- get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
  cntry <- "Mozambique"
  
  adm0 <- gisr::get_admin0(cntry)
  adm1 <- gisr::get_admin1(cntry)
  
  #get country level
  cntry_lvl = 3
  spdf_cntry <- spdf_pepfar %>% 
    extract_boundaries(country = cntry, 
                       level = cntry_lvl)
  
  #get snu level
  snu_lvl = 4
  
  spdf_snu <- spdf_pepfar %>% 
    extract_boundaries(country = cntry, 
                       level = snu_lvl) %>% 
    left_join(df_incidence, by = c("orgunit" = "area"))
  
  
# VIZ -------------------------------------------------------------------
  df_incidence %>%
    # filter(funding_agency == "USAID") %>% 
    ggplot() +
    geom_sf(data = spdf_cntry, aes(geometry = geometry), fill = "white") +
    #geom_sf(data = adm0_mwi, aes(geometry = geometry), fill = grey10k) +
    geom_sf(data = spdf_snu, aes(fill = mean), alpha = 0.8) +
    scale_fill_si(palette = "scooters") +
    ggplot2::geom_sf_text(data = spdf_snu,
                          ggplot2::aes(label = orgunit),
                          family = "Source Sans Pro Light") +
    ggplot2::geom_sf_text(data = spdf_snu,
                          nudge_y =-1,
                          ggplot2::aes(label = scales::percent(mean*10)),
                          family = "Source Sans Pro Light") +
    labs(x = NULL,
         y = NULL,
         title = "HIV incidence remains high among adults aged 15-49 in provinces supported by USAID clinical partners" %>% toupper()) +
    si_style_map()
  
  si_save("Graphics/moz_incidence_map.svg")
  
  
 