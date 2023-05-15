# PROJECT:  moz-support
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  map of Moz health facilities near MWI border for Cholera work
# LICENSE:  MIT
# DATE:     2023-04-11

#Dependencies -------------------------------------------

library(glamr)
library(gophr)
library(tidyverse)
library(sf)
library(gisr)
library(glitr)
library(scales)
library(patchwork)
library(glue)
library(here)
library(mozR)

load_secrets()

#Directories and globals  ------------------------------------

# Set up paths
merdata <- glamr::si_path("path_msd")
rasdata <- glamr::si_path("path_raster")
shpdata <- glamr::si_path("path_vector")
datim   <- glamr::si_path("path_datim")
dir_terr <- glamr::si_path("path_raster")

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_images <- "Images"

source_info <- source_info()

# IMPORT ------------------------------------------------------------------------

#LOAD MSD
df_site <- si_path() %>% 
  return_latest("MER_Structured_Datasets_Site_IM_FY21-23_20230317_v2_1_Mozambique") %>% 
  read_msd()

get_metadata()

#grab GIS AJUDA sitemap
ajuda_map <- pull_sitemap(sheetname = "map_gis")


# Load the shapefiles to grab boundaries from below
spdf_pepfar <- get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
cntry <- "Mozambique"


## Raster data
terr <- gisr::get_raster(path = dir_terr)

adm0 <- gisr::get_admin0(cntry)
adm1 <- gisr::get_admin1(cntry)

adm0_mwi <- gisr::get_admin0("Malawi")
adm0_zam <- gisr::get_admin0("Zambia")
adm0_zim <- gisr::get_admin0("Zimbabwe")
adm0_SA <- gisr::get_admin0("South Africa")

box <- adm1 %>%
  filter(name == "Tete") %>% 
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

aoi <- adm1 %>%
  filter(name == "Tete") %>% 
  st_geometry() %>% 
  st_transform(crs = 3857) %>% 
  st_centroid() %>% 
  st_buffer(400000) %>% 
  st_transform(crs = 4326) %>%
  st_as_sf()

bb <- aoi %>%
  st_bbox() %>% 
  as.list()

#get country level
cntry_lvl = 3
spdf_cntry <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = cntry_lvl)

spdf_cntry <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = cntry_lvl)

#get snu level
snu_lvl = 4

spdf_snu <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = snu_lvl)

#add color
spdf_snu <- spdf_snu %>%
  mutate(fill_color = ifelse(orgunit == "Tete", golden_sand, grey10k))


#get psnu level
psnu_lvl = 5

spdf_psnu <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = psnu_lvl)

plot(spdf_psnu) # The sf object inside is your psnu level
cntry_map <- plot(spdf_cntry) # ths sf object inside is your country level

# MUNGE -----------------------------------------------------------------

#filter ajuda site map and get distinct names
ajuda_map1 <- ajuda_map %>% 
  filter(!is.na(datim_uid))

ajuda_map1 <- ajuda_map1 %>% 
  distinct(datim_uid, sitename, latitude, longitude) %>% 
  filter(sitename %ni% c("HD Quissico", "HR Chokw√©"))

df_site_gis <- df_site %>%
  clean_agency() %>% 
  filter(fiscal_year %in% c(2022),
         funding_agency %in% c("CDC", "USAID"),
         facilityuid != "~",
         indicator %ni% c("SC_ARVDISP", "SC_CURR")) %>% 
  distinct(operatingunit, operatingunituid, sitename, facilityuid, snu1, funding_agency) %>% 
  left_join(ajuda_map %>% select(datim_uid, latitude, longitude),
            by = c("facilityuid" = "datim_uid"))

df_site_gis %>%
  filter(funding_agency == "USAID") %>% 
  ggplot() +
  geom_sf(data = spdf_cntry, aes(geometry = geometry), fill = grey10k) +
  #geom_sf(data = adm0_mwi, aes(geometry = geometry), fill = grey10k) +
  geom_sf(data = spdf_snu, aes(fill = fill_color), alpha = 0.5) +
  geom_point(aes(x = longitude, y = latitude, color = funding_agency), alpha = 0.5) +
 # st_buffer() +
  scale_color_manual(values = c("USAID" = old_rose)) +
  scale_fill_identity() +
  si_style_map() +
  labs(x = NULL, y = NULL, 
       caption = glue::glue("Source: {metadata$source} & DATIM API"),
       title = "USAID FY22 HEALTH FACILITIES") +
  theme(legend.position = "none")

#zoom box
ggplot() +
  geom_sf(data = box, color = old_rose) +
  geom_sf(data = aoi, fill = NA, color = scooter) +
  geom_sf(data = adm0_mwi, aes(geometry = geometry), fill = grey10k, size = 3, color = grey80k) +
  geom_sf(data = adm0_zam, aes(geometry = geometry), fill = grey10k, size = 3, color = grey80k) +
  geom_sf(data = adm0_zim, aes(geometry = geometry), fill = grey10k, size = 3, color = grey80k) +
  geom_sf(data = adm0, aes(geometry = geometry), fill = "#639344", size = 3, color = grey80k) +
  geom_sf(data = adm1, aes(geometry = geometry), fill = NA, color = grey20k) +
  geom_sf(data = adm0, aes(geometry = geometry), fill = NA, size = 2, color = grey10k) +
  geom_sf(data = adm1 %>% filter(name == "Tete"), alpha = 0.5) +
  geom_sf_text(data = adm0_mwi, aes(label = admin)) +
  geom_sf_text(data = adm1, aes(label = name)) +
  geom_sf_text(data = adm0_zam, aes(label = admin)) +
  geom_sf_text(data = adm0_zim, aes(label = admin)) +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  geom_point(data = df_site_gis %>% filter(snu1 == "Tete"), aes(x = longitude, y = latitude, color = funding_agency), alpha = 0.5) +
  #geom_sf_text(data = df_site_gis, aes(label = sitename)) +
  # st_buffer() +
  scale_color_manual(values = c("USAID" = old_rose)) +
  scale_fill_identity() +
  si_style_map() +
  labs(x = NULL, y = NULL, 
       caption = glue::glue("Source: {metadata$source} & DATIM API"),
       title = "USAID FY22 HEALTH FACILITIES") +
  theme(legend.position = "none")

