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

# ggplot() +
#   geom_sf(data = box, fill = NA, color = old_rose) +
#   geom_sf(data = aoi, fill = NA, color = scooter) +
#   geom_sf(data = adm0, aes(geometry = geometry), fill = NA) +
#   geom_sf(data = adm1, aes(geometry = geometry), fill = NA, color = grey20k) +
#   geom_sf(data = adm0, aes(geometry = geometry), fill = NA) +
#   geom_sf(data = adm1 %>% filter(name == "Tete"), alpha = 0.5)

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
