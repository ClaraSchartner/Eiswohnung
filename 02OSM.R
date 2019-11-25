q <- opq (getbb ("Vienna")) %>%
  add_osm_feature(key = "public_transport")%>%
  osmdata_sf()

poin <- q$osm_points %>% 
  dplyr::select(name, geometry)%>%  
  st_as_sf() 
saveRDS(poin, "poi.rds")

