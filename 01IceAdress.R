source("00libraries.R")
# get adressees
vegurl <- "https://www.veganista.at/shops"
vegadr <- read_html(vegurl) %>%
  html_nodes('.txtNew') %>%
  html_text()
# Select only adresses & only in Vienna
vegadr <- vegadr[vegadr %>% grepl("Wien", .)]

vegadr <- substr(x = vegadr, 1, 
                 regexpr("Wien", vegadr) + 3)
vegadr <- gsub("\n\n", " ", vegadr)
vegadr <- c(vegadr, "Rotenturmstraße 14, 1010 Wien", "Mariahilfer Straße 33, 1060 Wien",
            "Neubaugasse 9, 1070 Wien", "Reumannplatz 13, 1100 Wien")
# get coordinates for adresses (=geocode)
Eiscoor <- tibble(Adr= vegadr) %>%
  geocode(Adr, "osm") 

Eis <- st_as_sf(Eiscoor, coords= c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
st_write(Eis, "www/Eis.shp", update = TRUE)
Eis <- st_read("www/Eis.shp")

# Visualize ---------------------------------------------------------------
Stanizel <- makeIcon("data/StanizelIcon.png",
                          iconWidth = 20, iconHeight = 30)
leafIce <- leaflet() %>% addTiles() %>% setView(lat=48.2,16.4,zoom=12) %>% 
 # addMarkers(Eis$geometry, icon = Stanizel)
  addMarkers(st_coordinates(Eis)[,1],st_coordinates(Eis)[,2] , icon = Stanizel)
leafIce

# Isochrone ---------------------------------------------------------------

# Loop making isochrone for every ice cream
# within the loop union
# bins <- c(0,5,8,10)
# for(i in seq_along(Eiscoor$long)){
#    Sys.sleep(rnorm(1,500,40))
# EisIso <- osrmIsochrone(loc =c(Eiscoor$long[i], Eiscoor$lat[i]), 
#                         breaks = bins, 
#                         returnclass="sf")
# # rest, so that API does not block
# 
#  # combine the current polygon with the polygon containing the other ice creams
# if( i ==1){
#   EisAlle <- EisIso %>% select(min)
# }
# EisAlle <- st_union(EisIso, EisAlle)
# # polygos from different ice creams overlap
# EisAlle$min <- apply(EisAlle,1, function(x)min(x$min, x$min.1))
# EisAlle<- select(EisAlle, min)
# }
# saveRDS(EisAlle, "data/IsoIce.rds")
# EisAlle <- readRDS("data/IsoIce.rds")
# 
# 
# EisAlle1 <- st_union(EisAlle)
# 
# 
# # visualise
# pal <- colorNumeric(
#   palette = "Blues",
#   domain = EisAlle$min)
# leaflet(EisAlle) %>% addTiles() %>% setView(lat=48.2,16.4,zoom=12) %>% 
#   addPolygons(
#   fillColor= ~pal(EisAlle$min),weight = 2,
#   opacity = 0.1,
#   color = "white",
#   fillOpacity = 0.3) %>% addMarkers(Eiscoor$long, Eiscoor$lat, icon = Stanizel)
