source("00libraries.R")
vegurl <- "https://www.veganista.at/shops"
vegadr <- read_html(vegurl) %>%
  html_nodes('.txtNew') %>%
  html_text()
# Select only adresses & only in Vienna
vegadr <- vegadr[vegadr %>% grepl("Wien", .)]

vegadr <- substr(x = vegadr, 1, 
                 regexpr("Wien", vegadr) + 3)
vegadr <- gsub("\n\n", " ", vegadr)
Eis <- tibble(Adr= vegadr) %>%
  geocode(Adr, "osm")


# Visualize ---------------------------------------------------------------
Stanizel <- makeIcon("StanizelIcon.png",
                          iconWidth = 20, iconHeight = 30)
leaflet() %>% addTiles() %>% setView(lat=48.2,16.4,zoom=11) %>% 
  addMarkers(Eis$long, Eis$lat, icon = Stanizel)

