Anzei <- readLines("Anzeige.txt")
Anzei <- strsplit(Anzei, ";") %>% unlist()


if(Anzei[2] != "[]"){
  Locations <- tibble(Adr= Anzei[2]) %>%
    geocode(Adr, "osm") %>% 
    st_as_sf(coords= c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
} else{
antxt <- Anzei[4] %>% 
gsub("\\n", " ", ., fixed=TRUE)%>% strsplit(" ") %>% unlist()

antxt <- Anzei[4] %>% 
 strsplit(" |[\r\\n]") %>% unlist()
# Keywords which could be followed by info about the location
Offi <- grep("U1|U2|U3|U4|U6|U-Bahn|Bus|Haltestelle|Minuten|Fußweg|min|Meter|Straße|Weg|gasse", antxt)
Loc <- lapply(Offi, function(x)antxt[(x-4):(x+6)]) %>% unlist() %>% unique() 
# filter out numbers
Loc <- Loc[Loc %>% as.numeric() %>% is.na()]
# filter out stopwords
Loc <- Loc[!Loc %in% tm::stopwords("german")]


poi <- readRDS("poi.rds")
Locations <- Loc[Loc %in% poi$name]


Locations<- poi[which(poi$name==Locations),] 
Locations<- Locations[!Locations$name %>% duplicated(),]
}

Eis <- st_read("data/Eis.shp")

minD <- st_distance(Eis, Locations) %>% apply(2, min) %>% mean()
minD
                                      