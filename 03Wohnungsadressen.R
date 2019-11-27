
url <- "https://www.immowelt.at/expose/2s8d74t"
url <-"https://www.immowelt.at/expose/2qqw64v"
url <- "https://www.immowelt.at/expose/2skw84t"
text <- read_html(url) %>%
  html_nodes('.section_content.iw_right') %>%
  html_text()


Addr <- read_html(url) %>%
  html_nodes('.location') %>%
  html_text()
Anzei <- list()

Anzei[[1]] <- Addr
Anzei[[2]] <- text 
# format text
Anzei <- Anzei %>% gsub("\r\n", "", ., fixed=TRUE) %>% 
  gsub("\\r\\n", "", ., fixed=TRUE) %>% 
  gsub("\n", "", ., fixed=TRUE) %>% 
strsplit(" ") %>% 
  lapply(function(x)x[unlist(lapply(x, function(x)x != ""))])


Anzei[[1]]<- Anzei[[1]][1:grep("Wien", Anzei[[1]])]

if(length(Anzei[[1]])>2){
  Anzei[[1]] <- paste(Anzei[[1]], collapse = " ")
  Locations <- tibble(Adr= Anzei[[1]]) %>%
    geocode(Adr, "osm") %>% 
    st_as_sf(coords= c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
} else{
# Keywords which could be followed by info about the location
  Anzei[[2]]<-tolower(Anzei[[2]])
Offi <- grep(
  "U1|U2|U3|U4|U6|U-Bahn|Bus|Haltestelle|Minuten|Fußweg|min|Meter|Straße|strasse|platz|Weg|gasse|nahe|befindet|station", 
  Anzei[[2]], ignore.case = TRUE )
Loc <- lapply(Offi, function(x)Anzei[[2]][(x-4):(x+6)]) %>% unlist() %>% unique() 
# filter out numbers
Loc <- Loc[Loc %>% as.numeric() %>% is.na()]
# filter out stopwords
Loc <- Loc[!Loc %in% tm::stopwords("german")]


poi <- readRDS("poi.rds")
poi$name<-tolower(poi$name)
Locations <- Loc[Loc %in% poi$name]
Locations <- poi[which(poi$name==Locations),]
Locations<- Locations[!Locations$name %in% c(letters, 1:9, LETTERS),]

str <- Anzei[[2]][grep(
  "Straße|strasse|platz|Weg|gasse", 
  Anzei[[2]], ignore.case = TRUE )]
str <- str[!grepl( "tiefgarage|park|bahn|stell", str, ignore.case = TRUE)]
str <- str[sapply(str, nchar)>5]
str <- paste(str, "Wien")

Locations<- tibble(name= str) %>%
  geocode(name, "osm") %>%
  filter(!is.na(lat)) %>% 
  st_as_sf( coords= c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  rbind(Locations)

Locations <- Locations[!Locations$name %>% duplicated(),]


}
if(nrow(Locations)==0){
  paste("No location information retrieved")
} else{

Eis <- st_read("data/Eis.shp")

minD <- st_distance(Eis, Locations) %>% apply(2, min) %>% mean()
minD}
                                      
