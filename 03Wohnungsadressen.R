source("00libraries.R")


Eisfunc <- function(url){
  if(class(url)!="character"){
    as.character(url)
  }
  bez <- st_read("www/BEZIRKSGRENZEOGDPolygon.shp", quiet=TRUE) %>% select(DISTRICT_C)
  text <- read_html(url) %>%
    html_nodes('.section_content.iw_right') %>%
    html_text()
  
  ub <- read_html(url) %>%
    html_nodes('.quickfacts.iw_left') %>%
    html_text()
  
  Addr <- read_html(url) %>%
    html_nodes('.location') %>%
    html_text()
  Anzei <- list()
  
  Anzei[[1]] <- Addr
  Anzei[[2]] <- c(text, ub) 
  # format text
  Anzei <- Anzei %>% gsub("\r\n", "", ., fixed=TRUE) %>% 
    gsub("\\r\\n", "", ., fixed=TRUE) %>% 
    gsub("\n", "", ., fixed=TRUE) %>% 
    strsplit(" ") %>% 
    lapply(function(x)x[unlist(lapply(x, function(x)x != ""))])
  stranz <- grep(
    "Straße|strasse|platz|Weg|gasse|str", 
    Anzei[[1]], ignore.case = TRUE)
  stranz <- stranz[!stranz %in% grep("Landstraße", Anzei[[1]], ignore.case = TRUE)][1]
  
  hausn <- as.numeric(Anzei[[1]][stranz+1])
  hausn <- ifelse(!is.na(hausn)&hausn<1000,
                  stranz+1, NA)
  Anzei[[1]] <- Anzei[[1]][c(1:2, stranz, hausn)]
  Anzei[[1]]<- na.omit(Anzei[[1]])
  
  if(length(Anzei[[1]])>2){
    Anzei[[1]] <- paste(Anzei[[1]], collapse = " ")
    Locations <- tibble(name= Anzei[[1]]) %>%
      geocode(name, "osm") %>% 
      st_as_sf(coords= c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
  } else{
    # Keywords which could be followed by info about the location
    Anzei[[2]]<-tolower(Anzei[[2]])
    Offi <- grep(
      "U1|U2|U3|U4|U6|U-Bahn|Bus|Haltestelle|Minuten|Fußweg|min|Meter|Straße|str|strasse|platz|Weg|gasse|nahe|befindet|station|park", 
      Anzei[[2]], ignore.case = TRUE )
    Loc <- lapply(Offi, function(x)Anzei[[2]][(x-4):(x+6)]) %>% unlist() %>% unique() 
    # filter out stopwords
    Loc <- Loc[!Loc %in% tm::stopwords("german")]

    poi <- readRDS("www/poi.rds")
    poi$name<-tolower(poi$name)
    Locations <- Loc[Loc %in% poi$name]
    Locations <- poi[which(poi$name==Locations),]
    Locations<- Locations[!Locations$name %in% c(letters, 1:9, LETTERS),]
    
    str <- Anzei[[2]][grep(
      "Straße|strasse|platz|Weg|gasse|park", 
      Anzei[[2]], ignore.case = TRUE )]
    str <- str[!grepl( "tiefgarage|park|bahn|stell", str, ignore.case = TRUE)]
    str <- str[sapply(str, nchar)>5]
    # introduce more checks
    str <- paste(str, "Wien")
    
    Locations<- tibble(name= str) %>%
      geocode(name, "osm") %>%
      filter(!is.na(lat)) %>% 
      st_as_sf( coords= c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
      rbind(Locations)
  
    bez <- bez %>% filter(DISTRICT_C == as.numeric(Anzei[[1]][1])) %>% st_buffer(0.005)
    # maybe add small buffer later
    Locations <- Locations[bez,]
  }
  Locations
}




