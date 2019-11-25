Anzei <- readLines("Anzeige.txt")
Anzei <- strsplit(Anzei, ";") %>% unlist()
antxt <- Anzei[5] %>% strsplit(" ") %>% unlist()
# Keywords which could be followed by info about the location
Offi <- grep("U1|U2|U3|U4|U6|U-Bahn|Bus|Haltestelle|Minuten|Fußweg|min|Meter|Straße|Weg|gasse", antxt)
Loc <- lapply(Offi, function(x)antxt[(x-4):(x+6)]) %>% unlist() %>% unique() 
Loc[!Loc %in% tm::stopwords("german")]

