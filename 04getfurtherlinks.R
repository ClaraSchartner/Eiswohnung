extracturls <- function(urlov = "https://www.immowelt.at/liste/wien/wohnungen/mieten?sort=createdate%2Bdesc"){

html <- paste(readLines(urlov), collapse="\n")
html <- str_match_all(html, "<a href=\"(.*?)\"")
html <- html[[1]][lapply(html, function(x)grepl(x,pattern = "expose"))[[1]]]

posurl <- str_locate_all(html, "expose")

posurl<- lapply(posurl, function(x)x[,2]) %>% unlist()
urllinks <- substr(html, posurl+2, posurl+8)
url <- paste0("https://www.immowelt.at/expose/", urllinks)
url
}
