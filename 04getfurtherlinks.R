urlov <- "https://www.immowelt.at/liste/wien/wohnungen/mieten?sort=createdate%2Bdesc"

html <- paste(readLines(urlov), collapse="\n")
html <- str_match_all(html, "<a href=\"(.*?)\"")
html <- html[[1]][lapply(html, function(x)grepl(x,pattern = "expose"))[[1]]]

posurl <- str_locate_all(html, "expose")

posurl<- lapply(posurl, function(x)x[,2]) %>% unlist()
urllinks <- substr(html, posurl+2, posurl+8)

url <- paste0("https://www.immowelt.at/expose/", urllinks)
text <- read_html(url[4]) %>%
  html_nodes('.quickfacts.iw_left') %>%
  html_text()
text



ub <- read_html(urlov) %>%
  html_nodes('js-object   listitem_wrap ') %>%
  html_text()
ub
<div class="js-object   listitem_wrap " data-gok="20DC3E3A-208A-4BF6-86F3-DB3ADDF96F6A" data-oid="2sa644z" data-estateid="59738249">
