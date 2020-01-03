url <- "https://www.immowelt.at/liste/wien/wohnungen/mieten?sort=relevanz"

text <- read_html(url)
html <- paste(readLines(url), collapse="\n")
posurl <- str_locate_all(html, "data-oid=")[[1]][,2]
urllinks <-substr(html, posurl+2, posurl+8)
