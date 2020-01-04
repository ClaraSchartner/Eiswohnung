#mieten?roomi=2&wflmi=50&sort=createdate%2Bdesc
#https://www.immowelt.at/liste/wien/wohnungen/mieten?roomi=2&prima=700&wflmi=50&sort=createdate%2Bdesc
#https://www.immowelt.at/liste/wien/wohnungen/mieten?sort=createdate%2Bdesc

extracturls <- function(MinRoom, MinM2, PriceMax){
  inp <- c(!missing(MinRoom), !missing(PriceMax), !missing(MinM2))
  inpvar <- c(ifelse(!missing(MinRoom), paste0("roomi=",MinRoom),""),
              ifelse(!missing(PriceMax), paste0("prima=",PriceMax), ""), 
                ifelse(!missing(MinM2), paste0("wflmi=",MinM2), ""))[inp]
  if(length(inpvar>0)){
    strurl <- paste0(inpvar,"&", collapse="")
  } else{
    strurl<-""
  }

  urlov = paste0("https://www.immowelt.at/liste/wien/wohnungen/mieten?", strurl, "sort=createdate%2Bdesc")
         
  
html <- paste(readLines(urlov), collapse="\n")
html <- str_match_all(html, "<a href=\"(.*?)\"")
html <- html[[1]][lapply(html, function(x)grepl(x,pattern = "expose"))[[1]]]

posurl <- str_locate_all(html, "expose")

posurl<- lapply(posurl, function(x)x[,2]) %>% unlist()
urllinks <- substr(html, posurl+2, posurl+8)
urllinks <- urllinks[!duplicated(urllinks)]
url <- paste0("https://www.immowelt.at/expose/", urllinks)
url
}
