url <- "https://www.immobilienscout24.at/expose/5db8685a6bb5c22100be7fea"


woh <- read_html(vegurl) 
plz <-woh%>%
  html_nodes('.postalcode') %>%
  html_text()
anzText <- woh %>% html_text()