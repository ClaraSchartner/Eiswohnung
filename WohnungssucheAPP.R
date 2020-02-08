library(shiny)
library(shinythemes)
library(rsconnect)
source('03Wohnungsadressen.R', local = TRUE)
source('03aGetGeofromAddress.R', local = TRUE)
source('04getfurtherlinks.R', local = TRUE)
#source('04Visualize.R', local = TRUE)



ui <- fluidPage(theme = shinytheme("yeti"),
                headerPanel(
                  "Distance to Ice cream shop from appartment listed"
                ),
                column(4, wellPanel(
                textInput(
                  "Rooms",
                  label = "Mininum Number of Rooms",
                  2
                ),
                textInput(
                  "Rent",
                  label = "Max Rent",
                  800
                ),
                textInput(
                  "m2",
                  label = "Mininum m2",
                  40
                ))),
                column(8, wellPanel(
                tableOutput("table")
                
                )))

server <- function(input, output) {
  Info <- reactive({
    urls <- extracturls(MinRoom = input$Rooms, MinM2 = input$m2, PriceMax = input$Rent)
    # Eis
    EisDist<- lapply(urls, FUN = Eisfunc) %>% 
      lapply(function(x) st_distance(Eis, x)) %>% 
   lapply(function(x)apply(x,2, min)) %>% lapply(mean) %>%  unlist()
     # Merkmale
    
   mm <- lapply(urls, function(x) read_html(x) %>%
      html_nodes('.merkmale') %>%
      html_text() %>% grepl(
     "Terasse|Balkon|Garten", 
     ., ignore.case = TRUE)) %>% unlist()
   mm <- ifelse(mm, "Balkon/Garten", "-")
   Neubau <- lapply(urls, function(x) read_html(x) %>%
                  html_nodes('.merkmale') %>%
                  html_text() %>% grepl(
                    "Neubau", 
                    ., ignore.case = TRUE)) %>% unlist()
   Altbau <- lapply(urls, function(x) read_html(x) %>%
                      html_nodes('.merkmale') %>%
                      html_text() %>% grepl(
                        "Altbau", 
                        ., ignore.case = TRUE)) %>% unlist()
   Bauart<-ifelse(Neubau, "Neubau", ifelse(Altbau, "Altbau", "-"))
    Info <- data.frame(Distanz = EisDist, Balkon = mm, Bauart = Bauart, Link = urls)
    
  })
  output$table <- renderTable(Info())
  }
shinyApp(ui, server)
