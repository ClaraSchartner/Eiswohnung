library(shiny)
source('03Wohnungsadressen.R', local = TRUE)
  
  ui <- fluidPage(
    textInput("caption", "Caption", "https://www.immowelt.at/expose/2s89a4t"),
    
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    
    output$value <- renderText({ 
      url <- input$caption
      paste("Die Luftlinie zum nächsten Eis ist", Eisfunc(url))
       })
  }
  shinyApp(ui, server)