library(shiny)
source('03Wohnungsadressen.R', local = TRUE)
source('04Visualize.R', local = TRUE)



ui <- fluidPage(
  textInput("caption", "Caption", "https://www.immowelt.at/expose/2s89a4t"),
  
  verbatimTextOutput("value"),
  leafletOutput("map")
)
server <- function(input, output) {
  reac <- reactiveValues()
  
 # reac$numE <- which(apply(dis,1, min)==as.numeric(min(dis)))
  
  output$value <- renderText({ 
    Locations <-  Eisfunc(input$caption)
    
    if(nrow(Locations)==0){
      paste("No location information retrieved")
    } else{
      dis <- st_distance(Eis, Locations) 
      minD <- dis %>% apply(2, min)  %>% mean()
      
      Locationmean <- st_sf(name = "mean",st_sfc(st_point(c(mean(st_coordinates(Locations)[,1]),
                                                            mean(st_coordinates(Locations)[,2])))), 
                            crs = "+proj=longlat +datum=WGS84 +no_defs")
      route <- tryCatch({
        paste("Die Distanz zum nächsten Eis ist", 
              round(st_length(osrmRoute(src = Eis[numE,], dst = Locationmean,
                                        overview = "full", returnclass = "sf"))),
              "m zum Eisgeschäft in der", Eis[numE,]$Adr %>% as.character() )
      },
      error=function(cond) {
        paste("Time out des OSM Routingdienstes: Die Luftlinie zum nächsten Eis ist", 
              round(minD),
              "zur", 
              Eis[numE,]$Adr %>% as.character()
        )}
      )
      
      
    }
  }
  )
  
  
  
  output$map <- renderLeaflet({
    url <- input$caption
    Locations <-  Eisfunc(url)
  
      Locationsh <- Locations[!duplicated(Locations$name),]
      Locationmean <- st_sf(name = "mean",st_sfc(st_point(c(mean(st_coordinates(Locationsh)[,1]),
                                                            mean(st_coordinates(Locationsh)[,2])))), 
                            crs = "+proj=longlat +datum=WGS84 +no_defs")
      
      Stanizel <- makeIcon("data/StanizelIcon.png",
                           iconWidth = 20, iconHeight = 30)
      leafIce <- leaflet() %>% addTiles() %>% 
        setView(mean(st_coordinates(Locationsh)[,1]),
                         mean(st_coordinates(Locationsh)[,2]),zoom=14) %>% 
        # addMarkers(Eis$geometry, icon = Stanizel)
        addMarkers(st_coordinates(Eis)[,1],st_coordinates(Eis)[,2] , icon = Stanizel)
      Haus <- makeIcon("data/hausIcon.png",
                       iconWidth = 20, iconHeight = 30)
      
      
      leafIce <- tryCatch({
        route <- osrmRoute(src = Eis[numE,], dst = Locationmean,
                           overview = "full", returnclass = "sf")
        leafIce %>% 
          addMarkers(st_coordinates(Locationsh)[,1],st_coordinates(Locationsh)[,2] , icon = Haus) %>%
          addPolylines(st_coordinates(route)[,1], st_coordinates(route)[,2])
        
      }, error = function()
        leafIce %>% 
        addMarkers(st_coordinates(Locationsh)[,1],st_coordinates(Locationsh)[,2] , icon = Haus)
      
      )
      
    }
  )
}
shinyApp(ui, server)