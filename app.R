library(shiny)
source('03Wohnungsadressen.R', local = TRUE)
#source('04Visualize.R', local = TRUE)



ui <- fluidPage(
  textInput("caption", "Input a link from www.immowelt.at", "https://www.immowelt.at/expose/2s2hc4x"),
  
  verbatimTextOutput("value"),
  leafletOutput("map")
)
server <- function(input, output) {
 # numE <- reactive({
  #   which(apply(dis,1, min)==as.numeric(min(dis)))
  #})
  Info <- reactive({ 
    Locations <- Eisfunc(input$caption)
    dis <- st_distance(Eis, Locations) 
    minD <- dis %>% apply(2, min)  %>% mean()
    numE <- which(apply(dis,1, min)==as.numeric(min(dis)))
    Locationsh <- Locations[!duplicated(Locations$name),]
    Locationmean <- st_sf(name = "mean",st_sfc(
      st_point(c(mean(st_coordinates(Locationsh)[,1]),
                 mean(st_coordinates(Locationsh)[,2])))), 
      crs = "+proj=longlat +datum=WGS84 +no_defs")
    route <- osrmRoute(src = Eis[numE,], dst = Locationmean,
              overview = "full", returnclass = "sf")
    # if error due to too many requests, try again
    i<-1
    while(is.null(route)&&i<5){
      Sys.sleep(i)
      route <- osrmRoute(src = Eis[numE,], dst = Locationmean,
                         overview = "full", returnclass = "sf")
      i<-i+1}
    
    Info <- list("Locations"=Locations, "dis" = dis, "minD" = minD, "numE" = numE,
                 "Locationsh" = Locationsh, "Locationmean" = Locationmean,
                 "Route"= route)
    Info
    })
  
  
 #reac$numE <- which(apply(dis,1, min)==as.numeric(min(dis)))
  
  output$value <- renderText({ 
    
    
    if(nrow(Info()[["Locations"]])==0){
      paste("No location information retrieved")
    } else{
  
      if(class(Info()[["Route"]])[1]=="sf"){
        paste("Die Distanz zum nächsten Eis ist", 
              round(st_length(Info()[["Route"]])),
              "m zum Eisgeschäft in der", Eis[Info()[["numE"]],]$Adr %>% as.character() )}
      else{
        paste("Time out des OSM Routingdienstes: Die Luftlinie zum nächsten Eis ist", 
              round(Info()[["minD"]]),
              "Meter zur", 
              Eis[Info()[["numE"]],]$Adr %>% as.character())
      }
      
      
      
      
    }
  }
  )
  
  
  
  output$map <- renderLeaflet({
                
      
      Stanizel <- makeIcon("data/StanizelIcon.png",
                           iconWidth = 20, iconHeight = 30)
      leafIce <- leaflet() %>% addTiles() %>% 
        setView(mean(c(st_coordinates(Info()[["Locationmean"]])[,1], st_coordinates(Eis[Info()[["numE"]],])[,1])),
                         mean(c(st_coordinates(Info()[["Locationmean"]])[,2], st_coordinates(Eis[Info()[["numE"]],])[,2])),
                zoom=14) %>% 
        addMarkers(st_coordinates(Eis)[,1],st_coordinates(Eis)[,2] , icon = Stanizel)
      Haus <- makeIcon("data/hausIcon.png",
                       iconWidth = 20, iconHeight = 30)
      
      
      if(class(Info()[["Route"]])[1]=="sf"){
        leafIce %>% 
          addMarkers(st_coordinates(Info()[["Locationsh"]])[,1],st_coordinates(Info()[["Locationsh"]])[,2] , icon = Haus) %>%
          addPolylines(st_coordinates(Info()[["Route"]])[,1], st_coordinates(Info()[["Route"]])[,2])} 
      else{
            leafIce %>% 
              addMarkers(st_coordinates(Info()[["Locationsh"]])[,1],st_coordinates(Info()[["Locationsh"]])[,2] , icon = Haus)
            
          }
   
      
    }
  )
}
shinyApp(ui, server)