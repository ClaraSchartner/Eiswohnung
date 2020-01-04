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
    helpText("Input an apartment listing and receive information and its ice-cream infrastructure"),
  textInput(
    "caption",
    label = "Input a link from www.immowelt.at or an address:",
    extracturls()[1]
  ),
  helpText("Calculates the distance to closest Veganista, Eisgreissler and Tichy")
 # helpText(a("clickediclick"), href = "www.immowelt.at")
  ),
  wellPanel(verbatimTextOutput("value")),
 wellPanel(
           helpText("Note:
                    If no address is given in the apartment listing, 
                    the locations description is used and subway/bus/tram stops, streets and other points of interest 
are used as an indiation of location. 
                    All geocoding and routing is done using osm trough the osrm package."))),
  column(8, wellPanel(
   # helpText("Please be patient while the osm routing service calculates."),
  leafletOutput("map"))
))
server <- function(input, output) {
  Info <- reactive({
    if(grepl(input$caption,pattern = "immowelt")){
      Locations <- Eisfunc(input$caption)
    } else {
      Locations <- tibble(name= paste(input$caption, "Wien")) %>%
        geocode(name, "osm") %>% 
        st_as_sf( coords= c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs") 
    }
    Info <- getGeoInfo(Locations = Locations, Eis = Eis)

  })
  
  output$value <- renderText({
    if (nrow(Info()[["Locations"]]) == 0) {
      paste("No location \n information retrieved")
    } else{
      if (class(Info()[["Route"]])[1] == "sf") {
        paste(
          "The closest ice cream \n is",
          round(st_length(Info()[["Route"]])),
          "m away. \n It is located at \n",
          Eis[Info()[["numE"]], ]$Adr %>% as.character()
        )[1]
      }
      else{
        paste(
          "Time out des OSM Routingdienstes: \n The linear Distance is",
          round(Info()[["minD"]]),
          "m to \n",
          Eis[Info()[["numE"]], ]$Adr %>% as.character()
        )[1]
      }
    }
  })
  
  
  
  output$map <- renderLeaflet({
    Stanizel <- makeIcon("www/StanizelIcon.png",
                         iconWidth = 20,
                         iconHeight = 30)
    Haus <- makeIcon("www/hausIcon.png",
                     iconWidth = 20,
                     iconHeight = 30)
    leafIce <- leaflet() %>%   addTiles() %>%
      addMarkers(st_coordinates(Eis)[, 1], st_coordinates(Eis)[, 2] , icon = Stanizel)
    if (class(Info()[["Route"]])[1] == "sf") {
    zoom <- ifelse(Info()[["minD"]] > 3000, 13, 14)

    leafIce %>% 
      setView(mean(c(
        st_coordinates(Info()[["Locationmean"]])[, 1], st_coordinates(Eis[Info()[["numE"]], ])[, 1]
      )),
      mean(c(
        st_coordinates(Info()[["Locationmean"]])[, 2], st_coordinates(Eis[Info()[["numE"]], ])[, 2]
      )),
      zoom = zoom)

    
    
  
      if (nrow(st_coordinates(Info()[["Locationsh"]])) == 1) {
        leafIce %>%
          addMarkers(st_coordinates(Info()[["Locationsh"]])[, 1],
                     st_coordinates(Info()[["Locationsh"]])[, 2] ,
                     icon = Haus) %>%
          addPolylines(st_coordinates(Info()[["Route"]])[, 1], st_coordinates(Info()[["Route"]])[, 2])
      }
      else{
        leafIce %>%
          addPolygons(st_coordinates(Info()[["Locationsh"]])[, 1],
                      st_coordinates(Info()[["Locationsh"]])[, 2], 
                      color = "green", fillOpacity = 0.5) %>% 
          addPolylines(st_coordinates(Info()[["Route"]])[, 1], st_coordinates(Info()[["Route"]])[, 2])

          
      }
    }
    else{ if(nrow(st_coordinates(Info()[["Locationsh"]]))>0){
      leafIce %>%
        addMarkers(st_coordinates(Info()[["Locationsh"]])[, 1],
                   st_coordinates(Info()[["Locationsh"]])[, 2] ,
                   icon = Haus)} else{
                     leaflet() %>% addTiles() %>%
                       setView(mean(st_coordinates(Eis)[, 1]
                       ),
                       mean( st_coordinates(Eis)[, 2]
                       ),
                       zoom = 12) %>%
                       addMarkers(st_coordinates(Eis)[, 1], st_coordinates(Eis)[, 2] , icon = Stanizel)
                   }
      
    }
    
    
  })
}
shinyApp(ui, server)