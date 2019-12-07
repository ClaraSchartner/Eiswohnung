library(shiny)
source('03Wohnungsadressen.R', local = TRUE)
#source('04Visualize.R', local = TRUE)



ui <- fluidPage(
  headerPanel(
    "Distance to Ice cream shop from appartment listed"
  ),
  column(4, wellPanel(
    helpText("Input an apartment listing and receive information and its ice-cream infrastructure"),
  textInput(
    "caption",
    label = "Input a link from www.immowelt.at:",
    "https://www.immowelt.at/expose/2s2hc4x"
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
    Locations <- Eisfunc(input$caption)
    dis <- st_distance(Eis, Locations)
    minD <- dis %>% apply(2, min)  %>% mean()
    numE <- which(apply(dis, 1, min) == as.numeric(min(dis)))
    Locationsh <- Locations[!duplicated(Locations$name), ]
    Locationmean <- st_sf(name = "mean", st_sfc(st_point(c(
      mean(st_coordinates(Locationsh)[, 1]),
      mean(st_coordinates(Locationsh)[, 2])
    ))),
    crs = "+proj=longlat +datum=WGS84 +no_defs")
    route <- osrmRoute(
      src = Eis[numE, ],
      dst = Locationmean,
      overview = "full",
      returnclass = "sf"
    )
    # if error due to too many requests, try again
    i <- 1
    while (is.null(route) && i < 5) {
      Sys.sleep(i)
      route <- osrmRoute(
        src = Eis[numE, ],
        dst = Locationmean,
        overview = "full",
        returnclass = "sf"
      )
      i <- i + 1
    }
    
    Info <-
      list(
        "Locations" = Locations,
        "dis" = dis,
        "minD" = minD,
        "numE" = numE,
        "Locationsh" = Locationsh,
        "Locationmean" = Locationmean,
        "Route" = route
      )
    Info
  })
  
  
  #reac$numE <- which(apply(dis,1, min)==as.numeric(min(dis)))
  
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
        )
      }
      
      
      
      
    }
  })
  
  
  
  output$map <- renderLeaflet({
    if (class(Info()[["Route"]])[1] == "sf") {
    zoom <- ifelse(Info()[["minD"]] > 3000, 13, 14)
    Stanizel <- makeIcon("data/StanizelIcon.png",
                         iconWidth = 20,
                         iconHeight = 30)
    leafIce <- leaflet() %>% addTiles() %>%
      setView(mean(c(
        st_coordinates(Info()[["Locationmean"]])[, 1], st_coordinates(Eis[Info()[["numE"]], ])[, 1]
      )),
      mean(c(
        st_coordinates(Info()[["Locationmean"]])[, 2], st_coordinates(Eis[Info()[["numE"]], ])[, 2]
      )),
      zoom = zoom) %>%
      addMarkers(st_coordinates(Eis)[, 1], st_coordinates(Eis)[, 2] , icon = Stanizel)
    Haus <- makeIcon("data/hausIcon.png",
                     iconWidth = 20,
                     iconHeight = 30)
    
    
  
      if (nrow(st_coordinates(Info()[["Locationsh"]])) == 1) {
        leafIce %>%
          addMarkers(st_coordinates(Info()[["Locationsh"]])[, 1],
                     st_coordinates(Info()[["Locationsh"]])[, 2] ,
                     icon = Haus) %>%
          addPolylines(st_coordinates(Info()[["Route"]])[, 1], st_coordinates(Info()[["Route"]])[, 2])
      }
      else{
        leafIce %>%
          addCircles(
            st_coordinates(Info()[["Locationsh"]])[, 1],
            st_coordinates(Info()[["Locationsh"]])[, 2] ,
            radius = 10,
            color = "red"
          ) %>%
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