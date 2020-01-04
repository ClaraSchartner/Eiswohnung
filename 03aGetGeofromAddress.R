
Eis <- st_read("www/Eis.shp", quiet=TRUE)

getGeoInfo <- function(Eis, Locations){
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
  Info}