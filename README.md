# Eiswohnung
This Shiny app helps to choose an apartment close to the best ice-cream shops in Vienna.
Plug in the url of an apartment add from www.immowelt.at get the distance and directions to the closest Veganista, Eisgreissler and Tichy in Vienna. (more to follow)
If no address is given in the apartment listing, the locations description is used and subway/bus/tram stops and other streets are used as an indiation of location. All geocoding and routing is done using osm trough the osrm package.

Any locations given in the description laying outside the discrict + a small buffer are discarted.