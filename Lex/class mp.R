library(leaflet)

leaflet() %>% addTiles()
leaflet() %>% setView(#put longitute and latitude here too)

  #shows you specific places
leaflet(#dataset here witrh lat and lng) %>%
  setView(lng = -79, lat = 37, zoom = 12)
addTiles() %>%
  addMarkers(popup = ~place, clusterOptions - markerClusterOptions())


