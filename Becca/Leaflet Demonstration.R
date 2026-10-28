library(leaflet)

leaflet()
leaflet() %>% addTiles()

leaflet() %>% setView(lng = -79.442778, lat = 37.783889, zoom = 12) %>% addTiles()




lex <- data.frame( lat    = c(37.789444 , 37.787673  ,  37.785624  )
                   , lon    = c(-79.441725, -79.443623 , -79.441544  )
                   , place  = c("Lab"     , "Classroom", "Thai food!")
                   , rating = c(7         , 5          , 10          )
                   , stringsAsFactors = FALSE
)


leaflet(data = lex) %>% 
  setView(lng = -79.442778, lat = 37.783889, zoom = 12) %>%
  addTiles() %>%
addMarkers(popup = ~place, clusterOptions = markerClusterOptions(
)) 


leaflet(data = lex) %>% 
  setView(lng = -79.442778, lat = 37.783889, zoom = 12) %>%
  addTiles() %>%
  addCircles(radius = ~rating, label = ~place)
