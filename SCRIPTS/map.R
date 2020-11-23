library(leaflet)
listings_small <- listings[c(1:1000), ]
listings_small %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = as.character(listings_small$revenue_365),clusterOptions = markerClusterOptions())
