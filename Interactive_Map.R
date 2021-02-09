# library(dplyr)
# library(ggplot2)
# library(rjson)
# library(jsonlite)
library(leaflet)
# library(RCurl)
# library("tmap")
# library("sf")
# library("mapview")
# library("spData")
# library(shiny)


rm(list = ls())


# And one more leaflet
# make a simple track line
lin = data.frame(lon = c(-65.17536, -65.37423, -65.64541, -66.06122, -66.15161),
                 lat = c(43.30837, 42.94679, 42.87448, 42.92871, 42.72985))

# make a few points
pts = data.frame(lon = c(-65.3, -65.7, -64.1),
                 lat = c(43.4, 43, 42.9))

# build a polygon (in this case the 'Roseway Basin Area To Be Avoided')
ply = data.frame(lon = c(-64.916667, -64.983333, -65.516667, -66.083333),
                 lat = c(43.266667, 42.783333, 42.65, 42.866667))


# start basemap
map <- leaflet() %>%
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # add graticules from a NOAA webserver
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL, group = 'Graticules') %>%
  # focus map in a certain area / zoom level
  setView(lng = -65, lat = 43, zoom = 7) %>%
  # add layers control
  addLayersControl(overlayGroups = c('Place names',
                                     'Graticules',
                                     'Points',
                                     'Lines',
                                     'Polygons'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'topright') %>%
  # list groups to hide on startup
  hideGroup(c('Place names'))
map


# add polygons
map <- map %>%
  addPolygons(data=ply, lng=~lon, lat=~lat,
              weight = 1,
              color = 'grey',
              fillColor = 'grey',
              fill = T,
              fillOpacity = 0.25,
              stroke = T,
              dashArray = c(5,5),
              smoothFactor = 3,
              options = pathOptions(clickable = F),
              group = 'Polygons')
map


map <- map %>%
  # add a map scalebar
  addScaleBar(position = 'topright') #%>%
  # add measurement tool
  # addMeasure(
  #   primaryLengthUnit = "kilometers",
  #   secondaryLengthUnit = 'miles',
  #   primaryAreaUnit = "hectares",
  #   secondaryAreaUnit="acres",
  #   position = 'topleft')

# show map
map


# # # save a stand-alone, interactive map as an html file
library(htmlwidgets)
saveWidget(widget = map, file = 'map.html', selfcontained = T)


# # # save a snapshot as a png file
# library(mapview)
# mapshot(map, file = 'map.png')
