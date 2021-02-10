# library(dplyr)
# library(ggplot2)
# library(rjson)
# library(jsonlite)
library("leaflet")
# library(RCurl)
# library("tmap")
# library("sf")
# library("mapview")
# library("spData")
# library(shiny)
library("rgdal")
library("RColorBrewer")
library("htmlwidgets")


# rm(list = ls())



# Choropleth map with leaflet
## Download the shapefile with world borders. Note: overwrites existing files!
if(!file.exists("TM_WORLD_BORDERS-0.3.shp")){
  print("File not there, downloading now:")
  download.file("thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                destfile="world_shape_file.zip")
  system("unzip -u world_shape_file.zip;
         rm world_shape_file.zip Readme.txt")
}



## Read the shape file
world_spdf <- readOGR(dsn = ".",
                      layer = "TM_WORLD_BORDERS-0.3",
                      verbose = FALSE)
class(world_spdf)
summary(world_spdf)


## Collate countries and their names (as soon as it comes to map the world,
## geopolitics come to the fore...)
length(world_spdf$NAME) # many more countries in the shp file
length(epi$country)

sort(world_spdf$NAME)
sort(epi$country)

sort(setdiff(world_spdf$NAME, epi$country)) # differences due to naming and numbers
sort(setdiff(epi$country, world_spdf$NAME))


#### Rename countries with their more up-to-date or shorter name
epi$country[epi$country == "Cabo Verde"] <- "Cape Verde"
epi$country[epi$country == "Dem. Rep. Congo"] <-
  "Democratic Republic of the Congo"
epi$country[epi$country == "Eswatini"] <- "Swaziland"
world_spdf$NAME[world_spdf$NAME == "Iran (Islamic Republic of)"] <- "Iran"
world_spdf$NAME[world_spdf$NAME == "Lao People's Democratic Republic"] <- "Laos"
world_spdf$NAME[world_spdf$NAME == "Micronesia, Federated States of"] <-
  "Micronesia"
world_spdf$NAME[world_spdf$NAME == "Republic of Moldova"] <- "Moldova"
world_spdf$NAME[world_spdf$NAME == "Burma"] <- "Myanmar"
world_spdf$NAME[world_spdf$NAME ==
                  "The former Yugoslav Republic of Macedonia"] <-
  "North Macedonia"
world_spdf$NAME[world_spdf$NAME == "Congo"] <- "Republic of Congo"
world_spdf$NAME[world_spdf$NAME == "Korea, Republic of"] <-
  "South Korea"
world_spdf$NAME[world_spdf$NAME == "United Republic of Tanzania"] <- "Tanzania"
world_spdf$NAME[world_spdf$NAME == "United States"] <-
  "United States of America"



#### Control if all EPI countries are present in the world map
sort(setdiff(epi$country, world_spdf$NAME))


#### Countries without an EPI are miniature states, islands or states currently
#### in an armed conflict. For now, keep it as is. Later possibly merge islands
#### into their mainlands...
sort(setdiff(world_spdf$NAME, epi$country))


## Add EPI to shp file
world_spdf <- merge(world_spdf, epi[, c(1, 4)], by.x = "NAME", by.y = "country")


## Clean the data object
world_spdf@data$POP2005[which(world_spdf@data$POP2005 == 0)] <- NA
world_spdf@data$POP2005 <- as.integer(world_spdf@data$POP2005) / 1000000 %>%
  round(2)


# Create a color palette for the map
mypalette <- colorNumeric(palette = "viridis", domain = world_spdf@data$EPI.new,
                          na.color = "gray")
mypalette(c(45, 43))


# Basic  leafletchoropleth map
leaflet(world_spdf) %>%
  addTiles()  %>%
  setView(lat = 10, lng = 0, zoom = 2) %>%
  addPolygons(fillColor = ~ mypalette(EPI.new), stroke = FALSE)


# Color by quantile
leaflet(world_spdf)%>%
  addTiles() %>%
  setView(lat = 5, lng = 0, zoom = 2) %>%
  addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
               color = ~ colorQuantile("YlOrRd", EPI.new)(EPI.new))


# Color by binning
# mybins <- c(0, 10, 20, 50, 100, 500, Inf)
# mypalette <- colorBin(palette = "YlOrBr", domain = world_spdf@data$POP2005,
#                       na.color = "transparent", bins = mybins)


# Prepare the text for tooltips
tooltips <- paste("Country:", world_spdf@data$NAME, "<br/>",
                  "Area:", world_spdf@data$AREA, "<br/>",
                  "Population:", round(world_spdf@data$POP2005, 2), "M", "<br/>",
                  "EPI 29020:", world_spdf@data$EPI.new) %>%
  lapply(htmltools::HTML)


# The map
m <- leaflet(world_spdf) %>%
  addTiles() %>%
  setView(lat = 10, lng = 0, zoom = 3) %>%
  addPolygons(fillColor = ~ mypalette(EPI.new), stroke = TRUE,
              fillOpacity = 0.9, color = "white", weight = 0.3, label = tooltips,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "13px",
                                          direction = "auto")) %>%
  addLegend(pal = mypalette, values = ~ EPI.new, opacity = 0.9,
            title = "EPI", position = "topleft")
m


# Export to html
saveWidget(m, file = "../index.html")


################################################################################
################################################################################
