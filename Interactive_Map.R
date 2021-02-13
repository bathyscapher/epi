###############################################################################
###############################################################################
# Visualization of the Environmental Performance Index (EPI)


library("reshape2")
library("ggplot2")
theme_set(theme_bw(base_size = 12) +
            theme(rect = element_rect(fill = "transparent")))
library("leaflet")
library("rgdal")
library("htmlwidgets")


## Clear workspace, set working directory
rm(list = ls())
setwd("~/epi")


## Load utility functions
source("UtilityFunctions.R")


## Import EPI data
epi.list <- importEPI()


list2env(epi.list, .GlobalEnv)
rm(epi.list)


## Choropleth map with leaflet
### Download the shapefile with world borders. Note: overwrites existing files!
if(!file.exists("2020/TM_WORLD_BORDERS-0.3.shp")){
  print("File does not exist, downloading it now:")
  download.file("thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                destfile="world_shape_file.zip")
  system("unzip -u world_shape_file.zip;
         rm world_shape_file.zip Readme.txt")
}


### Read the shape file
world.spdf <- readOGR(dsn = "2020", layer = "TM_WORLD_BORDERS-0.3",
                      verbose = TRUE)
class(world.spdf)
summary(world.spdf)


### Collate countries and their names (as soon as it comes to map the world,
### geopolitics come to the fore...)
length(world.spdf$NAME) # many more countries in the shp file
length(epi$country)

sort(world.spdf$NAME)
sort(epi$country)

sort(setdiff(world.spdf$NAME, epi$country)) # differences due to naming and numbers
sort(setdiff(epi$country, world.spdf$NAME))


epi.world.spdf <- collateCountryNames(epi, world.spdf)
list2env(epi.world.spdf, .GlobalEnv)
rm(epi.world.spdf)


##### Control if all EPI countries are present in the world map
sort(setdiff(epi$country, world.spdf$NAME))


##### Countries without an EPI are miniature states, islands or states currently
##### in an armed conflict. For now, keep it as is. Later possibly merge islands
##### into their mainlands...
sort(setdiff(world.spdf$NAME, epi$country))


### Add EPI and regions to shp file
world.spdf <- merge(world.spdf, epi[, c(1, 4, 50)], by.x = "NAME",
                    by.y = "country")
names(world.spdf)


## Encode population numbers as millions
world.spdf@data$POP2005[which(world.spdf@data$POP2005 == 0)] <- NA
world.spdf@data$POP2005 <- as.integer(world.spdf@data$POP2005) / 1000000 %>%
  round(2)


# Create a color palette for the map
## EPI color scheme
epi.cols <- colorNumeric(palette = "viridis", domain = world.spdf@data$EPI.new,
                         na.color = "gray", reverse = TRUE)


## Region color scheme
region.cols <- colorFactor(palette = "plasma", domain = world.spdf@data$region,
                           na.color = "gray")


## Prepare the text for tooltips
tooltips <- paste("Country:", world.spdf@data$NAME, "<br/>",
                  "Region:", world.spdf@data$region, "<br/r>",
                  # "Area:", world.spdf@data$AREA, "<br/>",
                  "Population:", round(world.spdf@data$POP2005, 2), "M",
                  "<br/>",
                  "EPI:", world.spdf@data$EPI.new) %>%
  lapply(htmltools::HTML)


## Choropleth map
cp.map <- leaflet(world.spdf) %>%
  addTiles() %>%
  setView(lat = 20, lng = 0, zoom = 2.5) %>%
  addPolygons(fillColor = ~ epi.cols(EPI.new), stroke = TRUE, group = "EPI",
              fillOpacity = 0.9, color = "white", weight = 0.5,
              label = tooltips,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "13px",
                                          direction = "auto")) %>%
  addPolygons(fillColor = ~ region.cols(region), stroke = TRUE,
              group = "Region", fillOpacity = 0.5, color = "white",
              weight = 0.5, label = tooltips,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "13px",
                                          direction = "auto")) %>%
  addLegend(pal = epi.cols, values = ~ EPI.new, opacity = 0.9,
            title = "EPI", position = "topleft", group = "EPI") %>%
  addLegend(pal = region.cols, values = ~ region, opacity = 0.5,
            title = "", position = "topleft", group = "Region") %>%
  addLayersControl(overlayGroups = c("EPI", "Region"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("Region")
cp.map


# Export to html
saveWidget(cp.map, file = "index.html")


## Explore
### Colorblind color scheme
cb.cols <- c("#56B4E9", "#cc79a7", "#D55E00", "#0072b2", "#E69F00", "#999999",
             "#F0E442", "#009E73")


### Boxplots sorted by median
ggplot(epi.long[epi.long$Type == "Indicator", ],
       aes(x = reorder(country, EPI.new.value, FUN = median, na.rm = TRUE),
           y = EPI.new.value, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  scale_fill_manual(values = cb.cols) +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 5)) +
  ylab("EPIs") +
  xlab("")
ggsave("tst.pdf", width = 20, height = 6.27, bg = "transparent")


ggplot(epi.long[epi.long$Type == "EPI", ],
       aes(x = reorder(region, EPI.new.value, FUN = median, na.rm = TRUE),
           y = EPI.new.value, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  scale_fill_manual(values = cb.cols) +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("EPI 2020") +
  xlab("")
ggsave("tst_region.pdf", width = 20, height = 6.27, bg = "transparent")


################################################################################
################################################################################
