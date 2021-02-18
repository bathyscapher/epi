## ----setup, include = FALSE--------------------------------------------------------


## ---- message = FALSE, results = "hide", warning = FALSE---------------------------
library("reshape2")
library("ggplot2")
theme_set(theme_bw(base_size = 12) +
            theme(rect = element_rect(fill = "transparent")))
library("leaflet")
library("rgdal")
library("htmlwidgets")
library("lavaan")
library("lavaanPlot")


## ----------------------------------------------------------------------------------
rm(list = ls())
setwd("~/epi")


## ----------------------------------------------------------------------------------
source("importEPI.R")


## ---- collapse = TRUE--------------------------------------------------------------
epi.list <- importEPI()

list2env(epi.list, .GlobalEnv)
rm(epi.list)


## ----------------------------------------------------------------------------------
if(!file.exists("2020/TM_WORLD_BORDERS-0.3.shp")){
  print("File does not exist, downloading it now:")
  download.file("thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                destfile="world_shape_file.zip")
  system("unzip -u world_shape_file.zip;
         rm world_shape_file.zip Readme.txt")
  }


## ----------------------------------------------------------------------------------
world.spdf <- readOGR(dsn = "2020", layer = "TM_WORLD_BORDERS-0.3",
                      verbose = TRUE)
summary(world.spdf)


## ---- collapse = TRUE--------------------------------------------------------------
length(world.spdf$NAME)
length(epi$country)


## ----------------------------------------------------------------------------------
sort(setdiff(world.spdf$NAME, epi$country))


## ----------------------------------------------------------------------------------
sort(setdiff(epi$country, world.spdf$NAME))


## ----------------------------------------------------------------------------------
world.spdf <- merge(world.spdf,
                    epi[, (colnames(epi) %in% c("iso", "EPI.new", "region",
                                                "X2019"))],
                    by.x = "ISO3", by.y = "iso")
names(world.spdf)


## ----------------------------------------------------------------------------------
world.spdf@data$POP2005[which(world.spdf@data$POP2005 == 0)] <- NA
world.spdf@data$POP2005 <- as.integer(world.spdf@data$POP2005) / 1000000 %>%
  round(2)


## ----------------------------------------------------------------------------------
epi.cols <- colorNumeric(palette = "viridis", domain = world.spdf@data$EPI.new,
                         na.color = "gray", reverse = TRUE)


## ----------------------------------------------------------------------------------
region.cols <- colorFactor(palette = "plasma", domain = world.spdf@data$region,
                           na.color = "gray")


## ----------------------------------------------------------------------------------
tooltips <- paste("Country:", world.spdf@data$NAME, "<br/>",
                  "Region:", world.spdf@data$region, "<br/r>",
                  "GDP:", round(world.spdf@data$X2019 / 1000000000, digits = 1),
                  "B US$", "<br/r>",
                  # "Area:", world.spdf@data$AREA, "<br/>",
                  "Population:", round(world.spdf@data$POP2005, 2), "M",
                  "<br/>",
                  "EPI:", world.spdf@data$EPI.new) %>%
  lapply(htmltools::HTML)


## ----------------------------------------------------------------------------------
epi.map <- leaflet(world.spdf) %>%
  addTiles() %>%
  setView(lat = 20, lng = 0, zoom = 1.5) %>%
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
epi.map


## ---- cache = TRUE-----------------------------------------------------------------
saveWidget(epi.map, file = "index.html")


## ----------------------------------------------------------------------------------
cols.region <- c("#0D0887", "#5402A3", "#8B0AA5", "#B93289", "#DB5C68",
                 "#F48849", "#FEBC2A", "#F0F921")


## ---- fig.width = 20, fig.height = 7, collapse = TRUE------------------------------
ggplot(epi.long[epi.long$Type == "Indicator", ],
       aes(x = reorder(country, EPI.new.value, FUN = median, na.rm = TRUE),
           y = EPI.new.value, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.5) +
  scale_fill_manual(values = cols.region) +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 5)) +
  ylab("2020 performance indicators") +
  xlab("")
# ggsave("EPI_nations.pdf", width = 20, height = 8.27, bg = "transparent")


## ---- collapse = TRUE--------------------------------------------------------------
ggplot(epi.long[epi.long$Type == "EPI", ],
       aes(x = reorder(region, EPI.new.value, FUN = median, na.rm = TRUE),
           y = EPI.new.value, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.5) +
  scale_fill_manual(values = cols.region) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 100) +
  ylab("2020 EPI") +
  xlab("")
# ggsave("EPI_regions.pdf", width = 11.29, height = 6.27, bg = "transparent")


## ---- collapse = TRUE--------------------------------------------------------------
ggplot(epi.long[epi.long$Type == "EPI", ],
       aes(x = log(X2019 / 1000000), y = EPI.new.value, fill = region)) +
  geom_smooth(aes(group = 1), color = "gray", lty = 2, method = "lm",
              formula = y ~ x, se = FALSE, show.legend = FALSE) +
  geom_point(alpha = 0.5, pch = 21) +
  scale_fill_manual(values = cols.region) +
  theme(legend.position = "top", legend.title = element_blank()) +
  ylim(0, 100) +
  ylab("2020 EPI") +
  xlab(expression(paste(log[e], "(2019 GDP)", " [US$]")))
# ggsave("EPI_GDP.pdf", width = 11.29, height = 6.27, bg = "transparent")


## ----------------------------------------------------------------------------------
epi <- merge(epi,
             world.spdf[, (names(world.spdf) %in% c("ISO3", "AREA",
                                                    "POP2005", "LAT"))],
             by.x = "iso", by.y = "ISO3")


## ----------------------------------------------------------------------------------
epi$LAT <- epi$LAT + 90


## ----------------------------------------------------------------------------------
epi[, c(4, 51:54)] <- apply(epi[, c(4, 51:54)], 2, scale)
summary(epi[, c(4, 51:54)])


## ----------------------------------------------------------------------------------
epi.gdp <- 'EPI.new ~ X2019 + POP2005 + LAT
  X2019 ~ LAT + POP2005
  POP2005 ~ AREA'


## ----------------------------------------------------------------------------------
fit.epi.gdp <- sem(epi.gdp, data = epi, estimator = "MLM")
summary(fit.epi.gdp, rsq = TRUE)


## ---- collapse = TRUE--------------------------------------------------------------
modificationindices(fit.epi.gdp, minimum.value = 3)
fit.epi.gdp.up <- update(fit.epi.gdp, add = "X2019 ~ AREA")
summary(fit.epi.gdp.up, rsq = TRUE)


## ----------------------------------------------------------------------------------
lavaanPlot(model = fit.epi.gdp.up, coefs = TRUE, stars = c("regress"),
           node_options = list(shape = "box", color = "gray",
                               fontname = "Helvetica"),
           edge_options = list(color = "black"),
           labels = list(AREA = "Area", POP2005 = "Population", X2019 = "GDP",
                         LAT = "Latitude", EPInew = "EPI"))


## ----------------------------------------------------------------------------------
sessionInfo()

