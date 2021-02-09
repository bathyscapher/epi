################################################################################
################################################################################
# Data preparation: Environmental Performance Index (EPI)


library("reshape")
library("ggplot2")
  theme_set(theme_bw(base_size = 14) +
              theme(rect = element_rect(fill = "transparent")))


## Clear workspace, set working directory
rm(list = ls())
setwd("~/epi/2020/")


## Load dataset
### Read all csv files
epi <- lapply(list.files(pattern = ".csv"),
              read.table, sep = ",", header = TRUE, quote = "")
epi


### Show data structure
lapply(epi, head)
lapply(epi, names)


### Name the list by retrieving the file names and extract the informative part
names(epi) <- gsub("\\d{8}.csv", "",
                   gsub("epi2020", "", list.files(pattern = ".csv")))
names(epi)


### Release data.frames from list into global environment
list2env(epi, .GlobalEnv)
rm(epi)


### Rename to more "speaking" names
epi <- results # EPIs by country
tla <- indicatortla # description of EPIs
countries <- countryattributes


rm(results, indicatortla, countryattributes)


## Explore
### Subset by selecting columns which match "new" (for the 2020 EPI values) and
### a negative lookbehind to exclude columns containing the new ranks. Also keep
### the columns "code", "iso" and "country" (i.e. all that contain only letters)
epi <- epi[, (colnames(epi) %in% grep("(?<!rnk).new|^[^.]*$", names(epi),
                                      value = TRUE, perl = TRUE)), drop = FALSE]
names(epi)


### Reshape
### Convert into long, "molten" form
epi.m <- melt(epi, id.vars = c("code", "iso", "country"))
str(epi.m)


### Convert to factor
epi.m$country <- as.factor(epi.m$country)


### Remove ".new" from EPIs
epi.m$variable <- as.factor(substr(epi.m$variable, 1, 3))
levels(epi.m$variable)




###
## Sort by median
# fac <- with(epi.m, reorder(EPI, value, median, order = TRUE))
# epi.m$EPI <- factor(epi.m$EPI, levels = levels(fac))

ggplot(iris, aes(x = Species, y = Sepal.Width)) + geom_boxplot()
ggplot(iris, aes(x = reorder(Species, Sepal.Width, FUN = median), y = Sepal.Width)) + geom_boxplot()
str(iris)


str(epi.m)

ggplot(epi.m, aes(x = reorder(country, value, FUN = median), y = value)) +
  geom_boxplot() +
  # geom_point(aes(x = as.factor(year), y = value, color = EPI)) +
  # facet_grid(EPI ~ .) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("EPIs") +
  xlab("")
ggsave("tst.pdf", width = 20, height = 6.27, bg = "transparent")



################################################################################
################################################################################
