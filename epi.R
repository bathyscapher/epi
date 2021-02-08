################################################################################
################################################################################
# Environmental Performance Index (EPI)


library("plyr")
library("reshape")
library("ggplot2")
theme_set(theme_bw(base_size = 14) +
            theme(rect = element_rect(fill = "transparent")))


## Clear workspace, set working directory
rm(list = ls())
setwd("~/epi/2020/")


## Load dataset
### Read all raw csv files containing NAs (rather than these encoded missing values¹ADD CITATION)
epi <- lapply(list.files(pattern = glob2rx("*_raw_na.csv")),
              read.table, sep = ",", header = TRUE)


### Extract file names and hand them over to the list
names(epi) <- substr(list.files(pattern = glob2rx("*_raw_na.csv")), 1, 3)
head(epi)


## With exception of the first three, shorten column names to year only
epi <- lapply(epi, function(x) {
  names(x)[-c(1:3)] <- substr(names(x)[-c(1:3)], 9, 12)
  x
  })


## Add column with the EPI ID to each data.frame
epi <- lapply(seq_along(epi),
       function(n, i) {
         epi[[i]]$EPI <- n[i] # populate column with EPI ID
         epi[[i]] # update data.frame
         },
       n = names(epi) # extract EPI IDs
       )


#### Add names once more (as they were lost in the last lapply call)
names(epi) <- substr(list.files(pattern = glob2rx("*_raw_na.csv")), 1, 3)
lapply(epi, names)


### Merge all data.frames in the list into a single data.frame
epi <- do.call("rbind.fill", epi)
sort(names(epi))


### Remove predicted columns in URP
epi <- epi[, !(colnames(epi) %in% c("2025", "2030", "2035", "2040", "2045")),
           drop = FALSE]
sort(names(epi))



## Explore data set
### Convert into long form
epi.m <- melt(epi, id.vars = c("EPI", "code", "iso", "country"),
              variable_name = "year")
head(epi.m)


str(epi.m)
cols2fac <- c("EPI", "code", "iso", "country")
epi.m[, cols2fac] <- lapply(epi.m[, cols2fac], as.factor)
epi.m$year <- as.integer(levels(epi.m$year))[epi.m$year]
str(epi.m)


### Remove data from before 1950
# epi.m <- epi.m[epi.m$year == 2020, ]


###
ggplot(data = epi.m) +
  geom_boxplot(aes(x = EPI, y = log1p(value))) +
  # geom_point(aes(x = as.factor(year), y = value, color = EPI)) +
  # facet_grid(EPI ~ .) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("EPI") +
  xlab("Year")


################################################################################
################################################################################
