################################################################################
################################################################################
# Data preparation: Environmental Performance Index (EPI)


library("reshape2")
library("ggplot2")
  theme_set(theme_bw(base_size = 12) +
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
tla <- indicatortla # description of EPIs = three-letter-abbreviation
countries <- countryattributes


rm(results, indicatortla, countryattributes)


### Add regions
epi <- merge(epi, countries[, 3:4], by = "country")
str(epi)


## Explore
### Subset by selecting columns which match "new" (for the 2020 EPI values) and
### a negative lookbehind to exclude columns containing the new ranks. Also keep
### the columns "code", "iso" and "country" (i.e. all that contain only letters)
epi <- epi[, (colnames(epi) %in% grep("(?<!rnk).new|^[^.]*$", names(epi),
                                      value = TRUE, perl = TRUE)), drop = FALSE]
names(epi)


### Reshape
### Convert into long, "molten" form
epi.long <- melt(epi, id.vars = c("code", "iso", "country", "region"),
                 value.name = "EPI.new.value", variable.name = "EPI.new")
str(epi.long)


### Convert to factor
epi.long$country <- as.factor(epi.long$country)
epi.long$region <- as.factor(epi.long$region)


### Remove ".new" from EPIs
epi.long$EPI.new <- as.factor(substr(epi.long$EPI.new, 1, 3))
str(epi.long)


### Add EPI levels
epi.long <- merge(epi.long, tla[, 1:3], by.x = "EPI.new", by.y = "Abbreviation")


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
        axis.text=element_text(size = 5)) +
  ylab("EPIs") +
  xlab("")
ggsave("../tst.pdf", width = 20, height = 6.27, bg = "transparent")


ggplot(epi.long[epi.long$Type == "EPI", ],
       aes(x = reorder(region, EPI.new.value, FUN = median, na.rm = TRUE),
           y = EPI.new.value, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  scale_fill_manual(values = cb.cols) +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("EPI 2020") +
  xlab("")


################################################################################
################################################################################
