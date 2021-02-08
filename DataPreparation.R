################################################################################
################################################################################
# Environmental Performance Index (EPI)


# library("plyr")
library("reshape")
library("ggplot2")
theme_set(theme_bw(base_size = 14) +
            theme(rect = element_rect(fill = "transparent")))


## Clear workspace, set working directory
rm(list = ls())
setwd("~/epi/2nd2020//")


## Load dataset
### Read all raw csv files containing NAs (rather than these encoded missing values¹ADD CITATION)
epi <- lapply(list.files(pattern = glob2rx("*EPI*.csv")),
              read.table, sep = ",", header = TRUE, quote = "")
epi

# tst <- read.table("2020-epi-Guide.csv", sep = ",", header = TRUE, quote = "")

lapply(epi, head)


### Name the list
names(epi) <- gsub(".csv", "",
                   gsub("2020-epi-\\d_EPI_", "",
                        list.files(pattern = glob2rx("*EPI*.csv"))))
names(epi)


### Release data.frames from list into global environment
list2env(epi, .GlobalEnv)


## Explore
### Select columns by matching "new" (for the new EPI values) and a negative
### lookbehind to exclude columns containing the new ranks. Also keep code, iso
### and country
tst <- grep("(?<!rnk).new|^[^.]*$", names(Results), value = TRUE, perl = TRUE)
tst


Results <- Results[, (colnames(Results) %in% tst), drop = FALSE]
names(Results)


### Reshape
### Convert into long form
Results.m <- melt(Results, id.vars = c("code", "iso", "country"),
              variable_name = "EPI")
str(Results.m)


Results.m$EPI <- as.factor(substr(Results.m$EPI, 1, 3))
levels(Results.m$EPI)


###
ggplot(data = Results.m) +
  geom_boxplot(aes(x = country, y = value)) +
  # geom_point(aes(x = as.factor(year), y = value, color = EPI)) +
  # facet_grid(EPI ~ .) +
  # scale_fill_manual(values = gradCol) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("EPI") +
  xlab("")
# ggsave("../SMP_2019_Prey.pdf", width = 11.69, height = 6.27, bg = "transparent")




################################################################################
################################################################################
