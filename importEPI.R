################################################################################
################################################################################
# Data preparation: Environmental Performance Index (EPI)

require("reshape2")


## Import EPI data
importEPI <- function(){
  # If necessary, download files to directory "2020"
  if(!file.exists("2020/epi2020results20200604.csv") |
     !file.exists("2020/epi2020indicatortla20200604.csv") |
     !file.exists("2020/epi2020countryattributes20200604.csv") |
     !file.exists("2020/epi2020variableattributes20200604.csv")) {
    "Files absent, downloading them now:"
    system("wget -P 2020 https://epi.yale.edu/downloads/epi2020results20200604.csv https://epi.yale.edu/downloads/epi2020indicatortla20200604.csv https://epi.yale.edu/downloads/epi2020countryattributes20200604.csv https://epi.yale.edu/downloads/epi2020variableattributes20200604.csv")
    }


  # Read all csv files
  epi <- lapply(list.files(pattern = glob2rx("epi2020*.csv"), recursive = TRUE),
                read.table, sep = ",", header = TRUE, quote = "")
  # lapply(epi, head)
  # lapply(epi, str)
  # class(epi)
  # epi


  # Name the list by retrieving the file names and extract the informative part
  names(epi) <- gsub("\\d{8}.csv", "",
                     gsub("2020/epi2020", "",
                          list.files(pattern = glob2rx("epi2020*.csv"),
                                     recursive = TRUE)))
  # names(epi)


  # Release data.frames from list into global environment
  list2env(epi, .GlobalEnv)
  rm(epi, envir = .GlobalEnv)


  # Rename to more "speaking" name
  epi <- results


  # Add regions
  epi <- merge(epi, countryattributes[, 3:4], by = "country")
  # str(epi)


  ## Add GDP from worldbank
  if(!file.exists("2020/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_2017804.csv")) {
    "File absent, downloading it now:"
    system("wget -O 2020/wb_gdp.zip 'http://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=csv' && unzip -d 2020 2020/wb_gdp.zip && rm 2020/wb_gdp.zip")
  }

  gdp <- read.table("2020/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_2017804.csv",
                    header = TRUE, sep = ",", skip = 4)
  gdp <- gdp[, (colnames(gdp) %in% c("Country.Name", "Country.Code", "X2019"))]

  ## Match country iso codes and add GDP to EPI data.frame
  setdiff(epi$iso, gdp$Country.Code) # Taiwan is missing in the world bank data

  epi <- merge(epi, gdp, by.x = "iso", by.y = "Country.Code")
  names(epi)


  # Subset by selecting columns which match "new" (for the 2020 EPI values) and
  # a negative lookbehind to exclude columns containing the new ranks. Also keep
  # the columns "code", "iso" and "country" (i.e. all that contain only letters)
  epi <- epi[, (colnames(epi) %in% grep("(?<!rnk).new|^[^.]*$", names(epi),
                                        value = TRUE, perl = TRUE)),
             drop = FALSE]
  # names(epi)


  # Convert into long form
  epi.long <- melt(epi, value.name = "EPI.new.value", variable.name = "EPI.new",
                   id.vars = c("code", "iso", "country", "region", "X2019"))
  # str(epi.long)
  # summary(epi.long$EPI.new.value)


  # Convert to factor
  epi.long$country <- as.factor(epi.long$country)
  epi.long$region <- as.factor(epi.long$region)


  ## Remove ".new" from EPIs
  epi.long$EPI.new <- as.factor(substr(epi.long$EPI.new, 1, 3))
  str(epi.long)


  ## Add EPI levels
  epi.long <- merge(epi.long, indicatortla[, 1:3],
                    by.x = "EPI.new", by.y = "Abbreviation")


  ## Clean workspace
  rm(results, countryattributes, variableattributes, indicatortla,
     envir = .GlobalEnv)


  epi.list <- list(epi = epi, epi.long = epi.long)
  return(epi.list)
}


################################################################################
################################################################################
