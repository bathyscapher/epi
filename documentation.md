# NASA dataset "[2020 Environmental Performance Index (EPI)](https://data.nasa.gov/dataset/2018-Environmental-Performance-Index-EPI-/ch9t-bz36)"
## Data set choice
1. Explored [homepage](https://nasa.github.io/data-nasa-gov-frontpage/data_visualizations.html) structure and content via browsing and [watching this introductory video](https://docs.google.com/presentation/d/10xoC0N1jWOxEFwGml-Ivnn6GYxJ2_tbAZjdl7aRzaI8/edit#slide=id.g61f57fe376_0_1725)
1. Chose the third among candidate datasets as it has a spatial as well as temporal component:
    * [Prediction Of Worldwide Energy Resources](https://data.nasa.gov/Earth-Science/Prediction-Of-Worldwide-Energy-Resources-POWER-/wn3p-qsan)
    * [THIR/Nimbus-6 Level 1 Meteorological Radiation Data](https://data.nasa.gov/Earth-Science/THIR-Nimbus-6-Level-1-Meteorological-Radiation-Dat/6bre-7tjd)
    * [Environmental Performance Index](https://data.nasa.gov/dataset/2018-Environmental-Performance-Index-EPI-/ch9t-bz36)
    

## Download data
1. Register to [EOSDIS Earthdata](https://earthdata.nasa.gov/) before data access or--as discovered later--simpler, [download csv files here](https://epi.yale.edu/downloads)
1. To visualize the EPIs as a choropleth map, download [world's country borders](thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip).


## Read, explore and visualize data
1. Read all the csv files and merge the components of interest into one data.frame (`epi`)
1. Plot some overviews over the EPIs with `ggplot2`
1. Collate country naming
1. Create choropleth map with [`R leaflet`](https://cran.r-project.org/web/packages/leaflet/index.html)


## To do
1. Unify all graphs in a [blogdown website](https://cran.r-project.org/web/packages/blogdown/index.html)
1. Add a [structural equation model](https://en.wikipedia.org/wiki/Structural_equation_modeling) of the EPIs and explanatory variables (EPIs, region etc)
1. Create a gif of EPIs over time



