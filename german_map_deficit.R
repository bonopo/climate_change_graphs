#german map of deficit

install.packages(c("rdwd", "geonames"))
sapply(c("dplyr", "magrittr", "utils", "tidyverse", "lubridate", "RCurl", "RColorBrewer", "rdwd", "geonames"), require, character.only = T )

setwd("t:/MAs/rmenke/r_gis_pro/r_projects/data/")
setwd("C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/")


geonames::GNsearch(name="Berlin")


indexFTP()

nearbyStations(lat, lon, radius=50, mindate = , res = "daily", var=variable)