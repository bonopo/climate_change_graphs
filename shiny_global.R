# mini functons####

check_packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = T)
  }
  sapply(pkg, require, character.only = T)
}

remove_row <- function(data, rows) {
  result <- data[-c(rows), ]
  return(result)
}


# packages
check_packages(c("shiny", "shinydashboard", "shinyjs", "shinyBS", "ggplot2", "scales", "lubridate", "geosphere", "rdwd", "tidyverse", "RCurl", "RColorBrewer", "shinyalert", "magrittr"))

# wd

setwd("C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/")
