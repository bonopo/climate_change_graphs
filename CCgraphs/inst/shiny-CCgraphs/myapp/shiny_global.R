#preamble####
##
check_packages = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = T)
    sapply(pkg, require, character.only=T)
}
## I am not sure what is the aim of check_package, but maybe this function does the job ###
### install the pacman package if not yet installed
# if (!require("pacman")) install.packages("pacman")
### p_load checks if a package is installed and if not it installs the needed package.
### actually this is not needed within a package as you define the dependencies in the package documentation
### thus when once instlling the new CCgraphs package all needed packages will be installed..
# pacman::p_load("data.table","raster", "sp","maptools", "spatstat")

#' @title Remove unused row (?)
#' @description FUNCThION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param rows PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname remove_row
#' @export
#'

  remove_row =function(data, rows){
    result = data[-c(rows),]
    return(result)
  }

# packages
check_packages(c("shiny", "shinydashboard", "shinyjs", "shinyBS", "ggplot2", "scales", "lubridate", "geosphere", "rdwd", "tidyverse", "RCurl", "RColorBrewer", "shinyalert", "magrittr"))


