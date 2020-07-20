# german map of deficit#german map of deficit
install.packages("leaflet")
library(rdwd, RCurl)
library(rdwd)
data(geoIndex)
library(leaflet)


data("metaIndex")
m <- metaIndex
m <- m[m$res == "monthly" & m$var == "kl" & m$per == "recent" & m$hasfile, ]

head(m)
msf <- sf::st_as_sf(m, coords = c("geoLaenge", "geoBreite"), crs = 4326)
# Read district shapefile
# downloaded from https://public.opendatasoft.com/explore/dataset/landkreise-in-germany/export/
# TODO: incorporate into package (Justus?)
lk <- sf::st_read("../../landkreise-in-germany.shp")

# intersections: list with msf rownumbers for each district getting all DWD Id that measure rain recently in  each Landkreis
int <- sf::st_intersects(lk, msf)

plot(lk[, "id_2"], reset = FALSE)
berryFunctions::colPoints("geoLaenge", "geoBreite", "Stationshoehe", data = m, add = T, legend = F)

nrow(lk)


landkreis_rain <- function(lki) # LandKreisIndex (row number in lk)
{
  rnr <- int[[lki]] # get which dwd station interacts with the Landkreis.
  #TODO: incorporate int as Dataset in package (justus)

  if (length(rnr) < 1) {
    warning("No rainfall data available for Landkreis ", lki, ": ", lk$name_2[lki], call. = FALSE)
    out <- data.frame(NA, NA)[FALSE, ]
    colnames(out) <- c("MESS_DATUM", as.character(lk$name_2[lki]))
    return(out)
  }
#urls
  # TODO: m has to be incorporated in package as Dataset of all hourly precipitation recent DWD stations
  urls <- selectDWD(id=m[rnr, "Stations_id"],
                  res="monthly", var="kl", per="r", outvec=TRUE)

    clims <- dataDWD(urls, varnames = FALSE, dir = "../localdata", quiet = T)
    #setting monthly mean
  if (length(urls) == 1) {
    rainmean <- clims$MO_RR
    monthlyrain <- clims[c("MESS_DATUM", "MO_RR")]
  } else {
    #if more than 1 station
    #it makes a list of the stations with each two columns

    monthlyrain <- lapply(seq_along(clims), function(n) {
      out <- clims[[n]][c("MESS_DATUM", "MO_RR")]
      colnames(out)[2] <- names(clims)[n] # no duplicate names
      out
    })
    monthlyrain <- Reduce(function(...) merge(..., by = "MESS_DATUM", all = TRUE), monthlyrain)
    rainmean <- rowMeans(monthlyrain[, -1], na.rm = TRUE) # check also with median, variation is huge!
    #making a mean of all stations (except the date column that is why the -1 is for)
  }
  out <- data.frame(monthlyrain[, 1], rainmean)
  colnames(out) <- c("MESS_DATUM", as.character(lk$name_2[lki]))
  return(out)
}

rainLK <- pbapply::pblapply(c(1:nrow(lk)), FUN= landkreis_rain)
#bla