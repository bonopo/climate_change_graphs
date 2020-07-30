# # german map of deficit#german map of deficit
# install.packages("data.table")
library(rdwd)
library(lubridate)
library(sf)
library(dplyr)

# data(geoIndex)
getwd()
load("./ws_data.RData")

plot(lk[, "id_2"], reset = FALSE) # from package sf sf::plot doesn't work!!
berryFunctions::colPoints("geoLaenge", "geoBreite", "Stationshoehe", data = m, add = T, legend = F)


head(lk)
# dataset that are needed:
# reference
ref_61 <- read.csv2("./DWDdata/Niederschlag_1961_90.csv")
ref_71 <- read.csv2("./DWDdata/Niederschlag_1971_00.csv")
ref_81 <- read.csv2("./DWDdata/Niederschlag_1981_10.csv")
# intersections
int

# metaInfo der DWD stations
m

# point feature of the dwd stations
msf

# landkreise polygon
lk

# year_of_interest = 2020
landkreis_rain <- function(lki, year_of_interest) # LandKreisIndex (row number in lk)
{
  rnr <- int[[lki]] # get which dwd station interacts with the Landkreis.
  # TODO: incorporate int as Dataset in package (justus)
  id <- m[int[[lki]], "Stations_id"]
  if (length(rnr) < 1) {
    warning("No rainfall data available for Landkreis ", lki, ": ", lk$name_2[lki], call. = FALSE)
    out <- data.frame(NA, NA)[FALSE, ]
    colnames(out) <- c("MESS_DATUM", as.character(lk$name_2[lki]))
    return(out)
  }
  # urls
  # TODO: m has to be incorporated in package as Dataset of all hourly precipitation recent DWD stations
  urls <- selectDWD(
    id = id,
    res = "monthly", var = "kl", per = "r", outvec = TRUE
  )

  clims <- dataDWD(urls, varnames = FALSE, dir = "../localdata", quiet = T, overwrite = TRUE, force = FALSE, progbar = 0)

  # setting monthly mean
  if (length(urls) == 1) {
    rainmean <- clims$MO_RR
    monthlyrain <- clims[c("MESS_DATUM", "MO_RR")]
  } else {
    # if more than 1 station
    # it makes a list of the stations with each two columns
    monthlyrain <- lapply(seq_along(clims), function(n) {
      out <- clims[[n]][c("MESS_DATUM", "MO_RR")]
      colnames(out)[2] <- names(clims)[n] # no duplicate names
      out
    })

    # merging the possible stations
    monthlyrain <- Reduce(function(...) merge(..., by = "MESS_DATUM", all = TRUE), monthlyrain)

    # making a mean
    rainmean <- rowMeans(monthlyrain[, -1], na.rm = TRUE) # check also with median, variation is huge!

    # making a mean of all stations (except the date column that is why the -1 is for)
  }
  out <- data.frame(monthlyrain[, 1], rainmean)
  colnames(out) <- c("MESS_DATUM", "mm_per_month")
  out <- out %>%
    dplyr::mutate(date = lubridate::ymd(out$MESS_DATUM)) %>%
    dplyr::filter(lubridate::year(date) == year_of_interest)

  # reading reference ####

  ref_station_61 =NA; ref_station_71 = NA; ref_station_81 = NA

  #selecting all reference stations with id (can be several staions per Landkreis!) and extracting their monthly mean sum. If severel stations the mean over all stations is extracted

  if (any(id %in% ref_61$Stations_id)) {
    ref_temp <- ref_61 %>%
      dplyr::filter(Stations_id %in% id) %>%
      dplyr::select(contains("."), "Jahr") %>%
      dplyr::mutate_if(is.character, as.numeric) %>%
      as.matrix() %>%
      apply(., 2, mean, na.rm = T) # if there are more than one station, mean of the stations.

    # extracting the correct sum of the reference period/station
    if (month(out$date[nrow(out)]) == 12) { #if one looks at the whole year
      ref_station_61 <- as.numeric(ref_temp$Jahr)
    } else {
      ref_station_61 <- as.numeric(ref_temp[3:(nrow(out) + 2)]) %>% sum() #taking the cumulative sum of all month of the reference stations
    }
  }
  if (any(id %in% ref_71$Stations_id)) {
    ref_temp <- ref_71 %>%
      dplyr::filter(Stations_id %in% id) %>%
      dplyr::select(contains("."), "Jahr") %>%
      dplyr::mutate_if(is.character, as.numeric) %>%
      as.matrix() %>%
      apply(., 2, mean, na.rm = T) # if there are more than one station, mean of the stations.

    # extracting the correct sum of the reference period/station
    if (month(out$date[nrow(out)]) == 12) {
      ref_station_71 <- as.numeric(ref_temp$Jahr)
    } else {
      ref_station_71 <- as.numeric(ref_temp[3:(nrow(out) + 2)]) %>% sum()
    }
  }
  if (any(id %in% ref_81$Stations_id)) {
    ref_temp <- ref_81 %>%
      dplyr::filter(Stations_id %in% id) %>%
      dplyr::select(contains("."), "Jahr") %>%
      dplyr::mutate_if(is.character, as.numeric) %>%
      as.matrix() %>%
      apply(., 2, mean, na.rm = T) # if there are more than one station, mean of the stations.

    # extracting the correct sum of the reference period/station
    if (month(out$date[nrow(out)]) == 12) {
      ref_station_81 <- as.numeric(ref_temp$Jahr)
    } else {
      ref_station_81 <- as.numeric(ref_temp[3:(nrow(out) + 2)]) %>% sum()
    }
  }

  # aggregating the data of interest
  int_station <- out %>%
    dplyr::summarise(int_sum = sum(mm_per_month)) # as the data are sums per month they needed to be added up

  result = cbind(id, int_station, ref_station_61, ref_station_71, ref_station_81)

  return(result)
}
#
# rainLK <- pbapply::pblapply(c(1:nrow(lk)), FUN= landkreis_rain)

rainLK <- pbapply::pbsapply(c(1:nrow(lk)), 2020, FUN = landkreis_rain)

landkreis_rain(lki = 3, 2020)

# rainLK is a large list with all stations with the recent rain data (since 12-2018). It is ordered by rows of the Landkreise i.e. the first element in the list is also the first row in the lk table.

# writing raindata to sf object to be able to print
# 1step get the cumulative rain amount per landkreis for the current year
anomaly <- round(int_station / ref_station * 100, 0)
head(rainLK)

plot(lk[, "rain_deficit"], reset = TRUE) # from package sf sf::plot doesn't work!!

head(lk)
# old####
#
#
# data("metaIndex")
# m <- metaIndex
# m <- m[m$res == "monthly" & m$var == "kl" & m$per == "recent" & m$hasfile, ]
#
# head(m)
# msf <- sf::st_as_sf(m, coords = c("geoLaenge", "geoBreite"), crs = 4326)
# # Read district shapefile
# # downloaded from https://public.opendatasoft.com/explore/dataset/landkreise-in-germany/export/
# # TODO: incorporate into package (Justus?)
# lk <- sf::st_read("../../landkreise-in-germany.shp")
#
# # intersections: list with msf rownumbers for each district getting all DWD Id that measure rain recently in  each Landkreis
# int <- sf::st_intersects(lk, msf)
#
# plot(lk[, "id_2"], reset = FALSE)
# berryFunctions::colPoints("geoLaenge", "geoBreite", "Stationshoehe", data = m, add = T, legend = F)
#
# nrow(lk)
#

save.image(file= "ws_data.RData")
