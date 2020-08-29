# # german map of deficit#german map of deficit
# install.packages("data.table")
library(rdwd)
library(lubridate)
library(sf)
library(dplyr)
library(tidyverse)
library(dichromat)


# data(geoIndex)
getwd()
load("./ws_data.RData")

plot(lk[, "id_2"], reset = FALSE) # from package sf sf::plot doesn't work!!
berryFunctions::colPoints("geoLaenge", "geoBreite", "Stationshoehe", data = m, add = T, legend = F)


#refresch DWD DATA :



# dataset that are needed:
# reference
ref_61 <- read.csv2("./DWDdata/Niederschlag_1961_90.csv")
ref_71 <- read.csv2("./DWDdata/Niederschlag_1971_00.csv")
ref_81 <- read.csv2("./DWDdata/Niederschlag_1981_10.csv")

data_ref <- rbind(ref_61, ref_71, ref_81) %>%
  dplyr::select(-X) %>%
  dplyr::mutate_at(3:15, as.numeric)

ref_ids <- unique(data_ref$Stations_id)
`%notin%` <- Negate(`%in%`)
#
# tidyr::pivot_longer(., cols= c("Jan.", "Feb.","März","Apr." ,"Mai","Jun." ,"Jul.","Aug.","Sept.","Okt.","Nov.","Dez.", "Jahr"), names_to = 't', values_to = "mm") %>%
#   dplyr::mutate(mm = as.numeric(mm))



# point feature of the dwd stations
msf

# intersections: list of with landkreis (rownumbers) has which DWD id. Mulitple stations per landkreis (row) possible. NAs also possible
int

# metaInfo der DWD stations
m

# landkreise polygon
lk
lki <- 260
# year_of_interest = 2020

# necessary data:
# list of unique ref ids
# refdata
# int
# m

landkreis_rain <- function(lki, year_of_interest) # LandKreisIndex (row number in lk)
{

  # checking if there is dwd stations in the landkreis and if there is reference data for ANY of the selected dwd stations. This is just a quick check a more detail checking is done in a later step. This is only to speed up the function, as it skips the whole function if there is ANY FALSE in the if statement
  id <- m[int[[lki]], "Stations_id"]
  def <- NA
  if (length(id) < 1) {
    warning("No recent rainfall data available for Landkreis ", lki, ": ", lk$name_2[lki], call. = FALSE)
    out <- data.frame(paste0("x-", year_of_interest), NA, lki, NA)
    colnames(out) <- c("last_month", "number_of_stations", "lki", "def")
    return(out)
  } else if (all(id %notin% ref_ids)) {
    warning("No historic rainfall data available for Landkreis ", lki, ": ", lk$name_2[lki], call. = FALSE)
    out <- data.frame(paste0("x-", year_of_interest), NA, lki, NA)
    colnames(out) <- c("last_month", "number_of_stations", "lki", "def")
    return(out)
  } else {
    # urls
    # TODO: m has to be incorporated in package as Dataset of all hourly precipitation recent DWD stations
    urls <- selectDWD(
      id = id,
      res = "monthly", var = "kl", per = "r", outvec = TRUE
    )

    clims <- dataDWD(urls, varnames = FALSE, dir = "../localdata", quiet = T, overwrite = TRUE, force = T, progbar = 0)

    # setting monthly mean
    if (length(urls) == 1) {
      rainmean <- clims$MO_RR
      monthlyrain_temp <- clims[c("MESS_DATUM", "MO_RR")] %>%
        dplyr::mutate(date = lubridate::ymd(MESS_DATUM)) %>%
        dplyr::filter(year(date) == year_of_interest)


      last_month_with_data <- nrow(monthlyrain_temp) # get which momth is the last month with data


      monthlyrain <- monthlyrain_temp %>%
        dplyr::summarise(sum(MO_RR, na.rm = F))
    } else {
      # if more than 1 station
      # it makes a list of the stations with each two columns
      monthlyrain_temp2 <- lapply(seq_along(clims), function(n) {
        out <- clims[[n]][c("MESS_DATUM", "MO_RR")]
        colnames(out)[2] <- names(clims)[n] # no duplicate names
        out
      })



      # merging the possible current (cur) stations and selecting the year of interest
      monthlyrain_temp <- Reduce(function(...) merge(..., by = "MESS_DATUM", all = TRUE), monthlyrain_temp2) %>%
        dplyr::mutate(date = lubridate::ymd(MESS_DATUM)) %>%
        dplyr::filter(year(date) == year_of_interest)

      last_month_with_data <- nrow(monthlyrain_temp) # get which momth is the last month with data

      monthlyrain <- monthlyrain_temp %>%
        dplyr::select(tidyr::starts_with("monthly")) %>%
        dplyr::summarise_all(sum, na.rm = F)
    }
    colnames(monthlyrain) <- id

    if (last_month_with_data > 0) {

      # adding reference (ref) stations
      # depending if the user wants the whole year or just the months  'sum-till-now' there has to be two different calculation methods
      if (last_month_with_data == 12) { # if one looks at the whole year
        data_ref_subset <- data_ref %>%
          dplyr::filter(Stations_id %in% id) %>% # getting the correct reference strations
          dplyr::group_by(Stations_id) %>%
          dplyr::select(Stations_id, Jahr) %>% # taking the cumulative sum of the whole year of the reference stations
          dplyr::summarise_all(mean) %>% # getting mean over all reference period per reference station
          t() %>%
          as.data.frame()

        data_ref_sum <- data_ref_subset[2, ] %>%
          matrix(byrow = T, nrow = 1) %>%
          as.data.frame()
      } else {
        data_ref_subset <- data_ref %>%
          dplyr::filter(Stations_id %in% id) %>% # getting the correct reference strations
          dplyr::group_by(Stations_id) %>%
          dplyr::select(Stations_id, 3:(last_month_with_data + 2)) %>% # taking the cumulative sum of all month of the reference stations
          dplyr::summarise_all(mean) # getting mean over all reference period per reference station


        data_ref_sum <- apply(data_ref_subset[, c(2:(last_month_with_data + 1))], 1, sum) %>% # getting a cumulative sum for every station
          matrix(byrow = T, nrow = 1) %>%
          as.data.frame()
      }


      # giving the ref station the correct id name
      colnames(data_ref_sum) <- data_ref_subset$Stations_id

      # if reference and year of interest do not have the same station id, only the data is used where BOTH is available: current AND reference, all other are thrown out. This doesn't make up for differences caused between reference and current by moving of the station etc...
      common_cols <- intersect(colnames(monthlyrain), colnames(data_ref_sum))

      # deficit/surplus calculation
      deficit_temp <- rbind(
        monthlyrain[common_cols],
        data_ref_sum[common_cols]
      )

      deficit <- deficit_temp[1, ] / deficit_temp[2, ]
      number_of_stations <- length(which(!is.na(deficit)))

      if (length(deficit) == 1) {
        def <- round(deficit * 100, 0)
      } else {
        def <- round(rowMeans(deficit[1, ], na.rm = T) * 100, 0)
      }

      # removing 0
      if (def == 0 && !is.na(def)) {
        def <- NA
      }
    }
    result <- cbind.data.frame(
      "last_month" = paste0(last_month_with_data, "-", year_of_interest),
      number_of_stations,
      lki,
      def
    )
  }

   return(result)
}



#
# styler::style_file(path = "C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/shiny_app/cc_graph/climate_change_graphs__vJR/CCgraphs/R/german_map_deficit.R")


rainLK <- pbapply::pblapply(c(1:nrow(lk)), 2020, FUN = landkreis_rain)

landkreis_rain(lki = 1, 2020)

# rainLK is a large list with all stations with the recent rain data (since 12-2018). It is ordered by rows of the Landkreise i.e. the first element in the list is also the first row in the lk table.

df_rainLK <- do.call("rbind", rainLK)

# removing LKI with only partly data (i.e. stopped measuring after month x)

remove <- which(df_rainLK$last_month != raster::modal(df_rainLK$last_month))
df_rainLK$def[remove] <- NA


lk[1,]

lk$rain_deficit <- df_rainLK$def


colour_palete <- dichromat::colorschemes$DarkRedtoBlue.18
# (max(lk$rain_deficit, na.rm = T)-min(lk$rain_deficit, na.rm=T))
which.min(lk$rain_deficit)

if ((100 - min(lk$rain_deficit, na.rm = T)) > (max(lk$rain_deficit, na.rm = T) - 100)) {
  limits_scale <- c(min(lk$rain_deficit, na.rm = T), 100 + (100 - min(lk$rain_deficit, na.rm = T)))
} else {
  limits_scale <- c(100 - (max(lk$rain_deficit, na.rm = T) - 100), max(lk$rain_deficit, na.rm = T))
}



myPalette <- colorRampPalette(brewer.pal(11, "RdBu"))
sc <- scale_fill_gradientn('Niederschlagsdefizit [%]',
  colours = myPalette(100),
  limits = limits_scale
)

# plot(lk[, "rain_deficit"], reset = TRUE) # from package sf sf::plot doesn't work!!
ggplot() +
  geom_sf(data = lk, aes(fill = rain_deficit)) +
  sc +
 theme_bw()

ggsave(filename = 'deficit.png', units = 'cm', device = 'png', width = 20, height = 30)


  scale_fill_gradient2(
    high = "#2400D9",
    mid = "white",
    low = "#e34a33",
    midpoint = 100,
    na.value = "grey50"
  ) +
  geom_blank()


save.image(file = "ws_data.RData")
