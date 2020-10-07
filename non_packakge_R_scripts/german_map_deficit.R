# # german map of deficit#german map of deficit
# install.packages("data.table")
library(rdwd)
library(lubridate)
library(sf)
library(dplyr)
library(tidyverse)
library(RCurl)
library(dichromat)


# data(geoIndex)
setwd('C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/shiny_app/cc_graph/climate_change_graphs__vJR')
getwd()
load("./data/ws_data.RData")

plot(lk[, "id_2"], reset = FALSE) # from package sf sf::plot doesn't work!!
berryFunctions::colPoints("geoLaenge", "geoBreite", "Stationshoehe", data = m, add = T, legend = F)


# refresch DWD DATA :



# dataset that are needed:
# reference
ref_61 <- read.csv2("./data/DWDdata/Niederschlag_1961_90.csv")
ref_71 <- read.csv2("./data/DWDdata/Niederschlag_1971_00.csv")
ref_81 <- read.csv2("./data/DWDdata/Niederschlag_1981_10.csv")

data_ref <- rbind(ref_61, ref_71, ref_81) %>%
  dplyr::select(-X) %>%
  dplyr::mutate_at(3:15, as.numeric)

ref_ids <- unique(data_ref$Stations_id)

#helpers function####

`%notin%` <- Negate(`%in%`)
all_na <- function(x) all(!is.na(x))
number.of.nas = function(x){
  res = sum(is.na(x))
  return(res)
}


# point feature of the dwd stations
msf

# intersections: list of with landkreis (rownumbers) has which DWD id. Mulitple stations per landkreis (row) possible. NAs also possible
int

# metaInfo der DWD stations
m

# landkreise polygon
lk
lki <- 260
# year_of_interest = 2018

# necessary data:
# list of unique ref ids
# refdata
# int
# m

year_of_interest <- 2018

lki <- 3
newdownload <- T
landkreis_rain <- function(lki, year_of_interest, newdownload = FALSE) # LandKreisIndex (row number in lk)
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

     urls_rec <- selectDWD(
        id = id,
        res = "daily", var = "kl", per = "r", outvec = TRUE
      )
     
     urls_hist <- selectDWD(
        id = id,
        res = "daily", var = "kl", per = "h", outvec = TRUE, current=T
      )
    

  # if only one station in historic and recent data set 
    if (length(urls_rec) == 1 && length(urls_hist) == 1) {
      
     

      clims_new =  dataDWD(urls_rec, varnames = T, dir = "./data/downloaded_dwd_data", quiet = T, overwrite = TRUE, force = newdownload, progbar = 0, read = T)

    if(year_of_interest !=  as.integer(substr(date(), 21,24))){
       

       #to make sure that there is not days with overlapping measures. The days from the historic dwd data is thrown out if they are still in the recent dwd data set
       
       oldest_day_with_data = ymd(clims_new$MESS_DATUM[nrow(clims_new)])
       
       clims_old =  dataDWD(urls_hist, varnames = T, dir = "./data/downloaded_dwd_data", quiet = T, overwrite = TRUE, force = newdownload, progbar = 0, read = T) %>% filter(ymd(MESS_DATUM) < oldest_day_with_data)

clims = rbind(clims_new, clims_old)

    }else{
      clims = clims_new
    }
      
      #aggregating the data and building monthly sums
clims_summary = clims %>% 
  filter(year(MESS_DATUM) == year_of_interest) %>% 
  dplyr::group_by(month = month(MESS_DATUM)) %>% 
  dplyr::select(month, RSK = contains('Niederschlagshoehe')) %>% 
  dplyr::summarise(monthly_mm = sum(RSK, na.rm=T), 
                   number_NAs = (sum(is.na(RSK))))

      last_month_with_data <- clims_summary$month[nrow(clims_summary)] # get which momth is the last month with data

#yearly sums
      monthlyrain <- clims_summary %>%
        dplyr::summarise(sum(monthly_mm, na.rm = F))
      
      colnames(monthlyrain) <- id
    } else {
      # if more than 1 station
      
       clims_new =  dataDWD(urls_rec, varnames = T, dir = "./data/downloaded_dwd_data", quiet = T, overwrite = TRUE, force = newdownload, progbar = 0, read = T)

   
      
      
      monthlyrain_temp2 <- lapply(seq_along(clims_new), function(n) {
        out <- clims_new[[n]][c("MESS_DATUM", "RSK.Niederschlagshoehe")]
        colnames(out)[2] <- names(clims_new)[n] # no duplicate names
        out
      })


      # merging the possible current (cur) stations and selecting the year of interest
      monthlyrain_temp <- Reduce(function(...) merge(..., by = "MESS_DATUM", all = TRUE), monthlyrain_temp2) %>%
        dplyr::mutate(date = lubridate::ymd(MESS_DATUM)) %>%
        dplyr::select(-MESS_DATUM)
      
       if(year_of_interest !=  as.integer(substr(date(), 21,24))){
       

       #to make sure that there is not days with overlapping measures. The days from the historic dwd data is thrown out if they are still in the recent dwd data set
       
       oldest_day_with_data = ymd(monthlyrain_temp$date[nrow(monthlyrain_temp)])
       
       clims_old =  dataDWD(urls_hist, varnames = T, dir = "./data/downloaded_dwd_data", quiet = T, overwrite = TRUE, force = newdownload, progbar = 0, read = T) 

 # it makes a list of the stations with each two columns
      monthlyrain_temp2_hist <- lapply(seq_along(clims_old), function(n) {
        out <- clims_old[[n]][c("MESS_DATUM", "RSK.Niederschlagshoehe")]
        colnames(out)[2] <- names(clims_old)[n] # no duplicate names
        out
      })



      # merging the possible current (cur) stations and selecting the year of interest
      monthlyrain_temp_hist <- Reduce(function(...) merge(..., by = "MESS_DATUM", all = TRUE), monthlyrain_temp2_hist) %>%
        dplyr::mutate(date = lubridate::ymd(MESS_DATUM)) %>%
        dplyr::select(-MESS_DATUM) %>% 
        filter(date < oldest_day_with_data)
       
      colnames(monthlyrain_temp_hist) <- c(paste0('dwd_',id), 'date')
      colnames(monthlyrain_temp) = c(paste0('dwd_',id), 'date')
  
clims = rbind(monthlyrain_temp, monthlyrain_temp_hist) %>% filter(year(date) == year_of_interest)

clims_summary = clims %>% 
  dplyr::group_by(month = month(date)) %>% 
  dplyr::select(starts_with('dwd_')) %>% 
  dplyr::summarise_all(list(sum, number.of.nas))


# clims_summary = clims %>% 
#   dplyr::group_by(month = month(date)) %>% 
#   summarise(across(starts_with('dwd_')), list(mean = mean))#, n_miss = ~ sum(is.na(.x))))

    }
      
      
      monthlyrain <- monthlyrain_temp %>%
        dplyr::select(tidyr::starts_with("monthly")) %>%
        dplyr::summarise_all(sum, na.rm = F)
    }
    

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
      colnames(data_ref_sum) <- data_ref_subset[1,]

      # if reference and year of interest do not have the same station id, only the data is used where BOTH is available: current AND reference, all other are thrown out. This doesn't make up for differences caused between reference and current by moving of the station etc...
      common_cols <- intersect(colnames(monthlyrain), colnames(data_ref_sum))

      # deficit/surplus calculation
      deficit_temp <- rbind(
        monthlyrain[common_cols],
        data_ref_sum[common_cols]
      )

      #removing NAs
      deficit_temp = deficit_temp %>% select_if(all_na) %>% mutate_all(as.numeric)

      deficit <- deficit_temp[1,]/deficit_temp[2,]
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
      def,
      days_NA= sum(clims_summary$number_NAs)
    )
  }
  print(lki)
  return(result)
}




#
# styler::style_file(path = "C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/shiny_app/cc_graph/climate_change_graphs__vJR/CCgraphs/R/german_map_deficit.R")


rainLK2 <- pbapply::pblapply(c(1:nrow(lk)), 2018, FUN = landkreis_rain)

landkreis_rain(lki = 24, 2020)

# rainLK is a large list with all stations with the recent rain data (since 12-2018). It is ordered by rows of the Landkreise i.e. the first element in the list is also the first row in the lk table.

df_rainLK <- do.call("rbind", rainLK)

# removing LKI with only partly data (i.e. stopped measuring after month x)

remove <- which(df_rainLK$last_month != raster::modal(df_rainLK$last_month))
df_rainLK$def[remove] <- NA


lk$rain_deficit <- df_rainLK$def



# (max(lk$rain_deficit, na.rm = T)-min(lk$rain_deficit, na.rm=T))
which.min(lk$rain_deficit)



if ((100 - min(lk$rain_deficit, na.rm = T)) > (max(lk$rain_deficit, na.rm = T) - 100)) {
  limits_scale <- c(min(lk$rain_deficit, na.rm = T), 100 + (100 - min(lk$rain_deficit, na.rm = T)))
} else {
  limits_scale <- c(100 - (max(lk$rain_deficit, na.rm = T) - 100), max(lk$rain_deficit, na.rm = T))
}



myPalette <- colorRampPalette(brewer.pal(11, "RdBu"))
sc <- scale_fill_gradientn("Niederschlagsdefizit [%]",
  colours = myPalette(100),
  limits = limits_scale
)

# plot(lk[, "rain_deficit"], reset = TRUE) # from package sf sf::plot doesn't work!!
ggplot() +
  geom_sf(data = lk, aes(fill = rain_deficit)) +
  sc +
  theme_bw()

ggsave(filename = "deficit.png", units = "cm", device = "png", width = 20, height = 30)


scale_fill_gradient2(
  high = "#2400D9",
  mid = "white",
  low = "#e34a33",
  midpoint = 100,
  na.value = "grey50"
) +
  geom_blank()


# save.image(file = "ws_data.RData")
