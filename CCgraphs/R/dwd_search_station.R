#' @title search dwd stations near you!
#' @description search dwd station by coordinate
#' @param lon longitude
#' @param lat latitude
#' @param rad radius to search for nearby dwd stations
#' @param ref the reference period you want to compare the data to. This is important as it
#' then automatically filters out stations that don't have data in the time of the refernce period.
#' And no data means no data at all. It doesn't check for NAs. This is done in a later step
#' (see function montly.plot or dwd.cs.data)
#' @return this function produces a table with the nearby (input: radius)
#'  found dwd station near by an entered (input) coordinate.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname dwd.search
#' @export
#'
dwd.search <- function(lon, lat, rad, ref) {
  hist_stations <- nearbyStations(
    as.numeric(lat),
    as.numeric(lon),
    radius = rad,
    res = c("daily"),
    var = c("kl")
  )


  hist_stations_filt <- hist_stations %>%
    mutate(von_datum = ymd(von_datum)) %>%
    filter(von_datum <= ifelse(ref == "ref1", ymd("1961-01-01"), ymd("1981-01-01"))) %>%
    group_by(Stations_id) %>%
    summarise(rec_hist = length(per)) %>%
    filter(rec_hist == 2) %>%
    select(Stations_id)

  print_stations <- merge(x = hist_stations_filt, y = hist_stations, all.x = T, by = "Stations_id")[, c("Stations_id", "von_datum", "Stationshoehe", "Stationsname", "geoBreite", "geoLaenge")] %>% distinct()

  print_stations$von_datum <- as.character(print_stations$von_datum)
  print_stations$distance_km <- NULL

  print_stations$Stations_id <- print_stations$Stations_id %>% as.character()


  for (i in 1:nrow(print_stations)) {
    print_stations$distance_km[i] <- round(distm(x = c(as.numeric(lon), as.numeric(lat)), y = c(print_stations$geoLaenge[i], print_stations$geoBreite[i]), fun = distHaversine) / 1000, 0)

    if (str_count(print_stations$Stations_id[i]) < 4) {
      print_stations$Stations_id[i] <- paste0("00", print_stations$Stations_id[i])
    }
    if (str_count(print_stations$Stations_id[i]) < 5) {
      print_stations$Stations_id[i] <- paste0("0", print_stations$Stations_id[i])
    }
  }
  print_stations %<>% select(-geoBreite, -geoLaenge)
  print_stations %<>% .[order(print_stations$distance_km), ]
  print_stations$distance_km %<>% round(., 0)

  colnames(print_stations) <- c("Stations_id", "Messungen ab", "Stationshoehe [m.?.N.N.]", "Stationsname", "Distanz [km] von Eingabe Koordinaten")


  return(print_stations[c(1:6), ])
}
