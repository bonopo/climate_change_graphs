#preamble####

check_packages = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = T)
    sapply(pkg, require, character.only=T)
}

# packages 
check_packages(c("shiny","shinydashboard","shinyjs","shinyBS","ggplot2","scales","lubridate", "geosphere"))

#wd

setwd("C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/")


#functions####
dwd.search = function(lon, lat, rad, ref){
  
hist_stations = nearbyStations(as.numeric(lat), as.numeric(lon), radius=rad,res=c("daily"), var= c("kl"))

hist_stations_filt = hist_stations %>% 
          mutate(von_datum = ymd(von_datum)) %>% 
          filter(von_datum <= ifelse(ref=="ref1", ymd("1961-01-01"), ymd("1981-01-01"))) %>% 
  group_by(Stations_id) %>% 
  summarise(rec_hist = length(per)) %>% 
  filter(rec_hist ==2) %>% 
  select(Stations_id)

print_stations = merge(x = hist_stations_filt, y= hist_stations, all.x = T, by= "Stations_id")[,c("Stations_id", "von_datum","bis_datum","Stationshoehe","geoBreite", "geoLaenge", "Stationsname")] %>% distinct()

print_stations$von_datum = as.character(print_stations$von_datum)
print_stations$bis_datum = as.character(print_stations$bis_datum)
print_stations$distance_km = NULL

for(i in 1:nrow(print_stations)){
  print_stations$distance_km[i] = round(distm(x = c(as.numeric(lon), as.numeric(lat)), y = c(print_stations$geoLaenge[i], print_stations$geoBreite[i]), fun = distHaversine)/1000,0) 
}



return(print_stations[c(1:6),])

}
