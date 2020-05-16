#preamble####

check_packages = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = T)
    sapply(pkg, require, character.only=T)
}

# packages 
check_packages(c("shiny","shinydashboard","shinyjs","shinyBS","ggplot2","scales","lubridate", "geosphere", "rdwd","tidyverse", "RCurl", "RColorBrewer", "shinyalert"))

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

print_stations = merge(x = hist_stations_filt, y= hist_stations, all.x = T, by= "Stations_id")[,c("Stations_id", "von_datum","Stationshoehe","Stationsname")] %>% distinct()

print_stations$von_datum = as.character(print_stations$von_datum)
print_stations$distance_km = NULL

print_stations$Stations_id = print_stations$Stations_id %>% as.character()


for(i in 1:nrow(print_stations)){
  print_stations$distance_km[i] = round(distm(x = c(as.numeric(lon), as.numeric(lat)), y = c(print_stations$geoLaenge[i], print_stations$geoBreite[i]), fun = distHaversine)/1000,0) 
  
 if(str_count(print_stations$Stations_id[i]) <4) {
      print_stations$Stations_id[i] =paste0("00", print_stations$Stations_id[i])
 }
   if(str_count(print_stations$Stations_id[i]) <5) {
      print_stations$Stations_id[i] =paste0("0", print_stations$Stations_id[i])
  }
}

colnames(print_stations) = c("Stations_id", "Messungen ab","Stationshoehe [m.�.N.N.]","Stationsname", "Distanz [km] von Eingabe Koordinaten")



return(print_stations[c(1:6),])

}


dwd.plot = function(
  id ,
  cnp ,
  year , 
  updateProgress
  ){
  
  remove_row =function(data, rows){
    result = data[-c(rows),]
    return(result)
  }
  
  
  if(cnp == 1){
    cnp_begin = ymd("19610101")
    cnp_end = ymd("19901231")
  }else{
    cnp_begin = ymd("19810101")
    cnp_end = ymd("20101231")
  }
  
#reducing years to max 5
 year = year[order(as.numeric(year), decreasing = T)] %>% c()
 
 #https://stackoverflow.com/questions/48383010/character-vector-of-length-1-all-but-the-first-element-will-be-ignored-error-whe
 
  
  if(length(year)>5){
      year= year[1:5] #taking first 5 inputs
      years_removed = as.character(year[6:length(year)])
      showModal(modalDialog(
          title = "Too many years selected",
          HTML(paste(years_removed, "will be removed")),
          easyClose = T,
          footer = NULL
      ))  
      }
  
#preambel
do.call(file.remove, list(list.files("./extr_data/rec/", full.names = TRUE)))
do.call(file.remove, list(list.files("./extr_data/old/", full.names = TRUE)))
hist_file = NULL


#downloading recent
download.file(paste0("ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_", id, "_akt.zip"), destfile = "rec.zip",  mode="wb")

    if (is.function(updateProgress)) {
      text <- paste0("Downloading and importing the recent DWD data")
      updateProgress(detail = text)
    }


unzip("./rec.zip", exdir = "./extr_data/rec")

#importing recent
files= list.files("./extr_data/rec")
clima_rec = read.csv2(paste0("./extr_data/rec/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill=F, sep=";", dec=".")%>% 
  mutate(date=ymd(MESS_DATUM))

#downloading historic
url <- "ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
filenames <- getURL(url,ftp.use.epsv = FALSE,dirlistonly = TRUE) 
filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
hist_file = grep(as.character(id), filenames) 
  if(is.null(hist_file))
    {
    shinyalert("Oops!", "Something went wrong.", type = "error")
    stop("No historic Data was found online.")
  }

    if (is.function(updateProgress)) {
      text <- paste0("Downloading and importing the historic DWD data")
      updateProgress(detail = text)
    }


download.file(filenames[hist_file], destfile = "old.zip",  mode="wb")
unzip("./old.zip", exdir = "./extr_data/old")

#importing historic
files= list.files("./extr_data/old")
clima_old = read.csv2(paste0("./extr_data/old/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill=F, sep=";", dec=".") %>% 
  mutate(date=ymd(MESS_DATUM))

#rbinding the two df together
#correcting date for merging

clima_cpl = clima_rec %>% 
  filter(date > clima_old$date %>% tail(.,1)) %>% 
  rbind(clima_old,. ) %>% 
  select(date, QN_4, RSK) %>% 
  filter(year(date) %in% c(year(cnp_begin):year(cnp_end), year)) #getting only the relevant period


#handling NAs
   
   
if(length(clima_cpl$RSK < 0) != 0 | length(is.na(clima_cpl$RSK)) != 0){
  
  my_nas = clima_cpl[which(clima_cpl$RSK < 0 | is.na(clima_cpl$RSK)),]
  
  showModal(modalDialog(
       title = "These are the dates with NAs:",
       HTML(paste(apply(my_nas,1,function(x) {paste(x,collapse=': ')}),collapse='<br>')),
       easyClose = T,
       footer = NULL
      ))
  
}   

#handling gaps
time_seq=list()
for(i in 1:length(year)){
  year_int = clima_cpl %>% filter(year[i] ==year(date)) #year to check for completeness
  time_seq[[i]] = data.frame(date = seq.Date(from = dmy(paste0("01-01-", year[i])), to =  ymd(tail(year_int$date,1)), by="day")) #defining ideal time sequence
  if(NROW(time_seq[[i]]) != NROW(year_int)){
    time_check = base::merge(x= year_int, y= time_seq[[i]],  by= "date", all.y =T)
    my_gaps = time_check[which(is.na(time_check$RSK)),]
    
    showModal(modalDialog(
       title = "These are the missing dates",
       HTML(paste(
         apply(my_gaps,1,function(x) {paste(x,collapse=': ')}),
          collapse='<br>')),
        easyClose = T, 
        footer = NULL
      ))
    
  }
}
  

#no NAs (if not error) the NA have to defined as 0
 clima_cpl$RSK[which(clima_cpl$RSK<0)] = 0 
 #can not do NA if not cumsum doesn't work (better average of that period)
 clima_cpl$RSK[is.na(clima_cpl$RSK<0)] = 0  
 
 
 clima_int = matrix(nrow = 0, ncol=2) %>% as.data.frame()

 #climate data of interest subset

clima_int = clima_cpl %>% 
        mutate(year_date = year(date)) %>% 
        filter(year_date %in% year) %>% 
        group_by(year_date) %>% 
        mutate(cs_ns = cumsum(RSK)) %>% 
        dplyr::select(date, year_date, cs_ns) %>% 
        group_split()
 
#reference year 
#removing leap year in reference years
#if not calculating the average rainfall per day will compare different days in those years where there was a leap year. since in the first step the average rainsum per day is calculated. which is then summed up in the 2nd step, this would lead to wrong matching in the average calculation. This is because I use the number of the day in the year (lubridate::yday) to calculate the average. 

clima_leap = clima_cpl %>% 
  filter(date >= cnp_begin & date <= cnp_end) %>%
   mutate(leap_y = leap_year(date), ydy= yday(date)) 
 
to_be_removed = which(clima_leap$date %in% clima_leap$date[which(clima_leap$leap_y == T & clima_leap$ydy == 60)])
clima_ref_without_leap = remove_row(clima_leap, to_be_removed) %>% 
  group_by(year(date)) %>% 
  mutate(ydy = 1:365) %>% #if not the leap years will still have 366 as the yday
  ungroup() %>%  
  group_by(ydy) %>%  
  summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>% 
  mutate(cum_sum = cumsum(mn_dy_ns)) 


clima_ref =  clima_cpl %>% 
  filter(year(date) >= cnp_begin & year(date) < cnp_end+1) %>%
  mutate(ydy = yday(date)) %>% 
  group_by(ydy) %>%  
  summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>% 
  mutate(cum_sum = cumsum(mn_dy_ns)) %>% 
  mutate(date_plot = as.Date(ydy, origin="2000-01-01"))

  if (is.function(updateProgress)) {
      text <- paste0("Removing leap year and aggregating reference period")
      updateProgress(detail = text)
  }

#percentage of rain of what normaly would fall for every table in list of clima_int
int=c();ratio_precip = c()
for(i in 1:length(year)){
    if(yday(tail(clima_int[[i]]$date,1)) == 366){ #if the year of interest is a leap year it will always compare it's 'sum-till-today' with the 'sum-till-yesterday' in the reference period.
      int[i] = clima_ref_without_leap$ydy[365] #since the reference is only 365 days
    }else{
    int[i] = which(yday(tail(clima_int[[i]]$date,1)) == clima_ref_without_leap$ydy)
    }
  
 
    if(is.numeric(int[i])){
      ratio_precip[i] = (clima_int[[i]]$cs_ns[int[i]]/clima_ref_without_leap$cum_sum[int[i]]) *100
    }else{
      ratio_precip[i] = NA
    }
}

#plotting
clima_int_plot = do.call( "rbind",clima_int) %>% 
  mutate(ydy = yday(ymd(date)),
         date_plot = ymd(paste0("2000-",month(date), "-", day(date)))) %>% 
  dplyr::select(ydy, year_date, date_plot, cs_ns) %>% as.tbl



if(length(year) >2){
  n <- length(year)
  palette =brewer.pal.info["Set1",] #set1 has no yellow and is seen well on white foreground
  col_vector = unlist(mapply(brewer.pal, palette$maxcolors, rownames(palette)))[1:n]
 }else{
    if(length(year)==2){
  col_vector = c("#7FC97F", "#BEAED4")
    }else{
  col_vector=c("#7FC97F")
    }
 }

  print(
  
  ggplot(data = clima_int_plot)+
    geom_line(aes(x=date_plot, y= cs_ns, color = as.factor(year_date)))+
    geom_line(data= clima_ref, aes(x=date_plot, y=cum_sum, lty=as.character(paste0(year(cnp_begin), "-", year(cnp_end)))), col="red", lwd=1.4) +
    ylab("cumulative precipitaion [mm]")+
    scale_color_manual(values = col_vector)+
    scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%b")+
    labs(lty ="Reference",color ="" )+
    theme(text = element_text(size = 6))+
    theme_bw()+
    xlab("")
      
  ) 
}


 
