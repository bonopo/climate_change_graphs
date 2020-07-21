# #cumulative sum
#
# #install.packages(c("dplyr", "magrittr", "utils", "tidyverse", "lubridate"))
# #library(RColorBrewer)
#
# sapply(c("dplyr", "magrittr", "utils", "tidyverse", "lubridate", "RCurl", "RColorBrewer", "rdwd"), require, character.only = T )
#
# setwd("t:/MAs/rmenke/r_gis_pro/r_projects/data/")
# setwd("C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/")
#
# id = "01443";cnp = FALSE;year = c(2019:2020); lat = "47.999"; lon= "7.8421043"; ref="ref1"; rad = 50;cnp=1
#
# hist_stations = nearbyStations(as.numeric(lat), as.numeric(lon), radius=50,res=c("daily"), var= c("kl"))
#
# hist_stations_filt = hist_stations %>%
#           mutate(von_datum = ymd(von_datum)) %>%
#           filter(von_datum <= ifelse(ref=="ref1", ymd("1961-01-01"), ymd("1981-01-01"))) %>%
#   group_by(Stations_id) %>%
#   summarise(rec_hist = length(per)) %>%
#   filter(rec_hist ==2) %>%
#   select(Stations_id)
#
# print_stations = merge(x = hist_stations_filt, y= hist_stations, all.x = T, by= "Stations_id")[,c("von_datum","bis_datum","Stationshoehe","geoBreite", "geoLaenge", "Stationsname")]
#
# names(print_stations)
#
# filter(hist_stations, Station)
# hist_stations$von_datum
# hist_stations$Stations_id
#
#
# rec_stations = nearbyStations(as.numeric(lat), as.numeric(lon), radius=30,res=c("daily"), var= c("kl"))
#
# mindate=ifelse(as.integer(substr(Sys.Date(), 1,4)) %in% year, Sys.Date()-2, as.Date(paste0(max(year),"-05-30")))
#
#
#
# precip.cumsum = function(
#   id = "01443",
#   cnp = FALSE,
#   year = as.integer(substr(date(), 21,24)),
#   labels=FALSE
#   ){
#
#   remove_row =function(data, rows){
#     result = data[-c(rows),]
#     return(result)
#   }
#
#
# #reducing years to max 5
#
#   if(length(year)>4){
#     year= year[1:5] #taking first 5 inputs
#   }
#
# #preambel
# do.call(file.remove, list(list.files("./extr_data/rec/", full.names = TRUE)))
# do.call(file.remove, list(list.files("./extr_data/old/", full.names = TRUE)))
# hist_file = NULL
#
#
# #downloading recent
# download.file(paste0("ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_", id, "_akt.zip"), destfile = "rec.zip",  mode="wb")
#
#
#
#
# unzip("./rec.zip", exdir = "./extr_data/rec")
#
# #importing recent
# files= list.files("./extr_data/rec")
# clima_rec = read.csv2(paste0("./extr_data/rec/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill=F, sep=";", dec=".")%>%
#   mutate(date=ymd(MESS_DATUM))
#
# #downloading historic
# url <- "ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
# filenames <- getURL(url,ftp.use.epsv = FALSE,dirlistonly = TRUE)
# filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
# hist_file = grep(as.character(id), filenames)
#   if(is.null(hist_file))
#     {
#     stop("No historic Data was found online.")
#   }
#
# download.file(filenames[hist_file], destfile = "old.zip",  mode="wb")
# unzip("./old.zip", exdir = "./extr_data/old")
#
# #importing historic
# files= list.files("./extr_data/old")
# clima_old = read.csv2(paste0("./extr_data/old/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill=F, sep=";", dec=".") %>%
#   mutate(date=ymd(MESS_DATUM))
#
# #rbinding the two df together
# #correcting date for merging
#
# clima_cpl = clima_rec %>%
#   filter(date > clima_old$date %>% tail(.,1)) %>%
#   rbind(clima_old,. ) %>%
#   select(date, QN_4, RSK)
#
#
#
# #climate normal period
# if(cnp == TRUE){
# cat("Please enter Climate Normal Period (Default: 1961 - 1990; leave empty for default!!)")
# cnp_begin = readline(prompt = paste("The Time series begins", clima_cpl$date[1],"\n")) %>% as.integer()
# cnp_end = readline(prompt = paste("The Time Series ends", tail(clima_cpl$date,1),"\n")) %>% as.integer()
# } else {
#   cnp_begin = 1961
#   cnp_end = 1990
# }
#
# #handling NAs
# error_data = filter(clima_cpl, year(date) %in% c(cnp_begin:cnp_end, year))
# print("These are the dates with NAs:")
# print(error_data[which(error_data$RSK < 0 | is.na(error_data$RSK)),])
#
# #handling gaps
# time_seq=list()
# for(i in 1:length(year)){
#   year_int = clima_cpl %>% filter(year[i] ==year(date)) #year to check for completeness
#   time_seq[[i]] = data.frame(date = seq.Date(from = dmy(paste0("01-01-", year[i])), to =  ymd(tail(year_int$date,1)), by="day")) #defining ideal time sequence
#   if(NROW(time_seq[[i]]) != NROW(year_int)){
#     time_check = base::merge(x= year_int, y= time_seq[[i]],  by= "date", all.y =T)
#     print("These are the missing dates")
#     print(time_check[which(is.na(time_check$RSK)),])
#   }
# }
#
#
# #no NAs (if not error) the NA have to defined as 0
#  clima_cpl$RSK[which(clima_cpl$RSK<0)] = 0
#  #can not do NA if not cumsum doesn't work (better average of that period)
#  clima_cpl$RSK[is.na(clima_cpl$RSK<0)] = 0
#
#
#  year_ordered = year[order(as.numeric(year), decreasing = T)] %>% c()
#  clima_int = matrix(nrow = 0, ncol=2) %>% as.data.frame()
#
#  #climate data of interest subset
#
# clima_int = clima_cpl %>%
#         mutate(year_date = year(date)) %>%
#         filter(year_date %in% year_ordered) %>%
#         group_by(year_date) %>%
#         mutate(cs_ns = cumsum(RSK)) %>%
#         dplyr::select(date, year_date, cs_ns) %>%
#         group_split()
#
# #reference year
# #removing leap year in reference years
# #if not calculating the average rainfall per day will compare different days in those years where there was a leap year. since in the first step the average rainsum per day is calculated. which is then summed up in the 2nd step, this would lead to wrong matching in the average calculation. This is because I use the number of the day in the year (lubridate::yday) to calculate the average.
#
# clima_leap = clima_cpl %>%
#   filter(year(date) >= cnp_begin & year(date) < cnp_end+1) %>%
#    mutate(leap_y = leap_year(date), ydy= yday(date))
#
# to_be_removed = which(clima_leap$date %in% clima_leap$date[which(clima_leap$leap_y == T & clima_leap$ydy == 60)])
# clima_ref_without_leap = remove_row(clima_leap, to_be_removed) %>%
#   group_by(year(date)) %>%
#   mutate(ydy = 1:365) %>% #if not the leap years will still have 366 as the yday
#   ungroup() %>%
#   group_by(ydy) %>%
#   summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>%
#   mutate(cum_sum = cumsum(mn_dy_ns))
#
#
# clima_ref =  clima_cpl %>%
#   filter(year(date) >= cnp_begin & year(date) < cnp_end+1) %>%
#   mutate(ydy = yday(date)) %>%
#   group_by(ydy) %>%
#   summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>%
#   mutate(cum_sum = cumsum(mn_dy_ns)) %>%
#   mutate(date_plot = as.Date(ydy, origin="2000-01-01"))
#
# #percentage of rain of what normaly would fall for every table in list of clima_int
# int=c();ratio_precip = c()
# for(i in 1:length(year_ordered)){
#     if(yday(tail(clima_int[[i]]$date,1)) == 366){ #if the year of interest is a leap year it will always compare it's 'sum-till-today' with the 'sum-till-yesterday' in the reference period.
#       int[i] = clima_ref_without_leap$ydy[365] #since the reference is only 365 days
#     }else{
#     int[i] = which(yday(tail(clima_int[[i]]$date,1)) == clima_ref_without_leap$ydy)
#     }
#
#
#     if(is.numeric(int[i])){
#       ratio_precip[i] = (clima_int[[i]]$cs_ns[int[i]]/clima_ref_without_leap$cum_sum[int[i]]) *100
#     }else{
#       ratio_precip[i] = NA
#     }
# }
#
# #plotting
# clima_int_plot = do.call( "rbind",clima_int) %>%
#   mutate(ydy = yday(ymd(date)),
#          date_plot = ymd(paste0("2000-",month(date), "-", day(date)))) %>%
#   dplyr::select(ydy, year_date, date_plot, cs_ns) %>% as.tbl
#
#
#
# if(length(year) >2){
#   n <- length(year)
#   palette =brewer.pal.info["Set1",] #set1 has no yellow and is seen well on white foreground
#   col_vector = unlist(mapply(brewer.pal, palette$maxcolors, rownames(palette)))[1:n]
#  }else{
#     if(length(years)==2){
#   col_vector = c("#7FC97F", "#BEAED4")
#     }else{
#   col_vector=c("#7FC97F")
#     }
#  }
#
# if(labels==F){
#   print(
#
#   ggplot(data = clima_int_plot)+
#     geom_line(aes(x=date_plot, y= cs_ns, color = as.factor(year_date)))+
#     geom_line(data= clima_ref, aes(x=date_plot, y=cum_sum, lty='Climatological normal'), col="red", lwd=1.4) +
#     ylab("cumulative precipitaion [mm]")+
#     scale_color_manual(values = col_vector)+
#     scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%b")+
#     labs(lty ="Reference",color ="" )+
#     theme(text = element_text(size = 6))+
#     theme_bw()+
#     xlab("")
#
#   )
# }
#
#
#  # annotate(geom="text",  max(clima_int_plot$ydy),  hjust = -0.2, vjust = -1, label=paste0(round(ratio_precip[i],0)," %"))+
#  # ggtitle(paste("id:",id, clima_int$date[nrow(clima_int)]))+
# #xlab(paste0("1.1.",cnp_begin," - ","31.12.",cnp_end))+
#  }
#
# #cnp = edit climate normal period (year to compate to)
# #year= which year to analyse
# #id= which city (freiburg is default)
# # stationsname:
# #freiburg = 01443
# #hannover =02014
# # dessau (sachsen-anhalt) = 00948 #gibts net
# #HH = 01975
# #neu strelitz = 03577 #gibts net
#
# #hannover
# precip.cumsum(cnp=F, year = 2017, id="02014")
# precip.cumsum(cnp=F, year = 2018, id="02014")
# precip.cumsum(cnp=F, year = 2019, id="02014")
# precip.cumsum(cnp=F, year = 2020, id="02014")
# #freiburg
# precip.cumsum(cnp=F, year = 2003, id="01443")
#
# #hamburg
# precip.cumsum(cnp=F, year = 2019, id="01975")
#
