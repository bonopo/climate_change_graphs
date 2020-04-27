#cumulative sum

#install.packages(c("dplyr", "magrittr", "utils", "tidyverse", "lubridate"))

sapply(c("dplyr", "magrittr", "utils", "tidyverse", "lubridate", "RCurl","imager"), require, character.only = T )

setwd("t:/MAs/rmenke/r_gis_pro/r_projects/data/")


id = "01443";cnp = FALSE;year = c(2013:2018)

precip.cumsum = function(
  id = "01443",
  cnp = FALSE,
  year = as.integer(substr(date(), 21,24))
  ){
  
  remove_row =function(data, rows){
    result = data[-c(rows),]
    return(result)
  }
  
   #preambel
  do.call(file.remove, list(list.files("./extr_data/rec/", full.names = TRUE)))
  do.call(file.remove, list(list.files("./extr_data/old/", full.names = TRUE)))
  hist_file = NULL


  #downloading recent
download.file(paste0("ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_", id, "_akt.zip"), destfile = "rec.zip",  mode="wb")




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
    stop("No historic Data was found online.")
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
  select(date, QN_4, RSK) 


  
#climate normal period
if(cnp == TRUE){
cat("Please enter Climate Normal Period (Default: 1961 - 1990; leave empty for default!!)")
cnp_begin = readline(prompt = paste("The Time series begins", clima_cpl$date[1],"\n")) %>% as.integer()
cnp_end = readline(prompt = paste("The Time Series ends", tail(clima_cpl$date,1),"\n")) %>% as.integer()
} else { 
  cnp_begin = 1961
  cnp_end = 1990
}
  
#handling NAs
error_data = filter(clima_cpl, year(date) %in% c(cnp_begin:cnp_end, year)) 
print("These are the dates with NAs:")
print(error_data[which(error_data$RSK < 0 | is.na(error_data$RSK)),])  

#handling gaps
time_seq=list()
for(i in 1:length(year)){
  year_int = clima_cpl %>% filter(year[i] ==year(date)) #year to check for completeness
  time_seq[[i]] = data.frame(date = seq.Date(from = dmy(paste0("01-01-", year[i])), to =  ymd(tail(year_int$date,1)), by="day")) #defining ideal time sequence
  if(NROW(time_seq[[i]]) != NROW(year_int)){
    time_check = base::merge(x= year_int, y= time_seq[[i]],  by= "date", all.y =T)
    print("These are the missing dates")
    print(time_check[which(is.na(time_check$RSK)),])
  }
}
  

#no NAs (if not error) the NA have to defined as 0
 clima_cpl$RSK[which(clima_cpl$RSK<0)] = 0 
 #can not do NA if not cumsum doesn't work (better average of that period)
 clima_cpl$RSK[is.na(clima_cpl$RSK<0)] = 0  
 
 
 year_ordered = year[order(as.numeric(year), decreasing = T)] %>% c()
 clima_int = matrix(nrow = 0, ncol=2) %>% as.data.frame()

 #climate data of interest subset

clima_int = clima_cpl %>% 
        mutate(year_date = year(date)) %>% 
        filter(year_date %in% year_ordered) %>% 
        group_by(year_date) %>% 
        mutate(cs_ns = cumsum(RSK)) %>% 
        dplyr::select(date, year_date, cs_ns) %>% 
        group_split()
 
#reference year 
#removing leap year in reference years
clima_leap = clima_cpl %>% 
  filter(year(date) >= cnp_begin & year(date) < cnp_end+1) %>%
   mutate(leap_y = leap_year(date), ydy= yday(date)) 
 
to_be_removed = which(clima_leap$date %in% clima_leap$date[which(clima_leap$leap_y == T & clima_leap$ydy == 60)])
clima_ref_without_leap = remove_row(clima_leap, to_be_removed) %>% 
  dplyr::mutate(ydy = if_else(ydy==366,  365, ydy)) %>%  #if not the leap years will still have 366 as the yday
  group_by(ydy) %>%  
  summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>% 
  mutate(cum_sum = cumsum(mn_dy_ns)) 


clima_ref =  clima_cpl %>% 
  filter(year(date) >= cnp_begin & year(date) < cnp_end+1) %>%
  mutate(ydy = yday(date)) %>% 
  group_by(ydy) %>%  
  summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>% 
  mutate(cum_sum = cumsum(mn_dy_ns))

#percentage of rain of what normaly would fall for every table in list of clima_int
int=c();ratio_precip = c()
for(i in 1:length(year_ordered)){
    if(yday(tail(clima_int[[i]]$date,1)) == 366){
      int[i] = clima_ref_without_leap$ydy[365]
    }else{
    int[i] = which(yday(tail(clima_int[[i]]$date,1)) == clima_ref_without_leap$ydy)
    }
  

    if(is.numeric(int[i])){
      ratio_precip[i] = (clima_int[[i]]$cs_ns[int[i]]/clima_ref$cum_sum[int[i]]) *100
    }else{
      ratio_precip[i] = NA
    }
}
clima_int %<>% select(date, cs_ns) %>% as.tbl
clima_ref %<>% select(date=day, cum_sum) %>% as.tbl()

plot_data = merge(x=clima_int, y = clima_ref, by= "date", all.x = T )

print(
  ggplot(data = plot_data)+
  geom_line(aes(x=date, y= cs_ns))+
  geom_line(data= clima_ref, aes(x=date, y=cum_sum), col="red", show.legend = T)+
  ylab("cumulative precipitaion [mm]")+
     xlab(paste0("1.1.",cnp_begin," - ","31.12.",cnp_end))+
    theme(axis.title.x = element_text(colour = "red"))+
    annotate(geom="text", clima_int$date[1], max(clima_int$cs_ns),  hjust = -0.2, vjust = -1, label=paste0(round(ratio_precip,1)," %"))+
    ggtitle(paste("id:",id, clima_int$date[nrow(clima_int)]))
    
) 


 }

#cnp = edit climate normal period (year to compate to)
#year= which year to analyse
#id= which city (freiburg is default)
# stationsname: 
#freiburg = 01443
#hannover =02014 
# dessau (sachsen-anhalt) = 00948 #gibts net
#HH = 01975  
#neu strelitz = 03577 #gibts net

#hannover
precip.cumsum(cnp=F, year = 2017, id="02014")
precip.cumsum(cnp=F, year = 2018, id="02014")
precip.cumsum(cnp=F, year = 2019, id="02014")
precip.cumsum(cnp=F, year = 2020, id="02014")
#freiburg
precip.cumsum(cnp=F, year = 2003, id="01443")

#hamburg
precip.cumsum(cnp=F, year = 2019, id="01975")

