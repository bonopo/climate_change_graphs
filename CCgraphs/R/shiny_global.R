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
# check_packages(c("shiny","shinydashboard","shinyjs","shinyBS","ggplot2","scales",
#  "lubridate", "geosphere", "rdwd","tidyverse", "RCurl", "RColorBrewer", "shinyalert", "magrittr"))

#wd

# setwd("C:/Users/Menke/Documents/Uni/R_practice/climate_change_graphs/")


#functions####
  #' @title FUNCTION_TITLE
  #' @description FUNCTION_DESCRIPTION
  #' @param lon PARAM_DESCRIPTION
  #' @param lat PARAM_DESCRIPTION
  #' @param rad PARAM_DESCRIPTION
  #' @param ref PARAM_DESCRIPTION
  #' @return OUTPUT_DESCRIPTION
  #' @details DETAILS
  #' @examples
  #' \dontrun{
  #' if(interactive()){
  #'  #EXAMPLE1
  #'  }
  #' }
  #' @rdname dwd.search
  #' @export
dwd.search = function(lon, lat, rad, ref){
# define the station
hist_stations = nearbyStations(as.numeric(lat), as.numeric(lon), radius=rad,
                               res=c("daily"), var= c("kl"))

# ??
hist_stations_filt = hist_stations %>%
          mutate(von_datum = ymd(von_datum)) %>%
          filter(von_datum <= ifelse(ref=="ref1", ymd("1961-01-01"), ymd("1981-01-01"))) %>%
          group_by(Stations_id) %>%
          summarise(rec_hist = length(per)) %>%
          filter(rec_hist ==2) %>%
          select(Stations_id)

# merge the station information
print_stations = merge(x = hist_stations_filt, y= hist_stations,
                       all.x = T, by= "Stations_id")[,c("Stations_id", "von_datum",
                                                        "Stationshoehe","Stationsname",
                                                        "geoBreite", "geoLaenge")] %>% distinct()
# ??
print_stations$von_datum = as.character(print_stations$von_datum)
print_stations$distance_km = NULL
print_stations$Stations_id = print_stations$Stations_id %>% as.character()

# ??
for(i in 1:nrow(print_stations)){
  print_stations$distance_km[i] = round(distm(x = c(as.numeric(lon), as.numeric(lat)),
                                              y = c(print_stations$geoLaenge[i],
                                                    print_stations$geoBreite[i]),
                                              fun = distHaversine)/1000,0)

 if(str_count(print_stations$Stations_id[i]) <4) {
      print_stations$Stations_id[i] =paste0("00", print_stations$Stations_id[i])
 }
   if(str_count(print_stations$Stations_id[i]) <5) {
      print_stations$Stations_id[i] =paste0("0", print_stations$Stations_id[i])
  }
}
# print the location
print_stations %<>% select(-geoBreite, -geoLaenge)
print_stations %<>% .[order(print_stations$distance_km),]
print_stations$distance_km %<>% round(.,0)

#  ??
colnames(print_stations) = c("Stations_id", "Messungen ab","Stationshoehe [m.?.N.N.]",
                             "Stationsname", "Distanz [km] von Eingabe Koordinaten")
#
return(print_stations[c(1:6),])

}

# id="01443"; cnp=1;year=c(2020, 2019)


# Next function
  #' @title FUNCTION_TITLE
  #' @description FUNCTION_DESCRIPTION
  #' @param id PARAM_DESCRIPTION
  #' @param cnp PARAM_DESCRIPTION
  #' @param year PARAM_DESCRIPTION
  #' @param updateProgress PARAM_DESCRIPTION
  #' @return OUTPUT_DESCRIPTION
  #' @details DETAILS
  #' @examples
  #' \dontrun{
  #' if(interactive()){
  #'  #EXAMPLE1
  #'  }
  #' }
  #' @seealso
  #'  \code{\link[base]{merge}}
  #' @rdname dwd.cs.data
  #' @export
  #' @importFrom base merge
dwd.cs.data = function(
  id ,
  cnp ,
  year ,
  updateProgress
  ){



  if(cnp == 1){
    cnp_begin = ymd("19610101")
    cnp_end = ymd("19901231")
  }else{
    cnp_begin = ymd("19810101")
    cnp_end = ymd("20101231")
  }

#reducing years to max 5
 year = year[order(as.numeric(year), decreasing = F)] %>% c()

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


if(any(isTRUE(clima_cpl$RSK < 0)) | any(is.na(clima_cpl$RSK))){

  my_nas = clima_cpl[which(clima_cpl$RSK < 0 | is.na(clima_cpl$RSK)),]

  showModal(modalDialog(
       title = "These are the dates with NAs:",
       HTML(paste(apply(my_nas,1,function(x) {paste(x,collapse=': ')}),collapse='<br>')),
       easyClose = T,
       footer = NULL
      ))

}

#handling gaps####
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

  if(is.function(updateProgress)){
      text <- paste0("Removing leap year and aggregating reference period")
      updateProgress(detail = text)
  }



#no NAs (if not error) the NA have to defined as 0
 clima_cpl$RSK[which(clima_cpl$RSK<0)] = 0
 #can not do NA if not cumsum doesn't work (better average of that period)
 clima_cpl$RSK[is.na(clima_cpl$RSK<0)] = 0




  return(list(clima_cpl, year, cnp))
  #res= list(clima_cpl, year, cnp)
}


#res = list(clima_cpl,year, cnp)
#data_list = res


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname plot.cs
#' @export
#' @importFrom dplyr select
plot.cs = function(data_list){
  clima_cpl = data_list[[1]]
  year = data_list[[2]]
  cnp  = data_list[[3]]


  if(cnp == 1){
    cnp_begin = ymd("19610101")
    cnp_end = ymd("19901231")
  }else{
    cnp_begin = ymd("19810101")
    cnp_end = ymd("20101231")
  }

  clima_int = matrix(nrow = 0, ncol=2) %>% as.data.frame()

 #climate data of interest subset

clima_int = clima_cpl %>%
              mutate(year_date = year(date)) %>%
              filter(year_date %in% year) %>%
              group_by(year_date) %>%
              mutate(cs_ns = cumsum(RSK)) %>%
              dplyr::select(date, year_date, cs_ns) %>%
              group_split()

#climate data of reference period (including 29. February in reference)
clima_ref = clima_cpl %>%
  filter(date >= cnp_begin & date <= cnp_end)  %>%
  mutate(date_plot = dmy(paste0(day(date), "-", month(date), "-2000"))) %>%
  group_by(date_plot) %>%
  summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>%
  mutate(cum_sum = cumsum(mn_dy_ns), ydy = yday(date_plot))


  #data for plots
clima_int_plot = do.call("rbind",clima_int) %>%
      mutate(ydy = yday(ymd(date)),
             date_plot = ymd(paste0("2000-",month(date), "-", day(date)))) %>%
      dplyr::select(ydy, year_date, date_plot, cs_ns) %>% as.tbl


#setting colors
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


  #percentage of rain####
  #output as table
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname table.cs
#' @export
#' @importFrom dplyr select
 table.cs = function(data_list){

  clima_cpl = data_list[[1]]
  cnp  = data_list[[3]]
  year = data_list[[2]]

  if(cnp == 1){
    cnp_begin = ymd("19610101")
    cnp_end = ymd("19901231")
  }else{
    cnp_begin = ymd("19810101")
    cnp_end = ymd("20101231")
  }

  clima_int = matrix(nrow = 0, ncol=2) %>% as.data.frame()

 #climate data of interest subset

clima_int = clima_cpl %>%
              mutate(year_date = year(date)) %>%
              filter(year_date %in% year) %>%
              group_by(year_date) %>%
              mutate(cs_ns = cumsum(RSK)) %>%
              dplyr::select(date, year_date, cs_ns) %>%
              group_split()

#climate data of reference period (including 29. February in reference)
clima_ref = clima_cpl %>%
  filter(date >= cnp_begin & date <= cnp_end)  %>%
  mutate(date_plot = dmy(paste0(day(date), "-", month(date), "-2000"))) %>%
  group_by(date_plot) %>%
  summarise(mn_dy_ns = mean(RSK, na.rm=T)) %>%
  mutate(cum_sum = cumsum(mn_dy_ns), ydy = yday(date_plot))




  #percentage of rain of what normaly would fall for every table in list of clima_int
int=c();ratio_precip = c();absolute_ref=c();absolute_int=c()
for(i in 1:length(year)){
    int[i] = which(yday(tail(clima_int[[i]]$date,1)) == clima_ref$ydy)

    if(is.numeric(int[i])){
      ratio_precip[i] = round((clima_int[[i]]$cs_ns[int[i]]/clima_ref$cum_sum[int[i]]) *100,0)
      absolute_ref[i] = clima_ref$cum_sum[int[i]]
      absolute_int[i] = clima_int[[i]]$cs_ns[int[i]]
    }else{
      ratio_precip[i] = NA
      absolute_ref[i] = NA
      absolute_int[i] = NA
    }
}

  #creating table

  percent_res = data.frame(year=year,
                           percent = ratio_precip,
                           ref = absolute_ref,
                           int=absolute_int)

  colnames(percent_res) = c("Year",
                            "difference [%]",
                            "reference sum [mm]",
                            "actual sum [mm]")

  return(percent_res)
}


#monthly anamolies ####
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @param cnp PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param updateProgress PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[base]{merge}}
#' @rdname monthly.plot
#' @export
#' @importFrom base merge
monthly.plot = function(
  id,
  cnp,
  year,
  updateProgress
  ){

  if(cnp == 1){
    cnp_begin = ymd("19610101")
    cnp_end = ymd("19901231")
  }else{
    cnp_begin = ymd("19810101")
    cnp_end = ymd("20101231")
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
  select(date, QN_4, RSK, TMK) %>%
  filter(year(date) %in% c(year(cnp_begin):year(cnp_end), year)) #getting only the relevant period


#handling NAs


if(any(isTRUE(clima_cpl$RSK < 0)) | any(is.na(clima_cpl$RSK))){

  my_nas = clima_cpl[which(clima_cpl$RSK < 0 | is.na(clima_cpl$RSK)),]

  showModal(modalDialog(
       title = "These are the dates with NAs:",
       HTML(paste(apply(my_nas,1,function(x) {paste(x,collapse=': ')}),collapse='<br>')),
       easyClose = T,
       footer = NULL
      ))

}

#handling gaps####
time_seq=list()

  year_int = clima_cpl %>% filter(year == year(date)) #year to check for completeness
  time_seq = data.frame(date = seq.Date(from = dmy(paste0("01-01-", year)), to =  ymd(tail(year_int$date,1)), by="day")) #defining ideal time sequence
  if(NROW(time_seq) != NROW(year_int)){
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


  return(list(clima_cpl, year, cnp))

}

#data = clima_cpl
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname precip.plot
#' @export
precip.plot = function(data_list){

  data = data_list[[1]]
  year = data_list[[2]]
  cnp  = data_list[[3]]

  if(cnp == 1){
    cnp_begin = ymd("19610101")
    cnp_end = ymd("19901231")
  }else{
    cnp_begin = ymd("19810101")
    cnp_end = ymd("20101231")
  }

  #period of interest

  clima_int = data %>%
        filter(year(date) == year) %>%
        group_by(month = month(date)) %>%
        summarise(monthly_sum = sum(RSK))

#climate data of reference period (including 29. February in reference)
clima_ref = data %>%
  filter(date >= cnp_begin & date <= cnp_end)  %>%
  mutate(month_year = dmy(paste0("15-", month(date), "-" , year(date)))) %>%
  group_by(month_year) %>%
  summarise(sum = sum(RSK)) %>%
  mutate(month =  month(month_year)) %>%
  group_by(month) %>%
  summarise(month_ref = mean(sum))


#data for plots
clima_merge = merge(x= clima_ref, y = clima_int, by="month", all.y=T) %>%
  mutate(plot_date = dmy(paste0("15-", month, "-", year))) %>% select(-month)

clima_int_plot = clima_merge %>%
  gather(key = "key", value="mm",-plot_date)

diff =  clima_merge %>%
            mutate(diff = round(( 100*((monthly_sum/month_ref)-1)),0)) %>%
            mutate(y = ifelse(month_ref>monthly_sum, month_ref, monthly_sum)) %>%
            mutate(diff = ifelse(diff>0, paste0("+",diff), diff)) %>%
            mutate(x_ref = plot_date-7, x_int = plot_date+7)

print(
ggplot(clima_int_plot)+
  geom_bar(aes(x=plot_date, y = mm, alpha = key),  stat = "identity", position = "dodge", fill="cadetblue")+
   geom_text(data = diff,aes(x = plot_date, y= y+5, label=paste0(diff,"%")), color="red",  position = position_dodge(0.9), size=3.5)+
  theme_bw()+
  scale_alpha_manual("",values=c(.5,1),label=c("Referenz",year))+
  scale_x_date("", breaks = seq(as.Date(diff$plot_date[1]), as.Date(diff$plot_date[nrow(diff)]), by="1 month"), date_labels = "%b")+
  geom_text(data = diff,aes(x= x_int, y = 0.5*monthly_sum, label=round(monthly_sum,0)), col="black")+
  geom_text(data = diff,aes(x= x_ref, y = 0.5*month_ref, label=round(month_ref,0)), col="black")+
  ylab("precipitation sum [mm/month]")
)
}

#data = clima_cpl
#temperature anomalies####
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname temp.plot
#' @export
temp.plot = function(data_list){

  data = data_list[[1]]
  year = data_list[[2]]
  cnp = data_list[[3]]

  if(cnp == 1){
    cnp_begin = ymd("19610101")
    cnp_end = ymd("19901231")
  }else{
    cnp_begin = ymd("19810101")
    cnp_end = ymd("20101231")
  }

  #period of interest

  clima_int = data %>%
        filter(year(date) == year) %>%
        group_by(month = month(date)) %>%
        summarise(interest = mean(TMK))

  #climate data of reference period (including 29. February in reference)
  clima_ref = data %>%
        filter(date >= cnp_begin & date <= cnp_end)  %>%
        group_by(month = month(date)) %>%
        summarise(month_ref = mean(TMK))

  #data for plots
    clima_merge = merge(x= clima_ref, y = clima_int, by="month", all.y=T) %>%
        mutate(plot_date = dmy(paste0("15-", month, "-", year))) %>%
        select(-month)

    colnames(clima_merge) = c("a_reference", "b_interest", "plot_date") #to change order of plotting need to change alpabetic order of the names


    clima_int_plot = clima_merge %>%
      gather(key = "key", value="mm",-plot_date)

    diff=  clima_merge %>%
              mutate(diff = round(( 100*((b_interest/a_reference)-1)),0)) %>%
              mutate(y = ifelse(a_reference>b_interest, a_reference, b_interest)) %>%
              mutate(diff = ifelse(diff>0, paste0("+",diff), diff)) %>%
              mutate(x_int = plot_date-7, x_ref = plot_date+7)

    print(
        ggplot(clima_int_plot)+
          geom_bar(aes(x=plot_date, y = mm, alpha = key),
                   stat = "identity", position = "dodge", fill="brown1")+
          geom_text(data = diff,
                    aes(x = plot_date, y= y+1, label=paste0(diff,"%")),
                    color="red",  position = position_dodge(0.9), size=3.5)+
          theme_bw()+
          scale_alpha_manual("",values=c(.5,1),label=c("Referenz",year))+
          scale_x_date("",
                       breaks = seq(as.Date(diff$plot_date[1]),
                                    as.Date(diff$plot_date[nrow(diff)]), by="1 month"),
                       date_labels = "%b")+
          geom_text(data = diff,aes(x= x_int,
                                    y = 0.5*b_interest,
                                    label=round(b_interest,0)),
                      col="black")+
          geom_text(data = diff,
                    aes(x= x_ref, y = 0.5*a_reference, label=round(a_reference,0)),
                    col="black")+
          ylab("mean monthly temperature [?C]")
    )



}




