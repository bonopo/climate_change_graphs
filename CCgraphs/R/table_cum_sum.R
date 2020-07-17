#' @title FUNCTION_TITLE
#' @description percentage of rain that should have fallen of x years compared to the normal
#' @param data_list list of dwd.cs.data function in get_dwd_for_cumsum.R script
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
