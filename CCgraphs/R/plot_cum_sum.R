#' @title plotting cum sum
#' @description cumsum plot of x years compared to reference period
#' @param data_list list of dwd.cs.data function in get_dwd_for_cumsum.R script
#' @return 1 ggplot with x lines plus one reference line
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
#'
plot.cs <- function(data_list) {
  clima_cpl <- data_list[[1]]
  year <- data_list[[2]]
  cnp <- data_list[[3]]


  if (cnp == 1) {
    cnp_begin <- ymd("19610101")
    cnp_end <- ymd("19901231")
  } else {
    cnp_begin <- ymd("19810101")
    cnp_end <- ymd("20101231")
  }

  clima_int <- matrix(nrow = 0, ncol = 2) %>% as.data.frame()

  # climate data of interest subset

  clima_int <- clima_cpl %>%
    mutate(year_date = year(date)) %>%
    filter(year_date %in% year) %>%
    group_by(year_date) %>%
    mutate(cs_ns = cumsum(RSK)) %>%
    dplyr::select(date, year_date, cs_ns) %>%
    group_split()

  # climate data of reference period (including 29. February in reference)
  clima_ref <- clima_cpl %>%
    filter(date >= cnp_begin & date <= cnp_end) %>%
    mutate(date_plot = dmy(paste0(day(date), "-", month(date), "-2000"))) %>%
    group_by(date_plot) %>%
    summarise(mn_dy_ns = mean(RSK, na.rm = T)) %>%
    mutate(cum_sum = cumsum(mn_dy_ns), ydy = yday(date_plot))


  # data for plots
  clima_int_plot <- do.call("rbind", clima_int) %>%
    mutate(
      ydy = yday(ymd(date)),
      date_plot = ymd(paste0("2000-", month(date), "-", day(date)))
    ) %>%
    dplyr::select(ydy, year_date, date_plot, cs_ns) %>%
    as.tbl()


  # setting colors
  if (length(year) > 2) {
    n <- length(year)
    palette <- brewer.pal.info["Set1", ] # set1 has no yellow and is seen well on white foreground
    col_vector <- unlist(mapply(brewer.pal, palette$maxcolors, rownames(palette)))[1:n]
  } else {
    if (length(year) == 2) {
      col_vector <- c("#7FC97F", "#BEAED4")
    } else {
      col_vector <- c("#7FC97F")
    }
  }

  print(
    ggplot(data = clima_int_plot) +
      geom_line(aes(x = date_plot, y = cs_ns, color = as.factor(year_date))) +
      geom_line(data = clima_ref, aes(x = date_plot, y = cum_sum, lty = as.character(paste0(year(cnp_begin), "-", year(cnp_end)))), col = "red", lwd = 1.4) +
      ylab("cumulative precipitaion [mm]") +
      scale_color_manual(values = col_vector) +
      scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%b") +
      labs(lty = "Reference", color = "") +
      theme(text = element_text(size = 6)) +
      theme_bw() +
      xlab("")
  )
}
