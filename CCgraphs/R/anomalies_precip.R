#' @title precipitation anomalies for a certain year
#' @description FUNCTION_DESCRIPTION
#' @param data_list list of monthly.plot function in get_dwd_for_anomalies.R script
#' @return ggplot barplot of the year x of precipitation anomalies for year x and station x
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname precip.plot
#' @export

precip.plot <- function(data_list) {
  data <- data_list[[1]]
  year <- data_list[[2]]
  cnp <- data_list[[3]]

  if (cnp == 1) {
    cnp_begin <- ymd("19610101")
    cnp_end <- ymd("19901231")
  } else {
    cnp_begin <- ymd("19810101")
    cnp_end <- ymd("20101231")
  }

  # period of interest

  clima_int <- data %>%
    filter(year(date) == year) %>%
    group_by(month = month(date)) %>%
    summarise(monthly_sum = sum(RSK))

  # climate data of reference period (including 29. February in reference)
  clima_ref <- data %>%
    filter(date >= cnp_begin & date <= cnp_end) %>%
    mutate(month_year = dmy(paste0("15-", month(date), "-", year(date)))) %>%
    group_by(month_year) %>%
    summarise(sum = sum(RSK)) %>%
    mutate(month = month(month_year)) %>%
    group_by(month) %>%
    summarise(month_ref = mean(sum))


  # data for plots
  clima_merge <- merge(x = clima_ref, y = clima_int, by = "month", all.y = T) %>%
    mutate(plot_date = dmy(paste0("15-", month, "-", year))) %>%
    select(-month)

  clima_int_plot <- clima_merge %>%
    gather(key = "key", value = "mm", -plot_date)

  diff <- clima_merge %>%
    mutate(diff = round((100 * ((monthly_sum / month_ref) - 1)), 0)) %>%
    mutate(y = ifelse(month_ref > monthly_sum, month_ref, monthly_sum)) %>%
    mutate(diff = ifelse(diff > 0, paste0("+", diff), diff)) %>%
    mutate(x_ref = plot_date - 7, x_int = plot_date + 7)

  print(
    ggplot(clima_int_plot) +
      geom_bar(aes(x = plot_date, y = mm, alpha = key), stat = "identity", position = "dodge", fill = "cadetblue") +
      geom_text(data = diff, aes(x = plot_date, y = y + 5, label = paste0(diff, "%")), color = "red", position = position_dodge(0.9), size = 3.5) +
      theme_bw() +
      scale_alpha_manual("", values = c(.5, 1), label = c("Referenz", year)) +
      scale_x_date("", breaks = seq(as.Date(diff$plot_date[1]), as.Date(diff$plot_date[nrow(diff)]), by = "1 month"), date_labels = "%b") +
      geom_text(data = diff, aes(x = x_int, y = 0.5 * monthly_sum, label = round(monthly_sum, 0)), col = "black") +
      geom_text(data = diff, aes(x = x_ref, y = 0.5 * month_ref, label = round(month_ref, 0)), col = "black") +
      ylab("precipitation sum [mm/month]")
  )
}
