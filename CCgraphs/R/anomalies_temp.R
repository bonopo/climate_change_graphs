#' @title FUNCTION_TITLE
#' @description temperature anomalies for a certain year
#' @param data_list list of monthly.plot function in get_dwd_for_anomalies.R script
#' @return ggplot barplot of the year x of temperature anomalies for year x and station x
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname temp.plot
#' @export

temp.plot <- function(data_list) {
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
    summarise(interest = mean(TMK))

  # climate data of reference period (including 29. February in reference)
  clima_ref <- data %>%
    filter(date >= cnp_begin & date <= cnp_end) %>%
    group_by(month = month(date)) %>%
    summarise(month_ref = mean(TMK))

  # data for plots
  clima_merge <- merge(x = clima_ref, y = clima_int, by = "month", all.y = T) %>%
    mutate(plot_date = dmy(paste0("15-", month, "-", year))) %>%
    select(-month)

  colnames(clima_merge) <- c("a_reference", "b_interest", "plot_date") # to change order of plotting need to change alpabetic order of the names


  clima_int_plot <- clima_merge %>%
    gather(key = "key", value = "mm", -plot_date)

  diff <- clima_merge %>%
    mutate(diff = round((100 * ((b_interest / a_reference) - 1)), 0)) %>%
    mutate(y = ifelse(a_reference > b_interest, a_reference, b_interest)) %>%
    mutate(diff = ifelse(diff > 0, paste0("+", diff), diff)) %>%
    mutate(x_int = plot_date - 7, x_ref = plot_date + 7)

  print(
    ggplot(clima_int_plot) +
      geom_bar(aes(x = plot_date, y = mm, alpha = key),
        stat = "identity", position = "dodge", fill = "brown1"
      ) +
      geom_text(
        data = diff,
        aes(x = plot_date, y = y + 1, label = paste0(diff, "%")),
        color = "red", position = position_dodge(0.9), size = 3.5
      ) +
      theme_bw() +
      scale_alpha_manual("", values = c(.5, 1), label = c("Referenz", year)) +
      scale_x_date("",
        breaks = seq(as.Date(diff$plot_date[1]),
          as.Date(diff$plot_date[nrow(diff)]),
          by = "1 month"
        ),
        date_labels = "%b"
      ) +
      geom_text(
        data = diff, aes(
          x = x_ref,
          y = 0.5 * b_interest,
          label = round(b_interest, 0)
        ),
        col = "black"
      ) +
      geom_text(
        data = diff,
        aes(x = x_int, y = 0.5 * a_reference, label = round(a_reference, 0)),
        col = "black"
      ) +
      ylab("mean monthly temperature [?C]")
  )
}
