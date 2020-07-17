#' @title FUNCTION_TITLE
#' @description getting dwd data of x years to use in other functions
#' @param id of dwd station
#' @param cnp climate normal period (1 or 2: either 1961- 1990 or 1981 - 2010)
#' @param year the year of which I want to have the climate anomalies
#' @param updateProgress PARAM_DESCRIPTION
#' @return a list of: aggregated and corrected data, the year (input) and the cnp (input)
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

dwd.cs.data <- function(
                        id,
                        cnp,
                        year,
                        updateProgress) {
  if (cnp == 1) {
    cnp_begin <- ymd("19610101")
    cnp_end <- ymd("19901231")
  } else {
    cnp_begin <- ymd("19810101")
    cnp_end <- ymd("20101231")
  }

  # reducing years to max 5
  year <- year[order(as.numeric(year), decreasing = F)] %>% c()

  # https://stackoverflow.com/questions/48383010/character-vector-of-length-1-all-but-the-first-element-will-be-ignored-error-whe


  if (length(year) > 5) {
    year <- year[1:5] # taking first 5 inputs
    years_removed <- as.character(year[6:length(year)])
    showModal(modalDialog(
      title = "Too many years selected",
      HTML(paste(years_removed, "will be removed")),
      easyClose = T,
      footer = NULL
    ))
  }

  # preambel
  do.call(file.remove, list(list.files("./extr_data/rec/", full.names = TRUE)))
  do.call(file.remove, list(list.files("./extr_data/old/", full.names = TRUE)))
  hist_file <- NULL


  # downloading recent
  download.file(paste0("ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_", id, "_akt.zip"), destfile = "rec.zip", mode = "wb")

  if (is.function(updateProgress)) {
    text <- paste0("Downloading and importing the recent DWD data")
    updateProgress(detail = text)
  }


  unzip("./rec.zip", exdir = "./extr_data/rec")

  # importing recent
  files <- list.files("./extr_data/rec")
  clima_rec <- read.csv2(paste0("./extr_data/rec/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill = F, sep = ";", dec = ".") %>%
    mutate(date = ymd(MESS_DATUM))

  # downloading historic
  url <- "ftp://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
  filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
  hist_file <- grep(as.character(id), filenames)
  if (is.null(hist_file)) {
    shinyalert("Oops!", "Something went wrong.", type = "error")
    stop("No historic Data was found online.")
  }

  if (is.function(updateProgress)) {
    text <- paste0("Downloading and importing the historic DWD data")
    updateProgress(detail = text)
  }


  download.file(filenames[hist_file], destfile = "old.zip", mode = "wb")
  unzip("./old.zip", exdir = "./extr_data/old")

  # importing historic
  files <- list.files("./extr_data/old")
  clima_old <- read.csv2(paste0("./extr_data/old/", files[str_detect(files, "produkt") %>% which()]), na.strings = "-999", fill = F, sep = ";", dec = ".") %>%
    mutate(date = ymd(MESS_DATUM))

  # rbinding the two df together
  # correcting date for merging

  clima_cpl <- clima_rec %>%
    filter(date > clima_old$date %>% tail(., 1)) %>%
    rbind(clima_old, .) %>%
    select(date, QN_4, RSK) %>%
    filter(year(date) %in% c(year(cnp_begin):year(cnp_end), year)) # getting only the relevant period


  # handling NAs


  if (any(isTRUE(clima_cpl$RSK < 0)) | any(is.na(clima_cpl$RSK))) {
    my_nas <- clima_cpl[which(clima_cpl$RSK < 0 | is.na(clima_cpl$RSK)), ]

    showModal(modalDialog(
      title = "These are the dates with NAs:",
      HTML(paste(apply(my_nas, 1, function(x) {
        paste(x, collapse = ": ")
      }), collapse = "<br>")),
      easyClose = T,
      footer = NULL
    ))
  }

  # handling gaps####
  time_seq <- list()
  for (i in 1:length(year)) {
    year_int <- clima_cpl %>% filter(year[i] == year(date)) # year to check for completeness
    time_seq[[i]] <- data.frame(date = seq.Date(from = dmy(paste0("01-01-", year[i])), to = ymd(tail(year_int$date, 1)), by = "day")) # defining ideal time sequence
    if (NROW(time_seq[[i]]) != NROW(year_int)) {
      time_check <- base::merge(x = year_int, y = time_seq[[i]], by = "date", all.y = T)
      my_gaps <- time_check[which(is.na(time_check$RSK)), ]

      showModal(modalDialog(
        title = "These are the missing dates",
        HTML(paste(
          apply(my_gaps, 1, function(x) {
            paste(x, collapse = ": ")
          }),
          collapse = "<br>"
        )),
        easyClose = T,
        footer = NULL
      ))
    }
  }

  if (is.function(updateProgress)) {
    text <- paste0("Removing leap year and aggregating reference period")
    updateProgress(detail = text)
  }



  # no NAs (if not error) the NA have to defined as 0
  clima_cpl$RSK[which(clima_cpl$RSK < 0)] <- 0
  # can not do NA if not cumsum doesn't work (better average of that period)
  clima_cpl$RSK[is.na(clima_cpl$RSK < 0)] <- 0




  return(list(clima_cpl, year, cnp))
  # res= list(clima_cpl, year, cnp)
}
