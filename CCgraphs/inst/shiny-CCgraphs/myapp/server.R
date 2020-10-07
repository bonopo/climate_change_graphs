#' @title Create shiny server (?)
#' @description The function creates a shiny server
#' @param input PARAM_DESCRIPTION
#' @param output PARAM_DESCRIPTION
#' @param session PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname server
#' @export
#'
server <- function(input, output, session) {


  # search dwd station ------------------------------------------------------
  search_dwd <- eventReactive(input$search, {
    validate(
      need(input$lat != "", "Please enter a latitude"),
      need(input$lon != "", "Please enter a longitude"),
      need(input$rad != "", "Please enter a radius"),
      need(input$ref != "", "Please choose a reference period")
    )

    withProgress(message = "Searching nearby stations", value = 0.14, {
      dwd.search(lat = input$lat, lon = input$lon, rad = input$rad, ref = input$ref)
    })
  })

  output$search_result <- renderTable({
    search_dwd()
  })



  # cumsum plot and table ---------------------------------------------------


  dwd_data <- eventReactive(input$create_plot, {
    validate(
      need(input$year != "", "Please select a year"),
      need(input$ref != "", "Please select a refence period"),
      need(input$id != "", "Please select a Station id")
    )


    progress <- Progress$new()
    progress$set(message = "plotting", value = 0)


    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }



    dwd.cs.data(id = input$id, year = input$year, cnp = input$ref, updateProgress)
  })

  output$ns_cum_sum_plot <- renderPlot({
    plot.cs(dwd_data())
  })

  output$ns_cum_sum_data <- renderTable({
    table.cs(dwd_data())
  })




  # monthly anomalies -------------------------------------------------------

  monthly_anomalies <- eventReactive(input$create_plot_anomalie, {
    validate(
      need(input$year_anomalie != "", "Please select a year"),
      need(input$ref_anomalie != "", "Please select a refence period"),
      need(input$id_anomalie != "", "Please select a Station id")
    )




    progress <- Progress$new()
    progress$set(message = "plotting", value = 0)



    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }


    monthly.plot(id = input$id_anomalie, year = input$year_anomalie, cnp = input$ref_anomalie, updateProgress)
  })

  output$anomalies <- renderPlot({
    precip.plot(monthly_anomalies())
  })

  output$anomalies2 <- renderPlot({
    temp.plot(monthly_anomalies())
  })
}
