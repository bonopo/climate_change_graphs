#' @title Create shiny server (?)
#' @description The function creates a shiny server
#' @param input PARAM_DESCRIPTION
#' @param output PARAM_DESCRIPTION
#' @param session PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname server
#' @export
#'
server <- function(input, output, session) {
<<<<<<< HEAD:CCgraphs/R/shiny_server.R

  search_dwd <- eventReactive(input$search, {

=======
  
# search dwd station ------------------------------------------------------  
  search_dwd <- eventReactive(input$search, {
>>>>>>> master:shiny_server.R
    validate(
      need(input$lat != "", "Please enter a latitude"),
      need(input$lon != "", "Please enter a longitude"),
      need(input$rad != "", "Please enter a radius"),
      need(input$ref != "", "Please choose a reference period")
<<<<<<< HEAD:CCgraphs/R/shiny_server.R


    )

    withProgress(message = 'Searching nearby stations', value = 0.14, {
        dwd.search(lat = input$lat, lon = input$lon,rad = input$rad, ref = input$ref)
         })
=======
    )




    withProgress(message = "Searching nearby stations", value = 0.14, {
      dwd.search(lat = input$lat, lon = input$lon, rad = input$rad, ref = input$ref)
    })
>>>>>>> master:shiny_server.R
  })

  output$search_result <- renderTable({
    search_dwd()
<<<<<<< HEAD:CCgraphs/R/shiny_server.R
    })

  #cumsum plot ####

  dwd_data <- eventReactive(input$create_plot, {

=======
  })



# cumsum plot and table ---------------------------------------------------


  dwd_data <- eventReactive(input$create_plot, {
>>>>>>> master:shiny_server.R
    validate(
      need(input$year != "", "Please select a year"),
      need(input$ref != "", "Please select a refence period"),
      need(input$id != "", "Please select a Station id")
<<<<<<< HEAD:CCgraphs/R/shiny_server.R

=======
>>>>>>> master:shiny_server.R
    )




<<<<<<< HEAD:CCgraphs/R/shiny_server.R
    progress = Progress$new()
    progress$set(message = "plotting", value=0)
=======
    progress <- Progress$new()
    progress$set(message = "plotting", value = 0)
>>>>>>> master:shiny_server.R

    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
<<<<<<< HEAD:CCgraphs/R/shiny_server.R


     dwd.cs.data(id = input$id, year = input$year, cnp = input$ref, updateProgress)

   })

    output$ns_cum_sum_plot <- renderPlot({
      plot.cs(dwd_data())
    })

    output$ns_cum_sum_data <- renderTable({
      table.cs(dwd_data())
    })



 # monthly anomalies ####
  monthly_anomalies <- eventReactive(input$create_plot_anomalie, {

=======


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
>>>>>>> master:shiny_server.R
    validate(
      need(input$year_anomalie != "", "Please select a year"),
      need(input$ref_anomalie != "", "Please select a refence period"),
      need(input$id_anomalie != "", "Please select a Station id")
<<<<<<< HEAD:CCgraphs/R/shiny_server.R

=======
>>>>>>> master:shiny_server.R
    )




<<<<<<< HEAD:CCgraphs/R/shiny_server.R
    progress = Progress$new()
    progress$set(message = "plotting", value=0)
=======


    progress <- Progress$new()
    progress$set(message = "plotting", value = 0)
>>>>>>> master:shiny_server.R

    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
<<<<<<< HEAD:CCgraphs/R/shiny_server.R


     monthly.plot(id = input$id_anomalie, year = input$year_anomalie, cnp = input$ref_anomalie, updateProgress)

  })

   output$anomalies <- renderPlot({
     precip.plot(monthly_anomalies())
   })

      output$anomalies2 <- renderPlot({
     temp.plot(monthly_anomalies())
   })
=======
>>>>>>> master:shiny_server.R


    monthly.plot(id = input$id_anomalie, year = input$year_anomalie, cnp = input$ref_anomalie, updateProgress)
  })

  output$anomalies <- renderPlot({
    precip.plot(monthly_anomalies())
  })

<<<<<<< HEAD:CCgraphs/R/shiny_server.R

}

=======
  output$anomalies2 <- renderPlot({
    temp.plot(monthly_anomalies())
  })
}
>>>>>>> master:shiny_server.R
