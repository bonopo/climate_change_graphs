server <- function(input, output, session) {
 
  search_dwd <- eventReactive(input$search, {
    
    validate(
      need(input$lat != "", "Please enter a latitude"),
      need(input$lon != "", "Please enter a longitude"),
      need(input$rad != "", "Please enter a radius"),
      need(input$ref != "", "Please choose a reference period")

      
    )
    
    withProgress(message = 'Searching nearby stations', value = 0.14, {
        dwd.search(lat = input$lat, lon = input$lon,rad = input$rad, ref = input$ref)
         })
  })
  
  output$search_result <- renderTable({
    search_dwd()
    })
    
  plot_dwd <- eventReactive(input$create_plot, {
    
    validate(
      need(input$year != "", "Please select a year"),
      need(input$id != "", "Please select a Station id")
      
    )
    
  
      
    
    progress = Progress$new()
    progress$set(message = "plotting", value=0)
    
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    
    #withProgress(message = 'Generating plot', value = 0.14, {
     dwd.plot(id = input$id, year = input$year, cnp = input$ref, updateProgress)
   #  })
   })
   
   output$ns_cum_sum_plot <- renderPlot({
     plot_dwd()
   })
       
    output$ns_cum_sum_data <- renderTable({
     plot_dwd()
   })

                
                
 # output$ns_cum_sum <- renderPlot({input$ns_cum_sum})
   
  
}


