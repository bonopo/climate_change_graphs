server <- function(input, output, session) {
 
  search_dwd <- eventReactive(input$search, {
    withProgress(message = 'Searching nearby stations', value = 0.14, {
    dwd.search(lat = input$lat, lon = input$lon,rad = input$rad, ref = input$ref)
         })
  })
  
  output$search_result <- renderTable({
    search_dwd()
    })
    
  plot_dwd <- eventReactive(input$create_plot, {
     withProgress(message = 'Generating plot', value = 0.14, {
     dwd.plot(id = input$id, year = input$year, cnp = input$ref)
     })
   })
   
   output$ns_cum_sum_plot <- renderPlot({
     plot_dwd()
   })
       
   

                
                
 # output$ns_cum_sum <- renderPlot({input$ns_cum_sum})
   
  
}


