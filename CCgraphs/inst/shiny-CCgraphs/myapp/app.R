# Shiny app####
source("inst/shiny-CCgraphs/myapp/shiny_global.R") #loading local variables and helper functions

#loading the necessary funtions to create the plots
#search dwd station
# source("./dwd_search_station.R")

#cum sum plot + table
# source("./get_dwd_for_cumsum.R")
# source("./plot_cum_sum.R")
# source("./table_cum_sum.R")

#anomalies
# source("./get_dwd_for_anomalies.R")
# source("./anomalies_precip.R")
# source("./anomalies_temp.R")

#shiny app
source("inst/shiny-CCgraphs/myapp/shiny_server.R") #server
source("inst/shiny-CCgraphs/myapp/shiny_ui.R")#user interface
shinyApp(ui = ui, server = server)
