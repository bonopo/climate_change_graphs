# Shiny app
source("./shiny_global.R")
source("./shiny_server.R")
source("./shiny_ui.R")
shinyApp(ui = ui, server = server)

