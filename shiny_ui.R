ui = shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Climate Graphics Germany"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Niederschlagssummen", tabName = "ns_cum_sum", icon = icon("edit")),
    menuItem("Monatliche Anomalien", tabName = "tabelle", icon = icon("table"))
    #menuItem("Niederschlagsdefizit Karte", tabName = "ausgabe", icon = icon("line-chart"))
  )),
  
  dashboardBody(
    #shinyjs::useShinyjs(), #use shiny js to disable the ID field
    tabItems(
      #year input ####
      tabItem(
        tabName = "ns_cum_sum",
        fluidRow(
           textInput("lon",
                     label = h3("Longitude (only in decimal degrees)"),
                     value = "7.8421043"),
           
            textInput("lat", 
                      label = h3("Latitude (only in decimal degrees)"),
                      value = "47.999"),
           
           numericInput("rad", 
                        label = "Search radius for DWD stations",
                        value = 50),
           
        selectInput("ref", 
                    label = "Reference year", 
                    choices = list("1961-1990"=1, "1981-2010" = 2),
                    multiple =F)),
        
        actionButton('search', 'Search for nearby DWD stations'),
        
        tableOutput('search_result'),
        
        selectInput("year", 
                    label = "Which years to compare (max. 5)",
                    choices = c(2020:1990),
                    multiple =T),
        
       
        textInput('id', label="Station_id of the DWD station", value = "01443"),
        
        actionButton('create_plot', 'Create a plot'),
        
        plotOutput("ns_cum_sum_plot"),
        actionButton('download', 'Download the plot')
        
        
        ))
    
    
    )
  
))

