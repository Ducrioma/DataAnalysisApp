library(shiny)

# See above for the definitions of ui and server
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello les kheys!"),
  
    
  # Main panel for displaying outputs ----
  mainPanel(
      
    tabsetPanel( type = "tabs",
                  tabPanel("Comparing Cities",
                           br(),
                           sidebarPanel(
                             checkboxGroupInput("cities1","Cities to compare:",
                                          c("Malaga" = "malaga",
                                            "Mallorca" = "mallorca",
                                            "Sevilla" = "sevilla"),
                                            ),
                             br(),
                             selectInput("features","Features to compare",
                                          c("Availability over 30 days",
                                            "Revenue over 30 days",
                                            "...")),
                             br(),
                             selectInput("aggreg","Aggregation type",
                                          c("Average",
                                            "Median",
                                            "...")),
                             br(),
                             selectInput("plot_type","Plot type",
                                          c("Histogram",
                                            "Density",
                                            "Boxplot",
                                            "...")),
                             br(),
                             checkboxGroupInput("ndim","New Dimensions",
                                                c("Room type",
                                                  "House size",
                                                  "Neighborhood",
                                                  "...")),
                             width = 5
                           ),
                           mainPanel(
                                     plotOutput(outputId = "testPlot1"),
                                     width = 7
                                     )),
                 
                  tabPanel("Deep Dive in one City Analysis",
                           br(),
                           sidebarPanel(
                             selectInput("cities2","City to analyze",
                                         c("Malaga",
                                           "Sevilla",
                                           "Mallorca")),
                             width = 5
                           ),
                           tabsetPanel(
                             tabPanel("Finer Grained Analysis",
                                      plotOutput(outputId = "fine_grained")),
                             tabPanel("Map of the population",
                                      plotOutput(outputId = "pop_map"))
                           ))
        
    ), width = 12
    
    
      
  )
  
)




