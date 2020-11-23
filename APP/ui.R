# COMMENTAIRE DE SOURCE A ENLEVER POUR LE RENDU
source("../SCRIPTS/preprocessing.R")

library(shiny)

# See above for the definitions of ui and server
ui <- fluidPage(
  
  # App title ----
  titlePanel("Instruction List 3"),
  
    
  # Main panel for displaying outputs ----
  mainPanel(
      
    tabsetPanel( type = "tabs",
                  tabPanel("Comparing Cities",
                           br(),
                           sidebarPanel(
                             # checkboxGroupInput("cities1","Cities to compare:",
                             #              c("Malaga" = "malaga",
                             #                "Mallorca" = "mallorca",
                             #                "Sevilla" = "sevilla"),
                             #              selected = "Malaga"
                             #                ),
                             checkboxGroupInput("cities1","Cities to compare:",
                                                choices = unique(listings$city)
                             ),
                             br(),
                             selectInput("features","Features to compare",
                                          c("Availability over 30 days" = "availability_30",
                                            "Availability over 60 days" = "availability_60",
                                            "Availability over 90 days" = "availability_90",
                                            "Availability over 365 days" = "availability_365",
                                            "Price over 30 days" = "price_30",
                                            "Price over 60 days" = "price_60",
                                            "Price over 90 days" = "price_90",
                                            "Price over 365 days" = "price_365",
                                            "Revenue over 30 days" = "revenue_30",
                                            "Revenue over 60 days" = "revenue_60",
                                            "Revenue over 90 days" = "revenue_90",
                                            "Revenue over 365 days" = "revenue_365",
                                            "...")),
                             br(),
                             selectInput("aggreg","Aggregation type",
                                          c("Mean" = "mean",
                                            "Median" = "median",
                                            "...")),
                             br(),
                             selectInput("plot_type","Plot type",
                                          c("Boxplot" = "geom_boxplot",
                                            "Histogram" = "histo",
                                            "Density" = "density",
                                            "...")),
                             br(),
                             selectInput("ndim","Add a criteria",
                                                c("No additional criteria" = "no_criteria",
                                                  "Room type" = "room_type",
                                                  "House size (# of bedrooms)" = "bedrooms",
                                                  "Neighborhood" = "neighbourhood_cleansed"
                                                  )),
                             width = 4
                           ),
                           mainPanel(
                                     mainPanel(
                                       plotOutput(outputId = "plotting1",
                                                  dblclick = "plot1_dblclick",
                                                  brush = brushOpts(
                                                    id = "plot1_brush",
                                                    resetOnNew = TRUE
                                                  )),
                                       width = 12
                                     ),
                                     mainPanel(
                                       tableOutput('aggregating')
                                     ),
                                     width = 8
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




