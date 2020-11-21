# COMMENTAIRE DE SOURCE A ENLEVER POUR LE RENDU
#source("data_preparation_script.R")

library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)


server <- function(input, output,session) {
  #Aggregation
  aggregrate_selected_cities = reactive({
    if(length(input$cities1) > 0){
      data1 = (listings[listings$city%in%input$cities1, ])
      feature = (data1[[input$features]])

      
      aggregate(feature,list(data1$city),input$aggreg)
    }

    
    
  })
  
  output$aggregating <- renderTable({
    aggregrate_selected_cities()
  })
  
  
  #Plotting cities
  data_selected_cities = reactive({
      if(length(input$cities1) > 0){
        
        data1 = (listings[listings$city%in%input$cities1, ])
        feature = (data1[[input$features]])
        
        if(length(input$ndim) < 1){
          return (ggplot(data=data1, aes(city, feature))
                  + geom_boxplot(aes(colour = "red"), outlier.shape = NA)
                  +  scale_y_continuous(limits = quantile(feature, c(0.1, 0.9), na.rm = T))
                  + labs(y = input$features))
        }else{
          new_dim = (data1[[input$ndim]])
          if(length(input$ndim) < 2){
            return (ggplot(data1, aes(new_dim, feature))
                    + geom_boxplot(aes(colour = "red"), outlier.shape = NA)
                    + scale_y_continuous(limits = quantile(feature, c(0.1, 0.9), na.rm = T))
                    + facet_wrap(~ data1$city)
                    + labs(x = input$ndim, y = input$features))
                    
          }
        }

                
      }
  })
  
  output$plotting1 <-renderPlot(
    data_selected_cities()
  )
  

  #We can select max 2 optionnal dimensions
  observe({
    my_max = 2
    if(length(input$ndim) > my_max) 
    { 
      updateCheckboxGroupInput(session, "ndim", selected= tail(input$ndim,my_max)) 
    } 

  })
}
