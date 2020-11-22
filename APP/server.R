# COMMENTAIRE DE SOURCE A ENLEVER POUR LE RENDU
#source("data_preparation_script.R")

library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)


server <- function(input, output,session) {
  #Zoom function
  ranges <- reactiveValues(x = NULL, y = NULL)

  
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
        new_dim = (data1[[input$ndim]])
        
        
        if(input$plot_type == "geom_boxplot"){
          
          if(input$ndim == "no_criteria"){
            return (ggplot(data=data1, aes(city, feature))
                    + geom_boxplot(aes(colour = "red"), outlier.shape = NA)
                    +  scale_y_continuous(limits = quantile(feature, c(0.1, 0.9), na.rm = T))
                    + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                    + labs(y = input$features))
          }else{

            return (ggplot(data1, aes(new_dim, feature))
                    + geom_boxplot(aes(colour = "red"), outlier.shape = NA)
                    + scale_y_continuous(limits = quantile(feature, c(0.1, 0.9), na.rm = T))
                    + facet_wrap(~ data1$city)
                    + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                    + labs(x = input$ndim, y = input$features))
          }
          
        }else if(input$plot_type == "histo"){
          
          if(input$ndim == "no_criteria"){
            return(ggplot(data1, aes(x=feature,color=city))
                   + geom_histogram(fill="white", position= 'identity', alpha = 0.5)
                   + scale_x_continuous(limits = quantile(feature,c(0.1,0.9), na.rm = T))
                   + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                   + labs(x = input$features))
          }else{
            return (ggplot(data1, aes(x=feature,color=new_dim))
                    + geom_histogram(fill="white", position= 'identity', alpha = 0.5)
                    + scale_x_continuous(limits = quantile(feature,c(0.1,0.9), na.rm = T))
                    + facet_wrap(~ data1$city)
                    + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                    + labs(x = input$features))
          }
          
        }else if(input$plot_type == "density"){
          if(input$ndim == "no_criteria"){
            return (ggplot(data=data1, aes(x= feature, color = city))
                    + geom_density(alpha = 0.2)
                    + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                    + labs(x=input$features)
                    + theme(legend.position="bottom"))
          }else{
            new_dim = (data1[[input$ndim]])
            return (ggplot(data=data1, aes(x=feature, fill=new_dim))
                    + geom_density(alpha = 0.2)
                    + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
                    + facet_wrap(~ data1$city)
                    + labs(x=input$features, fill=input$ndim)
                    + theme(legend.position="bottom"))
          }
          
        }


                
      }
  })
  
  output$plotting1 <-renderPlot(
    data_selected_cities()
  )
  
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

}
