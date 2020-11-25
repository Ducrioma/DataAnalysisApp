
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(leaflet)



server <- function(input, output,session) {
  #Zoom function
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    my_max = 3
    if(length(input$cities1) > my_max){
      updateCheckboxGroupInput(session, "cities1", selected= tail(input$cities1,my_max))
    }
  })
  
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
  
  
  #Get city available
  
  
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
                    + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
                    + labs(y = input$features))
          }else{
            

              return (ggplot(data1, aes(new_dim, feature))
                      + geom_boxplot(aes(colour = "red"), outlier.shape = NA)
                      + scale_y_continuous(limits = quantile(feature, c(0.1, 0.9), na.rm = T))
                      + facet_wrap(~ data1$city)
                      + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
                      + labs(x = input$ndim, y = input$features))
            


          }
          
        }else if(input$plot_type == "histo"){
          
          if(input$ndim == "no_criteria"){
            return(ggplot(data1, aes(x=feature,color=city))
                   + geom_histogram(fill="white", position= 'identity', alpha = 0.5)
                   + scale_x_continuous(limits = quantile(feature,c(0.1,0.9), na.rm = T))
                   + scale_y_continuous()
                   + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
                   + labs(x = input$features))
          }else{

              return (ggplot(data1, aes(x=feature,color=new_dim))
                      + geom_histogram(fill="white", position= 'identity', alpha = 0.5)
                      + scale_x_continuous(limits = quantile(feature,c(0.1,0.9), na.rm = T))
                      + scale_y_continuous()
                      + facet_wrap(~ data1$city)
                      + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
                      + labs(x = input$features))
            

          }
          
        }else if(input$plot_type == "density"){
          if(input$ndim == "no_criteria"){
            return (ggplot(data=data1, aes(x= feature, color = city))
                    + geom_density(alpha = 0.2)
                    + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
                    + labs(x=input$features)
                    + theme(legend.position="bottom"))
          }else{

              return (ggplot(data=data1, aes(x=feature, fill=new_dim))
                      + geom_density(alpha = 0.2)
                      + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
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
  
  
  #Finer grained analysis
  deep_dive_into_one_city_panel_1 = reactive({
    
    data_inter = (listings[listings$city%in%input$cities2, ])
    data2 = data_inter[data_inter$neighbourhood_cleansed%in%unique(data_inter$neighbourhood_cleansed)[1:10], ]
    feature = (data2[[input$features2]])
    

    return (ggplot(data2, aes(bedrooms, feature))
            +stat_summary(fun.y="mean", geom="point")
            +facet_grid(data2$room_type~data2$neighbourhood_cleansed)
            +theme(strip.text.x = element_text(angle = 90), strip.text.y = element_text(angle = 0))
            +labs(y = input$features2))
  })
  
  
  output$fine_grained <- renderPlot(
    deep_dive_into_one_city_panel_1()
  )
  
  #Leaflet Map
  
  load_data_map = reactive({
    data2 = (listings[listings$city%in%input$cities2, ])
    data_map = (data2[c(1:1000), ])
    
    return( data_map %>%
              leaflet() %>%
              addTiles() %>%
              addMarkers(popup = as.character(data_map$revenue_365),clusterOptions = markerClusterOptions())
    )
  })
  
  output$mymap <- renderLeaflet(
   load_data_map()
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
