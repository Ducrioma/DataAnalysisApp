# COMMENTAIRE DE SOURCE A ENLEVER POUR LE RENDU
#source("data_preparation_script.R")



server <- function(input, output) {
  
  output$display_cities <- renderText({
    paste("You have selected this",aggregate(listings$availability_30,list(listings$city),mean))
  })
  
  
  data_selected_cities = reactive({
    return(listings[listings$city%in%input$cities1, ])
  })
  
  output$testPlot1 <-renderPlot(

      ggplot(data=data_selected_cities(), aes(city, availability_30))
      + geom_boxplot(aes(colour = "red"), outlier.shape = NA)
      +  scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T))

  )
}
