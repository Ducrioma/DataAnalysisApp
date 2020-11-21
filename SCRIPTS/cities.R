library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)


# a generic function to fetch cities from a specific country
available_city <- function(country) {
    select_city <- all_data[all_data$country == country, ] %>% select("city") %>% unique()
    return(select_city)
}

# a generic function to fetch data from a specific city
available_data_per_city <- function(city) {
    available_data <- all_data[all_data$city == city, ] %>% select(c('data_date','listings_url','calendar_url'))
    return (available_data)
}

available_countries <- function(dataset){
    return(unique(dataset$country))
}
# First LOAD THE FILE all_data.csv
all_data <- read.csv(file="all_data.csv")

# Default countries for the app project
default_countries <- c('france','italy','germany','spain','the-netherlands','belgium')
# get the cities from the
selected_cities <- vector()
for (country in default_countries){
    cities <- available_city(country)$city
    selected_cities <- c(selected_cities,toString(cities))
}
# All cities are included in this vector
selected_cities <- unlist(strsplit(selected_cities, ","))
selected_cities <- gsub(" ", "", selected_cities, fixed = TRUE)
# Randomly choose 3 cities
sample_cities <- sample(selected_cities,3)
