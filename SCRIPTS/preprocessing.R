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


prepare_data <- function(city, data_date, listings_url, calendar_url)
{
    # Reading from the url provided
    print(paste0("reading listings data from ", listings_url))
    # Opening a connection with the provided url
    con <- gzcon(url(listings_url))
    # Instantiate a text connection object
    txt <- readLines(con)
    # reading from the file
    listings <- read.csv(textConnection(txt))
    print(paste0("reading calendar data from ", calendar_url))
    # Opening a connection with the provided url
    con <- gzcon(url(calendar_url))
    # Instantiate a text connection object
    txt <- readLines(con)
    # reading from the file
    calendar <- read.csv(textConnection(txt))

    ## Add Keys: columns city and day date
    listings$city <- city
    listings$data_date <- data_date

    ## Select interesting columns
    ### Most columns don't contain interesting information
    columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed",
                          "latitude", "longitude",
                          "property_type", "room_type", "accommodates", "bedrooms",
                          "beds", "price", "minimum_nights",  "maximum_nights")

    listings <- listings %>%
        select(columns_listings) %>%
        arrange(id)


    # Cleaning calendar dataframe

    ## arrange by id and date
    calendar <- calendar %>%
        arrange(listing_id, date)

    ## add day number (starting first day)
    calendar <- calendar %>%
        group_by(listing_id) %>%
        mutate(day_nb = row_number()) %>%
        ungroup()

    ## change available column to binary
    calendar <- calendar %>%
        mutate(available = ifelse(available=="t", 1, 0))

    ## clean price column and transform to numeric
    calendar <- calendar %>%
        mutate(price = str_replace(price, "\\$", ""),
               adjusted_price = str_replace(adjusted_price, "\\$", ""))
    calendar <- calendar %>%
        mutate(price = str_replace(price, ",", ""),
               adjusted_price = str_replace(adjusted_price, ",", ""))
    calendar <- calendar %>%
        mutate(price = as.numeric(price),
               adjusted_price = as.numeric(adjusted_price))

    ## calculate estimated revenue for upcoming day
    calendar <- calendar %>%
        mutate(revenue = price*(1-available))

    ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
    calendar <- calendar %>%
        group_by(listing_id) %>%
        summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                  availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
                  availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
                  availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
                  price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                  price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
                  price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
                  price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
                  revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                  revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
                  revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
                  revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)
        )

    listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))

    dir.create(file.path("data_cleansed", city, data_date), recursive = TRUE)

    write.csv(listings_cleansed, file.path("data_cleansed", city, data_date, "listings.csv"))
    print(paste0("saving data into ", file.path("data_cleansed", city, data_date, "listings.csv")))
}

# First LOAD THE FILE all_data.csv
all_data <- read.csv(file="../APP/data_source/all_data.csv")
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
# sample_cities <- sample(selected_cities,3)
sample_cities <- c('bordeaux','sevilla','malaga')
# Reading data for the 3 cities
for(i in 1:length(sample_cities)){
    city <- sample_cities[i]
    file_dir <- file.path(".", "data_cleansed", city)
    if(!dir.exists(file_dir)){
      city_data <- available_data_per_city(city)
      data_date <- city_data[1,'data_date']
      listings_city_url <- toString(city_data[1,'listings_url'])
      calendar_city_url <- toString(city_data[1,'calendar_url'])
      print("-------------------------------------------------")
      print(paste(c("Preparing data for", city, "compiled at", data_date), collapse = " "))
      prepare_data(city, data_date,listings_city_url,calendar_city_url)
    }
}
## Once data for multiple cities are prepared
min_date <- '2020-05-01'
max_date <- '2020-11-01'
## We can read these data and concatenate them together into one dataframe
files_paths <- c()
# Read data in cities between min_date and max_date
for(city in sample_cities){
    file_dir <- file.path(".", "data_cleansed", city)
    file_subdirs <- list.dirs(file_dir)
    file_subdirs <- file_subdirs[-1]

    for(file_subdir in file_subdirs){
        if(file_subdir < file.path(file_dir, min_date) | file_subdir > file.path(file_dir, max_date)  )
            file_subdirs = file_subdirs[file_subdirs != file_subdir]
    }
    files_paths <- c(files_paths, file_subdirs)
}
files_paths <- file.path(files_paths, "listings.csv")
listings <-
    do.call(rbind,
            lapply(files_paths, read.csv, row.names=1, nrows=2000))

# PREPROCESSING #
# BreakEvenPoint
cities_spain <- c("sevilla","malaga")
square_meter <- c(2516.67,2842.86)
charges <- c(126.44,112.01)

for(i in 1:length(cities_spain)){
    city <- cities_spain[i]
    bool_index <- listings$city==city
    # Creating fixed costs column
    fixed_cost<-(square_meter[i]*(listings[bool_index,]$bedrooms* 10 + (1.5*listings[bool_index,]$bedrooms+8.5)))
    listings[bool_index, 'fixed_cost'] <- fixed_cost
    # Creating var costs column
    listings[bool_index, 'var_cost'] <- ((charges[i]/31) * (365-as.integer(listings[bool_index, ]$availability_365)))
    # Break Even Point in Year
    listings[bool_index,'bep_year'] <- listings[bool_index,]$fixed_cost/(listings[bool_index,]$revenue_365-listings[bool_index,]$var_cost)
    # Break Even Point in Turnover
    listings[bool_index,'bep_turnover'] <- listings[bool_index,]$fixed_cost/((listings[bool_index,]$revenue_365-listings[bool_index,]$var_cost)/listings[bool_index,]$revenue_365)
}
# Preparing the rooms
listings$bedrooms <- ifelse(listings$bedrooms == 0, NaN, listings$bedrooms)
listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)
