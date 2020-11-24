listings_sevilla_neigh <- listings_full[listings_full$city=='sevilla', ] %>%
                         group_by(neighbourhood_cleansed) %>%
                        select (neighbourhood_cleansed,latitude,longitude,accommodates,bedrooms,availability_365,revenue_365,bep_year,bep_turnover)

listings_sevilla_neigh <-
    listings_sevilla_neigh %>%
    summarise(
        latitude = mean(latitude),
        longitude = mean(longitude),
        accommodates = mean(accommodates),
        bedrooms = mean(bedrooms),
        availability_365 = mean(availability_365),
        revenue_365 = mean(revenue_365),
        bep_year = mean(bep_year),
        bep_turnover = mean(bep_turnover)
    )
