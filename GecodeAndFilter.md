Gecode And Filter
================
Keming Zhang
11/22/2021

## Find all coordinates that are in Manhattan

``` r
#get distinct coordinate in 2020
month <- 1:12
year <- 2020:2021
coordinate_data_2020 = data.frame()
for (i in year) {
  for (j in month) {
    if (i == 2021 & j > 1) {
      break
    }
    year_temp <- as.character(i)
    month_temp <- as.character(j)
    data_path <- glue("data/citibike/{year_temp}_{month_temp}.csv")
    
    citibike_one_month <- fread(data_path) %>%
      janitor::clean_names()
    
    citibike_one_month <- citibike_one_month[,c('start_station_latitude','start_station_longitude',
                                   'end_station_latitude','end_station_longitude')] %>% 
      na.omit()
    
    print("success")
    print(j)
    
    colnames(citibike_one_month) <- c("start_station_latitude","start_station_longitude","end_station_latitude","end_station_longitude")

    start_list <- citibike_one_month[,c("start_station_latitude","start_station_longitude")]
    end_list <- citibike_one_month[,c("end_station_latitude","end_station_longitude")]
    
    colnames(start_list) <- c("latitude","longitude")
    colnames(end_list) <- c("latitude","longitude")
    
    coordinate_data_2020 <- bind_rows(coordinate_data_2020,start_list,end_list) %>%
      distinct()
  }
}
```

``` r
#get distinct coordinate in 2021
month <- 2:10
year <- 2021
coordinate_data_2021 = data.frame()
for (i in year) {
  for (j in month) {
    year_temp <- as.character(i)
    month_temp <- as.character(j)
    data_path <- glue("data/citibike/{year_temp}_{month_temp}.csv")
    
    citibike_one_month <- fread(data_path) %>%
      janitor::clean_names()
    
    citibike_one_month <- citibike_one_month[,c('start_lat','start_lng','end_lat','end_lng')] %>% 
      na.omit()
    
    print("success")
    print(j)
    
    colnames(citibike_one_month) <- c("start_station_latitude","start_station_longitude","end_station_latitude","end_station_longitude")

    start_list <- citibike_one_month[,c("start_station_latitude","start_station_longitude")]
    end_list <- citibike_one_month[,c("end_station_latitude","end_station_longitude")]
    
    colnames(start_list) <- c("latitude","longitude")
    colnames(end_list) <- c("latitude","longitude")
    
    coordinate_data_2021 <- bind_rows(coordinate_data_2021,start_list,end_list) %>%
      distinct()
  }
}
```

``` r
#combine coordinate list in 2020 and 2021
coordinate_data <- bind_rows(coordinate_data_2020,coordinate_data_2021) %>%
  distinct()
```

## Build a dictionary for neighborhoods and coordinates

``` r
#parse taxi zone geojson
geoobject_list = FROM_GeoJson(url_file_string = "data/nyc_taxi_zone.geojson")
manhattan_list = list()

for (i in 1:length(geoobject_list[[1]])) {
  if (geoobject_list$features[[i]]$properties$borough == "Manhattan") {
    manhattan_list <- append(manhattan_list,geoobject_list$features[i])
  }
}
```

``` r
#check point in which geojson object
identify_geojson <- function(latitude,longitude){
  for (i in 1:length(manhattan_list)) {
    longitude_region <- manhattan_list[[i]]$geometry$coordinates[[1]][,1]
    latitude_region <- manhattan_list[[i]]$geometry$coordinates[[1]][,2]
    if (point.in.polygon(longitude, latitude, longitude_region, latitude_region) >= 1) {
      out <- paste(as.character(manhattan_list[[i]]$properties$zone),as.character(manhattan_list[[i]]$properties$location_id),sep = "@")
      return(out)
    }
  }
  return("out")
}
```

``` r
#coordinate list to neighborhood
neighborhood_data <- coordinate_data %>%
  mutate(
    neighborhood = map2(latitude,longitude,identify_geojson)
  ) %>%
  filter(neighborhood != "out") %>%
  separate(neighborhood, c("neighborhood","neighborhood_id"), sep = "@") %>%
  mutate(
    neighborhood_id  = as.numeric(neighborhood_id)
  )

write_csv(neighborhood_data,"data/citibike_coordinate_neighborhood.csv")
```

``` r
neighborhood_data <- read_csv("data/citibike_coordinate_neighborhood.csv")
```

    ## Rows: 1601 Columns: 4

    ## ?????? Column specification ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
    ## Delimiter: ","
    ## chr (1): neighborhood
    ## dbl (3): latitude, longitude, neighborhood_id

    ## 
    ## ??? Use `spec()` to retrieve the full column specification for this data.
    ## ??? Specify the column types or set `show_col_types = FALSE` to quiet this message.

## FIlter all trips in Manhattan

``` r
#in 2020
month <- 1:12
year <- 2020:2021
trip_2020 = data.frame()
for (i in year) {
  for (j in month) {
    if (i == 2021 & j > 1) {
      break
    }
    year_temp <- as.character(i)
    month_temp <- as.character(j)
    data_path <- glue("data/citibike/{year_temp}_{month_temp}.csv")
    
    citibike_one_month <- fread(data_path) %>%
      janitor::clean_names()
    
    citibike_one_month <- citibike_one_month[,c('starttime','stoptime','start_station_latitude','start_station_longitude',
                                   'end_station_latitude','end_station_longitude')] %>% 
      na.omit()
    
    print("success")
    print(j)
    
    colnames(citibike_one_month) <- c("start_time","end_time","start_station_latitude","start_station_longitude","end_station_latitude","end_station_longitude")
    
    citibike_one_month_manhattan <- inner_join(citibike_one_month,neighborhood_data,
                                          by = c("start_station_longitude" = "longitude","start_station_latitude" = "latitude")) %>%
      inner_join(neighborhood_data, by = c("end_station_longitude" = "longitude","end_station_latitude" = "latitude")) %>%
      rename(start_neighborhood = neighborhood.x, start_neighborhood_id = neighborhood_id.x,
             end_neighborhood = neighborhood.y, end_neighborhood_id = neighborhood_id.y)
    
    trip_2020 <- bind_rows(trip_2020, citibike_one_month_manhattan)
  }
}
```

``` r
#in 2021
month <- 2:10
year <- 2021
trip_2021 = data.frame()
for (i in year) {
  for (j in month) {
    year_temp <- as.character(i)
    month_temp <- as.character(j)
    data_path <- glue("data/citibike/{year_temp}_{month_temp}.csv")
    
    citibike_one_month <- fread(data_path) %>%
      janitor::clean_names()
    
    citibike_one_month <- citibike_one_month[,c('started_at','ended_at','start_lat','start_lng','end_lat','end_lng')] %>% 
      na.omit()
    
    print("success")
    print(j)
    
    colnames(citibike_one_month) <- c("start_time","end_time","start_station_latitude","start_station_longitude","end_station_latitude","end_station_longitude")
    
    citibike_one_month_manhattan <- inner_join(citibike_one_month,neighborhood_data,
                                          by = c("start_station_longitude" = "longitude","start_station_latitude" = "latitude")) %>%
      inner_join(neighborhood_data, by = c("end_station_longitude" = "longitude","end_station_latitude" = "latitude")) %>%
      rename(start_neighborhood = neighborhood.x, start_neighborhood_id = neighborhood_id.x,
             end_neighborhood = neighborhood.y, end_neighborhood_id = neighborhood_id.y)
    
    trip_2021 <- bind_rows(trip_2021, citibike_one_month_manhattan)
  }
}
```

``` r
trip_data <- bind_rows(trip_2020,trip_2021) %>%
  write_csv("data/citibike_manhattan.csv")
```
