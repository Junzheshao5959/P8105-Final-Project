library(tidyverse)
library(data.table)
library(geojsonR)
library(sp)

#read subway data
subway_data = fread('data/subway/stop_times.txt', 'sep' = ',')

stop_data = fread('data/subway/stops.txt','sep' = ',') %>%
  select(stop_id,stop_lat,stop_lon)


#stops coordinate
subway_coord = inner_join(subway_data, stop_data, by = "stop_id")


#parse taxi zone geojson
geoobject_list = FROM_GeoJson(url_file_string = "data/nyc_taxi_zone.geojson")
manhattan_list = list()
for (i in 1:length(geoobject_list[[1]])) {
  if (geoobject_list$features[[i]]$properties$borough == "Manhattan") {
    manhattan_list = append(manhattan_list,geoobject_list$features[i])
  }
}


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


#gecode coordinate
subway_neighborhood <- subway_coord %>%
  mutate(
    neighborhood = map2(stop_lat,stop_lon,identify_geojson)
  ) %>%
  filter(neighborhood != "out") %>%
  separate(neighborhood, c("neighborhood","neighborhood_id"), sep = "@") %>%
  mutate(
    neighborhood_id  = as.numeric(neighborhood_id)
  )


#reselect data
subway_neighborhood_clean =
  subway_neighborhood %>%
  select(-c(stop_headsign, pickup_type, drop_off_type, shape_dist_traveled)) %>%
  separate(trip_id, sep = "_", into = c("day", "train_id", "line")) %>%
  mutate(
    day = ifelse(str_detect(day, "Weekday"), "Weekday", ifelse(str_detect(day,"Sunday"), "Sunday", "Saturday"))
  )

write_csv(subway_neighborhood_clean, "data/subway/subway_schedule_clean.csv", append = FALSE)

