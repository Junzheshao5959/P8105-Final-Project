library(tidyverse)


citibike =
  read_csv("./citibike_manhattan_sample.csv") %>%
  mutate(
    id = seq(1:nrow(citibike))
  )

distance = function(lat1, long1, lat2, long2) {
  rad = pi/180
  lat1_rad = lat1 * rad
  lat2_rad = lat2 * rad
  dlon = (long2 - long1) * rad
  dlat = (lat2 - lat2) * rad

  a = (sin(dlat/2))^2 + cos(lat1_rad) * cos(lat2_rad) * (sin(dlon/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  R = 6378.145
  d = R * c
  return(d)
}

distance(40.71755,
         -74.01322,
         40.74488,
         -73.99530)


citibike_1 =
  citibike %>%
  select(start_station_latitude:end_station_longitude) %>%
  rename(
    long1 = start_station_longitude,
    lat1 = start_station_latitude,
    long2 = end_station_longitude,
    lat2 = end_station_latitude
      ) %>%
  mutate(
    id = seq(1:nrow(citibike_1))
  )

citibike_dis =
  citibike_1 %>% pmap(distance)


citibike_dis =
  as.data.frame(citibike_dis) %>%
  pivot_longer(
    everything(),
    #names_to = "a",
    values_to = "dist"
  ) %>%
  select(dist) %>%
  mutate(
    id = seq(1:nrow(citibike_dis))
  )

citibike_2 = inner_join(citibike_1, citibike_dis, by = "id")

citibike_3 =
  inner_join(citibike, citibike_2, by = "id") %>%
  select(-c(id, long1, lat1, long2, lat2)) %>%
  mutate(
    end_time = as.POSIXct(end_time),
    start_time = as.POSIXct(start_time)) %>%
  mutate(
    time_diff_min = round(as.numeric(difftime(end_time, start_time, units = "mins")), digits = 2)) %>%
  separate(start_time, into = c("start_date", "start_time"), sep = " ") %>%
  separate(end_time, into = c("end_date", "end_time"), sep = " ") %>%
  relocate(time_diff_min, .before = start_station_latitude) %>%
  rename(date = start_date) %>%
  select(-end_date)



if (all(citibike_3$start_date == citibike_3$start_date))
{
  print("TRUE")
}






