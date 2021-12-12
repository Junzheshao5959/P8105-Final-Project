library(data.table)
library("dplyr")
library(stringr)
library(tidyverse)
library(ggplot2)
library(lubridate)

theme_set(theme_bw() + theme(legend.position = "bottom"))

setDTthreads(threads = 12)# customize your threads

set.seed(1)

# green taxi --------------------
gtaxi_test_dt = fread("raw_data/data/green_taxi/2020_5.csv")
zone_dt = fread("taxi+_zone_lookup (2).csv")
location_list = zone_dt[Borough == "Manhattan"][,list(LocationID,Zone)]
PUloation_list = location_list[,.(LocationID,PUZone = Zone)]
DOloation_list = location_list[,.(LocationID,DOZone = Zone)]
gtest_dt_zone = merge.data.table(gtaxi_test_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
gtest_dt_zone = merge.data.table(gtest_dt_zone, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")

# read, clean, and sample -----------
gtaxi_res_dt = data.table(matrix(ncol = 0, nrow = 0))
for (x in list.files("raw_data/data/green_taxi")){
  gtaxi_dt = fread(paste("raw_data/data/green_taxi/", x, sep =""))
  gtaxi_dt = merge.data.table(gtaxi_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
  gtaxi_dt = merge.data.table(gtaxi_dt, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
  gtaxi_dt = as.data.table(sample_n(as_tibble(gtaxi_dt),round(nrow(gtaxi_dt)/100)))# random sample 10%
  gtaxi_res_dt = rbind(gtaxi_res_dt, gtaxi_dt, fill = TRUE)
  #cat(colnames(gtaxi_res_dt))
  print(x)
  print(nrow(gtaxi_dt))
  print(nrow(gtaxi_res_dt))
}

gtaxi_clean = gtaxi_res_dt[,c(1,2,4,5,9,21,22)]

gtaxi_df =
  tibble(gtaxi_clean) %>%
  rename(dropoff_datetime = lpep_dropoff_datetime,
         pickup_datetime = lpep_pickup_datetime)

# yellow taxi --------------------
ytaxi_test_dt = fread("raw_data/data/yellow_taxi/2020_5.csv")
ytest_dt_zone = merge.data.table(ytaxi_test_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
ytest_dt_zone = merge.data.table(ytest_dt_zone, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")

# read, clean, and sample -----------
ytaxi_res_dt = data.table(matrix(ncol = 0, nrow = 0))

for (x in list.files("raw_data/data/yellow_taxi")) {
  ytaxi_dt = fread(paste("raw_data/data/yellow_taxi/",x,sep = ""))
  ytaxi_dt = merge.data.table(ytaxi_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
  ytaxi_dt = merge.data.table(ytaxi_dt, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
  ytaxi_dt = as.data.table(sample_n(as_tibble(ytaxi_dt),round(nrow(ytaxi_dt)/100)))# random sample 10%
  ytaxi_res_dt = rbind(ytaxi_res_dt,ytaxi_dt, fill = TRUE)
  #print(x)
  #print(nrow(ytaxi_dt))
  #print(nrow(ytaxi_res_dt))
}

ytaxi_clean = ytaxi_res_dt[,c(1,2,4,5,7,19,20)]


ytaxi_df =
  tibble(ytaxi_clean) %>%
  rename(dropoff_datetime = tpep_dropoff_datetime,
         pickup_datetime = tpep_pickup_datetime)


## combine all taxi dataset --------

taxi_df = bind_rows(gtaxi_df, ytaxi_df)

# create factor columns time_of_day and seasons, filter observations that have distance
# and duration greater than 99 percentile

taxi_df =
  taxi_df %>%
  mutate(duration = as.numeric(round(difftime(dropoff_datetime, pickup_datetime, units = "mins"), digits = 2)),
         DOLocationID = factor(DOLocationID),
         PULocationID = factor(PULocationID),
         loc_pair = str_c(DOLocationID,"-", PULocationID),
         month = month(pickup_datetime),
         day_type = case_when(
           wday(pickup_datetime, label = TRUE, abbr = FALSE) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "weekday",
           wday(pickup_datetime, label =TRUE, abbr = FALSE) %in% c("Saturday", "Sunday") ~ "weekend"),
         season = case_when(
           month(pickup_datetime) %in% c(12,1,2)  ~ "winter",
           month(pickup_datetime) %in% c(3,4,5)  ~ "spring",
           month(pickup_datetime) %in% c(6,7,8)  ~ "summer",
           month(pickup_datetime) %in% c(9,10,11)  ~ "fall"
         ),
         time_of_day = case_when(
           hour(pickup_datetime) %in% c(7,8,9) ~ "rush_hour_am",
           hour(pickup_datetime) %in% c(10,11,12,13,14,15) ~ "midday",
           hour(pickup_datetime) %in% c(16,17,18) ~ "rush_hour_pm",
           hour(pickup_datetime) %in% c(19,20,21,22,23) ~ "evening",
           hour(pickup_datetime) %in% c(00,1,2,3,4,5,6) ~ "early_morning"
         )
  ) %>%
  mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")),
         time_of_day = factor(time_of_day, levels = c("early_morning", "rush_hour_am", "midday", "rush_hour_pm", "evening"))) %>%
  filter(
    trip_distance > 0 & trip_distance < quantile(trip_distance, 0.99),
    duration > 0 & duration < quantile(duration, 0.99)) %>%
  mutate(velocity = round(trip_distance/duration*96.56, digits = 2)) %>%
  filter(velocity > 0 & velocity < quantile(velocity, 0.99))

#get a small sample taxi data to make plots
taxi_df_sample = sample_n(taxi_df, round(nrow(taxi_df)/100))

# taxi_df_sample %>%
#   group_by(season) %>%
#   summarize(n = n())
#
# taxi_df %>%
#   group_by(season) %>%
#   summarize(n = n())

# taxi heatmap for duration

taxi_df_sample %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(duration)) %>%
  select(season, time_of_day, grp_mean) %>%
  ggplot(aes(season, time_of_day)) +
  geom_tile(aes(fill = grp_mean), color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_gradient(low = "lightblue", high = "black") +
  geom_text(aes(label = round(grp_mean, digits = 2), color = "white")) +
  labs(title = "Duration of Taxi Heatmap", x = "Season", y ="Time of Day", fill = "Mean Duration (mins)")

# taxi heatmap for velocity
taxi_df_sample %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(velocity)) %>%
  select(season, time_of_day, grp_mean) %>%
  ggplot(aes(season, time_of_day)) +
  geom_tile(aes(fill = grp_mean), color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_gradient(low = "red", high = "white") +
  geom_text(aes(label = round(grp_mean, digits = 2))) +
  labs(title = "Velocity of Taxi Heatmap", x = "Season", y ="Time of Day", fill = "Mean Velocity(km/h)")

## bike data -----------------------
df = read_csv("./raw_data/test_dt_V1.csv")

bike_df =
  df %>%
  filter(type == "bike") %>%
  select(-c(type, month, week, time, velocity)) %>%
  rename(pickup_datetime = tpep_pickup_datetime,
         dropoff_datetime = tpep_dropoff_datetime) %>%
  mutate(duration = as.numeric(round(difftime(dropoff_datetime, pickup_datetime, units = "mins"), digits = 2)),
         DOLocationID = factor(DOLocationID),
         PULocationID = factor(PULocationID),
         loc_pair = str_c(DOLocationID,"-", PULocationID),
         month = month(pickup_datetime),
         day_type = case_when(
           wday(pickup_datetime, label = TRUE, abbr = FALSE) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "weekday",
           wday(pickup_datetime, label =TRUE, abbr = FALSE) %in% c("Saturday", "Sunday") ~ "weekend"),
         season = case_when(
           month(pickup_datetime) %in% c(12,1,2)  ~ "winter",
           month(pickup_datetime) %in% c(3,4,5)  ~ "spring",
           month(pickup_datetime) %in% c(6,7,8)  ~ "summer",
           month(pickup_datetime) %in% c(9,10,11)  ~ "fall"
         ),
         time_of_day = case_when(
           hour(pickup_datetime) %in% c(7,8,9) ~ "rush_hour_am",
           hour(pickup_datetime) %in% c(10,11,12,13,14,15) ~ "midday",
           hour(pickup_datetime) %in% c(16,17,18) ~ "rush_hour_pm",
           hour(pickup_datetime) %in% c(19,20,21,22,23) ~ "evening",
           hour(pickup_datetime) %in% c(00,1,2,3,4,5,6) ~ "early_morning"
         )
  ) %>%
  mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")),
         time_of_day = factor(time_of_day, levels = c("early_morning", "rush_hour_am", "midday", "rush_hour_pm", "evening"))) %>%
  filter(
    trip_distance > 0 & trip_distance < quantile(trip_distance, 0.99),
    duration > 0 & duration < quantile(duration, 0.95)) %>%
  mutate(velocity = round(trip_distance/1600/duration*96.56, digits = 2)) %>%
  filter(velocity > 0 & velocity < quantile(velocity, 0.99))

# get a small sample bike data to make plots
bike_df_sample = sample_n(bike_df, round(nrow(bike_df)/1000))


# bike_df_sample %>%
#   group_by(season) %>%
#   summarize(n = n())
#
# bike_df %>%
#   group_by(season, time_of_day) %>%
#   summarize(grp_mean = mean(duration))
#
# bike_df_sample %>%
#   group_by(season, time_of_day) %>%
#   summarize(grp_mean = mean(duration))


# heatmap for Duration for bike

bike_df_sample %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(duration)) %>%
  select(season, time_of_day, grp_mean) %>%
  ggplot(aes(season, time_of_day)) +
  geom_tile(aes(fill = grp_mean), color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_gradient(low = "lightblue", high = "black") +
  geom_text(aes(label = round(grp_mean, digits = 2), color = "white")) +
  labs(title = "Duration of Bike Heatmap", x = "Season", y ="Time of Day", fill = "Mean Duration(mins)")

# bike heatmap for velocity

bike_df_sample %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(velocity)) %>%
  select(season, time_of_day, grp_mean) %>%
  ggplot(aes(season, time_of_day)) +
  geom_tile(aes(fill = grp_mean), color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_gradient(low = "red", high = "white") +
  geom_text(aes(label = round(grp_mean, digits = 2))) +
  labs(title = "Velocity of Bike Heatmap", x = "Season", y ="Time of Day", fill = "Mean Velocity(km/h)")

# subway data ----------------

subway = read_csv("./raw_data/test_dt_mta.csv")

subway_df =
  subway %>%
  rename(pickup_datetime = tpep_pickup_datetime,
         dropoff_datetime = tpep_dropoff_datetime) %>%
  mutate(duration = as.numeric(round(difftime(dropoff_datetime, pickup_datetime, units = "mins"), digits = 2)),
         DOLocationID = factor(DOLocationID),
         PULocationID = factor(PULocationID),
         loc_pair = str_c(DOLocationID,"-", PULocationID),
         month = month(pickup_datetime),
         day_type = case_when(
           wday(pickup_datetime, label = TRUE, abbr = FALSE) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "weekday",
           wday(pickup_datetime, label =TRUE, abbr = FALSE) %in% c("Saturday", "Sunday") ~ "weekend"),
         season = case_when(
           month(pickup_datetime) %in% c(12,1,2)  ~ "winter",
           month(pickup_datetime) %in% c(3,4,5)  ~ "spring",
           month(pickup_datetime) %in% c(6,7,8)  ~ "summer",
           month(pickup_datetime) %in% c(9,10,11)  ~ "fall"
         ),
         time_of_day = case_when(
           hour(pickup_datetime) %in% c(7,8,9) ~ "rush_hour_am",
           hour(pickup_datetime) %in% c(10,11,12,13,14,15) ~ "midday",
           hour(pickup_datetime) %in% c(16,17,18) ~ "rush_hour_pm",
           hour(pickup_datetime) %in% c(19,20,21,22,23) ~ "evening",
           hour(pickup_datetime) %in% c(00,1,2,3,4,5,6) ~ "early_morning"
         )
  ) %>%
  mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")),
         time_of_day = factor(time_of_day, levels = c("early_morning", "rush_hour_am", "midday", "rush_hour_pm", "evening"))) %>%
  filter(
    trip_distance > 0 & trip_distance < quantile(trip_distance, 0.99),
    duration > 0 & duration < quantile(duration, 0.99)) %>%
  mutate(velocity = round(trip_distance/1600/duration*96.56, digits = 2)) %>%
  filter(velocity > 0 & velocity < quantile(velocity, 0.99))

subway_df_sample = sample_n(subway_df, round(nrow(subway_df)/100))



# combine taxi, bike, subway

taxi_df_sample =
  taxi_df_sample %>%
  mutate(type = "taxi")

bike_df_sample=
  bike_df_sample %>%
  mutate(type = "bike")

subway_df_sample=
  subway_df_sample %>%
  mutate(type = "subway")

comb_df = bind_rows(taxi_df_sample, bike_df_sample, subway_df_sample)

comb_df %>%
  ggplot(aes(x = type, y = duration, fill = day_type)) +
  geom_boxplot() +
  labs(title = "Duration Boxplot", x = "Transportation Type", y = "Duration (mins)")

comb_df %>%
  ggplot(aes(x = type, y = velocity, fill = day_type)) +
  geom_boxplot() +
  labs(title = "Velocity Boxplot", x = "Transportation Type", y = "Velocity (km/h)")


