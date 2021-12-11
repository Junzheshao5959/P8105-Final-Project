library(data.table)
library("dplyr")
library(stringr)
library(tidyverse)
library(ggplot2)
library(lubridate)
setDTthreads(threads = 12)# customize your threads

# graph setting
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

theme_set(theme_bw() + theme(legend.position = "bottom"))

# green taxi --------------------
gtaxi_test_dt = fread("raw_data/data/green_taxi/2020_1.csv")
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
ytaxi_test_dt = fread("raw_data/data/yellow_taxi/2020_1.csv")
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


## combine all cars dataset --------

car_df = bind_rows(gtaxi_df, ytaxi_df)

# create factor columns time_of_day and seasons, filter observations that have distance
# and duration greater than 99 percentile

car_df =
  car_df %>%
  mutate(duration = as.numeric(round(difftime(dropoff_datetime, pickup_datetime, units = "mins"), digits = 2)),
         DOLocationID = factor(DOLocationID),
         PULocationID = factor(PULocationID),
         loc_pair = str_c(DOLocationID,"-", PULocationID),
         month = month(pickup_datetime),
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
         time_of_day = factor(time_of_day, levels = c("early_morning", "rush_hour_am", "midday", "rush_hour_pm", "evening")),
         speed = round(trip_distance/duration, digits = 2)) %>%
  filter(
    trip_distance > 0 & trip_distance < quantile(trip_distance, 0.99),
    duration > 0 & duration < quantile(duration, 0.99),
    speed > 0 & speed < quantile(speed, 0.99))


skimr::skim(car_df)

car_df %>%
  select(loc_pair) %>%
  n_distinct()


car_df %>%
  ggplot(aes(trip_distance)) +
  geom_histogram(color = "black", fill = "white")

car_df %>%
  ggplot(aes(speed)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "white")

# visualization for duration by time of day and seasons

car_df %>%
  group_by(season) %>%
  summarize(mean = mean(duration))

car_df %>%
  group_by(time_of_day) %>%
  summarize(mean = mean(duration))


car_df %>%
  group_by(season) %>%
  mutate(grp_mean = mean(duration)) %>%
  ggplot(aes(x = duration)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color="black", fill="lightblue") +
  geom_vline(aes(xintercept = grp_mean), color = "blue", linetype = "dashed", size = 0.5) +
  facet_grid(season ~.) +
  labs(title = "Duration of Car Histogram By Seasons", x = "Duration", y ="Density")

car_df %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(duration)) %>%
  ggplot(aes(x = duration)) +
  geom_histogram(aes(y = ..density..), binwidth = 1,color="black", fill = "lightblue") +
  geom_vline(aes(xintercept = grp_mean), color = "blue", linetype = "dashed", size = 0.5) +
  facet_grid(time_of_day ~ season) +
  labs(title = "Duration of Car Histogram By Time of the Day", x = "Duration", y = "Count")


car_df %>%
  ggplot(aes(x = time_of_day, y = duration)) +
  geom_boxplot(aes(fill = time_of_day)) +
  facet_grid(season ~ .) +
  labs(title = "Duration of Car Boxplot", x = "Time of the day", y ="Duration")

car_df %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(duration)) %>%
  select(season, time_of_day, grp_mean) %>%
  ggplot(aes(season, time_of_day)) +
  geom_tile(aes(fill = grp_mean)) +
  scale_fill_gradient(low = "green", high = "black")

# look at speed

car_df %>%
  group_by(season) %>%
  summarize(mean = mean(speed))

car_df %>%
  group_by(time_of_day) %>%
  summarize(mean = mean(speed))

car_df %>%
  group_by(season, time_of_day) %>%
  summarize(mean = mean(speed))


car_df %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(speed)) %>%
  ggplot(aes(x = speed)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.02,color="black", fill = "lightblue") +
  geom_vline(aes(xintercept = grp_mean), color = "blue", linetype = "dashed", size = 0.5) +
  facet_grid(time_of_day ~ season) +
  labs(title = "Speed of Car Histogram By Time of the Day", x = "Speed", y = "Count")

car_df %>%
  ggplot(aes(x = time_of_day, y = speed)) +
  geom_boxplot(aes(fill = time_of_day)) +
  facet_grid(season ~ .) +
  labs(title = "Speed of Car Boxplot", x = "Time of the day", y ="Speed")

car_df %>%
  group_by(season, time_of_day) %>%
  mutate(grp_mean = mean(speed)) %>%
  select(season, time_of_day, grp_mean) %>%
  ggplot(aes(season, time_of_day)) +
  geom_tile(aes(fill = grp_mean)) +
  scale_fill_gradient(low = "green", high = "black")

