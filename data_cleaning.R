library(data.table)
library("dplyr")
library(lubridate)
library(dtplyr)
setDTthreads(threads = 12)# customize your threads
# data discovery --------------------
test_dt = fread("raw_data/data/fhv/2020_1.csv")
zone_dt = fread("taxi+_zone_lookup (2).csv")
location_list = zone_dt[Borough == "Manhattan"][,list(LocationID,Zone)]
PUloation_list = location_list[,.(LocationID,PUZone = Zone)]
DOloation_list = location_list[,.(LocationID,DOZone = Zone)]
test_dt_zone = merge.data.table(test_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
test_dt_zone = merge.data.table(test_dt_zone, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")

# read, clean, and sample -----------
res_dt = data.table(matrix( ncol = 9, nrow = 0))
colnames(res_dt) = colnames(test_dt_zone)
for (x in list.files("raw_data/data/fhv")){
  temp_dt = fread(paste("raw_data/data/fhv/",x,sep =""))
  temp_dt = merge.data.table(temp_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
  temp_dt = merge.data.table(temp_dt, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
  temp_dt = as.data.table(sample_n(as_tibble(temp_dt),round(nrow(temp_dt)/10)))# random sample 10%
  res_dt = rbind(res_dt,temp_dt)
  print(x)
  print(nrow(temp_dt))
  print(nrow(res_dt))
}

# yellow taxi ------------------------
test_dt = fread("raw_data/data/yellow_taxi/2020_1.csv")
zone_dt = fread("taxi+_zone_lookup (2).csv")
location_list = zone_dt[Borough == "Manhattan"][,list(LocationID,Zone)]
PUloation_list = location_list[,.(LocationID,PUZone = Zone)]
DOloation_list = location_list[,.(LocationID,DOZone = Zone)]
test_dt_zone = merge.data.table(test_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
test_dt_zone = merge.data.table(test_dt_zone, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
# read, clean, and sample -----------
res_dt_y = data.table(matrix( ncol = 0, nrow = 0))
#colnames(res_dt_y) = colnames(test_dt_zone)
for (x in list.files("raw_data/data/yellow_taxi")){
  temp_dt = fread(paste("raw_data/data/yellow_taxi/",x,sep =""))
  temp_dt = merge.data.table(temp_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
  temp_dt = merge.data.table(temp_dt, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
  temp_dt = as.data.table(sample_n(as_tibble(temp_dt),round(nrow(temp_dt)/10)))# random sample 10%
  cat(colnames(temp_dt))
  res_dt_y = rbind(res_dt_y,temp_dt, fill = T)
  print(x)
  print(nrow(temp_dt))
  print(nrow(res_dt_y))
}









# t test preparation: --------------------
timeDate <- as.POSIXct("2015-10-19 10:15")   
str(timeDate)
unlist(timeDate)
datetime <- as.POSIXct("2020-01-01T00:55:04Z",
                                    format = "%Y-%m-%dT%H:%M:%SZ",
                                    tz = "America/New_York")
datetime2 <- as.POSIXct("2020-01-01T00:56:18Z",
                       format = "%Y-%m-%dT%H:%M:%SZ",
                       tz = "America/New_York")
x = as.numeric(datetime2-datetime)

format(datetime,"%m")
format(datetime,"%w")
#time -------------------------------
test_dt_zone
library(geosphere)
x = distm(c(40.73140, -73.99698), c(40.73140, -73.97303), fun = distHaversine)
y = distm(c(40.76426, -73.97303), c(40.73140, -73.97303), fun = distHaversine)
manhattan_angle = atan(x/y)[1,1]
#  bike_dt --------------------------------
bike_dt = fread("citibike_manhattan_sample.csv")
bike_dt = bike_dt %>% janitor::clean_names() %>% as.data.table()
head(bike_dt)
bike_dt = bike_dt[, distance_hav := distHaversine(matrix(c(start_station_longitude, start_station_latitude), ncol = 2),
                                   matrix(c(end_station_longitude, end_station_latitude), ncol = 2))]
bike_dt = bike_dt[, distance_ew := distHaversine(matrix(c(start_station_longitude, start_station_latitude), ncol = 2),
                                                  matrix(c(end_station_longitude, start_station_latitude), ncol = 2))]
bike_dt = bike_dt[, distance_ns := distHaversine(matrix(c(end_station_longitude, start_station_latitude), ncol = 2),
                                                  matrix(c(end_station_longitude, end_station_latitude), ncol = 2))]

bike_dt = bike_dt[,angle_ns := atan(distance_ew/distance_ns)*2*(as.numeric(I(start_station_longitude < end_station_longitude))-0.5)*2*(as.numeric(I(start_station_latitude > end_station_latitude))-0.5) - manhattan_angle]
bike_dt = bike_dt[,distance := distance_hav*(abs(cos(angle_ns))+abs(sin(angle_ns)))]
colnames(bike_dt)
#bike_dt = lazy_dt(bike_dt)
#bike_dt %>% mutate(distance = distm(c(start_station_latitude,start_station_longitude),c(end_station_latitude,end_station_longitude),fun = distHaversine))
#bike_dt %>% filter(gender == 0)

#(as.numeric(I(bike_dt$start_station_longitude < bike_dt$end_station_longitude))-0.5)*2
#as.numeric(I(start_station_latitude > end_station_latitude))

fwrite(bike_dt,'bike_dt.csv')


colnames(test_dt_zone)

# green taxi ------------------------
test_dt = fread("raw_data/data/green_taxi/2020_1.csv")
zone_dt = fread("taxi+_zone_lookup (2).csv")
location_list = zone_dt[Borough == "Manhattan"][,list(LocationID,Zone)]
PUloation_list = location_list[,.(LocationID,PUZone = Zone)]
DOloation_list = location_list[,.(LocationID,DOZone = Zone)]
test_dt_zone = merge.data.table(test_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
test_dt_zone = merge.data.table(test_dt_zone, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
# read, clean, and sample
res_dt_g = data.table(matrix(ncol = 0, nrow = 0))
#colnames(res_dt) = colnames(test_dt_zone)
for (x in list.files("raw_data/data/green_taxi")){
  temp_dt = fread(paste("raw_data/data/green_taxi/",x,sep =""))
  temp_dt = merge.data.table(temp_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
  temp_dt = merge.data.table(temp_dt, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
  temp_dt = as.data.table(sample_n(as_tibble(temp_dt),round(nrow(temp_dt)/10)))# random sample 10%
  #cat(colnames(temp_dt))
  res_dt_g = rbind(res_dt_g,temp_dt, fill = T)
  print(x)
  print(nrow(temp_dt))
  print(nrow(res_dt_g))
}

# clean bike ------------------
bike_dt_select = data.table(DOLocationID = bike_dt$start_neighborhood_id,
                            PULocationID = bike_dt$end_neighborhood_id,
                            tpep_pickup_datetime = bike_dt$start_time,
                            tpep_dropoff_datetime = bike_dt$end_time,
                            trip_distance = bike_dt$distance,
                            PUZone = bike_dt$start_neighborhood,
                            DOZone = bike_dt$end_neighborhood)
bike_dt_select$type = "bike"
#test_dt_zone = fread('taxi_test_dt.csv')
setnames(res_dt_g, "lpep_dropoff_datetime", "tpep_dropoff_datetime")
setnames(res_dt_g, "lpep_pickup_datetime", "tpep_pickup_datetime")
test_dt_zone = rbind(res_dt_g, res_dt_y, fill = T)
fwrite(test_dt_zone,'taxi_test_dt.csv')
test_dt_zone = test_dt_zone[,.(DOLocationID,PULocationID,tpep_pickup_datetime,tpep_dropoff_datetime,trip_distance,PUZone,DOZone)]
test_dt_zone = test_dt_zone[, trip_distance := 1609.344* trip_distance]
test_dt_zone$type = "taxi"
test_dt = rbind(test_dt_zone,bike_dt_select)

test_dt = test_dt[trip_distance > 100]
test_dt = test_dt[,month := format(tpep_pickup_datetime,"%m")]
test_dt = test_dt[,week := format(tpep_pickup_datetime,"%w")]
test_dt = test_dt[,time:= as.numeric(tpep_dropoff_datetime - tpep_pickup_datetime)]
test_dt = test_dt[,velocity := trip_distance/time]
test_dt = test_dt[time > 60]
test_dt = test_dt[velocity > 0] [velocity < 40]
#test_dt %>% plot(distance ~ time)

#format(datetime,"%m")
#format(datetime,"%w")
month_in = 1
week_in = 1
PU ='Alphabet City'
DO = 'Alphabet City'

fwrite(test_dt, 'test_dt_V1.csv')

test_function = function(month_in, week_in, data = test_dt,PU,DO, distance_range_low = 0, distance_range_up = 6){
  x = test_dt[month %in% month_in][week %in% week_in][type == 'taxi'][trip_distance >= 1000*distance_range_low][trip_distance <= 1000*distance_range_up][PUZone == PU][DOZone == DO][,.(velocity)]
  print(x)
  y = test_dt[month %in% month_in][week %in% week_in][type == 'bike'][trip_distance >= 1000*distance_range_low][trip_distance <= 1000*distance_range_up][PUZone == PU][DOZone == DO][,.(velocity)]
  print(y)
  t.test(x,y)
}
test_function(month_in = c("01","06","07"),week_in = c(1,2), PU = "Washington Heights South", DO = "Morningside Heights",
              distance_range_low = 0, distance_range_up = 20)

x = c(1,2,3,4,5)
y = c(1,2,3)
res = t.test(x,y)

             