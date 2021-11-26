library(data.table)
library("dplyr")
library("dtplyr")

setDTthreads(threads = 12)# customize your threads
# data discovery --------------------
# import fhvhv dataset
test_dt = fread("data/fhvhv/2020_1.csv")
zone_dt = fread("data/taxi+_zone_lookup.csv")
location_list = zone_dt[Borough == "Manhattan"][,list(LocationID,Zone)]
PUloation_list = location_list[,.(LocationID,PUZone = Zone)]
DOloation_list = location_list[,.(LocationID,DOZone = Zone)]
test_dt_zone = merge.data.table(test_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
test_dt_zone = merge.data.table(test_dt_zone, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")

# read, clean, and sample -----------
res_dt = data.table(matrix( ncol = 0, nrow = 0))
colnames(res_dt) = colnames(test_dt_zone)
for (x in list.files("data/fhvhv")) {
  temp_dt = fread(paste("data/fhvhv/",x,sep = ""))
  temp_dt = merge.data.table(temp_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
  temp_dt = merge.data.table(temp_dt, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
  temp_dt = as.data.table(sample_n(as_tibble(temp_dt),round(nrow(temp_dt)/100)))# random sample 10%
  res_dt = rbind(res_dt,temp_dt, fill = TRUE)
  print(x)
  print(nrow(temp_dt))
  print(nrow(res_dt))
}

# Clean and filter data on fhvhv
# Note it exist a winter time change at 2020-11-01 2am.
res_dt_2 <-
  res_dt %>%
  select(DOLocationID, PULocationID, pickup_datetime, dropoff_datetime, PUZone, DOZone) %>%
  mutate(diff_datetime = dropoff_datetime - pickup_datetime) %>%
  as_tibble()

for (x in res_dt_2$dropoff_datetime) {
  if (format(x, format = '%Y-%m-%d %H') == "2020-11-01 01") {
    x = format(x, format = '%Y-%m-%d %H+1:%M:%S')
  } else {
    x = x
  }
}


