library(data.table)
library("dplyr")
setDTthreads(threads = 12)# customize your threads
# data discovery --------------------
test_dt = fread("data/fhvhv/2020_1.csv")
zone_dt = fread("data/taxi+_zone_lookup (2).csv")
location_list = zone_dt[Borough == "Manhattan"][,list(LocationID,Zone)]
PUloation_list = location_list[,.(LocationID,PUZone = Zone)]
DOloation_list = location_list[,.(LocationID,DOZone = Zone)]
test_dt_zone = merge.data.table(test_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
test_dt_zone = merge.data.table(test_dt_zone, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")

# read, clean, and sample -----------
res_dt = data.table(matrix( ncol = 0, nrow = 0))
colnames(res_dt) = colnames(test_dt_zone)
for (x in list.files("data/fhvhv")){
  temp_dt = fread(paste("data/fhvhv/",x,sep =""))
  temp_dt = merge.data.table(temp_dt, PUloation_list, by.y = "LocationID", by.x = "PULocationID")
  temp_dt = merge.data.table(temp_dt, DOloation_list, by.y = "LocationID", by.x = "DOLocationID")
  temp_dt = as.data.table(sample_n(as_tibble(temp_dt),round(nrow(temp_dt)/100)))# random sample 10%
  res_dt = rbind(res_dt,temp_dt, fill = TRUE)
  print(x)
  print(nrow(temp_dt))
  print(nrow(res_dt))
}
res_dt <-


