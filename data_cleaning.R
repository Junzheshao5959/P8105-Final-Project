library(data.table)
library("dplyr")
setDTthreads(threads = 12)# customize your threads

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

