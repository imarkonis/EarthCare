require(data.table)
source("./source/spatial_tools.R")
load("./data/experiment_3.rdata")  #Created in data_import

my_date <- as.Date("2016-9-16")

test_rdr <- rdr_prcp[time == my_date]
test_rdr <- merge(test_rdr, rdr_cells)

test_gpm <- gpm_d_prcp[time == my_date]
test_gpm <- merge(test_gpm, gpm_d_cells)

test_knmi <- knmi_prcp[time == my_date]
test_knmi <- merge(test_knmi, knmi_stations)
test_knmi <- test_knmi[complete.cases(test_knmi)]

test <- put_stations_to_cells(knmi_stations, gpm_d_cells)

knmi_stations <- put_stations_to_cells(noa_stations, gpm_d_cells) #Put stations to cells

##Satellite vs station

## pick knmi records that are within gpm cells
knmi_prcp_mean_cell <- merge(noa_stations[, .(id, nearest_cell)], noa_prcp)
noa_prcp_mean_cell <- noa_prcp_mean_cell[, mean(prcp, na.rm = T), .(nearest_cell, time)]
noa_prcp_mean_cell <- noa_prcp_mean_cell[, sum(V1, na.rm = T), .(nearest_cell, as.Date(time))]
colnames(noa_prcp_mean_cell) <- c("id", "time", "prcp") 

## Spatial aggregation from center -> outwards

gpm_cells_noa <- gpm_cells[id %in% noa_stations[, unique(nearest_cell)]]
#gpm_cells_noa <- gpm_cells_noa[lat < 38.3]
gpm_center <- cbind(lat = gpm_cells_noa[, mean(lat)], lon = gpm_cells_noa[, mean(lon)])
gpm_cells_noa$dist_rank <- dist_rank(gpm_cells_noa, gpm_center)

#monthly
gpm_month_prcp <- gpm_d_prcp[, sum(prcp), id]
colnames(gpm_month_prcp)[2] <- "prcp"

for(i in 1:nrow(gpm_cells_noa)){
  print(c(mean(gpm_month_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp]),
          range(gpm_month_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp])))
}

noa_month_prcp <- noa_prcp_mean_cell[, sum(prcp), id]  
colnames(noa_month_prcp)[2] <- "prcp"
for(i in 1:nrow(gpm_cells_noa)){
  print(c(mean(noa_month_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp]),
          range(noa_month_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp])))
}

#daily
my_date <- "2017-12-21"
gpm_oneday_prcp <- gpm_d_prcp[as.Date(time) == my_date]
noa_oneday_prcp <- noa_prcp_mean_cell[as.Date(time) == my_date]

for(i in 1:nrow(gpm_cells_noa)){
  print(c(mean(gpm_oneday_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp]),
          range(gpm_oneday_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp])))
}
for(i in 1:nrow(gpm_cells_noa)){
  print(c(mean(noa_oneday_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp]),
          range(noa_oneday_prcp[id %in% gpm_cells_noa[dist_rank <= i, id], prcp])))
}

#the max assign idea
gpm_test <- gpm_oneday_prcp[id %in% gpm_cells_noa$id]$prcp
noa_test <- noa_oneday_prcp$prcp
data.frame(sort(noa_test) - sort(gpm_test))






