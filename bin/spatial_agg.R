require(data.table)

load("./data/dataset.rdata") 

## pick noa records that are within gpm cells
noa_prcp_mean_cell <- merge(noa_stations[, .(id, nearest_cell)], noa_prcp)
noa_prcp_mean_cell <- noa_prcp_mean_cell[, mean(prcp, na.rm = T), .(nearest_cell, time)]
noa_prcp_mean_cell <- noa_prcp_mean_cell[, sum(V1, na.rm = T), .(nearest_cell, as.Date(time))]
colnames(noa_prcp_mean_cell) <- c("id", "time", "prcp") 

## Spatial aggregation from center -> outwards

gpm_cells_noa <- gpm_cells[id %in% noa_stations[,unique(nearest_cell)]]
#gpm_cells_noa <- gpm_cells_noa[lat < 38.3]

gpm_center <- cbind(lat = gpm_cells_noa[, mean(lat)], lon = gpm_cells_noa[, mean(lon)])
set_sp <- SpatialPoints(gpm_cells_noa[, c('lon', 'lat')])

gpm_cells_noa <- cbind(gpm_cells_noa, dist = as.numeric(gDistance(set_sp,  SpatialPoints(gpm_center), byid = TRUE)))
gpm_cells_noa$dist_rank <- rank(gpm_cells_noa$dist)

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






