require(data.table)
source("./source/spatial_tools.R")
load("./data/experiment_3.rdata")  #Created in data_import

#("2016-9-01")("2016-9-04")
#("2016-9-15")("2016-9-16")

my_date <- as.Date("2016-9-03")
test_rdr <- rdr_prcp[time == my_date]
test_rdr <- merge(test_rdr, rdr_cells)

test_gpm <- gpm_d_prcp[time == my_date]
test_gpm <- merge(test_gpm, gpm_d_cells)

test_knmi <- knmi_prcp[time == my_date]
test_knmi <- merge(test_knmi, knmi_stations)
test_knmi <- test_knmi[complete.cases(test_knmi)]

ggplot(test_rdr, aes(y = lat, x = lon, col = prcp)) + 
  geom_point() +
  geom_point(data = test_gpm[id %in% knmi_stations$nearest_cell], aes(y = lat, x = lon, size = prcp), col = "red") +
  geom_point(data = test_knmi, aes(y = lat, x = lon, size = prcp), col = "yellow") +
  theme_bw()

ggplot(test_rdr, aes(y = lat, x = lon, col = prcp)) + 
  geom_point() +
  geom_point(data = test_gpm[prcp > 1 & id %in% knmi_stations$nearest_cell], aes(y = lat, x = lon, size = prcp), col = "red") +
  geom_point(data = test_knmi[prcp > 1], aes(y = lat, x = lon, size = prcp), col = "yellow") +
  theme_bw()

##Satellite vs station

## pick knmi records that are within gpm cells and estimate the mean per gpm cell
knmi_prcp_mean_cell <- merge(knmi_stations[, .(id, nearest_cell)], knmi_prcp)
knmi_prcp_mean_cell <- knmi_prcp_mean_cell[, mean(prcp, na.rm = T), .(nearest_cell, time)]
colnames(knmi_prcp_mean_cell) <- c("id", "time", "prcp") 

gpm_knmi_prcp <- gpm_d_prcp[id %in% knmi_stations$nearest_cell]

my_date <- as.Date("2016-9-03")
gpm_oneday_prcp <- gpm_knmi_prcp[as.Date(time) == my_date]
knmi_oneday_prcp <- knmi_prcp_mean_cell[as.Date(time) == my_date]

max_prcp_id <- gpm_oneday_prcp[which.max(prcp), id]

## Spatial aggregation from maximum -> outwards
gpm_cells_knmi <- gpm_d_cells[id %in% gpm_oneday_prcp$id]
gpm_center <- cbind(lon = gpm_cells_knmi[id == max_prcp_id, lon], lat = gpm_cells_knmi[id == max_prcp_id, lat])
gpm_cells_knmi$dist_rank <- dist_rank(gpm_cells_knmi, gpm_center)

for(i in 1:nrow(gpm_cells_knmi)){
  print(c(mean(gpm_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp]),
          min(gpm_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp])))
}

for(i in 1:nrow(gpm_cells_knmi)){
  print(c(mean(knmi_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp], na.rm = T),
          min(knmi_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp], na.rm = T)))
}

## Spatial aggregation from center -> outwards
gpm_cells_knmi <- gpm_d_cells[id %in% knmi_stations[, unique(nearest_cell)]]
gpm_center <- cbind(lat = gpm_cells_knmi[, mean(lat)], lon = gpm_cells_knmi[, mean(lon)])
gpm_cells_knmi$dist_rank <- dist_rank(gpm_cells_knmi, gpm_center)

#daily
my_date <- as.Date("2016-9-03")
gpm_oneday_prcp <- gpm_d_prcp[as.Date(time) == my_date]
knmi_oneday_prcp <- knmi_prcp_mean_cell[as.Date(time) == my_date]

for(i in 1:nrow(gpm_cells_knmi)){
  print(c(mean(gpm_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp]),
          range(gpm_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp])))
}
for(i in 1:nrow(gpm_cells_knmi)){
  print(c(mean(knmi_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp]),
          range(knmi_oneday_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp])))
}

#monthly
gpm_month_prcp <- gpm_d_prcp[, sum(prcp), id]
colnames(gpm_month_prcp)[2] <- "prcp"

for(i in 1:nrow(gpm_cells_knmi)){
  print(c(mean(gpm_month_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp]),
          range(gpm_month_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp])))
}

knmi_month_prcp <- knmi_prcp_mean_cell[, sum(prcp), id]  
colnames(knmi_month_prcp)[2] <- "prcp"
for(i in 1:nrow(gpm_cells_knmi)){
  print(c(mean(knmi_month_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp]),
          range(knmi_month_prcp[id %in% gpm_cells_knmi[dist_rank <= i, id], prcp])))
}




#the max assign idea
gpm_test <- gpm_oneday_prcp[id %in% gpm_cells_knmi$id]$prcp
knmi_test <- knmi_oneday_prcp$prcp
data.frame(sort(knmi_test) - sort(gpm_test))






