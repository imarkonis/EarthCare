require(data.table)
require(ggplot2)
require(SDMTools)

source("./source/spatial_tools.R")
load("./data/experiment_3.rdata")  #Created in data_import

#("2016-9-01")("2016-9-04")
#("2016-9-15")("2016-9-16")

my_date <- as.Date("2016-9-16")
test_rdr <- rdr_prcp[time == my_date]
test_rdr <- merge(test_rdr, rdr_cells)

test_gpm <- gpm_d_prcp[time == my_date]
test_gpm <- merge(test_gpm, gpm_d_cells)

test_knmi <- knmi_prcp[time == my_date]
test_knmi <- merge(test_knmi, knmi_stations)
test_knmi <- test_knmi[complete.cases(test_knmi)]

ggplot(test_rdr, aes(y = lat, x = lon, col = prcp)) + 
  geom_point(data = test_gpm[id %in% knmi_stations$nearest_cell], aes(y = lat, x = lon, size = prcp), col = "orange", alpha = 0.5) +
  geom_point(data = test_knmi, aes(y = lat, x = lon, size = prcp), col = "red", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  theme_bw()

##Satellite vs station

## pick knmi records that are within gpm cells and estimate the mean per gpm cell (nearest_cell)
knmi_prcp_mean_cell <- merge(knmi_stations[, .(id, nearest_cell)], knmi_prcp) #Average of station prcp per GPM cell
knmi_prcp_mean_cell <- knmi_prcp_mean_cell[, mean(prcp, na.rm = T), .(nearest_cell, time)]
colnames(knmi_prcp_mean_cell) <- c("id", "time", "prcp") 

gpm_knmi_prcp <- gpm_d_prcp[id %in% knmi_stations$nearest_cell]

gpm_oneday_prcp <- gpm_knmi_prcp[as.Date(time) == my_date]
knmi_oneday_prcp <- knmi_prcp_mean_cell[as.Date(time) == my_date]

max_prcp_id <- gpm_oneday_prcp[which.max(prcp), id]
gpm_cells_knmi <- gpm_d_cells[id %in% gpm_oneday_prcp$id]
gpm_oneday_prcp <- merge(gpm_cells_knmi, gpm_oneday_prcp)[, c(1:3, 5)]
knmi_oneday_prcp <- merge(gpm_cells_knmi, knmi_oneday_prcp)[, c(1:3, 5)]

## Spatial aggregation from gpm maximum -> outwards (both data sets at same maximum)
gpm_center <- cbind(lon = gpm_oneday_prcp[id == max_prcp_id, lon], 
                    lat = gpm_oneday_prcp[id == max_prcp_id, lat])
gpm_oneday_prcp <- cbind(gpm_oneday_prcp, 
                         dist_rank(gpm_oneday_prcp, gpm_center))
knmi_oneday_prcp <- cbind(knmi_oneday_prcp, 
                         dist_rank(gpm_oneday_prcp, gpm_center))

event_20160903_gpm <- cbind(gpm_oneday_prcp[order(rank)], 
                            agg_prcp_out(gpm_oneday_prcp))
event_20160903_knmi <- cbind(knmi_oneday_prcp[order(rank)], 
                             agg_prcp_out(knmi_oneday_prcp))

ggplot(event_20160903_gpm, aes(distance, sum, size = prcp)) + #Think something about better representing this
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, sum, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(event_20160903_gpm, aes(distance, mean, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, mean, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
  theme_bw()

ggplot(event_20160903_gpm, aes(distance, sd, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, sd, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sd (mm)") + 
  theme_bw()

## Similar to previous but aggreagation starts at gravity center of 10 highest values
gpm_oneday_prcp_10 <- gpm_oneday_prcp[tail(order(prcp), 10)]
gravity_center_10 <- COGravity(gpm_oneday_prcp_10$lon, 
                               gpm_oneday_prcp_10$lat, 
                               gpm_oneday_prcp_10$prcp)

gpm_center <- cbind(lon = as.numeric(gravity_center_10[1]),
                    lat = as.numeric(gravity_center_10[3]))

gpm_oneday_prcp_10 <- cbind(gpm_oneday_prcp[, 1:4], 
                         dist_rank(gpm_oneday_prcp, gpm_center))
knmi_oneday_prcp_10 <- cbind(knmi_oneday_prcp[, 1:4], 
                          dist_rank(gpm_oneday_prcp, gpm_center))

event_20160903_gpm <- cbind(gpm_oneday_prcp_10[order(rank)], 
                            agg_prcp_out(gpm_oneday_prcp_10))
event_20160903_knmi <- cbind(knmi_oneday_prcp_10[order(rank)], 
                             agg_prcp_out(knmi_oneday_prcp_10))

ggplot(event_20160903_gpm, aes(distance, sum, size = prcp)) + #Think something about better representing this
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, sum, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(event_20160903_gpm, aes(distance, mean, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, mean, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
  theme_bw()

ggplot(event_20160903_gpm, aes(distance, sd, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, sd, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sd (mm)") + 
  theme_bw()

## Similar to previous but aggregation starts at gravity center of each dataset
gpm_oneday_prcp_10 <- gpm_oneday_prcp[tail(order(prcp), 10)]
knmi_oneday_prcp_10 <- knmi_oneday_prcp[tail(order(prcp), 10)]
gravity_center_10_gpm <- COGravity(gpm_oneday_prcp_10$lon, 
                               gpm_oneday_prcp_10$lat, 
                               gpm_oneday_prcp_10$prcp)

gpm_center  <- cbind(lon = as.numeric(gravity_center_10[1]),
                         lat = as.numeric(gravity_center_10[3]))

gravity_center_10_knmi <- COGravity(knmi_oneday_prcp_10$lon, 
                                    knmi_oneday_prcp_10$lat, 
                                    knmi_oneday_prcp_10$prcp)

knmi_center  <- cbind(lon = as.numeric(gravity_center_10_knmi[1]),
                         lat = as.numeric(gravity_center_10_knmi[3]))


gpm_oneday_prcp_10 <- cbind(gpm_oneday_prcp[, 1:4], 
                            dist_rank(gpm_oneday_prcp, gpm_center))
knmi_oneday_prcp_10 <- cbind(knmi_oneday_prcp[, 1:4], 
                             dist_rank(gpm_oneday_prcp, knmi_center))

event_20160903_gpm <- cbind(gpm_oneday_prcp_10[order(rank)], 
                            agg_prcp_out(gpm_oneday_prcp_10))
event_20160903_knmi <- cbind(knmi_oneday_prcp_10[order(rank)], 
                             agg_prcp_out(knmi_oneday_prcp_10))

ggplot(event_20160903_gpm, aes(distance, sum, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, sum, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(event_20160903_gpm, aes(distance, mean, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, mean, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
  theme_bw()

ggplot(event_20160903_gpm, aes(distance, sd, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = event_20160903_knmi, aes(distance, sd, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sd (mm)") + 
  theme_bw()

event_20160903_diff <- data.frame(cbind(distance = event_20160903_gpm$distance, diff = event_20160903_gpm$prcp - event_20160903_knmi$prcp))
event_20160903_diff <- event_20160903_diff[complete.cases(event_20160903_diff), ]
ggplot(event_20160903_diff, aes(distance, cumsum(diff))) + 
  geom_point(col = "orange", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation Difference (mm)") + 
  theme_bw()




