source("./source/graphics.R")
source("./source/spatial_tools.R")
load("./data/experiment_3.rdata")  #Created in data_import

## Align datasets in time and add lon/lat
knmi_prcp <- knmi_prcp[time >= min(gpm_d_prcp$time) & time <= max(gpm_d_prcp$time)]
rdr_prcp <- rdr_prcp[time >= min(gpm_d_prcp$time) & time <= max(gpm_d_prcp$time)]

## Align datasets in space
knmi_prcp <- merge(knmi_stations[, .(id, nearest_cell)], knmi_prcp) #Average of station prcp per GPM cell
knmi_prcp <- knmi_prcp[, mean(prcp, na.rm = T), .(nearest_cell, time)]
colnames(knmi_prcp) <- c("id", "time", "prcp") 
knmi_prcp <- merge(gpm_d_cells, knmi_prcp)

rdr_prcp <- merge(rdr_cells, rdr_prcp) #Average of radar prcp per GPM cell
rdr_prcp <- rdr_prcp[, mean(prcp, na.rm = T), .(nearest_cell, time)]
colnames(rdr_prcp) <- c("id", "time", "prcp") 
rdr_prcp <- merge(gpm_d_cells, rdr_prcp)

gpm_knmi_prcp <- gpm_d_prcp[id %in% knmi_stations$nearest_cell] #Keep only gpm cells close to station locations
gpm_rdr_prcp <- gpm_d_prcp[id %in% rdr_cells$nearest_cell] #Keep only gpm cells close to radar cells

knmi_prcp[, sum(prcp, na.rm = T)]/gpm_knmi_prcp[, sum(prcp, na.rm = T)]

## Determine wet days in stations: at least 5% have precipitation above 1 mm
wet_percent <- .05
knmi_prcp_qq_10 <- knmi_prcp[, quantile(prcp, 1 - wet_percent, na.rm = T), time]
gpm_prcp_qq_10 <- gpm_rdr_prcp[, quantile(prcp, 1 - wet_percent, na.rm = T), time]
knmi_wet_days <- knmi_prcp_qq_10[V1 > 1, time]
gpm_wet_days <- gpm_prcp_qq_10[V1 > 1, time]

match_wet_days <- match(knmi_wet_days, gpm_wet_days)
sum(is.na(match_wet_days)) / length(knmi_wet_days) # percentage of wet days missed by satellite

match_wet_days_rev <- match(gpm_wet_days, knmi_wet_days)
sum(is.na(match_wet_days_rev)) / length(gpm_wet_days) # percentage of false precipitation by satellite

## Allign wet days & add lon/lat
gpm_knmi_prcp <- gpm_knmi_prcp[time %in% knmi_wet_days]
rdr_prcp <- rdr_prcp[time %in% knmi_wet_days]

gpm_knmi_prcp <- merge(gpm_d_cells, gpm_knmi_prcp)
gpm_rdr_prcp <- merge(gpm_d_cells, gpm_rdr_prcp)

save(knmi_wet_days, gpm_wet_days, gpm_knmi_prcp, match_wet_days, match_wet_days_rev, file = "./data/experiment_3_wdays.rdata")

## Aggregation example for each day of 2016-09
my_date <- as.Date("2016-09-15")

gravity_center_gpm <- get_gravity_center(gpm_knmi_prcp[time %in% my_date & prcp], 50)
gravity_center_knmi <- get_gravity_center(knmi_prcp[time %in% my_date & prcp], 50)
gravity_center_rdr <- get_gravity_center(rdr_prcp[time %in% my_date & prcp], 50)

gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_knmi)) * 111
gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_rdr)) * 111
gDistance(SpatialPoints(gravity_center_knmi), SpatialPoints(gravity_center_rdr)) * 111

prcp_day_gpm_knmi <- agg_prcp(gpm_knmi_prcp, my_date, gravity_center_gpm)
prcp_day_gpm_rdr <- agg_prcp(gpm_rdr_prcp, my_date, gravity_center_gpm)
prcp_day_knmi <- agg_prcp(knmi_prcp, my_date, gravity_center_gpm)
prcp_day_rdr <- agg_prcp(rdr_prcp, my_date, gravity_center_gpm)

ggplot(prcp_day_gpm_knmi, aes(distance, sum, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = prcp_day_knmi, aes(distance, sum, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, sum, size = prcp)) + 
  geom_point(col = "tan4", alpha = 0.5) +
  geom_point(data = prcp_day_rdr, aes(distance, sum, size = prcp), col = "dark green", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_knmi, aes(distance, mean, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = prcp_day_gpm_rdr, aes(distance, mean, size = prcp), col = "tan4", alpha = 0.5) +
  geom_point(data = prcp_day_knmi, aes(distance, mean, size = prcp), col = "red", alpha = 0.5) +
  geom_point(data = prcp_day_rdr, aes(distance, mean, size = prcp), col = "dark green", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_knmi, aes(distance, sd/mean, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = prcp_day_gpm_rdr, aes(distance, sd/mean, size = prcp), col = "tan4", alpha = 0.5) +
  geom_point(data = prcp_day_knmi, aes(distance, sd/mean, size = prcp), col = "red", alpha = 0.5) +
  geom_point(data = prcp_day_rdr, aes(distance, sd/mean, size = prcp), col = "dark green", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation CoV") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(y = lat, x = lon, size = prcp)) + 
  geom_point(col = "orange") +
  geom_point(data = prcp_day_rdr, aes(y = lat, x = lon, size = prcp), col = "dark green", alpha = 0.5) +
  geom_point(data = prcp_day_knmi, aes(y = lat, x = lon, size = prcp), col = "red", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  theme_bw()

## Aggregation in time
my_period = rdr_prcp[, unique(time)]
for(i in 1:length(my_period)){
  period <- my_period[1:i]
  aa <- gpm_knmi_prcp[time %in% my_period, sum(prcp, na.rm = T), .(lon, lat)]
  colnames(aa)[3] <- "prcp"
  gravity_center_gpm <- get_gravity_center(aa, 50)
  #gravity_center_knmi <- get_gravity_center(knmi_prcp[time %in% my_date], 50)
  #gravity_center_rdr <- get_gravity_center(rdr_prcp[time %in% my_date], 50)
  
  #  print(c(gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_knmi)) * 111,
  #          gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_rdr)) * 111))
  #prcp_period_gpm_knmi <- agg_prcp_period(gpm_knmi_prcp, period, gravity_center_gpm)
  prcp_period_gpm_rdr <- agg_prcp_period(gpm_rdr_prcp, period, gravity_center_gpm)
  prcp_period_knmi <- agg_prcp_period(knmi_prcp, period, gravity_center_gpm)
  prcp_period_rdr <- agg_prcp_period(rdr_prcp, period, gravity_center_gpm)
  
  p1 <- ggplot(prcp_period_gpm_rdr, aes(distance, mean)) + 
    geom_smooth(col = "orange", size = 1, alpha = 0.5, se = F, span = 0.3) +
    geom_point(col = "orange", size = 1, alpha = 0.1) +
    #geom_smooth(data = prcp_period_gpm_rdr, aes(distance, mean), col = "tan4", size = 1, alpha = 0.5, se = F, span = 0.3) +
    #geom_point(data = prcp_period_gpm_rdr, aes(distance, mean), col = "tan4", size = 1, alpha = 0.1) +
    geom_smooth(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.5, se = F, span = 0.3) +
    geom_point(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.1) +
    geom_smooth(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.5, se = F, span = 0.3) +
    geom_point(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.1) +
    labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
    ggtitle(my_period[i], paste("number of days =", i)) + 
    theme_bw() 
  
  p2 <- ggplot(prcp_period_gpm_rdr, aes(distance, sd)) + 
    geom_smooth(col = "orange", size = 1, alpha = 0.5, se = F) +
    geom_point(col = "orange", size = 1, alpha = 0.1) +
    #geom_smooth(data = prcp_period_gpm_rdr, aes(distance, sd), col = "tan4", size = 1, alpha = 0.5, se = F, span = 0.2) +
    #geom_point(data = prcp_period_gpm_rdr, aes(distance, sd), col = "tan4", size = 1, alpha = 0.1) +
    geom_smooth(data = prcp_period_knmi, aes(distance, sd), col = "red", size = 1, alpha = 0.5, se = F, span = 0.2) +
    geom_point(data = prcp_period_knmi, aes(distance, sd), col = "red", size = 1, alpha = 0.1) +
    geom_smooth(data = prcp_period_rdr, aes(distance, sd), col = "dark green", size = 1, alpha = 0.5, se = F, span = 0.2) +
    geom_point(data = prcp_period_rdr, aes(distance, sd), col = "dark green", size = 1, alpha = 0.1) +
    labs(x = "Distance (km)", y = "Precipitation sd (mm)") + 
    theme_bw()
  print(grid.arrange(p1, p2, nrow = 2))
} 

  ggplot(prcp_period_gpm_knmi, aes(log10(distance), log10(sum))) + 
    geom_line(col = "orange", alpha = 0.5, size = 1) +
    geom_line(data = prcp_period_knmi, aes(log10(distance), log10(sum)), col = "red", size = 1, alpha = 0.5) +
    labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
    theme_bw()
  
  ggplot(prcp_period_gpm_rdr, aes(log10(distance), log10(sum))) + 
    geom_line(col = "tan4", alpha = 0.5, size = 1) +
    geom_line(data = prcp_period_rdr, aes(log10(distance), log10(sum)), col = "dark green", size = 1, alpha = 0.5) +
    labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
    theme_bw()
  ##light, mnoderate and heavy precipitation 0, 2.5, 7.5
  #period
  my_period = rdr_prcp[, unique(time)]
  for(i in 1:length(my_period)){
    period <- my_period[1:i]
    aa <- gpm_knmi_prcp[time %in% my_period & prcp > 7.5, sum(prcp, na.rm = T), .(lon, lat)]
    colnames(aa)[3] <- "prcp"
    gravity_center_gpm <- get_gravity_center(aa, 50)

    prcp_period_gpm_rdr <- agg_prcp_period(gpm_rdr_prcp[prcp > 7.5], period, gravity_center_gpm)
    prcp_period_knmi <- agg_prcp_period(knmi_prcp[prcp > 7.5], period, gravity_center_gpm)
    prcp_period_rdr <- agg_prcp_period(rdr_prcp[prcp > 7.5], period, gravity_center_gpm)
    
    p1 <- ggplot(prcp_period_gpm_rdr, aes(distance, mean)) + 
      geom_smooth(col = "orange", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(col = "orange", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.1) +
      labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
      ggtitle(my_period[i], paste("number of days =", i)) + 
      theme_bw() 
    
    p2 <- ggplot(prcp_period_gpm_rdr, aes(distance, sd)) + 
      geom_smooth(col = "orange", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(col = "orange", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_knmi, aes(distance, sd), col = "red", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_knmi, aes(distance, sd), col = "red", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_rdr, aes(distance, sd), col = "dark green", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_rdr, aes(distance, sd), col = "dark green", size = 1, alpha = 0.1) +
      labs(x = "Distance (km)", y = "Precipitation sd (mm)") + 
      theme_bw()
    print(grid.arrange(p1, p2, nrow = 2))
  } 
  
  my_period = rdr_prcp[, unique(time)]
  for(i in 1:length(my_period)){
    period <- my_period[1:i]
    aa <- gpm_knmi_prcp[time %in% my_period & prcp < 2.5, sum(prcp, na.rm = T), .(lon, lat)]
    colnames(aa)[3] <- "prcp"
    gravity_center_gpm <- get_gravity_center(aa, 50)
    
    prcp_period_gpm_rdr <- agg_prcp_period(gpm_rdr_prcp[prcp < 2.5], period, gravity_center_gpm)
    prcp_period_knmi <- agg_prcp_period(knmi_prcp[prcp < 2.5], period, gravity_center_gpm)
    prcp_period_rdr <- agg_prcp_period(rdr_prcp[prcp < 2.5], period, gravity_center_gpm)
    
    p1 <- ggplot(prcp_period_gpm_rdr, aes(distance, mean)) + 
      geom_smooth(col = "orange", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(col = "orange", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.1) +
      labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
      ggtitle(my_period[i], paste("number of days =", i)) + 
      theme_bw() 
    
    p2 <- ggplot(prcp_period_gpm_rdr, aes(distance, sd)) + 
      geom_smooth(col = "orange", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(col = "orange", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_knmi, aes(distance, sd), col = "red", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_knmi, aes(distance, sd), col = "red", size = 1, alpha = 0.1) +
      geom_smooth(data = prcp_period_rdr, aes(distance, sd), col = "dark green", size = 1, alpha = 0.5, se = F, span = 0.3) +
      geom_point(data = prcp_period_rdr, aes(distance, sd), col = "dark green", size = 1, alpha = 0.1) +
      labs(x = "Distance (km)", y = "Precipitation sd (mm)") + 
      theme_bw()
    print(grid.arrange(p1, p2, nrow = 2))
  } 
  
  #Can also compare sums of light vs heavy as light cells are more
  