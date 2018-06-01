source("./source/graphics.R")
source("./source/spatial_tools.R")
load("./data/experiment_3_main.rdata")  

# The comparison between gpm_rdr_prcp & gpm_knmi_prcp showed very small differences and thus gpm_rdr_prcp is used

## Aggregation example for each day of 2016-09
my_date <- as.Date("2016-09-02")
no_points <- 1

gravity_center_gpm <- get_gravity_center(gpm_rdr_prcp[time %in% my_date], no_points) 
gravity_center_knmi <- get_gravity_center(knmi_prcp[time %in% my_date], no_points)
gravity_center_rdr <- get_gravity_center(rdr_prcp[time %in% my_date], no_points)

prcp_day_gpm_rdr <- agg_prcp(gpm_rdr_prcp, my_date, gravity_center_gpm)
prcp_day_knmi <- agg_prcp(knmi_prcp, my_date, gravity_center_knmi)
prcp_day_rdr <- agg_prcp(rdr_prcp, my_date, gravity_center_rdr)

ggplot(prcp_day_gpm_knmi, aes(distance, sum, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = prcp_day_knmi, aes(distance, sum, size = prcp), col = "red", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, sum, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = prcp_day_rdr, aes(distance, sum, size = prcp), col = "dark green", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, mean, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = prcp_day_knmi, aes(distance, mean, size = prcp), col = "red", alpha = 0.5) +
  geom_point(data = prcp_day_rdr, aes(distance, mean, size = prcp), col = "dark green", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, sd, size = prcp)) + 
  geom_point(col = "orange", alpha = 0.5) +
  geom_point(data = prcp_day_knmi, aes(distance, sd, size = prcp), col = "red", alpha = 0.5) +
  geom_point(data = prcp_day_rdr, aes(distance, sd, size = prcp), col = "dark green", alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation St. Dev.") + 
  theme_bw()

map_plot(ground = prcp_day_knmi, radar = prcp_day_rdr, satellite = prcp_day_gpm_rdr, date = my_date)
#map_plot(ground = prcp_day_knmi, date = my_date)
#map_plot(radar = prcp_day_rdr, date = my_date)
#map_plot(satellite = prcp_day_gpm_rdr, date = my_date)

## Aggregation in time
my_period <- rdr_prcp[, unique(time)]
my_period <- my_period[c(1, 5, 15, 30, 90, length(my_period))]
for(i in 1:length(my_period)){
  period <- my_period[1:i]
  
  aa <- gpm_rdr_prcp[time %in% period, sum(prcp, na.rm = T), .(lat, lon)]
  colnames(aa)[3] <- "prcp"
  gravity_center_gpm <- get_gravity_center(aa, no_points)
  
  bb <- knmi_prcp[time %in% period, sum(prcp, na.rm = T), .(lat, lon)]
  colnames(bb)[3] <- "prcp"
  gravity_center_knmi <- get_gravity_center(bb, no_points)
  
  dd <- rdr_prcp[time %in% period, sum(prcp, na.rm = T), .(lat, lon)]
  colnames(dd)[3] <- "prcp"
  gravity_center_rdr <- get_gravity_center(dd, no_points)
  
  prcp_period_gpm_rdr <- agg_prcp_period(gpm_rdr_prcp, period, gravity_center_gpm)
  prcp_period_knmi <- agg_prcp_period(knmi_prcp, period, gravity_center_knmi)
  prcp_period_rdr <- agg_prcp_period(rdr_prcp, period, gravity_center_rdr)
  
  p1 <- ggplot(prcp_period_gpm_rdr, aes(distance, mean)) + 
    geom_smooth(col = "orange", size = 1, alpha = 0.5, se = F, span = 0.3) +
    geom_point(col = "orange", size = 1, alpha = 0.1) +
    geom_smooth(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.5, se = F, span = 0.3) +
    geom_point(data = prcp_period_knmi, aes(distance, mean), col = "red", size = 1, alpha = 0.1) +
    geom_smooth(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.5, se = F, span = 0.3) +
    geom_point(data = prcp_period_rdr, aes(distance, mean), col = "dark green", size = 1, alpha = 0.1) +
    labs(x = "Distance (km)", y = "Precipitation mean (mm)") + 
    ggtitle(my_period[i], my_period[i] - my_period[1]) + 
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
  
  #p3 <- map_plot(radar = rdr_prcp, satellite = gpm_rdr_prcp,ground = knmi_prcp, date = period)
  
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
  gravity_center_gpm <- get_gravity_center(aa, no_points)
  
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
  gravity_center_gpm <- get_gravity_center(aa, no_points)
  
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



##Comparison of gravity Centers

gravity_center_knmi <- get_gravity_center(knmi_prcp[time %in% my_date & prcp], no_points)
gravity_center_rdr <- get_gravity_center(rdr_prcp[time %in% my_date & prcp], no_points)

gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_knmi)) * 111
gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_rdr)) * 111
gDistance(SpatialPoints(gravity_center_knmi), SpatialPoints(gravity_center_rdr)) * 111

#Can also compare sums of light vs heavy as light cells are more


