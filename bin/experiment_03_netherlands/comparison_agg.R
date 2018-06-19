source("./source/graphics.R")
source("./source/spatial_tools.R")
load("./data/experiment_3_main.rdata") #Created in comparison prep 

# The comparison between gpm_rdr_prcp & gpm_knmi_prcp showed very small differences and thus gpm_rdr_prcp is used

## Aggregation example for each day of 2016-09
my_date <- as.Date("2016-09-03")
no_points <- 1

#gravity_center_gpm <- get_gravity_center(gpm_rdr_prcp[time %in% my_date], no_points) 
#gravity_center_knmi <- get_gravity_center(c, no_points)
#gravity_center_rdr <- get_gravity_center(rdr_prcp[time %in% my_date], no_points)

#prcp_day_gpm_rdr <- agg_prcp(gpm_rdr_prcp, my_date, gravity_center_gpm)
#prcp_day_knmi <- agg_prcp(prcp_knmi, my_date, gravity_center_knmi)
#prcp_day_rdr <- agg_prcp(rdr_prcp, my_date, gravity_center_rdr)

prcp_day_gpm_rdr <- gpm_rdr_prcp[time %in% my_date]
prcp_day_gpm_rdr <- agg_prcp(prcp_day_gpm_rdr, get_max_location(prcp_day_gpm_rdr))
prcp_day_knmi <- knmi_prcp[time %in% my_date]
prcp_day_knmi <- agg_prcp(prcp_day_knmi, get_max_location(prcp_day_knmi))
prcp_day_rdr <- rdr_prcp[time %in% my_date]
prcp_day_rdr <- agg_prcp(prcp_day_rdr, get_max_location(prcp_day_knmi))

transp <- 0.3

ggplot(prcp_day_gpm_rdr, aes(distance, mean)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 0.5, se = T, fill = "orange") +
  geom_smooth(data = prcp_day_knmi,  aes(distance, mean), method = "loess", col = "red", size = 1, span = 1, se = T, fill = "red") +
  geom_smooth(data = prcp_day_rdr,  aes(distance, mean), method = "loess", col = "dark green", size = 1, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Mean (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, sd/mean)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 0.5, se = T, fill = "orange") +
  geom_smooth(data = prcp_day_knmi,  aes(distance, sd/mean), method = "loess", col = "red", size = 1, span = 1, se = T, fill = "red") +
  geom_smooth(data = prcp_day_rdr,  aes(distance, sd/mean), method = "loess", col = "dark green", size = 1, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Mean (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, sum, size = prcp)) + 
  geom_point(col = "orange", alpha = transp) +
  geom_point(data = prcp_day_knmi, aes(distance, sum, size = prcp), col = "red", alpha = transp) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, sum, size = prcp)) + 
  geom_point(col = "orange", alpha = transp) +
  geom_point(data = prcp_day_rdr, aes(distance, sum, size = prcp), col = "dark green", alpha = transp) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, mean, size = prcp)) + 
  geom_point(col = "orange", alpha = transp) +
  geom_point(data = prcp_day_knmi, aes(distance, mean, size = prcp), col = "red", alpha = transp) +
  geom_point(data = prcp_day_rdr, aes(distance, mean, size = prcp), col = "dark green", alpha = transp) +
  labs(x = "Distance (km)", y = "Mean (mm)") + 
  theme_bw()

ggplot(prcp_day_gpm_rdr, aes(distance, sd/mean, size = prcp)) + 
  geom_point(col = "orange", alpha = transp) +
  geom_point(data = prcp_day_knmi, aes(distance, sd/mean, size = prcp), col = "red", alpha = transp) +
  geom_point(data = prcp_day_rdr, aes(distance, sd/mean, size = prcp), col = "dark green", alpha = transp) +
  labs(x = "Distance (km)", y = "Coef. of Variation") + 
  theme_bw()

map_plot(stations = prcp_day_knmi, radar = prcp_day_rdr, satellite = prcp_day_gpm_rdr, date = my_date)

## Aggregation in time 
my_period <- rdr_prcp[time >= my_date, unique(time)]

#Spatial scaling in monthly aggregates
period <- my_period

prcp_month_gpm_rdr <- agg_prcp_period_sum(gpm_rdr_prcp, period, 
                                      get_max_location(gpm_rdr_prcp[time %in% period]))
prcp_month_knmi <- agg_prcp_period_sum(knmi_prcp, period, 
                                   get_max_location(knmi_prcp[time %in% period]))
prcp_month_rdr <- agg_prcp_period_sum(rdr_prcp,  period, 
                                  get_max_location(rdr_prcp[time %in% period]))

ggplot(prcp_month_gpm_rdr, aes(distance, mean, size = prcp)) + 
  geom_point(col = "orange", alpha = transp) +
  geom_point(data = prcp_month_knmi, aes(distance, mean, size = prcp), col = "red", alpha = transp) +
  geom_point(data = prcp_month_rdr, aes(distance, mean, size = prcp), col = "dark green", alpha = transp) +
  labs(x = "Distance (km)", y = "Mean (mm)") + 
  theme_bw()

ggplot(prcp_month_gpm_rdr, aes(distance, sd/mean, size = prcp)) + 
  geom_point(col = "orange", alpha = transp) +
  geom_point(data = prcp_month_knmi, aes(distance, sd/mean, size = prcp), col = "red", alpha = transp) +
  geom_point(data = prcp_month_rdr, aes(distance, sd/mean, size = prcp), col = "dark green", alpha = transp) +
  labs(x = "Distance (km)", y = "Coef. of Variation") + 
  theme_bw()

#Mean daily precipitation for september
prcp_prd_gpm_rdr <- gpm_rdr_prcp[time %in% my_period[1]]
prcp_prd_gpm_rdr <- agg_prcp(prcp_prd_gpm_rdr, get_max_location(prcp_prd_gpm_rdr))
prcp_prd_gpm_rdr <- cbind(prcp_prd_gpm_rdr, time = my_period[1])

prcp_prd_knmi <- gpm_rdr_prcp[time %in% my_period[1]]
prcp_prd_knmi <- agg_prcp(prcp_prd_knmi, get_max_location(prcp_prd_knmi))
prcp_prd_knmi <- cbind(prcp_prd_knmi, time = my_period[1])

prcp_prd_rdr <- gpm_rdr_prcp[time %in% my_period[1]]
prcp_prd_rdr <- agg_prcp(prcp_prd_rdr, get_max_location(prcp_prd_rdr))
prcp_prd_rdr <- cbind(prcp_prd_rdr, time = my_period[1])

for(i in 2:length(my_period)){
  prcp_day_gpm_rdr <- gpm_rdr_prcp[time %in% my_period[i]]
  prcp_day_gpm_rdr <- agg_prcp(prcp_day_gpm_rdr, get_max_location(prcp_day_gpm_rdr))
  prcp_day_gpm_rdr <- cbind(prcp_day_gpm_rdr, time = my_period[i])
  prcp_prd_gpm_rdr <- rbind(prcp_prd_gpm_rdr, prcp_day_gpm_rdr)
  prcp_day_knmi <- knmi_prcp[time %in% my_period[i]]
  prcp_day_knmi <- agg_prcp(prcp_day_knmi, get_max_location(prcp_day_knmi))
  prcp_day_knmi <- cbind(prcp_day_knmi, time = my_period[i])
  prcp_prd_knmi <- rbind(prcp_prd_knmi, prcp_day_knmi)  
  prcp_day_rdr <- rdr_prcp[time %in% my_period[i]]
  prcp_day_rdr <- agg_prcp(prcp_day_rdr, get_max_location(prcp_day_rdr))
  prcp_day_rdr <- cbind(prcp_day_rdr, time = my_period[i])
  prcp_prd_rdr <- rbind(prcp_prd_rdr, prcp_day_rdr)  
  print(i)
}

prcp_mean_gpm_rdr <- prcp_prd_gpm_rdr[, mean(mean), .(distance)]
colnames(prcp_mean_gpm_rdr)[2] <- "mean"
prcp_mean_knmi <- prcp_prd_knmi[, mean(mean), .(distance)]
colnames(prcp_mean_knmi)[2] <- "mean"
prcp_mean_rdr <- prcp_prd_rdr[, mean(mean), .(distance)]
colnames(prcp_mean_rdr)[2] <- "mean"

ggplot(prcp_mean_gpm_rdr, aes(distance, mean)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 1, se = T, fill = "orange") +
  geom_smooth(data = prcp_mean_knmi,  aes(distance, mean), method = "loess", col = "red", size = 1, span = 1, se = T, fill = "red") +
  geom_smooth(data = prcp_mean_rdr,  aes(distance, mean), method = "loess", col = "dark green", size = 1, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Mean (mm)") + 
  theme_bw()

prcp_cv_gpm_rdr <- prcp_prd_gpm_rdr[, mean(sd/mean), .(distance)]
colnames(prcp_cv_gpm_rdr)[2] <- "cv"
prcp_cv_knmi <- prcp_prd_knmi[, mean(sd/mean), .(distance)]
colnames(prcp_cv_knmi)[2] <- "cv"
prcp_cv_rdr <- prcp_prd_rdr[, mean(sd/mean), .(distance)]
colnames(prcp_cv_rdr)[2] <- "cv"

ggplot(prcp_cv_gpm_rdr,  aes(distance, cv)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 1, se = T, fill = "orange") +
  geom_smooth(data = prcp_cv_knmi,  aes(distance, cv), method = "loess", col = "red", size = 1, span = 1, se = T, fill = "red") +
  geom_smooth(data = prcp_cv_rdr,  aes(distance, cv), method = "loess", col = "dark green", size = 1, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Coef. of Variation") + 
  theme_bw()

#Mean daily precipitation for whole period

my_period <- rdr_prcp[, unique(time)]

prcp_prd_gpm_rdr <- gpm_rdr_prcp[time %in% my_period[1]]
prcp_prd_gpm_rdr <- agg_prcp(prcp_prd_gpm_rdr, get_max_location(prcp_prd_gpm_rdr))
prcp_prd_gpm_rdr <- cbind(prcp_prd_gpm_rdr, time = my_period[1])

prcp_prd_knmi <- gpm_rdr_prcp[time %in% my_period[1]]
prcp_prd_knmi <- agg_prcp(prcp_prd_knmi, get_max_location(prcp_prd_knmi))
prcp_prd_knmi <- cbind(prcp_prd_knmi, time = my_period[1])

prcp_prd_rdr <- gpm_rdr_prcp[time %in% my_period[1]]
prcp_prd_rdr <- agg_prcp(prcp_prd_rdr, get_max_location(prcp_prd_rdr))
prcp_prd_rdr <- cbind(prcp_prd_rdr, time = my_period[1])

for(i in 2:length(my_period)){
  prcp_day_gpm_rdr <- gpm_rdr_prcp[time %in% my_period[i]]
  prcp_day_gpm_rdr <- agg_prcp(prcp_day_gpm_rdr, get_max_location(prcp_day_gpm_rdr))
  prcp_day_gpm_rdr <- cbind(prcp_day_gpm_rdr, time = my_period[i])
  prcp_prd_gpm_rdr <- rbind(prcp_prd_gpm_rdr, prcp_day_gpm_rdr)
  prcp_day_knmi <- knmi_prcp[time %in% my_period[i]]
  prcp_day_knmi <- agg_prcp(prcp_day_knmi, get_max_location(prcp_day_knmi))
  prcp_day_knmi <- cbind(prcp_day_knmi, time = my_period[i])
  prcp_prd_knmi <- rbind(prcp_prd_knmi, prcp_day_knmi)  
  prcp_day_rdr <- rdr_prcp[time %in% my_period[i]]
  prcp_day_rdr <- agg_prcp(prcp_day_rdr, get_max_location(prcp_day_rdr))
  prcp_day_rdr <- cbind(prcp_day_rdr, time = my_period[i])
  prcp_prd_rdr <- rbind(prcp_prd_rdr, prcp_day_rdr)  
  print(i)
}

prcp_mean_gpm_rdr <- prcp_prd_gpm_rdr[, mean(mean), .(distance)]
colnames(prcp_mean_gpm_rdr)[2] <- "mean"
prcp_mean_knmi <- prcp_prd_knmi[, mean(mean), .(distance)]
colnames(prcp_mean_knmi)[2] <- "mean"
prcp_mean_rdr <- prcp_prd_rdr[, mean(mean), .(distance)]
colnames(prcp_mean_rdr)[2] <- "mean"

ggplot(prcp_mean_gpm_rdr, aes(distance, mean)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 1, se = T, fill = "orange") +
  geom_smooth(data = prcp_mean_knmi,  aes(distance, mean), method = "loess", col = "red", size = 1, span = 1, se = T, fill = "red") +
  geom_smooth(data = prcp_mean_rdr,  aes(distance, mean), method = "loess", col = "dark green", size = 1, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Mean (mm)") + 
  theme_bw()

prcp_cv_gpm_rdr <- prcp_prd_gpm_rdr[, mean(sd/mean), .(distance)]
colnames(prcp_cv_gpm_rdr)[2] <- "cv"
prcp_cv_knmi <- prcp_prd_knmi[, mean(sd/mean), .(distance)]
colnames(prcp_cv_knmi)[2] <- "cv"
prcp_cv_rdr <- prcp_prd_rdr[, mean(sd/mean), .(distance)]
colnames(prcp_cv_rdr)[2] <- "cv"

ggplot(prcp_cv_gpm_rdr,  aes(distance, cv)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 1, se = T, fill = "orange") +
  geom_smooth(data = prcp_cv_knmi,  aes(distance, cv), method = "loess", col = "red", size = 1, span = 1, se = T, fill = "red") +
  geom_smooth(data = prcp_cv_rdr,  aes(distance, cv), method = "loess", col = "dark green", size = 1, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Coef. of Variation") + 
  theme_bw()

#Mean heavy daily precipitation for JAS
my_period <- rdr_prcp[time >= "2016-07-01" & prcp > 7.5, unique(time)] 
prcp_prd_gpm_rdr <- gpm_rdr_prcp[time %in% my_period[1] & prcp > 7.5]
prcp_prd_gpm_rdr <- agg_prcp(prcp_prd_gpm_rdr, get_max_location(prcp_prd_gpm_rdr))
prcp_prd_gpm_rdr <- cbind(prcp_prd_gpm_rdr, time = my_period[1])

prcp_prd_knmi <- gpm_rdr_prcp[time %in% my_period[1] & prcp > 7.5]
prcp_prd_knmi <- agg_prcp(prcp_prd_knmi, get_max_location(prcp_prd_knmi))
prcp_prd_knmi <- cbind(prcp_prd_knmi, time = my_period[1])

prcp_prd_rdr <- gpm_rdr_prcp[time %in% my_period[1] & prcp > 7.5]
prcp_prd_rdr <- agg_prcp(prcp_prd_rdr, get_max_location(prcp_prd_rdr))
prcp_prd_rdr <- cbind(prcp_prd_rdr, time = my_period[1])

for(i in 2:length(my_period)){
  prcp_day_gpm_rdr <- gpm_rdr_prcp[time %in% my_period[i]]
  prcp_day_gpm_rdr <- agg_prcp(prcp_day_gpm_rdr, get_max_location(prcp_day_gpm_rdr))
  prcp_day_gpm_rdr <- cbind(prcp_day_gpm_rdr, time = my_period[i])
  prcp_prd_gpm_rdr <- rbind(prcp_prd_gpm_rdr, prcp_day_gpm_rdr)
  prcp_day_knmi <- knmi_prcp[time %in% my_period[i] & prcp > 7.5]
  prcp_day_knmi <- agg_prcp(prcp_day_knmi, get_max_location(prcp_day_knmi))
  prcp_day_knmi <- cbind(prcp_day_knmi, time = my_period[i])
  prcp_prd_knmi <- rbind(prcp_prd_knmi, prcp_day_knmi)  
  prcp_day_rdr <- rdr_prcp[time %in% my_period[i]]
  prcp_day_rdr <- agg_prcp(prcp_day_rdr, get_max_location(prcp_day_rdr))
  prcp_day_rdr <- cbind(prcp_day_rdr, time = my_period[i])
  prcp_prd_rdr <- rbind(prcp_prd_rdr, prcp_day_rdr)  
  print(i)
}

prcp_mean_gpm_rdr <- prcp_prd_gpm_rdr[, mean(mean), .(distance)]
colnames(prcp_mean_gpm_rdr)[2] <- "mean"
prcp_mean_knmi <- prcp_prd_knmi[, mean(mean), .(distance)]
colnames(prcp_mean_knmi)[2] <- "mean"
prcp_mean_rdr <- prcp_prd_rdr[, mean(mean), .(distance)]
colnames(prcp_mean_rdr)[2] <- "mean"

ggplot(prcp_mean_gpm_rdr, aes(distance, mean)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 0.5, se = T, fill = "orange") +
  geom_smooth(data = prcp_mean_knmi,  aes(distance, mean), 
              method = "loess", col = "red", size = 1, span = 0.5, se = T, fill = "red") +
  geom_smooth(data = prcp_mean_rdr,  aes(distance, mean), 
              method = "loess", col = "dark green", size = 0.5, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Mean (mm)") + 
  theme_bw()

prcp_cv_gpm_rdr <- prcp_prd_gpm_rdr[, mean(sd/mean), .(distance)]
colnames(prcp_cv_gpm_rdr)[2] <- "cv"
prcp_cv_knmi <- prcp_prd_knmi[, mean(sd/mean), .(distance)]
colnames(prcp_cv_knmi)[2] <- "cv"
prcp_cv_rdr <- prcp_prd_rdr[, mean(sd/mean), .(distance)]
colnames(prcp_cv_rdr)[2] <- "cv"

ggplot(prcp_cv_gpm_rdr,  aes(distance, cv)) +
  geom_smooth(method = "loess", col = "orange", size = 1, span = 1, se = T, fill = "orange") +
  geom_smooth(data = prcp_cv_knmi,  aes(distance, cv), method = "loess", col = "red", size = 1, span = 1, se = T, fill = "red") +
  geom_smooth(data = prcp_cv_rdr,  aes(distance, cv), method = "loess", col = "dark green", size = 1, span = 1, se = T, fill = "dark green") +
  labs(x = "Distance (km)", y = "Coef. of Variance") + 
  theme_bw()


#p3 <- map_plot(radar = rdr_prcp, satellite = gpm_rdr_prcp,ground = knmi_prcp, date = period)

#Look more into this!!!
#period <- my_period

# <- gpm_rdr_prcp[time %in% period]
#prcp_prd_gpm_rdr <- agg_prcp(prcp_prd_gpm_rdr, get_max_location(prcp_prd_gpm_rdr))

#prcp_prd_knmi <- knmi_prcp[time %in% period]
#prcp_prd_knmi <- agg_prcp(prcp_prd_knmi, get_max_location(prcp_prd_knmi))

#prcp_prd_rdr <- rdr_prcp[time %in% period]
#prcp_prd_rdr <- agg_prcp(prcp_prd_rdr, get_max_location(prcp_prd_rdr))

#my_period <- my_period[c(1, 5, 15, 30, 90, length(my_period))]

ggplot(prcp_period_gpm_knmi, aes(log10(distance), log10(sum))) + 
  geom_line(col = "orange", alpha = 0.5, size = 1) +
  geom_line(data = prcp_period_knmi, aes(log10(distance), log10(sum)), col = "red", size = 1, alpha = 0.5) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()

ggplot(prcp_period_gpm_rdr, aes(log10(distance), log10(sum))) + 
  geom_line(col = "orange", size = 1) +
  geom_line(data = prcp_period_rdr, aes(log10(distance), log10(sum)), col = "dark green", size = 1) +
  labs(x = "Distance (km)", y = "Precipitation sum (mm)") + 
  theme_bw()



##Comparison of gravity Centers

gravity_center_knmi <- get_gravity_center(knmi_prcp[time %in% my_date & prcp], no_points)
gravity_center_rdr <- get_gravity_center(rdr_prcp[time %in% my_date & prcp], no_points)

gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_knmi)) * 111
gDistance(SpatialPoints(gravity_center_gpm), SpatialPoints(gravity_center_rdr)) * 111
gDistance(SpatialPoints(gravity_center_knmi), SpatialPoints(gravity_center_rdr)) * 111

#Can also compare sums of light vs heavy as light cells are more


