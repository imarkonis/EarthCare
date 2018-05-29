require(data.table)
require(ggplot2)

source("./source/spatial_tools.R")
load("./data/experiment_3.rdata")  #Created in data_import

## Align datasets in time
knmi_prcp <- knmi_prcp[time >= min(gpm_d_prcp$time) & time <= max(gpm_d_prcp$time)]
rdr_prcp <- rdr_prcp[time >= min(gpm_d_prcp$time) & time <= max(gpm_d_prcp$time)]

## Align datasets in space
knmi_prcp <- merge(knmi_stations[, .(id, nearest_cell)], knmi_prcp) #Average of station prcp per GPM cell
knmi_prcp <- knmi_prcp[, mean(prcp, na.rm = T), .(nearest_cell, time)]
colnames(knmi_prcp) <- c("id", "time", "prcp") 

rdr_prcp <- merge(rdr_cells[, .(id, nearest_cell)], rdr_prcp) #Average of station prcp per GPM cell
rdr_prcp <- rdr_prcp[, mean(prcp, na.rm = T), .(nearest_cell, time)]
colnames(rdr_prcp) <- c("id", "time", "prcp") 

gpm_knmi_prcp <- gpm_d_prcp[id %in% knmi_stations$nearest_cell]
gpm_rdr_prcp <- gpm_d_prcp[id %in% rdr_cells$nearest_cell]

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

## Explore missed wet days in gpm data
missed_wet_days <- knmi_wet_days[which(is.na(match_wet_days))]
missed_wet_days_prcp <- knmi_prcp[time %in% missed_wet_days]

ggplot(missed_wet_days_prcp, aes(x = prcp, col = time, group = time)) + ## Better representation here
  geom_density() +
  theme_bw()
barplot(table(month(unique(missed_wet_days_prcp$time))))

missed_wet_days_prcp[, sum(prcp, na.rm = T)] / knmi_prcp[, sum(prcp, na.rm = T)] #percentage of prcp sum that is missed
missed_wet_days_prcp[, sum(prcp, na.rm = T)] 

aa <- missed_wet_days_prcp[, median(prcp, na.rm = T), time]
missed_wet_days_signif <- aa[V1 > 2, time]

ggplot(missed_wet_days_prcp[time %in% missed_wet_days_signif], aes(x = prcp, col = time, group = time)) + ## Better representation here
  geom_density() +
  theme_bw()

## Explore false wet days in gpm data
false_wet_days <- gpm_wet_days[which(is.na(match_wet_days_rev))]
false_wet_days_prcp <- gpm_knmi_prcp[time %in% false_wet_days]

ggplot(false_wet_days_prcp, aes(x = prcp, col = time, group = time)) + ## Better representation here
  geom_density() +
  theme_bw()
barplot(table(month(unique(false_wet_days_prcp$time))))

false_wet_days_prcp[, sum(prcp, na.rm = T)] / gpm_knmi_prcp[, sum(prcp, na.rm = T)] #percentage of prcp sum that is false
false_wet_days_prcp[, sum(prcp, na.rm = T)]

aa <- false_wet_days_prcp[, median(prcp, na.rm = T), time]
false_wet_days_signif <- aa[V1 > 2, time]

ggplot(false_wet_days_prcp[time %in% false_wet_days_signif], aes(x = prcp, col = time, group = time)) + ## Better representation here
  geom_density() +
  theme_bw()

## Allign wet days
gpm_knmi_prcp <- gpm_knmi_prcp[time %in% knmi_wet_days]
rdr_prcp <- rdr_prcp[time %in% knmi_wet_days]

## 














