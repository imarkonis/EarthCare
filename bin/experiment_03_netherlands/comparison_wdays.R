library(ggplot2)
library(data.table)

load("./data/experiment_3.rdata")  #Created in data_import.R
load("./data/experiment_3_wdays.rdata") #Created in comparison.R


knmi_prcp[, sum(prcp, na.rm = T)]/gpm_knmi_prcp[, sum(prcp, na.rm = T)]

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