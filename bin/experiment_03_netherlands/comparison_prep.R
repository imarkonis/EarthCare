source("./source/spatial_tools.R")
load("./data/experiment_3_raw.rdata")  #Created in data_import

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

## Determine wet days in stations: at least 5% have precipitation above 1 mm
wet_percent <- .05
knmi_prcp_qq_10 <- knmi_prcp[, quantile(prcp, 1 - wet_percent, na.rm = T), time]
gpm_prcp_qq_10 <- gpm_rdr_prcp[, quantile(prcp, 1 - wet_percent, na.rm = T), time]
knmi_wet_days <- knmi_prcp_qq_10[V1 > 1, time]
gpm_wet_days <- gpm_prcp_qq_10[V1 > 1, time]

## Allign wet days & add lon/lat
gpm_knmi_prcp <- gpm_knmi_prcp[time %in% knmi_wet_days]
rdr_prcp <- rdr_prcp[time %in% knmi_wet_days]

gpm_knmi_prcp <- merge(gpm_d_cells, gpm_knmi_prcp)
gpm_rdr_prcp <- merge(gpm_d_cells, gpm_rdr_prcp)

save(knmi_prcp, gpm_knmi_prcp, rdr_prcp, gpm_rdr_prcp, file = "./data/experiment_3_main.rdata")
save(knmi_wet_days, gpm_wet_days, gpm_knmi_prcp, file = "./data/experiment_3_wdays.rdata")
rm(knmi_wet_days, gpm_wet_days, knmi_prcp_qq_10, gpm_prcp_qq_10); gc()