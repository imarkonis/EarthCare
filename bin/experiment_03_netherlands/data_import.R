# Import and prepare station, radar and satellite data for netherlands. Each pair 
# of data (values and coordinates) is saved to experiment_3.rdata.
source("./source/import.R")
source("./source/spatial_tools.R")
source("./source/paths.R") 

data_gpm_experiment_day_path <- paste0(data_gpm_path, "/netherlands/")

#### GPM daily (KNMI)
gpm_nc_file <- paste0(data_gpm_experiment_day_path, "imerg_daily_3.37-7.22E_50.66-53.56N.nc")

gpm_d <- read_gpm_day(gpm_nc_file)
gpm_d_cells <- gpm_d[[2]]
gpm_d_prcp <- gpm_d[[1]]

#### Radar
rdr_nc_file <- paste0(data_knmi_radar_path, "/radar_sum.nc")
rdr_nc <- ncdf4::nc_open(rdr_nc_file)  
rdr = ncdf4::ncvar_get(rdr_nc)
dimnames(rdr)[[3]] <- rdr_nc$dim$time$vals 
dimnames(rdr)[[2]] <- rdr_nc$dim$lat$vals 
dimnames(rdr)[[1]] <- rdr_nc$dim$lon$vals
rdr <- rdr[, , 1896:3376]  ## Record length to be imported
kk = ncdf4::nc_close(rdr_nc)
rdr <- data.table(reshape2::melt(rdr, varnames = c("lon", "lat", "time"), value.name = "prcp")) 
rdr$time <- rdr$time + as.Date("2009-01-01")
rdr_prcp <- rdr[complete.cases(rdr)]
rdr_prcp[, lat := round(as.numeric(as.character(lat)), 2)]
rdr_prcp[, lon := round(as.numeric(as.character(lon)), 2)]
rdr_prcp[, id := .GRP, .(lon, lat)]
rdr_prcp$id <- paste0("rdr_", rdr_prcp$id)

rdr_cells <- rdr_prcp[, c(5, 1:2)]
rdr_cells <- rdr_cells[!duplicated(rdr_cells)]
rdr_prcp <- rdr_prcp[, c(5, 3:4)]

rm(rdr); gc()

#### Stations - downloaded from https://climexp.knmi.nl/getdutchstations.cgi?id=312456c83e660703df1bfea9ba4fba50&TYPE=preciphom1951
knmi <- data.table(read.delim(paste0(data_knmi_stations_path, "/precip_hom1951_NL.txt"), header = T, sep = ""))
knmi_dates <- data.table(read.delim(paste0(data_knmi_stations_path, "/precip_hom1951_NL.txt"), header = T, sep = " "))
knmi_dates <- knmi_dates[, 1]
no_stations <- ncol(knmi)
colnames(knmi) <- substr(colnames(knmi), 2, 4)
knmi_prcp <- melt(knmi)
knmi_prcp$time <- rep(as.Date(paste0(substr(knmi_dates$X010, 1, 4), "-", 
                                     substr(knmi_dates$X010, 5, 6), "-", 
                                     substr(knmi_dates$X010, 7, 8))),
                      no_stations) - 1 #remove one day to fix sync issue with the radar/satellite sets  

knmi_prcp <- knmi_prcp[, c(1, 3, 2)]
colnames(knmi_prcp) <- c("id", "time", "prcp")
knmi_prcp$id <- as.character(knmi_prcp$id)
knmi_prcp[prcp > 1000, prcp := NA]

knmi_stations <- data.table(read.csv(paste0(data_knmi_stations_path, "/stations_hom_1951.csv"), header = F))
knmi_stations <- knmi_stations[, c(1:3, 6)]
colnames(knmi_stations) <- c("id", "lon", "lat", "station")
knmi_stations$id <- colnames(knmi)

rm(knmi); gc()

#Find the nearest gpm cell center for each station or radar cell center
knmi_stations <- put_stations_to_cells(knmi_stations, gpm_d_cells) 
rdr_cells <- put_stations_to_cells(rdr_cells, gpm_d_cells) 

save(knmi_stations, knmi_prcp, gpm_d_cells, gpm_d_prcp, rdr_cells, rdr_prcp, file = "./data/experiment_3_raw.rdata")

#### Reanalysis

#todo
