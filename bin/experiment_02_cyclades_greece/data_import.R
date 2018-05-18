source("source/import_functions.R")
source("source/paths.R")

#### NOA

noa_stations <- read_noa_stations()
noa_stations_islands <- noa_stations[lat > 36 & lat < 37.5 & lon > 24 & lon < 26.5] 
noa_stations_names_islands <- noa_stations_islands$station
noa_prcp_islands <- read_noa_data(noa_stations_names_islands)
noa_prcp_islands <- merge(noa_stations_islands[, 1:2], noa_prcp_islands)
noa_prcp_islands <- noa_prcp_islands[, 2:4]

#### GPM daily (KNMI)

gpm_d_nc <- ncdf4::nc_open(paste0(data_gpm_day_example_path, "imerg_daily_23.269-23.906E_37.786-38.467N.nc"))  
gpm_d = ncdf4::ncvar_get(gpm_d_nc)
dimnames(gpm_d)[[3]] <- gpm_d_nc$dim$time$vals 
dimnames(gpm_d)[[2]] <- gpm_d_nc$dim$lat$vals 
dimnames(gpm_d)[[1]] <- gpm_d_nc$dim$lon$vals
kk = ncdf4::nc_close(gpm_d_nc)
gpm_d <- data.table::data.table(reshape2::melt(gpm_d, varnames = c("lon", "lat", "time"), value.name = "prcp")) 
gpm_d$time <- gpm_d$time + as.Date("2014-03-12")
gpm_d <- gpm_d[complete.cases(gpm_d)]
gpm_d_prcp <- gpm_d[time > "2017-12-01" & time < "2017-12-31"] 
gpm_d_prcp[, lat := factor(round(lat, 2))]
gpm_d_prcp[, lon := factor(round(lon, 2))]
gpm_d_cells <- gpm_d_prcp[, 1:2]
gpm_d_cells <- gpm_d_cells[!duplicated(gpm_d_cells)]

gpm_cells[, lat := factor(round(lat, 2))]
gpm_cells[, lon := factor(round(lon, 2))]
gpm_d_cells <- merge(gpm_cells, gpm_d_cells)
gpm_d_prcp <- merge(gpm_d_cells, gpm_d_prcp,  by = c("lon", "lat"), all = TRUE)

gpm_d_prcp[, lon <- NULL]
gpm_d_prcp[, lat <- NULL]

gpm_cells$lon <- as.numeric(as.character(gpm_cells$lon))
gpm_cells$lat <- as.numeric(as.character(gpm_cells$lat))
gpm_d_cells$lon <- as.numeric(as.character(gpm_d_cells$lon))
gpm_d_cells$lat <- as.numeric(as.character(gpm_d_cells$lat))