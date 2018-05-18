source("./source/import.R")
source("./source/spatial_tools.R")
source("./source/paths.R") 

data_gpm_experiment_day_path <- paste0(data_gpm_path, "netherlands/")

#### GPM daily (KNMI)
gpm_nc_file <- paste0(data_gpm_experiment_day_path, "imerg_daily_3.37-7.22E_50.66-53.56N.nc")

gpm_d <- read_gpm_day(gpm_nc_file)
gpm_d_cells <- gpm_d[[2]]
gpm_d_prcp <- gpm_d[[1]]

#### Radar
rdr_nc_file <- paste0(data_knmi_rdr_path, "nl_2009_2016.nc")
rdr <- read_gpm_day(rdr_nc_file)
#noa_stations <- match_noa_gpm(noa_stations, gpm_d_cells)
#save(gpm_prcp, gpm_cells, gpm_d_prcp, gpm_cells, noa_prcp, noa_stations, file = "./data/experiment_3.rdata")

rdr_nc <- ncdf4::nc_open(rdr_nc_file)  
rdr = ncdf4::ncvar_get(rdr_nc)
dimnames(rdr)[[3]] <- rdr_nc$dim$time$vals 
dimnames(rdr)[[2]] <- rdr_nc$dim$lat$vals 
dimnames(rdr)[[1]] <- rdr_nc$dim$lon$vals
rdr <- rdr[, , 2800:2829]
kk = ncdf4::nc_close(rdr_nc)
rdr <- data.table::data.table(reshape2::melt(rdr, varnames = c("lon", "lat", "time"), value.name = "prcp")) 
rdr$time <- rdr$time + as.Date("2009-01-01")
rdr_prcp <- rdr[complete.cases(rdr)]
rdr_prcp[, lat := round(as.numeric(as.character(lat)), 2)]
rdr_prcp[, lon := round(as.numeric(as.character(lon)), 2)]
rdr_prcp[, id := .GRP, .(lon, lat)]
rdr_prcp$id <- paste0("gpm_", rdr_prcp$id)

rdr_cells <- rdr_prcp[, c(5, 1:2)]
rdr_cells <- rdr_cells[!duplicated(rdr_cells)]
rdr_prcp <- rdr_prcp[, c(5, 3:4)]

#### Stations


#### Reanalysis


#testing
require(ggplot2)
"2016-09-16"
my_date <- "2016-09-15"
test_rdr <- rdr_prcp[time == my_date]
test_rdr <- merge(test_rdr, rdr_cells)

test_gpm <- gpm_d_prcp[time == my_date]
test_gpm <- merge(test_gpm, gpm_d_cells)

ggplot(test_rdr[prcp > 0], aes(y = lat, x = lon, col = prcp)) + 
  geom_point() +
  geom_point(data = test_gpm[prcp > 0], aes(y = lat, x = lon), col = "red") +
  theme_bw()

ggplot(test_rdr[prcp > 1], aes(y = lat, x = lon, col = prcp)) + 
  geom_point() +
  geom_point(data = test_gpm[prcp > 1], aes(y = lat, x = lon), col = "red") +
  theme_bw()

ggplot(test_rdr[prcp > 5], aes(y = lat, x = lon, col = prcp)) + 
  geom_point() +
  geom_point(data = test_gpm[prcp > 10], aes(y = lat, x = lon), col = "red") +
  theme_bw()

ggplot(test_rdr[prcp > 10], aes(y = lat, x = lon, col = prcp)) + 
  geom_point() +
  geom_point(data = test_gpm[prcp > 10], aes(y = lat, x = lon), col = "red") +
  theme_bw()
