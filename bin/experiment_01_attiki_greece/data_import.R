source("./source/import.R")
source("./source/spatial_tools.R")
source("./source/paths.R") 

data_noa_experiment_path <- paste0(data_noa_path, "dec2017/")
data_gpm_experiment_path <- paste0(data_gpm_path, "dec2017/")
data_gpm_experiment_day_path <- paste0(data_gpm_path, "attiki/")

#### NOA
noa_stations <- read_noa_stations()
noa_stations <- noa_stations[lat > 37 & lat < 39 & lon > 23 & lon < 25] #Region of interest 23.269 - 23.906, 37.786 - 38.467
noa_stations <- noa_stations[elev < 400] #Low elevation stations
noa_stations_names <- noa_stations$station

noa_prcp <- read_noa_data(noa_stations_names, data_noa_experiment_path)
noa_prcp <- merge(noa_stations[, 1:2], noa_prcp)
noa_prcp <- noa_prcp[, 2:4]

#### GPM

# To download data you have to run the folllowing (pwd: Faro1930) in cmd (download wget first at https://eternallybored.org/misc/wget/):
# NUL > .urs_cookies
# wget --load-cookies C:\.urs_cookies --save-cookies C:\.urs_cookies --auth-no-challenge=on --keep-session-cookies --user=imarkonis --ask-password --content-disposition -i attiki_GPM_3IMERGHH_V05_20180515_130556.txt
# See also https://disc.gsfc.nasa.gov/data-access#windows_wget

gpm_prcp <- read_gpm_30min(data_gpm_experiment_path)
gpm_cells <- gpm_prcp[, c(5, 1:2)]
gpm_cells <- gpm_cells[!duplicated(gpm_cells)]
gpm_prcp <- gpm_prcp[, c(5, 4, 3)]
gpm_prcp <- gpm_prcp[time > as.POSIXct("2017-12-1 00:00:00", tz = "UTC")]

#### GPM daily (KNMI)

gpm_d_nc <- ncdf4::nc_open(paste0(data_gpm_day_path, "imerg_daily_23.269-23.906E_37.786-38.467N.nc"))  
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

noa_stations <- put_stations_to_cells(noa_stations, gpm_d_cells) #Put stations to cells
save(gpm_prcp, gpm_cells, gpm_d_prcp, gpm_cells, noa_prcp, noa_stations, file = "./data/experiment_1.rdata")

#### EOBS - Fail

eobs_nc <- ncdf4::nc_open(paste0(data_eobs_example_path, "attiki_0.25deg_reg_v17.0u_23.269-23.906E_37.786-38.467N.nc"))  
eobs = ncdf4::ncvar_get(eobs_nc)
dimnames(eobs)[[3]] <- eobs_nc$dim$time$vals 
dimnames(eobs)[[2]] <- eobs_nc$dim$lat$vals 
dimnames(eobs)[[1]] <- eobs_nc$dim$lon$vals
kk = ncdf4::nc_close(eobs_nc)
eobs <- data.table::data.table(reshape2::melt(eobs, varnames = c("lon", "lat", "time"), value.name = "prcp")) #Only 3 points over land!!
eobs$time <- eobs$time + as.Date("1950-01-01")
eobs <- eobs[complete.cases(eobs)]
eobs_prcp <- eobs[time > "2017-12-01" & time < "2017-12-31"] #No data during this period!!!




