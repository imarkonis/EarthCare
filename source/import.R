library(data.table); library(ncdf4); library(doSNOW)

source("./source/paths.R")

read_noa_stations <- function(){
  stations_noa <- data.table(read.delim(paste0(data_noa_path, "stations_list.txt"), header = FALSE, sep = ""))
  colnames(stations_noa) <- c("id", "station", "lat", "lon", "elev")
  stations_noa$id <- paste0("noa_", stations_noa$id)
  stations_noa[, lat := round(lat, 2)]
  stations_noa[, lon := round(lon, 2)]
  return(stations_noa )
}

read_noa_data <- function(station_names, data_path){
  no_stations <- length(station_names)
  station_fnames <- paste0("RAW.", station_names, ".txt")
  noa_prcp <- data.table(read.delim(paste0(data_path, station_fnames[1]), sep = ""))
  noa_prcp[, rain := as.numeric(as.character(rain))]
  noa_prcp$station <- station_names[1]
  for(i in 2:no_stations){
    dummy <- data.table(read.delim(paste0(data_path, station_fnames[i]), sep = ""))
    dummy[, rain := as.numeric(as.character(rain))] 
    dummy$station <- station_names[i]
    noa_prcp <- rbind(noa_prcp, dummy)
  } 
  noa_prcp[, time := as.POSIXct(paste(as.Date(date), as.character(time)), "EET")]
  attributes(noa_prcp$time)$tzone <- "UTC" # Change time zone to Universal
  noa_prcp[, date := NULL]
  colnames(noa_prcp)[2] <- "prcp"
  return(noa_prcp)
}

read_gpm_30min <- function(data_path){
  file_list <- list.files(data_path)
  data_gpm_example_path <- data_path #needed to make foreach work
  
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)

  gpm_prcp <- foreach (i = 1:length(file_list), .combine = 'rbind') %dopar%  {
    gpm_nc <- ncdf4::nc_open(paste0(data_gpm_example_path, file_list[i]))  
    gpm = ncdf4::ncvar_get(gpm_nc)
    dimnames(gpm)[[2]] <- gpm_nc$dim$lat$vals 
    dimnames(gpm)[[1]] <- gpm_nc$dim$lon$vals
    kk = ncdf4::nc_close(gpm_nc)
    gpm <- data.table::data.table(reshape2::melt(gpm, varnames = c("lon", "lat"), value.name = "prcp"))
    gpm$date <- as.Date(paste0(substr(file_list[i], 22, 25), "-",
                               substr(file_list[i], 26, 27), "-",
                               substr(file_list[i], 28, 29))) 
    gpm$time <- data.table::as.ITime(paste0(substr(file_list[i], 40, 41), ":",
                                            substr(file_list[i], 42, 43), ":"))
    gpm[, time := as.POSIXct(paste(date, time), "UTC")]
    gpm$date <- NULL
    gpm$time <- gpm$time + 60
    gpm[, id := .GRP, .(lon, lat)]
    gpm$id <- paste0("gpm_", gpm$id)
    gpm
  } 
  stopCluster(cluster)
  return(gpm_prcp)
}  

read_gpm_day <- function(nc_file){
  gpm_d_nc <- ncdf4::nc_open(nc_file)  
  gpm_d = ncdf4::ncvar_get(gpm_d_nc)
  dimnames(gpm_d)[[3]] <- gpm_d_nc$dim$time$vals 
  dimnames(gpm_d)[[2]] <- gpm_d_nc$dim$lat$vals 
  dimnames(gpm_d)[[1]] <- gpm_d_nc$dim$lon$vals
  kk = ncdf4::nc_close(gpm_d_nc)
  gpm_d <- data.table::data.table(reshape2::melt(gpm_d, varnames = c("lon", "lat", "time"), value.name = "prcp")) 
  gpm_d$time <- gpm_d$time + as.Date("2014-03-12")
  gpm_d_prcp <- gpm_d[complete.cases(gpm_d)]
  gpm_d_prcp[, lat := round(as.numeric(as.character(lat)), 2)]
  gpm_d_prcp[, lon := round(as.numeric(as.character(lon)), 2)]
  gpm_d_prcp[, id := .GRP, .(lon, lat)]
  gpm_d_prcp$id <- paste0("gpm_", gpm_d_prcp$id)
  
  gpm_d_cells <- gpm_d_prcp[, c(5, 1:2)]
  gpm_d_cells <- gpm_d_cells[!duplicated(gpm_d_cells)]
  gpm_d_prcp <- gpm_d_prcp[, c(5, 3:4)]
  return(list(gpm_d_prcp, gpm_d_cells))
}

