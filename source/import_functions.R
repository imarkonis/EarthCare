source("source/paths.R")

require(data.table)
require(ncdf4)
require(doSNOW)

read_noa_stations <- function(){
  stations_noa <- data.table(read.delim(paste0(data_noa_path, "stations_list.txt"), header = FALSE, sep = ""))
  colnames(stations_noa) <- c("id", "station", "lat", "lon", "elev")
  stations_noa$id <- paste0("noa_", stations_noa$id)
  return(stations_noa )
}

read_noa_data <- function(station_names){
  no_stations <- length(station_names)
  station_fnames <- paste0("RAW.", station_names, ".txt")
  noa_prcp <- data.table(read.delim(paste0(data_noa_example_path, station_fnames[1]), sep = ""))
  noa_prcp[, rain := as.numeric(as.character(rain))]
  noa_prcp$station <- station_names[1]
  for(i in 2:no_stations){
    dummy <- data.table(read.delim(paste0(data_noa_example_path, station_fnames[i]), sep = ""))
    dummy[, rain := as.numeric(as.character(rain))] 
    dummy$station <- station_names[i]
    noa_prcp <- rbind(noa_prcp, dummy)
  } 
  noa_prcp[, date := as.Date(date)]
  noa_prcp[, time := as.ITime(as.character(time))] #this is ITime class 
  colnames(noa_prcp)[3] <- "prcp"
  return(noa_prcp)
}


read_gpm_data <- function(){
  file_list <- list.files(data_gpm_example_path)
  data_gpm_example_path <- data_gpm_example_path #needed to make foreach work
  
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
    gpm
  } 
  stopCluster(cluster)
  return(gpm_prcp)
}  
