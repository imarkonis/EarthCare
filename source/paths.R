data_path <- "C:/Users/markonis/Documents/Yannis/Various_Proj/Grants/2018ESA_Validation/rawdata"
data_noa_path <- "C:/Users/markonis/Documents/Yannis/Various_Proj/Grants/2018ESA_Validation/rawdata/noa/"
data_gpm_path <- "C:/Users/markonis/Documents/Yannis/Various_Proj/Grants/2018ESA_Validation/rawdata/gpm/"
data_eobs_path <- "C:/Users/markonis/Documents/Yannis/Various_Proj/Grants/2018ESA_Validation/rawdata/eobs/"
data_knmi_rdr_path <- "C:/Users/markonis/Documents/Yannis/Various_Proj/Grants/2018ESA_Validation/rawdata/knmi_radar/"
data_knmi_station_path <- "C:/Users/markonis/Documents/Yannis/Various_Proj/Grants/2018ESA_Validation/rawdata/knmi_stations/"

## paths v. 2
## from now on a new user just need to enter his/hers root dir with the data - data_path

if(.Platform$OS.type == 'unix') {
  
  if(Sys.getenv('USER') == 'phill') {
    
    data_path <- path.expand('~/Desktop/test') # still just a test directory
  }
} else {
  
  if(Sys.getenv('USERNAME') == 'markonis') {
    
    data_path <- path.expand('C:/Users/markonis/Documents/Yannis/Various_Proj/Grants/2018ESA_Validation/rawdata')
  }
}

dirs <- list.dirs(path = data_path, recursive = F)

if(length(dirs) > 0) {
  
  lapply(seq_along(dirs), function(i) assign(paste('data', gsub(paste0(data_path, '/'), '', dirs[i]), 'path', sep = '_'),
                                             dirs[i], 
                                             envir = .GlobalEnv))
} else {
  
  message('No directories found in "data_path"')
}

