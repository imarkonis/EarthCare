if(.Platform$OS.type == 'unix') {
  
  if(Sys.getenv('USER') == 'phill') {
    
    data_path <- tools::file_path_as_absolute('~/Desktop/test') # still just a test directory
  }
} else {
  
  if(Sys.getenv('USERNAME') == 'markonis') {
    
    data_path <- tools::file_path_as_absolute('C:/Users/markonis/Documents/Yannis/ResearchProjects/2018ESA_Validation/rawdata')
  }
}

dirs <- list.dirs(path = data_path, recursive = F)

lapply(seq_along(dirs), function(i) assign(paste('data', gsub(paste0(data_path, '/'), '', dirs[i]), 'path', sep = '_'),
                                           dirs[i],
                                           envir = .GlobalEnv))