if(.Platform$OS.type == 'unix') {
  
  if(Sys.getenv('USER') == 'phill') {
    
    data_path <- path.expand('~/ownCloud/Data/esa_project')
  }
} else {
  
  if(Sys.getenv('USERNAME') == 'markonis') {
    
    data_path <- path.expand('C:/Users/markonis/Documents/Yannis/ResearchProjects/2018ESA_Validation/rawdata')
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

