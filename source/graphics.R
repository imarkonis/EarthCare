require(rgdal); require(maptools); require(ggplot2); require(ggsn); require(data.table); require(gridExtra)

noa_gpm_compare_plot <- function(gpm_cell){
  stations <- noa_d_prcp[id %in% noa_stations[nearest_cell == gpm_cell, id]]
  cell <- gpm_d_prcp[id == gpm_cell]
  
  g <- ggplot(stations, aes(x = time, y = prcp, group = time)) +
    geom_point(col = "tan", size = 2) +
    geom_point(data = cell, col = "black", shape = 13, size = 3) +
    theme_bw()
  g
}

aux_fun_id_time <- function(df, date, name) {
  
  if(!is.null(df)) {
    
    df <- df[time == date,]
    # df[, id := gsub('_.*','', id)]
    df[, id := name]
  }
  
  df
}


#' Title #####
#'
#' @param radar radar data
#' @param satelite 
#' @param ground 
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
map_plot <- function(radar = NULL, satellite = NULL, ground = NULL, date = '2016-1-1') {
  
  date <- as.Date(date)
  
  dta_src <- c('radar', 'satellite', 'ground') ## string vector containg all available data sources (same as all possible data arguments)
  
  ## load spatial data & convert it to data.frame
  poly <- readOGR('./data/geodata/gadm36_NLD_1.shp', verbose = F) #####
  poly_f <- suppressMessages(fortify(poly))
  
  ## data.table manipulation in order to get str: 'id', 'time' 'prcp', 'lon', 'lat'
  prc_list <- lapply(seq_along(dta_src), function(i, ...) {aux_fun_id_time(df = get(dta_src[i]), date = date, name = dta_src[i])})

  ## bind all available datasources to one data.frame
  prc_all <- rbindlist(prc_list)
  prc_all[, id := factor(id, levels = dta_src)]
  
  ## select colours
  cols <- c('red', 'dark orange', 'dark green')
  names(cols) <- levels(prc_all[, id])
  
  ## plot
  mp <- ggplot() +
    geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
    geom_point(data = prc_all, aes(y = lat, x = lon, size = prcp, col = id), alpha = 0.5) +
    theme_bw() +
    scale_color_manual(values = cols, name = 'Data \nsource') +
    scale_size_continuous(name = 'Precipitation') +
    scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scalebar(data = poly_f, x.min = min(long), x.max = max(long), y.min = min(lat), y.max = max(lat), 
             dist = 25, dd2km = TRUE, model = 'WGS84', st.size = 2.5, height = .01) +
    labs(x = '', y = '') +
    coord_map()
  
  return(mp)
}
