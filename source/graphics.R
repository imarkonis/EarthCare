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
  
  ## data.table manipulation in order to get str: 'id', 'time' 'prcp', 'lon', 'lat'
  radar <- aux_fun_id_time(df = radar, date, 'radar')
  satellite <- aux_fun_id_time(df = satellite, date, 'satellite')
  ground <- aux_fun_id_time(df = ground, date, 'ground')
  
  ## load spatial data & convert it to data.frame
  poly <- readOGR('./data/geodata/gadm36_NLD_1.shp', verbose = F) #####
  poly_f <- suppressMessages(fortify(poly))

  ## bind all available datasources to one data.frame
  prc_all <- rbind(radar, satellite, ground)
  prc_all[, id := factor(id, levels = c('radar', 'satellite', 'ground'))]
  
  ## select colours
  cols <- c('dark green', 'orange', 'red')
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
