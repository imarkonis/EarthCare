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

map_plot <- function(radar = NULL, satelite = NULL, ground = NULL, date = '2017-1-1') {
  
  date <- as.Date(date)
  
  radar <- aux_fun_id_time(df = radar, date, 'radar')
  satelite <- aux_fun_id_time(df = satelite, date, 'satelite')
  ground <- aux_fun_id_time(df = ground, date, 'ground')
  
  poly <- readOGR('./data/gadm36_NLD_1.shp', verbose = F) #####
  
  poly_f <- suppressMessages(fortify(poly))
  
  prc_all <- rbind(radar, satelite, ground)
  
  mp <- ggplot() +
    geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
    geom_point(data = prc_all, aes(y = lat, x = lon, size = prcp, col = id), alpha = 0.5) +
    theme_bw() +
    scale_color_manual(values = c('red', 'steelblue4', 'limegreen'), name = 'Data \nsource') +
    scale_size_continuous(name = 'Precipitation') +
    scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scalebar(data = poly_f, x.min = min(long), x.max = max(long), y.min = min(lat), y.max = max(lat), 
             dist = 25, dd2km = TRUE, model = 'WGS84', st.size = 2.5, height = .01) +
    labs(x = '', y = '') +
    coord_map()
  
  return(mp)
}
