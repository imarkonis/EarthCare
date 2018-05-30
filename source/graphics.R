library(rgdal); library(maptools); library(ggplot2); library(ggsn); library(data.table)

noa_gpm_compare_plot <- function(gpm_cell){
  stations <- noa_d_prcp[id %in% noa_stations[nearest_cell == gpm_cell, id]]
  cell <- gpm_d_prcp[id == gpm_cell]
  
  g <- ggplot(stations, aes(x = time, y = prcp, group = time)) +
    geom_point(col = "tan", size = 2) +
    geom_point(data = cell, col = "black", shape = 13, size = 3) +
    theme_bw()
  g
}

map_plot <- function(..., date = '2016-5-21') {
  
  date <- as.Date(date)
  poly <- readOGR('./data/gadm36_NLD_0.shp', verbose = F) #####
  
  poly_f <- suppressMessages(fortify(poly))
  
  rdr <- rdr_prcp[time == date,]
  rdr <- rdr[rdr_cells, on = 'id']
  rdr <- rdr[id %in% knmi_stations$nearest_cell,]
  rdr[, `:=`(id = 'rdr', time = NULL, nearest_cell = NULL)]
  
  gpm <- gpm_d_prcp[time == date,]
  gpm <- gpm[gpm_d_cells, on = 'id']
  gpm <- gpm[id %in% knmi_stations$nearest_cell,]
  gpm[, `:=`(id = 'gpm', time = NULL)]
  
  knmi <- knmi_prcp[time == date]
  knmi <- knmi[knmi_stations, on = 'id']
  knmi[, `:=`(id = 'knmi', time = NULL, station = NULL, nearest_cell = NULL)]
  
  prc_all <- rbind(rdr, gpm, knmi)
  
  mp <- ggplot() +
    geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
    geom_point(data = prc_all, aes(y = lat, x = lon, size = prcp, col = id), alpha = 0.5) +
    theme_bw() +
    scale_color_manual(values = c('red4', 'steelblue4', 'limegreen'), name = 'Data \nsource') +
    scale_size_continuous(name = 'Precipitation') +
    scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scalebar(data = poly_f, x.min = min(long), x.max = max(long), y.min = min(lat), y.max = max(lat), 
             dist = 25, dd2km = TRUE, model = 'WGS84', st.size = 2.5, height = .01) +
    labs(x = '', y = '') +
    coord_map()
  
  return(mp)
}
