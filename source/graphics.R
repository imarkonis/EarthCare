library(rgdal); library(maptools); library(ggplot2); library(ggsn); library(data.table); library(gridExtra); library(scales); library(dplyr)

source('./source/auxiliary_functions.R')

noa_gpm_compare_plot <- function(gpm_cell){
  stations <- noa_d_prcp[id %in% noa_stations[nearest_cell == gpm_cell, id]]
  cell <- gpm_d_prcp[id == gpm_cell]
  
  g <- ggplot(stations, aes(x = time, y = prcp, group = time)) +
    geom_point(col = 'tan', size = 2) +
    geom_point(data = cell, col = 'black', shape = 13, size = 3) +
    theme_bw()
  g
}


map_plot <- function(radar = NULL, 
                     satellite = NULL, 
                     stations = NULL, 
                     date = '2016-1-1',
                     title = '') {
  
  date <- as.Date(date)
  
  dta_src <- c('satellite', 'radar', 'stations') 
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := names(dta)[i]])
  
  dta_all <- rbindlist(dta)
  dta_all[, id := factor(id, levels = dta_src)]
  dta_all <- dta_all[time == date,]
  
  poly <- readOGR('data/geodata/gadm36_NLD_1.shp', verbose = F)
  poly_f <- fortify(poly)
  
  cols <- c('orange', 'dark green', 'red')
  names(cols) <- levels(dta_all[, id])
  
  mp <- ggplot() +
    geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
    geom_point(data = dta_all, aes(y = lat, x = lon, size = prcp, col = id), alpha = 0.5) +
    theme_bw() +
    scale_color_manual(values = cols, name = 'Data \nsource') +
    scale_size_continuous(name = 'Precipitation [mm]') +
    scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    labs(x = '', y = '', title = title) +
    coord_map()
  
  mp
}

desc_stat <- function(radar = NULL, 
                      satellite = NULL, 
                      stations = NULL, 
                      period = c('2015-10-1', '2016-9-30'),
                      wet_par = c(.05, 1)) {
  
  period <- as.Date(period)
  
  dta_src <- c('satellite', 'radar', 'stations') 
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := names(dta)[i]])
  
  dta_all <- rbindlist(dta)
  dta_all[, id := factor(id, levels = dta_src)]
  
  wet_dta <- na.omit(dta_all[dta_all[, .I[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])], by = id]$V1])
  
  stat <- wet_dta[, .(`Mean` = mean(prcp, na.rm = TRUE),
                      `Minimum` = min(prcp, na.rm = TRUE),
                      `5% Quantile` = quantile(prcp, .05, na.rm = TRUE),
                      `25% Quantile` = quantile(prcp, .25, na.rm = TRUE),
                      `Median` = median(prcp, na.rm = TRUE),
                      `75% Quantile` = quantile(prcp, .75, na.rm = TRUE),
                      `95% Quantile` = quantile(prcp, .95, na.rm = TRUE),
                      `Maximum` = max(prcp, na.rm = TRUE),
                      `Standard Deviation` = sd(prcp, na.rm = TRUE),
                      `Coeficient of Variation` = sd(prcp, na.rm = TRUE)/mean(prcp, na.rm = TRUE),
                      `Interquartile Range` = IQR(prcp, na.rm = TRUE)),
                  by = id]
  
  stat <- setNames(as.data.frame(round(t(stat[, !'id', with = F]), digits = 2)), as.character(stat[,id]))
  stat
}

ggcdf <- function(radar = NULL, 
                  satellite = NULL, 
                  stations = NULL, 
                  period = c('2015-10-1', '2016-9-30'),
                  wet_par = c(.05, 1),
                  title = 'Transformed seasonal empirical distribution functions'
                  ) {
  
  period <- as.Date(period)
  
  dta_src <- c('satellite', 'radar', 'stations') 
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := names(dta)[i]])
  
  dta_all <- rbindlist(dta)
  dta_all[, id := factor(id, levels = dta_src)]
  
  wet_dta <- na.omit(dta_all[dta_all[, .I[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])], by = id]$V1])
  wet_dta <- wet_dta[, seasons := factor(get_season(time), levels = c('Spring', 'Summer', 'Autumn', 'Winter'))]
  
  cols <- c('orange', 'dark green', 'red')
  names(cols) <- levels(dta_all[, id])
  
  cdf <- ggplot() +
    stat_ecdf(data = wet_dta, aes(x = prcp, y = -log(-log(..y..)), group = id, colour = id)) +
    # stat_density(data = wet_dta, aes(x = prcp, group = id, colour = id)) +
    facet_wrap(~seasons, nrow = 2) +
    scale_colour_manual(values = cols, name = 'Data \nsource') +
    theme_bw() +
    # theme(strip.background = element_blank()) +
    labs(x = 'Precipitation [mm]', y = expression(-log(-log(p))), title = title)
  
  cdf
}


ggbox <- function(radar = NULL, 
                  satellite = NULL, 
                  stations = NULL, 
                  period = c('2015-10-1', '2016-9-30'),
                  wet_par = c(.05, 1),
                  seasonality = '%m',
                  title = 'Monthly precipitation box-plots of wet days') {
  
  period <- as.Date(period)
  
  dta_src <- c('satellite', 'radar', 'stations') 
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := names(dta)[i]])
  
  dta_all <- rbindlist(dta)
  dta_all[, mnth := format(time, seasonality)]
  
  dta_all[, id := factor(id, levels = dta_src)]
  
  wet_dta <- na.omit(dta_all[dta_all[, .I[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])], by = id]$V1])
  wet_dta[, mnth := factor(mnth)]
  
  cols <- c('orange', 'dark green', 'red')
  names(cols) <- levels(dta_all[, id])
  
  ggb <- ggplot() +
    geom_boxplot(data = na.omit(wet_dta), aes(x = mnth, y = prcp, fill = id)) +
    scale_fill_manual(values = cols, name = 'Data \nsource') +
    scale_y_log10() +
    theme_bw() +
    labs(x = 'Month', y = 'Precipitation [mm] \nlog-scale', title = title) +
    scale_x_discrete(limits = c('10', '11', '12', paste0('0' , 1:9)))
  
  ggb
}


wet_days_plot <- function(radar = NULL,
                          satellite = NULL,
                          stations = NULL,
                          period = c('2015-10-1', '2016-9-30'),
                          wet_par = c(.05, 1),
                          title = 'Wet days comparsion timeline',
                          method = 'tile') {
  
  period <- as.Date(period)
  
  dta_src <- c('satellite', 'radar', 'stations') 
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := names(dta)[i]])
  
  dta_all <- rbindlist(dta)
  
  dta_all[, id := factor(id, levels = dta_src)]
  
  wet_dta <- na.omit(dta_all[dta_all[, .I[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])], by = id]$V1])
  
  setkey(wet_dta)
  wet_days <- unique(wet_dta[, .(id, time)])
  
  cols <- c('orange', 'dark green', 'red')
  names(cols) <- levels(dta_all[, id])
  
  if(method == 'tile') {
    
    wd <- ggplot() +
      geom_tile(data = wet_days, aes(x = time, y = id, fill = id), show.legend = F) +
      scale_fill_manual(values = cols, name = 'Data \nsource') +
      theme_bw() +
      labs(x = '', y = '', title = title) +
      scale_x_date(labels = date_format("%m-%Y")) +
      coord_fixed(20)
  }
  
  if(method == 'upset') {
    
    wet_days[, val := 1]
    wet_days <- dcast(wet_days, time ~ id, value.var = 'val')
    aux_date <- data.table(time = seq(from = period[1], to = period[2], by = 'day'))
    
    aux <- as.data.table(full_join(wet_days, aux_date, by = 'time'))
    aux[is.na(aux)] <- 0
    
    wd <- UpSetR::upset(aux)
    
  }
  
  
  wd
}

raster_maps <- function(radar,
                        satellite,
                        poly,
                        period = c('2015-10-1', '2016-9-30'),
                        wet_par = c(.05, 1)) {
  
  dta_r <- radar
  dta_r <- dta_r[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2]),]
  dta_r[, annual_radar := sum(prcp), by = id]
  setkey(dta_r)
  dta_r <- unique(dta_r[,.(lon, lat, annual_radar)])
  
  dta_s <- satellite
  dta_s <- dta_s[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2]),]
  dta_s <- dta_s[time %between% period,]
  dta_s[, annual_satellite := sum(prcp), by = id]
  dta_s <- unique(dta_s[,.(lon, lat, annual_satellite)])
  
  dta_dif <- merge(dta_r, dta_s, by = c('lon', 'lat'))
  
  poly_f <- fortify(poly)
  
  rdr <- ggplot() +
    geom_raster(data = dta_dif, aes(x = lon, y = lat, fill = annual_radar)) +
    geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
    coord_quickmap() +
    scale_fill_gradient(low = '#f7fcf5', high = '#005a32', name = 'Precipitation [mm]') +
    scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    labs(x = '', y = '') +
    theme_bw()
  
  gpm <- ggplot() +
    geom_raster(data = dta_dif, aes(x = lon, y = lat, fill = annual_satellite)) +
    geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
    coord_quickmap() +
    scale_fill_gradient(low = '#fff5eb', high = '#8c2d04', name = 'Precipitation [mm]') +
    scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    labs(x = '', y = '') +
    theme_bw()
  
  dif <- ggplot() +
    geom_raster(data = dta_dif, aes(x = lon, y = lat, fill = annual_radar / annual_satellite)) +
    geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
    coord_quickmap() +
    scale_fill_gradient2(midpoint = 1, name = 'Relative \nDifference [-]') +
    scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
    labs(x = '', y = '', title = '') +
    theme_bw()
  
  list(radar = rdr, satellite = gpm, difference = dif)
}

# wet_days_provinces <- function(radar = NULL,
#                                satellite = NULL,
#                                stations = NULL,
#                                spatial_polygon,
#                                period = c('2015-10-1', '2016-9-30'),
#                                wet_par = c(.05, 1)) {
#   
#   period <- as.Date(period)
#   
#   dta_src <- c('satellite', 'radar', 'stations') 
#   
#   dta <- mget(dta_src)
#   dta <- dta[sapply(dta, function(x) !is.null(x))]
#   
#   dta <- lapply(seq_along(dta), function(i) merge(dta[[i]], points_to_provinces(dta[[i]], spatial_polygon)))
#   dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := names(dta)[i]])
#   
#   dta_all <- rbindlist(dta)
#   
#   dta_all[, id := factor(id, levels = dta_src)]
#   
#   wet_dta <- na.omit(dta_all[dta_all[, .I[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])], by = id]$V1])
#   
#   setkey(wet_dta)
#   wet_days <- unique(wet_dta[, .(id, time, .id)])
#   
#   cols <- c('orange', 'dark green', 'red')
#   names(cols) <- levels(dta_all[, id])
#   
#   wdp <- ggplot() +
#     geom_tile(data = wet_days, aes(x = time, y = id, fill = id)) +
#     scale_fill_manual(values = cols, name = 'Data \nsource') +
#     theme_bw() +
#     labs(x = 'Days', title = 'Wet days in Provinces') +
#     coord_fixed(20) +
#     facet_wrap(~.id, ncol = 1) +
#     theme(strip.background = element_blank(),
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank())
#   
#   wdp
# }