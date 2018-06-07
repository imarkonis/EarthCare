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

getSeason <- function(dates) {
  
  WS <- as.Date('2012-12-15', format = '%Y-%m-%d') # Winter Solstice
  SE <- as.Date('2012-3-15',  format = '%Y-%m-%d') # Spring Equinox
  SS <- as.Date('2012-6-15',  format = '%Y-%m-%d') # Summer Solstice
  FE <- as.Date('2012-9-15',  format = '%Y-%m-%d') # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format ='2012-%m-%d'))
  
  ifelse (d >= WS | d < SE, 'Winter',
          ifelse (d >= SE & d < SS, 'Spring',
                  ifelse (d >= SS & d < FE, 'Summer', 'Fall')))
}

aux_fun_id_time <- function(df, 
                            date, 
                            name) {
  
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
map_plot <- function(radar = NULL, 
                     satellite = NULL, 
                     ground = NULL, 
                     date = '2016-1-1') {
  
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

desc_stat <- function(Radar = NULL, 
                      Satellite = NULL, 
                      Ground = NULL, 
                      period = c('2015-10-1', '2016-9-1'),
                      wet_par = c(.5, 1)) {
  
  period <- as.Date(period)
  
  dta_src <- c('Radar', 'Satellite', 'Ground') ## string vector containg all available data sources (same as all possible data arguments)
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := dta_src[i]])
  
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

ggcdf <- function(Radar = NULL, 
                  Satellite = NULL, 
                  Ground = NULL, 
                  period = c('2015-10-1', '2016-9-1'),
                  wet_par = c(.5, 1)) {
  
  period <- as.Date(period)
  
  dta_src <- c('Radar', 'Satellite', 'Ground') ## string vector containg all available data sources (same as all possible data arguments)
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := dta_src[i]])
  
  dta_all <- rbindlist(dta)
  dta_all[, id := factor(id, levels = dta_src)]
  
  wet_dta <- na.omit(dta_all[dta_all[, .I[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])], by = id]$V1])
  wet_dta <- wet_dta[, seasons := getSeason(time)]
  
  cols <- c('red', 'dark orange', 'dark green')
  names(cols) <- levels(dta_all[, id])
  
  cdf <- ggplot() +
    stat_ecdf(data = wet_dta, aes(x = prcp, y = -log(-log(..y..)), group = id, colour = id)) +
    facet_wrap(~seasons) +
    scale_colour_manual(values = cols, name = 'Data \nsource') +
    theme_bw() +
    theme(strip.background = element_blank()) +
    labs(x = 'Precipitation', y = expression(-log(-log(p))), title = 'Transformed seasonal empirical distribution functions')
  
  cdf
}

ggbox <- function(Radar = NULL, 
                  Satellite = NULL, 
                  Ground = NULL, 
                  period = c('2015-10-1', '2016-9-1'),
                  wet_par = c(.5, 1),
                  seasonality = 'month') {
  
  period <- as.Date(period)
  
  dta_src <- c('Radar', 'Satellite', 'Ground') ## string vector containg all available data sources (same as all possible data arguments)
  
  dta <- mget(dta_src)
  dta <- dta[sapply(dta, function(x) !is.null(x))]
  dta <- lapply(seq_along(dta), function(i) dta[[i]][, id := dta_src[i]])
  
  dta_all <- rbindlist(dta)
  dta_all[, mnth := do.call(seasonality, list(time))]
  
  dta_all[, id := factor(id, levels = dta_src)]
  
  wet_dta <- na.omit(dta_all[dta_all[, .I[(time %between% period) & (quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])], by = id]$V1])
  
  cols <- c('red', 'dark orange', 'dark green')
  names(cols) <- levels(dta_all[, id])
  
  ggb <- ggplot() +
    geom_boxplot(data = na.omit(wet_dta), aes(x = factor(mnth), y = prcp, fill = id)) +
    scale_fill_manual(values = cols, name = 'Data \nsource') +
    scale_y_log10() +
    theme_bw() +
    labs(x = 'Month', y = 'Precipitation \nlog-scale', title = 'Monthly precipitation box-plots of wet days')
  
  ggb
}