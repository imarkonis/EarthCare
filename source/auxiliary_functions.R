points_to_provinces <- function(stations,
                                polyg) {
  
  coordinates(stations) <- ~ lon + lat
  proj4string(stations) <- proj4string(poly)
  
  aux <- over(poly, stations, returnList = T)
  
  names(aux) <- poly@data$NAME_1
  
  out <- as.data.table(rbindlist(aux, idcol = T))
  out[,.(.id ,id)]
  
}

get_season <- function(dates) {
  
  WS <- as.Date('2012-12-15', format = '%Y-%m-%d') # Winter Solstice
  SE <- as.Date('2012-3-15',  format = '%Y-%m-%d') # Spring Equinox
  SS <- as.Date('2012-6-15',  format = '%Y-%m-%d') # Summer Solstice
  FE <- as.Date('2012-9-15',  format = '%Y-%m-%d') # Autumn Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format ='2012-%m-%d'))
  
  ifelse (d >= WS | d < SE, 'Winter',
          ifelse (d >= SE & d < SS, 'Spring',
                  ifelse (d >= SS & d < FE, 'Summer', 'Autumn')))
}

get_binary_wet_dta <- function(x, period, wet_par) {
  
  x <- x[time %between% period,]
  x[, bin := ifelse(!((quantile(prcp, 1 - wet_par[1], na.rm = T)) & (prcp > wet_par[2])), 0, 1)]
  
  x
}