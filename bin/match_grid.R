require(rgeos)
require(maptools)

#Find noa stations within gpm grid cells
match_noa_gpm <- function(noa_stations, gpm_cells){
  set1sp <- SpatialPoints(noa_stations[, c('lon', 'lat')])
  set2sp <- SpatialPoints(gpm_cells[, c('lon', 'lat')])

  station_cell_dist <- gDistance(set2sp, set1sp, byid = TRUE)
  noa_stations$nearest_cell <- apply(station_cell_dist, 1, which.min)
  noa_stations$nearest_value <- apply(station_cell_dist, 1, min)
  noa_stations <- noa_stations[noa_stations$nearest_value < 0.05 * 2^0.5, ]
  noa_stations$nearest_value <- NULL
  noa_stations$nearest_cell <- gpm_cells[noa_stations$nearest_cell]$id
  return(noa_stations)
}











