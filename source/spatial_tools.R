require(rgeos)
require(maptools)

#Find noa stations within gpm grid cells
put_stations_to_cells <- function(stations, cells, cell_dist = 0.5){
  set1sp <- SpatialPoints(stations[, c('lon', 'lat')])
  set2sp <- SpatialPoints(cells[, c('lon', 'lat')])

  station_cell_dist <- gDistance(set2sp, set1sp, byid = TRUE)
  stations$nearest_cell <- apply(station_cell_dist, 1, which.min)
  stations$nearest_value <- apply(station_cell_dist, 1, min)
  stations <- stations[stations$nearest_value < cell_dist * 2^0.5, ]
  stations$nearest_value <- NULL
  stations$nearest_cell <- cells[stations$nearest_cell]$id
  return(stations)
}

dist_rank <- function(x, pt){ #x is c(id, lon, lat) and pt is the point to estimate rank of distances (lon, lat)
  set_sp <- SpatialPoints(x[, c('lon', 'lat')])
  x <- cbind(x, dist = as.numeric(gDistance(set_sp,  SpatialPoints(pt), byid = TRUE)))
  return(rank(distance, ties.method = "random"))
}











