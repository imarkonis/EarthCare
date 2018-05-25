require(rgeos)
require(maptools)

#Find noa stations within gpm grid cells
#' Title
#'
#' @param stations 
#' @param cells 
#' @param cell_dist 
#'
#' @return
#' @export
#'
#' @examples
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

#Ranks distances of a matrix of points to a single point (lon, lat)
#' Title
#'
#' @param x 
#' @param pt 
#' @param km_per_deg 
#'
#' @return
#' @export
#'
#' @examples
dist_rank <- function(x, pt, km_per_deg = 111){ #x is c(id, lon, lat) and pt is the point to estimate rank of distances (lon, lat)
  set_sp <- SpatialPoints(x[, c('lon', 'lat')])
  distance <- as.numeric(gDistance(set_sp,  SpatialPoints(pt), byid = TRUE))
  return(cbind(distance = km_per_deg * distance, rank = rank(distance, ties.method = "random"))) #the output is in km
}

#Aggregates precipitation from smaller to higher distance rank
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
agg_prcp_out <- function(x){  #add error msg for not using rank and number of missing values
  no_cells = nrow(x)
  out = matrix(data = NA, no_cells, 3)
  for(i in 1:no_cells){
    out[i, ] = c(sum(x[id %in% x[rank <= i, id], prcp], na.rm = T),
                 mean(x[id %in% x[rank <= i, id], prcp], na.rm = T),
                 sd(x[id %in% x[rank <= i, id], prcp], na.rm = T))
  }
  colnames(out) <- c("sum", "mean", "sd")
  return(out)
}










