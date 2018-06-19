library(rgeos); library(maptools); library(SDMTools); library(data.table)

#' Title
#' 
#' Find noa stations within gpm grid cells
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

#' Title
#' 
#' Ranks distances of a matrix of points to a single point (lon, lat)
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

#' Title
#' 
#' Aggregates precipitation from smaller to higher distance rank
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
  for(i in 1:no_cells){ # this could be done using apply?
    out[i, ] = c(sum(x[id %in% x[rank <= i, id], prcp], na.rm = T),
                 mean(x[id %in% x[rank <= i, id], prcp], na.rm = T),
                 sd(x[id %in% x[rank <= i, id], prcp], na.rm = T))
  }
  colnames(out) <- c("sum", "mean", "sd")
  return(out)
}

get_gravity_center <- function(x, no_values = 10){
  x_tail <- x[tail(order(prcp), no_values)]
  gravity_center <- COGravity(x_tail$lon, 
                              x_tail$lat, 
                              x_tail$prcp)
  
  gravity_center  <- cbind(lon = as.numeric(gravity_center[1]),
                           lat = as.numeric(gravity_center[3]))
  
  return(gravity_center)
}

get_max_location <- function(x){
  out <- x[which.max(prcp)][, 2:3]
  return(out)
}

agg_prcp <- function(x, starting_point){
  x <- cbind(x, dist_rank(x, starting_point))
  x <- cbind(x[order(rank)], agg_prcp_out(x))
  return(x[complete.cases(x), ])
}

agg_prcp_period <- function(x, period, starting_point){
  x_agg <- x[time %in% period, mean(prcp), id]
  colnames(x_agg)[2] <- "prcp"
  x <- merge(x_agg, unique(x[, 1:3]))
  x <- cbind(x, dist_rank(x, starting_point))
  x <- cbind(x[order(rank)], agg_prcp_out(x))
  return(x[complete.cases(x), ])
}

agg_prcp_period_sum <- function(x, period, starting_point){
  x_agg <- x[time %in% period, sum(prcp), id]
  colnames(x_agg)[2] <- "prcp"
  x <- merge(x_agg, unique(x[, 1:3]))
  x <- cbind(x, dist_rank(x, starting_point))
  x <- cbind(x[order(rank)], agg_prcp_out(x))
  return(x[complete.cases(x), ])
}


