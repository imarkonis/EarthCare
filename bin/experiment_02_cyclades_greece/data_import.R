noa_stations <- read_noa_stations()
noa_stations_islands <- noa_stations[lat > 36 & lat < 37.5 & lon > 24 & lon < 26.5] 
noa_stations_names_islands <- noa_stations_islands$station
noa_prcp_islands <- read_noa_data(noa_stations_names_islands)
noa_prcp_islands <- merge(noa_stations_islands[, 1:2], noa_prcp_islands)
noa_prcp_islands <- noa_prcp_islands[, 2:4]