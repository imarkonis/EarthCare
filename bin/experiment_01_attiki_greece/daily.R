require(data.table)
require(ggplot2)

source("./source/graphics.R")
load("./data/experiment_1.rdata") #Created in data_import

noa_d_prcp <- noa_prcp[, sum(prcp, na.rm = T), .(as.Date(time), id)]
colnames(noa_d_prcp)[1] <- "time"
colnames(noa_d_prcp)[3] <- "prcp"

noa_in_gpm <- noa_stations[, .N, nearest_cell]
gpm_multi_stations <- noa_in_gpm[N > 1, nearest_cell]

for(i in 1:length(gpm_multi_stations)){
  print(noa_gpm_compare_plot(gpm_multi_stations[i]) + labs( title = gpm_multi_stations[i]))
}

for(i in 1:nrow(gpm_cells)){
  print(noa_gpm_compare_plot(gpm_cells$id[i]) + ggtitle(gpm_cells$id[i]))
}

gpm_cells[id %in% gpm_multi_stations]$id



  