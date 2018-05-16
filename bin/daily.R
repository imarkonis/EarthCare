require(data.table)
source("source/paths.R")

load("./data/dataset.rdata") 

noa_d_prcp <- noa_prcp[, sum(prcp, na.rm = T), .(as.Date(time), id)]

gpm_d_prcp[id == "gpm_52", plot(cumsum(prcp))]
noa_d_prcp[id == "noa_237", plot(cumsum(V1))]

gpm_d_prcp[id == "gpm_23", plot(cumsum(prcp))]
noa_d_prcp[id == "noa_355", plot(cumsum(V1))]
