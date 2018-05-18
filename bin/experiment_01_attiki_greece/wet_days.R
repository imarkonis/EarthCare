source("./source/paths.R")
load("./data/experiment_1.rdata") #Created in data_import

wday_thres <- 0.8 #percentage of stations/cells that are wet during day

noa_prcp_wet <- noa_prcp[prcp > 0]
noa_prcp_wdays <- noa_prcp_wet[, unique(as.Date(time)), id]
colnames(noa_prcp_wdays)[2] <- "wday" 
noa_no_wdays <- noa_prcp_wdays[, .N, wday]
noa_wdays <- noa_no_wdays[N > round(wday_thres * max(N)), wday]

gpm_prcp_wet <- gpm_prcp[prcp > 0]
gpm_prcp_wdays <- gpm_prcp_wet[, unique(as.Date(time)), id]
colnames(gpm_prcp_wdays)[2] <- "wday" 
gpm_prcp_wdays[, .N, wday]
gpm_prcp_wdays[, unique(wday)]
gpm_no_wdays <- gpm_prcp_wdays[, .N, wday]
gpm_wdays <- gpm_no_wdays[N > round(wday_thres * max(N)), wday]

#Find common wet days in both data sets

common_wdays <- noa_wdays[match(gpm_wdays, noa_wdays)]
common_wdays <- common_wdays[!is.na(common_wdays)]

length(common_wdays)/length(noa_wdays) #percentage of efficiently detecting wet days
length(common_wdays)/length(gpm_wdays)

mean(gpm_prcp[as.Date(time) %in% common_wdays, sum(prcp), id]$V1)
mean(noa_prcp[as.Date(time) %in% common_wdays, sum(prcp, na.rm = TRUE), id]$V1)

gpm_prcp_day <- gpm_prcp[as.Date(time) %in% common_wdays, sum(prcp), .(as.Date(time), id)]
noa_prcp_day <- noa_prcp[as.Date(time) %in% common_wdays, sum(prcp, na.rm = TRUE), .(as.Date(time), id)]

#gpm_22

test_noa <- noa_prcp_wet[id %in% noa_stations[nearest_cell == "gpm_22"]$id]
test_gpm <- gpm_prcp_wet[id == "gpm_22"]
test_noa[, ceiling(.N / 3), .(as.Date(time), id)]
test_gpm[, .N, as.Date(time)]

test_noa[, sum(prcp), .(as.Date(time), id)]
test_gpm[, sum(prcp), as.Date(time)]
test_noa[, sum(prcp) / 7, .(as.Date(time))]

test_noa[, sum(prcp), .(id)]
test_gpm[, sum(prcp)]

#gpm_21

test_noa <- noa_prcp_wet[id %in% noa_stations[nearest_cell == "gpm_21"]$id]
test_gpm <- gpm_prcp_wet[id == "gpm_21"]
test_noa[, ceiling(.N / 3), .(as.Date(time), id)]
test_gpm[, .N, as.Date(time)]

test_noa[, sum(prcp), .(as.Date(time), id)]
test_gpm[, sum(prcp), as.Date(time)]
test_noa[, sum(prcp)/ 7, .(as.Date(time))]

test_noa[, sum(prcp), .(id)]
test_gpm[, sum(prcp)]
