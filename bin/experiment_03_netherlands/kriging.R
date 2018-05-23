require(data.table)
require(sp)
require(gstat)

suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})

source("./source/paths.R") 

load("./data/experiment_3.rdata")


my_date <- as.Date("2016-9-16")

test_rdr <- rdr_prcp[time == my_date]
test_rdr <- merge(test_rdr, rdr_cells)

test_gpm <- gpm_d_prcp[time == my_date]
test_gpm <- merge(test_gpm, gpm_d_cells)

test_knmi <- knmi_prcp[time == my_date]
test_knmi <- merge(test_knmi, knmi_stations)
test_knmi <- test_knmi[complete.cases(test_knmi)]
coordinates(test_knmi) <- ~ lon + lat


prcp_vgm <- variogram(prcp ~ 1, test_knmi) # calculates sample variogram values 
prcp_fit <- fit.variogram(prcp_vgm, model = vgm(1, "Exp", 900, 1)) # fit model
plot(prcp_vgm, prcp_fit)
