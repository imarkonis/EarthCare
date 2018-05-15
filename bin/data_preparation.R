source("source/import_functions.R")
source("source/paths.R")

require(ncdf4)
require(doSNOW)

#### NOA
noa_stations <- read_noa_stations()
noa_stations <- noa_stations[lat > 37.786 & lat < 38.467 & lon > 23.269 & lon < 23.906] #Region of interest 23.269 - 23.906, 37.786 - 38.467
noa_stations <- noa_stations[elev < 400] #Low elevation stations
noa_stations_names <- noa_stations$station

noa_prcp <- read_noa_data(noa_stations_names)
noa_prcp <- merge(noa_stations[, 1:2], noa_prcp)
noa_prcp <- noa_prcp[, 2:5]

#### GPM

# To download data you have to run the folllowing (pwd: Faro1930) in cmd (download wget first at https://eternallybored.org/misc/wget/):
# NUL > .urs_cookies
# wget --load-cookies C:\.urs_cookies --save-cookies C:\.urs_cookies --auth-no-challenge=on --keep-session-cookies --user=imarkonis --ask-password --content-disposition -i attiki_GPM_3IMERGHH_V05_20180515_130556.txt
# See also https://disc.gsfc.nasa.gov/data-access#windows_wget

gpm_prcp <- read_gpm_data()
gpm_prcp[, id := .GRP, .(lon, lat)]
gpm_prcp$id <- paste0("gpm_", gpm_prcp$id)
gpm_cells <- gpm_prcp[, c(6, 1:2)]
gpm_prcp <- gpm_prcp[, c(6, 4, 5, 3)]

save(gpm_prcp, gpm_cells, noa_prcp, noa_stations, file = "./data/dataset.rdata")

#Correct timezone


#Aggregate to 30min




#noa_prcp_wet <- noa_prcp[rain > 0]
#noa_prcp_wet_days <- noa_prcp_wet[, unique(date), station]
#colnames(noa_prcp_wet_days)[2] <- "wday" #to examine aggregation of wet vs not wet
#noa_prcp_wet_days[, .N, station]
