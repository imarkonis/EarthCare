source('./source/spatial_tools.R')
source('./source/graphics.R')
load('./data/experiment_3_raw.rdata')

poly <- readOGR('data/geodata/gadm36_NLD_1.shp', verbose = F)
poly_f <- fortify(poly)

ggplot() + #original station points vs grid cells
  geom_path(data = poly_f, aes(x = long, y = lat, group = group)) +
  geom_point(data = knmi_stations, aes(x = lon, y = lat), colour = 'red', size = 2.5) +
  geom_point(data = gpm_d_cells, aes(x = lon, y = lat), colour = 'orange', size = .95) +
  coord_map() +
  scale_y_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
  scale_x_continuous(labels = function(x) paste0(sprintf('%.1f', x),'°')) +
  labs(x = '', y = '') +
  theme_bw()

load('./data/experiment_3_main.rdata')
map_plot(radar = rdr_prcp, stations = knmi_prcp)
ggbox(radar = rdr_prcp, satellite = gpm_rdr_prcp, stations = knmi_prcp,
      period = c('2014-12-3', '2018-3-30'),
      title = '')
desc_stat(radar = rdr_prcp, satellite = gpm_rdr_prcp, stations = knmi_prcp, 
          period = c('2014-12-3', '2018-3-30'))
ggcdf(radar = rdr_prcp, satellite = gpm_rdr_prcp, stations = knmi_prcp,
      period = c('2014-12-3', '2018-3-30'),
      title = '')
wet_days_plot(radar = rdr_prcp, satellite = gpm_rdr_prcp, stations = knmi_prcp,
              wet_par = c(.01, 7.5), 
              period = c('2014-12-3', '2018-3-30'),
              title = '', 
              method = 'upset')
wet_days_plot(radar = rdr_prcp, satellite = gpm_rdr_prcp, stations = knmi_prcp,
              wet_par = c(.01, 7.5), 
              period = c('2014-12-3', '2018-3-30'),
              title = '', 
              method = 'tile')

x <- raster_maps(rdr_prcp, gpm_rdr_prcp, poly, wet_par = c(.01, 7.5))

x$radar
x$satellite
x$Difference


