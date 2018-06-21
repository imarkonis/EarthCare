---
output:
  html_document:
    keep_md: TRUE
---





# Experiment 3 

## KNMI station vs. KNMI radar vs. GPM-IMERG satellite over Netherlands

### Introduction

This is a presentation of the scalegram methodology, applied for the validation of satellite precipitation product. Here, we use the [GPM IMERGHH data product](https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGDF_V05/summary) and the KNMI radar, the dense precipitation network of KNMI for Netherlands.

***

### Data

**Satellite data (gpm)**

* Dataset: GPM IMERG Final Precipitation 
* Temporal Resolution: 1 day
* Spatial Resolution: 0.1 deg
* Date Range: 2014-03-12 to 2018-05-15 
* Spatial Region: latitude: 50.75, 53.55, longitude: 3.45, 7.15
* Variables: precipitationCal
* Format: netCDF 4
* Variables: gpm_d_cells (coordinates) & gpm_d_prcp (values) in _experiment_3.rdata_.
* IDs: gpm_1
* Size: 1681652 total values, over 1102 grid cells
* Downloaded from: https://climexp.knmi.nl/select.cgi?id=312456c83e660703df1bfea9ba4fba50&field=imerg_daily 

**Instrumental data (knmi)**

* Dataset: 240 homogenised stations 1951-now (raw)
* Temporal Resolution: 24hours
* Spatial Resolution: -
* Date Range: 1950-12-31 to 2018-04-29 
* Spatial Region: latitude: 50.78, 53.48, longitude: 3.4, 7.11
* Format: *.txt
* IDs: 010
* Size: 5902080 total values, over 240 stations
* Downloaded from: https://climexp.knmi.nl/PhomNL.cgi?id=312456c83e660703df1bfea9ba4fba50

**Radar data (knmi)**

* Dataset: RAD_NL25_RAC_MFBS_24H_NC
* Temporal Resolution: 24hours
* Spatial Resolution: 1x1km
* Date Range: 2015-10-01 to 2016-09-29 
* Spatial Region: latitude: 50.76, 53.56, longitude: 3.37, 7.22
* Format: *.nc
* Variables: 
* IDs: rdr_1
* Size: 12400443 total values, over 30816 grid cells
* Downloaded from: https://climexp.knmi.nl/select.cgi?id=312456c83e660703df1bfea9ba4fba50&field=knmi_radar_daily

***

### Code structure

_data_import.R_ : Import and prepare station, radar and satellite data for the Netherlands. Each pair 
of data (values and coordinates) is then saved to **experiment_3_raw.rdata**.

_comparison_prep.R_ : Prepare datasets for comparison: align datasets in time and space and save them to **experiment_3_main.rdata**, find wet days
and save them to **experiment_3_wdays.rdata**.

_comparison_eda.R_ : Exploratory analysis contains comparison of first moments, extreme values (gumbel probability), wet days (intersection plots). 

_comparison_agg.R_ : Aggregation of rain events over space and time.

***

### Exploratory Data Analysis

_Results of comparison_eda.R_

![](experiment_03_netherlands_files/figure-html/EDA -1.png)<!-- -->![](experiment_03_netherlands_files/figure-html/EDA -2.png)<!-- -->![](experiment_03_netherlands_files/figure-html/EDA -3.png)<!-- -->![](experiment_03_netherlands_files/figure-html/EDA -4.png)<!-- -->

```
## NULL
```

![](experiment_03_netherlands_files/figure-html/EDA -5.png)<!-- -->

<!--html_preserve--><div id="htmlwidget-e80a54089df43bc4f7c7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e80a54089df43bc4f7c7">{"x":{"filter":"none","data":[["Mean","Minimum","5% Quantile","25% Quantile","Median","75% Quantile","95% Quantile","Maximum","Standard Deviation","Coeficient of Variation","Interquartile Range"],[7.96,1,1.2,2.35,4.86,10.33,26.16,83.45,8.34,1.05,7.98],[6.22,1,1.2,2.3,4.51,8.26,16.9,90.24,5.63,0.91,5.96],[5.6,1.01,1.22,2.16,4,7.33,15.23,78.4,5.12,0.91,5.17]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Satellite<\/th>\n      <th>Radar<\/th>\n      <th>Stations<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

![](experiment_03_netherlands_files/figure-html/EDA rasters-1.png)<!-- -->

