# Preparing data products

Finalized CTD data from GAP surveys are packaged in netCDF files that
contain vertical profiles, station level data, and metadata. Variables
and metadata in the files follow [CF Metadata
Conventions](https://cfconventions.org/index.html) to facilitate data
sharing and transfer. Because the gapctd package is still in
development, the code below is shared as a placeholder for a full
working example.

``` r
# Generate data products using make_oce_ncdf()
library(gapctd)

# Example cast file.
cast_files <- c(
  system.file("extdata/example/2021_06_24_0001_final.rds", package = "gapctd")
)

# Example metadata file that includes metadata for the example cast
metadata_files <- c(
  system.file("extdata/example/ex_metadata.rds", package = "gapctd")
)

# Example data for required fields
data_set_name <- "EXAMPLE CTD Data from AFSC 2021 EBS Shelf and NBS Bottom Trawl Surveys"
cruise_name <- "EXAMPLE  2021 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Surveys"
ctd_team <- "CTD Team"
creator_name <- "Creator name"
creator_email <- "Cretor email"
dataset_doi <- "doi"
ctd_unit <- "SBE19plus V2"


# Make an example netCDF file called example_gapctd.nc
gapctd::make_oce_ncdf(
  cast_files = cast_files,
  metadata_files = metadata_files,
  output_file = "example_gapctd.nc",
  global_attributes = list(title = data_set_name, 
                           references = "",
                           id = dataset_doi,
                           cdm_data_type = "Point",
                           cruise = cruise_name,
                           institution = "NOAA Alaska Fisheries Science Center",
                           contributor_name = ctd_team,
                           creator_name = creator_name,
                           creator_institution = "NOAA Alaska Fisheries Science Center",
                           creator_email = creator_email,
                           publisher = "NOAA Alaska Fisheries Science Center",
                           publisher_type = "institution",
                           publisher_url = "https://github.com/afsc-gap-products/gapctd",
                           geospatial_bounds_crs = "EPSG:4326",
                           license = "http://www.usa.gov/publicdomain/label/1.0/",
                           metadata_link = "",
                           instrument = "CTD",
                           Conventions = c("CF-1.8"),
                           standard_name_vocabulary = "CF Standard Name Table v79",
                           source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd"))
  )
)
```

    ## netcdf netcdf4 {
    ## dimensions:
    ##  index = 2 ;
    ##  depth = 37 ;
    ## variables:
    ##  NC_INT index(index) ;
    ##      NC_STRING index:long_name = "Sample index" ;
    ##  NC_INT depth(depth) ;
    ##      NC_STRING depth:units = "m" ;
    ##      NC_STRING depth:long_name = "Depth in meters" ;
    ##      NC_STRING depth:positive = "down" ;
    ##  NC_FLOAT latitude(index) ;
    ##      NC_STRING latitude:units = "degree_north" ;
    ##      NC_STRING latitude:long_name = "Latitude (decimal degrees)" ;
    ##  NC_FLOAT longitude(index) ;
    ##      NC_STRING longitude:units = "degree_east" ;
    ##      NC_STRING longitude:long_name = "Longitude (decimal degrees)" ;
    ##  NC_STRING time(index) ;
    ##      NC_STRING time:units = "time" ;
    ##      NC_STRING time:long_name = "Time in Coordinated Universal Time (UTC)" ;
    ##  NC_STRING stationid(index) ;
    ##      NC_STRING stationid:units = "1" ;
    ##      NC_STRING stationid:long_name = "AFSC/RACE/GAP Survey Station Name" ;
    ##  NC_STRING profile(index) ;
    ##      NC_STRING profile:units = "1" ;
    ##      NC_STRING profile:long_name = "Profile Number and Direction" ;
    ##  NC_INT vessel(index) ;
    ##      NC_STRING vessel:units = "1" ;
    ##      NC_STRING vessel:long_name = "AFSC/RACE/GAP Vessel Code" ;
    ##  NC_INT cruise(index) ;
    ##      NC_STRING cruise:units = "1" ;
    ##      NC_STRING cruise:long_name = "AFSC/RACE/GAP Cruise Code" ;
    ##  NC_INT haul(index) ;
    ##      NC_STRING haul:units = "1" ;
    ##      NC_STRING haul:long_name = "Haul Number" ;
    ##  NC_INT haul_depth(index) ;
    ##      NC_STRING haul_depth:units = "m" ;
    ##      NC_STRING haul_depth:long_name = "Mean towed depth of CTD during haul" ;
    ##  NC_FLOAT sea_floor_temperature(index) ;
    ##      NC_STRING sea_floor_temperature:units = "degree_C" ;
    ##      NC_STRING sea_floor_temperature:long_name = "Mean bottom temperature (ITS-90) at towed depth" ;
    ##  NC_FLOAT sea_floor_practical_salinity(index) ;
    ##      NC_STRING sea_floor_practical_salinity:units = "1" ;
    ##      NC_STRING sea_floor_practical_salinity:long_name = "Mean Practical Salinity (PSS-78) at towed depth" ;
    ##  NC_FLOAT sea_floor_salinity(index) ;
    ##      NC_STRING sea_floor_salinity:units = "g kg-1" ;
    ##      NC_STRING sea_floor_salinity:long_name = "Mean Absolute Salinity (TEOS-10 GSW) at towed depth" ;
    ##  NC_FLOAT sea_floor_sound_speed_in_sea_water(index) ;
    ##      NC_STRING sea_floor_sound_speed_in_sea_water:units = "m s-1" ;
    ##      NC_STRING sea_floor_sound_speed_in_sea_water:long_name = "Mean speed of sound during haul (Chen-Millero)" ;
    ##  NC_FLOAT time_elapsed(depth, index) ;
    ##      NC_FLOAT time_elapsed:_FillValue = -9999 ;
    ##      NC_STRING time_elapsed:units = "s" ;
    ##      NC_STRING time_elapsed:long_name = "Time Elapsed Since Start of Deployment" ;
    ##  NC_FLOAT sea_water_pressure(depth, index) ;
    ##      NC_FLOAT sea_water_pressure:_FillValue = -9999 ;
    ##      NC_STRING sea_water_pressure:units = "dbar" ;
    ##      NC_STRING sea_water_pressure:long_name = "Strain Gauge Pressure" ;
    ##  NC_FLOAT sea_water_temperature(depth, index) ;
    ##      NC_FLOAT sea_water_temperature:_FillValue = -9999 ;
    ##      NC_STRING sea_water_temperature:units = "degree_C" ;
    ##      NC_STRING sea_water_temperature:long_name = "ITS-90 Temperature" ;
    ##  NC_FLOAT sea_water_practical_salinity(depth, index) ;
    ##      NC_FLOAT sea_water_practical_salinity:_FillValue = -9999 ;
    ##      NC_STRING sea_water_practical_salinity:units = "1" ;
    ##      NC_STRING sea_water_practical_salinity:long_name = "PSS-78 Practical Salinity" ;
    ##  NC_FLOAT sea_water_salinity(depth, index) ;
    ##      NC_FLOAT sea_water_salinity:_FillValue = -9999 ;
    ##      NC_STRING sea_water_salinity:units = "g kg-1" ;
    ##      NC_STRING sea_water_salinity:long_name = "TEOS-10 GSW Absolute Salinity" ;
    ##  NC_FLOAT sea_water_density(depth, index) ;
    ##      NC_FLOAT sea_water_density:_FillValue = -9999 ;
    ##      NC_STRING sea_water_density:units = "kg m-3" ;
    ##      NC_STRING sea_water_density:long_name = "TEOS-10 GSW Density" ;
    ##  NC_FLOAT buoyancy_frequency(depth, index) ;
    ##      NC_FLOAT buoyancy_frequency:_FillValue = -9999 ;
    ##      NC_STRING buoyancy_frequency:units = "s-2" ;
    ##      NC_STRING buoyancy_frequency:long_name = "Squared Brunt-Vaisala Buoyancy Frequency" ;
    ##  NC_FLOAT sea_water_electrical_conductivity(depth, index) ;
    ##      NC_FLOAT sea_water_electrical_conductivity:_FillValue = -9999 ;
    ##      NC_STRING sea_water_electrical_conductivity:units = "S m-1)" ;
    ##      NC_STRING sea_water_electrical_conductivity:long_name = "Electrical Conductivity" ;
    ##  NC_FLOAT sound_speed_in_sea_water(depth, index) ;
    ##      NC_FLOAT sound_speed_in_sea_water:_FillValue = -9999 ;
    ##      NC_STRING sound_speed_in_sea_water:units = "m s-1" ;
    ##      NC_STRING sound_speed_in_sea_water:long_name = "Speed of Sound (Chen-Millero)" ;
    ##  NC_INT quality_flag(depth, index) ;
    ##      NC_INT quality_flag:_FillValue = -9999 ;
    ##      NC_STRING quality_flag:units = "1" ;
    ##      NC_STRING quality_flag:long_name = "Data Quality Assurance Flag" ;
    ##      NC_INT quality_flag:flag_values = -6, 0, 6, 7 ;
    ##      NC_STRING quality_flag:flag_meanings = "Interpolated value. Density inversion error detected based on buoyancy frequency and remains after automatic removal and interpolation of point.", "Good quality, no issues detected.", "Interpolated value. Density inversion error detected based on buoyancy frequency and corrected using automatic removal and interpolation of point.", "Interpolated value. Point mannually flagged and removed during visual inspection then estimated through interpolation." ;
    ##  NC_STRING instrument ;
    ##      NC_STRING instrument:make_model = "Sea-Bird SBE19plus V2" ;
    ##      NC_STRING instrument:serial_number = "01908102" ;
    ##      NC_STRING instrument:calibration_date = "07-Jan-21" ;
    ##      NC_INT instrument:vessel = 162 ;
    ## 
    ## // global attributes:
    ##      NC_STRING :references = "" ;
    ##      NC_STRING :id = "doi" ;
    ##      NC_STRING :cruise = "EXAMPLE  2021 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Surveys" ;
    ##      NC_STRING :institution = "NOAA Alaska Fisheries Science Center" ;
    ##      NC_STRING :contributor_name = "CTD Team" ;
    ##      NC_STRING :creator_name = "Creator name" ;
    ##      NC_STRING :creator_institution = "NOAA Alaska Fisheries Science Center" ;
    ##      NC_STRING :creator_email = "Cretor email" ;
    ##      NC_STRING :publisher = "NOAA Alaska Fisheries Science Center" ;
    ##      NC_STRING :publisher_type = "institution" ;
    ##      NC_STRING :publisher_url = "https://github.com/afsc-gap-products/gapctd" ;
    ##      NC_STRING :geospatial_bounds = "POLYGON ((60.00184 -168.70713, 60.00184 -168.65472, 60.00488 -168.65472, 60.00488 -168.70713, 60.00184 -168.70713))" ;
    ##      NC_STRING :geospatial_bounds_crs = "EPSG:4326" ;
    ##      NC_STRING :license = "http://www.usa.gov/publicdomain/label/1.0/" ;
    ##      NC_STRING :metadata_link = "" ;
    ##      NC_STRING :date_created = "2022-10-12" ;
    ##      NC_STRING :instrument = "CTD" ;
    ##      NC_STRING :Conventions = "CF-1.8" ;
    ##      NC_STRING :standard_name_vocabulary = "CF Standard Name Table v79" ;
    ##      NC_STRING :cdm_data_type = "Point" ;
    ##      NC_STRING :time_coverage_start = "2021-06-24 15:06:37 UTC" ;
    ##      NC_STRING :time_coverage_end = "2021-06-24 15:35:59 UTC" ;
    ##      NC_STRING :source = "CTD data processed using gapctd 1.0.6" ;
    ## }

# Examine the contents of the file

``` r
# Open a connection to the object
con <- RNetCDF::open.nc("example_gapctd.nc")

# Examine the contents of the object
RNetCDF::print.nc(con)
```

    ## netcdf netcdf4 {
    ## dimensions:
    ##  index = 2 ;
    ##  depth = 37 ;
    ## variables:
    ##  NC_INT index(index) ;
    ##      NC_STRING index:long_name = "Sample index" ;
    ##  NC_INT depth(depth) ;
    ##      NC_STRING depth:units = "m" ;
    ##      NC_STRING depth:long_name = "Depth in meters" ;
    ##      NC_STRING depth:positive = "down" ;
    ##  NC_FLOAT latitude(index) ;
    ##      NC_STRING latitude:units = "degree_north" ;
    ##      NC_STRING latitude:long_name = "Latitude (decimal degrees)" ;
    ##  NC_FLOAT longitude(index) ;
    ##      NC_STRING longitude:units = "degree_east" ;
    ##      NC_STRING longitude:long_name = "Longitude (decimal degrees)" ;
    ##  NC_STRING time(index) ;
    ##      NC_STRING time:units = "time" ;
    ##      NC_STRING time:long_name = "Time in Coordinated Universal Time (UTC)" ;
    ##  NC_STRING stationid(index) ;
    ##      NC_STRING stationid:units = "1" ;
    ##      NC_STRING stationid:long_name = "AFSC/RACE/GAP Survey Station Name" ;
    ##  NC_STRING profile(index) ;
    ##      NC_STRING profile:units = "1" ;
    ##      NC_STRING profile:long_name = "Profile Number and Direction" ;
    ##  NC_INT vessel(index) ;
    ##      NC_STRING vessel:units = "1" ;
    ##      NC_STRING vessel:long_name = "AFSC/RACE/GAP Vessel Code" ;
    ##  NC_INT cruise(index) ;
    ##      NC_STRING cruise:units = "1" ;
    ##      NC_STRING cruise:long_name = "AFSC/RACE/GAP Cruise Code" ;
    ##  NC_INT haul(index) ;
    ##      NC_STRING haul:units = "1" ;
    ##      NC_STRING haul:long_name = "Haul Number" ;
    ##  NC_INT haul_depth(index) ;
    ##      NC_STRING haul_depth:units = "m" ;
    ##      NC_STRING haul_depth:long_name = "Mean towed depth of CTD during haul" ;
    ##  NC_FLOAT sea_floor_temperature(index) ;
    ##      NC_STRING sea_floor_temperature:units = "degree_C" ;
    ##      NC_STRING sea_floor_temperature:long_name = "Mean bottom temperature (ITS-90) at towed depth" ;
    ##  NC_FLOAT sea_floor_practical_salinity(index) ;
    ##      NC_STRING sea_floor_practical_salinity:units = "1" ;
    ##      NC_STRING sea_floor_practical_salinity:long_name = "Mean Practical Salinity (PSS-78) at towed depth" ;
    ##  NC_FLOAT sea_floor_salinity(index) ;
    ##      NC_STRING sea_floor_salinity:units = "g kg-1" ;
    ##      NC_STRING sea_floor_salinity:long_name = "Mean Absolute Salinity (TEOS-10 GSW) at towed depth" ;
    ##  NC_FLOAT sea_floor_sound_speed_in_sea_water(index) ;
    ##      NC_STRING sea_floor_sound_speed_in_sea_water:units = "m s-1" ;
    ##      NC_STRING sea_floor_sound_speed_in_sea_water:long_name = "Mean speed of sound during haul (Chen-Millero)" ;
    ##  NC_FLOAT time_elapsed(depth, index) ;
    ##      NC_FLOAT time_elapsed:_FillValue = -9999 ;
    ##      NC_STRING time_elapsed:units = "s" ;
    ##      NC_STRING time_elapsed:long_name = "Time Elapsed Since Start of Deployment" ;
    ##  NC_FLOAT sea_water_pressure(depth, index) ;
    ##      NC_FLOAT sea_water_pressure:_FillValue = -9999 ;
    ##      NC_STRING sea_water_pressure:units = "dbar" ;
    ##      NC_STRING sea_water_pressure:long_name = "Strain Gauge Pressure" ;
    ##  NC_FLOAT sea_water_temperature(depth, index) ;
    ##      NC_FLOAT sea_water_temperature:_FillValue = -9999 ;
    ##      NC_STRING sea_water_temperature:units = "degree_C" ;
    ##      NC_STRING sea_water_temperature:long_name = "ITS-90 Temperature" ;
    ##  NC_FLOAT sea_water_practical_salinity(depth, index) ;
    ##      NC_FLOAT sea_water_practical_salinity:_FillValue = -9999 ;
    ##      NC_STRING sea_water_practical_salinity:units = "1" ;
    ##      NC_STRING sea_water_practical_salinity:long_name = "PSS-78 Practical Salinity" ;
    ##  NC_FLOAT sea_water_salinity(depth, index) ;
    ##      NC_FLOAT sea_water_salinity:_FillValue = -9999 ;
    ##      NC_STRING sea_water_salinity:units = "g kg-1" ;
    ##      NC_STRING sea_water_salinity:long_name = "TEOS-10 GSW Absolute Salinity" ;
    ##  NC_FLOAT sea_water_density(depth, index) ;
    ##      NC_FLOAT sea_water_density:_FillValue = -9999 ;
    ##      NC_STRING sea_water_density:units = "kg m-3" ;
    ##      NC_STRING sea_water_density:long_name = "TEOS-10 GSW Density" ;
    ##  NC_FLOAT buoyancy_frequency(depth, index) ;
    ##      NC_FLOAT buoyancy_frequency:_FillValue = -9999 ;
    ##      NC_STRING buoyancy_frequency:units = "s-2" ;
    ##      NC_STRING buoyancy_frequency:long_name = "Squared Brunt-Vaisala Buoyancy Frequency" ;
    ##  NC_FLOAT sea_water_electrical_conductivity(depth, index) ;
    ##      NC_FLOAT sea_water_electrical_conductivity:_FillValue = -9999 ;
    ##      NC_STRING sea_water_electrical_conductivity:units = "S m-1)" ;
    ##      NC_STRING sea_water_electrical_conductivity:long_name = "Electrical Conductivity" ;
    ##  NC_FLOAT sound_speed_in_sea_water(depth, index) ;
    ##      NC_FLOAT sound_speed_in_sea_water:_FillValue = -9999 ;
    ##      NC_STRING sound_speed_in_sea_water:units = "m s-1" ;
    ##      NC_STRING sound_speed_in_sea_water:long_name = "Speed of Sound (Chen-Millero)" ;
    ##  NC_INT quality_flag(depth, index) ;
    ##      NC_INT quality_flag:_FillValue = -9999 ;
    ##      NC_STRING quality_flag:units = "1" ;
    ##      NC_STRING quality_flag:long_name = "Data Quality Assurance Flag" ;
    ##      NC_INT quality_flag:flag_values = -6, 0, 6, 7 ;
    ##      NC_STRING quality_flag:flag_meanings = "Interpolated value. Density inversion error detected based on buoyancy frequency and remains after automatic removal and interpolation of point.", "Good quality, no issues detected.", "Interpolated value. Density inversion error detected based on buoyancy frequency and corrected using automatic removal and interpolation of point.", "Interpolated value. Point mannually flagged and removed during visual inspection then estimated through interpolation." ;
    ##  NC_STRING instrument ;
    ##      NC_STRING instrument:make_model = "Sea-Bird SBE19plus V2" ;
    ##      NC_STRING instrument:serial_number = "01908102" ;
    ##      NC_STRING instrument:calibration_date = "07-Jan-21" ;
    ##      NC_INT instrument:vessel = 162 ;
    ## 
    ## // global attributes:
    ##      NC_STRING :references = "" ;
    ##      NC_STRING :id = "doi" ;
    ##      NC_STRING :cruise = "EXAMPLE  2021 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Surveys" ;
    ##      NC_STRING :institution = "NOAA Alaska Fisheries Science Center" ;
    ##      NC_STRING :contributor_name = "CTD Team" ;
    ##      NC_STRING :creator_name = "Creator name" ;
    ##      NC_STRING :creator_institution = "NOAA Alaska Fisheries Science Center" ;
    ##      NC_STRING :creator_email = "Cretor email" ;
    ##      NC_STRING :publisher = "NOAA Alaska Fisheries Science Center" ;
    ##      NC_STRING :publisher_type = "institution" ;
    ##      NC_STRING :publisher_url = "https://github.com/afsc-gap-products/gapctd" ;
    ##      NC_STRING :geospatial_bounds = "POLYGON ((60.00184 -168.70713, 60.00184 -168.65472, 60.00488 -168.65472, 60.00488 -168.70713, 60.00184 -168.70713))" ;
    ##      NC_STRING :geospatial_bounds_crs = "EPSG:4326" ;
    ##      NC_STRING :license = "http://www.usa.gov/publicdomain/label/1.0/" ;
    ##      NC_STRING :metadata_link = "" ;
    ##      NC_STRING :date_created = "2022-10-12" ;
    ##      NC_STRING :instrument = "CTD" ;
    ##      NC_STRING :Conventions = "CF-1.8" ;
    ##      NC_STRING :standard_name_vocabulary = "CF Standard Name Table v79" ;
    ##      NC_STRING :cdm_data_type = "Point" ;
    ##      NC_STRING :time_coverage_start = "2021-06-24 15:06:37 UTC" ;
    ##      NC_STRING :time_coverage_end = "2021-06-24 15:35:59 UTC" ;
    ##      NC_STRING :source = "CTD data processed using gapctd 1.0.6" ;
    ## }

``` r
# Open an attribute
RNetCDF::var.get.nc(con, variable = "depth")
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## [26] 26 27 28 29 30 31 32 33 34 35 36 37

``` r
RNetCDF::var.get.nc(con, variable = "sea_water_temperature")
```

    ##         [,1]   [,2]
    ##  [1,] 4.4348 5.0171
    ##  [2,] 4.4260 5.0308
    ##  [3,] 4.4260 5.0243
    ##  [4,] 4.4219 4.9943
    ##  [5,] 4.4045 4.9746
    ##  [6,] 4.4075 4.9511
    ##  [7,] 4.4086 4.9440
    ##  [8,] 4.4183 4.9658
    ##  [9,] 4.4079 4.8860
    ## [10,] 4.3702 4.6952
    ## [11,] 4.3204 4.6049
    ## [12,] 4.2872 4.4962
    ## [13,] 4.2529 4.3653
    ## [14,] 4.2129 4.3474
    ## [15,] 4.1523 4.1766
    ## [16,] 4.1243 4.2420
    ## [17,] 4.1060 4.1834
    ## [18,] 4.0884 4.1211
    ## [19,] 4.0676 4.0685
    ## [20,] 4.0450 4.0279
    ## [21,] 4.0359 4.0011
    ## [22,] 4.0215 3.9795
    ## [23,] 4.0053 3.9717
    ## [24,] 3.9941 3.9555
    ## [25,] 3.9785 3.9485
    ## [26,] 3.9738 3.9401
    ## [27,] 3.9664 3.9344
    ## [28,] 3.9541 3.9317
    ## [29,] 3.9530 3.9226
    ## [30,] 3.9512 3.9127
    ## [31,] 3.9503 3.8928
    ## [32,] 3.9501 3.8953
    ## [33,] 3.9491 3.8862
    ## [34,] 3.9489 3.8790
    ## [35,] 3.9474 3.8798
    ## [36,]     NA 3.8802
    ## [37,]     NA 3.8804

``` r
RNetCDF::att.get.nc(con, variable = "sea_water_temperature", attribute = "units")
```

    ## [1] "degree_C"

``` r
RNetCDF::att.get.nc(con, variable = "sea_water_temperature", attribute = "long_name")
```

    ## [1] "ITS-90 Temperature"

``` r
RNetCDF::var.get.nc(con, variable = "sea_floor_practical_salinity")
```

    ## [1] 31.6177 31.6177

``` r
RNetCDF::att.get.nc(con, variable = "sea_floor_practical_salinity", attribute = "units")
```

    ## [1] "1"

``` r
RNetCDF::att.get.nc(con, variable = "sea_floor_practical_salinity", attribute = "long_name")
```

    ## [1] "Mean Practical Salinity (PSS-78) at towed depth"

``` r
RNetCDF::var.get.nc(con, variable = "stationid")
```

    ## [1] "P-18" "P-18"

``` r
RNetCDF::var.get.nc(con, variable = "haul")
```

    ## [1] 91 91
