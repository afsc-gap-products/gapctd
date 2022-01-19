# GAP CTD Data Batch Processing

*This package is in development.*

Import, clean, and process CTD data collected during surveys conducted by the Groundfish Assessment Program at NOAA's Alaska Fisheries Science Center. Current support for data collected using SBE19 and SBE19plus V2.

The most recent version of this package was built in R version 4.0.5 for use with SBE Data Processing (Version 7.26.7).

## Legal disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.

# Installation 

To install this package use the following

```
devtools::install_github("sean-rohan-noaa/gapctd")
library(gapctd)
```
                                 
# Instructions for batch processing in R

CTD data processing in R is conducted using batch processing options specified in SBEDataProcessing software. The gapctd R package provides a wrapper around shell commands and SQL queries in AFSC/RACE/GAP RACEBASE to perform batch processing of CTD data and plots for QA/QC.

1. Create a new local directory and set it as the working directory.
2. Load the gapctd package (```library(gapctd)```).
3. Copy .hex and .xmlcon files from a CTD directory to your working directory and set up the working directory structure using ```setup_ctd_processing_dir()```. In the example below, .hex and .xmlcon files are copied from G:/RACE_CTD/data/2019/ebs/v162 to a working directory and the directory is setup to process data from an SBE19 (non-plus).

```
gapctd::setup_ctd_processing_dir(ctd_dir = "G:/RACE_CTD/data/2019/ebs/v162",
                                 ctd_unit = "sbe19")
```

4. Open an ODBC database connection in R using the RODBC package.
5. Batch process CTD files using ```run_sbe_batch```. In the example below, batch processing is conducted for vessel 162, year 2019, from the eastern Bering Sea ('BS'), and haul data is queried from RACEBASE using the user-provided RODBC connection (channel). Alternatively, a .csv file containing haul data could be provided instead of using the rodbc_channel. 

```
gapctd::run_sbe_batch(vessel = 162,
                      year = 2019,
                      region = "BS",
                      rodbc_channel = channel)
```

Profiles of upcasts and downcasts with TEOS-10 seawater properties will be included in the /final_cnv/ folder of the working directory. 
6. Run make_cast_plots() to generate plots for visual QA/QC inspection<br/>.
