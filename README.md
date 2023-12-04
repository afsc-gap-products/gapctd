  <!-- badges: start -->
  [![R-CMD-check](https://github.com/sean-rohan-NOAA/gapctd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sean-rohan-NOAA/gapctd/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# gapctd R Package

This package contains functions to process CTD data that are collected during Alaska Fisheries Science Center bottom trawl surveys using trawl-mounted SBE 19plus V2 SeaCAT Profiler CTDs. Data processing methods are described in a NOAA Technical Memorandum [(Rohan et al., 2023)](https://doi.org/10.25923/8ape-q461) The package includes functions to:

- Convert hexadecimal (.hex) files from select SBE CTDs to .cnv.
- Use select SBE Data Processing modules for data processing.
- Implement optimization methods from Rohan et al. (2023)
- Inspect and flag CTD data using R's plot interface.
- Prepare data products in NetCDF (.nc), plain-text (.csv), and R data (.rds) formats.

Functions in this package extensively leverage functionality from the [_oce_ R package for Oceanographic Analysis](https://dankelley.github.io/oce/).

_Please submit an issue if you find an error or encounter unexpected function behavior._

# Installation 

This package is not hosted on CRAN and should be installed through GitHub. To install this package:

```
library(remotes)
install_github("afsc-gap-products/gapctd")
```

# Documentation and usage

- [Convert hexadecimal (.hex) files to .cnv](./doc/convert_hex_files.md)
- [Data processing modules](./doc/data_processing_modules.md)
- [Batch processing steps](./doc/batch_processing_steps.md)
- [Batch processing instructions and example](./doc/0_batch_processing.md)
- [Data file structure](./doc/ctd_data_files.md)

#  Data products

Data products from 2021-2023 NOAA/AFSC/RACE Groundfish Assessment Program bottom trawl surveys of the Bering Sea, Gulf of Alaska, and Aleutian Islands will be uploaded to the National Centers for Environmental Information for download by early 2024. Links to the data product, including download links, will be updated here when the data products are available.

# References

Rohan, S. K., Charriere, N. E., Riggle, B., O’Leary, C. A., and Raring, N. W. 2023. A flexible approach for processing data collected using trawl-mounted CTDs during Alaska bottom-trawl surveys. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-AFSC-475, 43 p. [https://doi.org/10.25923/8ape-q461](https://doi.org/10.25923/8ape-q461)

## NOAA README
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License
Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.
<br>
<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>
<br>
[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)
