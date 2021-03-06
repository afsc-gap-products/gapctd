# GAP CTD Data Processing

*This package is in development.*

Process CTD data collected during AFSC bottom trawl surveys using trawl-mounted CTDs. Current support for data collected using SBE19 and SBE19plus V2.

The most recent version of this package was built for use with SBE Data Processing (Version 7.26.7).


# Installation 

To install this package:

```
remotes::install_github("afsc-gap-products/gapctd")
library(gapctd)
```

# Documentation

- [Instructions for batch processing GAP CTD data](0_batch_processing.md)
- [SBE19plus V2 data processing methods](./doc/batch_processing_methods.md)
- [Data processing errors](./doc/SBE_data_processing_errors.md)
- [Manually flagging and interpolating 'bad' data](./doc/manual_flag_interpolate.md)
- [Optimizing temperature alignment](./doc/estimating_alignment.md)
- [Optimizing cell thermal mass correction](./doc/estimating_ctm.md)

## Legal disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
