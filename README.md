# GAP CTD Data Processing

Import, clean, and process CTD data.

# Set up the directory

This code copies data from a CTD directory and sets up processing files in your working directory. Processing files: hex, psa, xmlcon, and batch files.<br/>
Run this code after connecting to the VPN. **Tip**: if you get "No xmlcon file found in the CTD directory" error message, you sometimes need to open that G drive folder before running code. 

devtools::install_github("sean-rohan-noaa/gapctd")<br/>
library(gapctd)<br/>
gapctd::setup_ctd_processing_dir(ctd_dir = "G:/RACE_CTD/data/2019/ebs/v162",
                                 output_dir = NA,
                                 ctd_unit = "sbe19")

# Batch processing

1. Open Command Line/Powershell
2. Navigate to the local directory in your command line that you set up for processing.
3. Run the batch file by typing:
    sbebatch sbe19.bat [working directory], where working directory is the local directory you have set up for processing (e.g., C:\user\work\ctd\ebs2019\162)

