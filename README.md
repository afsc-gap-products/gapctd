# GAP CTD Data Processing

Import, clean, and process CTD data.

# Set up the directory

This code copies data from a CTD directory and sets up processing files in your working directory. Processing files: hex, psa, xmlcon, and batch files.

library(gapctd)
gapctd::setup_ctd_processing_dir(ctd_dir = "G:/RACE_CTD/data/2019/ebs/v162",
                                 output_dir = NA,
                                 ctd_unit = "sbe19")

# Batch processing

1. Open Command Line/Powershell
2. Navigate to the directory
3. Run the batch file by typing sbebatch sbe19.bat [working directory], where working directory is the local directory you have set up for processing (i.e. C:\user\work\ctd\ebs2019\162)

