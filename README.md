# GAP CTD Data Processing

Import, clean, and process CTD data. Calculates absolute salinity, practical salinity, temperature, density.

# Set up the directory

This code copies data from a CTD directory and sets up processing files in your working directory. Processing files: hex, psa, xmlcon, and batch files.<br/>
Run this code after connecting to the VPN. **Tip**: if you get "No xmlcon file found in the CTD directory" error message, you sometimes need to open that G drive folder before running code.
devtools::install_github("sean-rohan-noaa/gapctd")<br/>
library(gapctd)<br/>

# Set up CTD files for batch processsing in a local directory
gapctd::setup_ctd_processing_dir(ctd_dir = "G:/RACE_CTD/data/2019/ebs/v162",
                                 output_dir = NA,
                                 ctd_unit = "sbe19")<br/>
                                 
# Automatically start batch processing in R

1. Set up a new directory and set it as the working directory. 
2. Run run_sbe_batch() after setting up the CTD directory for batch processing as descrbed above.
3. Run make_cast_plots() to generate plots for visual QA/QC inspection<br/>.

# Batch processing in Command Line

1. Open Command Line/Powershell
2. Navigate to the local directory in your command line that you set up for processing.
3. Run the batch file by typing:
    sbebatch sbe19.bat [working directory] [xmlcon filename], where working directory is the local directory you have set up for processing (e.g., C:\user\work\ctd\ebs2019\162) and xlmcon filename is the name of the .xmlcon file in the local /psa_xmlcon/ subdirectory (e.g., SBE19_2290.xmlcon).

