# List of processing steps for gapctd

Steps 2-XX are performed inside of the `run_gapctd()` function.

Step \| Description \|  
1 \| Convert CTD data from .hex to .cnv, extracting Pressure,
Conductivity, Time Elapsed, and Data Flags. This is performed using the
`gapctd::setup_gapctd_directory()` function, which uses system commands
to run SBE Data Processing with .hex files and .xmlcon files from
G:/RACE_CTD/. The batch script (.bat) and program setup file (.psa)
needed to execute the system command are in the
[/inst/extdata/gapctd/](/inst/extdata/gapctd/) directory of the gapctd
repo (i.e., /extdata/gapctd/ of the package directory when the package
is installed). \|  
2 \| Read-in decoded data from a single file as a `ctd` object file
using `oce::read.ctd()` \|  
3 \| Convert timezone for `ctd` object to America/Anchorage \|  
4 \| Append haul metadata to `ctd` object using
`append_haul_data(haul_df = {haul_data})`, where \[haul_data\] is the
data.frame returned by `gapctd:::get_haul_data()` \|
