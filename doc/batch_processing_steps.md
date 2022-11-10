# Batch Processing Steps

![](/doc/assets/gapctd_workflow.png)

This document lists data processing steps for the four methods used to
process GAP CTD data (Table 1). The four methods (Typical, Typical CTM,
Temperature-Salinity Area, and Salinity Path Distance) use different
parameters for temperature alignment (Step \#7) and conductivity cell
thermal mass correction (Step \#8), which are described in Table 2 and
Table 3. Steps 2-13 are performed inside of the `run_gapctd()` function.
<br><br>

<b>Table 1. </b> Data processing steps for Typical, Typical CTM,
Temperature-Salinity Area (TSA), and Salinity Path Distance (SPD)
methods for processing data using the gapctd package. \| Step \|
Description \| \|————————————\|————————————\| \| 1 \| <b>Convert</b> CTD
data from .hex to .cnv, extracting Pressure, Conductivity, Time Elapsed,
and Data Flags. This is performed using the
`gapctd::setup_gapctd_directory()` function, which uses system commands
to run SBE Data Processing with .hex files and .xmlcon files from
G:/RACE_CTD/. The batch script (.bat) and program setup file (.psa)
needed to execute the system command are in the
[/inst/extdata/gapctd/](/inst/extdata/gapctd/) directory of the gapctd
repo (i.e., /extdata/gapctd/ of the package directory when the package
is installed). \| \| 2 \| Load the converted .cnv files into a `ctd`
object using `oce::read.oce()`. \| \| 3 \| Assign haul metadata to the
`ctd` object using `gapctd::assign_metadata(x = [ctd])`. \| \| 4 \|
<b>Split</b> data in `ctd` object into downcast, bottom, and upcast
sections based on CTD data time stamps and haul metadata using
`gapctd::section_oce(x = [ctd], by = 'datetime')` \| \| 5 \| <b>Median
filter</b> temperature and conductivity channels with five (5) scan
windows for each channel using
`gapctd::median_filter(x = [ctd], variables = c("temperature", "conductivity"), window = c(5,5))`
\| \| 6 \| <b>Low pass filter</b> temperature, conductivity and pressure
using 0.5, 0.5, and 1.0 second time constants, respectively, using
`gapctd::lowpass_filter(x = [ctd], variables = c("temperature", "conductivity", "pressure"), time_constant = c(0.5, 0.5, 1), precision = c(4, 6, 3), freq_n = 0.25)`
\| \| 7 \| <b>Align temperature</b>. Apply optimal alignment from step
\#5 to temperature channel using
`gapctd::align_var(x = [ctd], variables = "temperature", interp_method = "linear", offset = [offset])`
\| \| 8 \| <b>Conductivity cell thermal mass correction (Cell TM)</b>.
Apply optimal conductivity cell thermal mass correction to upcast and
downcast data using
`gapctd::conductivity_correction(x = [ctd], alpha_C = [alpha], beta_C = [beta])`.
Parameters vary depending on the method (Typical, Typical CTM, TSA, SPD)
\| \| 9 \| <b>Slowdown</b>. Flag data where the mean ascent/descent
speed of the CTD for a five scan window around an observation was \< 0.1
meters per second, except near-bottom. Uses:
`gapctd::slowdown(x = [ctd], min_speed = 0.1, window = 5)` \| \| 10 \|
<b>Derive</b> depth, salinity, density, absolute salinity, sound speed,
and squared buoyancy frequency for downcast, bottom, and upcast data,
using `gapctd::derive_eos(x = [ctd])` \| \| 11 \| <b>Bin average</b>.
Calculate averages by 1-m depth bin for all variable channels (excluding
scans flagged ‘bad’). gapctd:::bin_average(downcast, by = “depth”,
bin_width = 1). Uses
`gapctd::bin_average(x = [ctd], by = "depth", bin_width = 1)` \| \| 12
\| <b>Density inversion check</b>. Check for unreasonable density
inversions based on the squared buoyancy frequency, N2, where inversions
are considered unrealistic when N2 \< -1e-4 using
`gapctd::check_density_inversion(threshold  = -1e-4, threshold_method = "bv", correct_inversion = TRUE)`
\| \| 13 \| <b>Completeness check</b>. Check for casts with an
unreasonable amount of missing data using
`gapctd::qc_check(prop_max_flag = 0.1, prop_min_bin = 0.9)`. \| \| 14 \|
<b>Visual error inspection</b>. Visually inspect downcast and upcast
profile data for errors. Remove and interpolate the errors. Uses
`gapctd:::wrapper_flag_interpolate()` as a wrapper for
`gapctd::qc_flag_interpolate(x = [ctd], review = c("density", "salinity"))`
\| \| 15 \| <b>Review profiles</b>. Review downcast and upcast profile
data for quality and select profiles to include in the final data
product. Uses `gapctd::review_profiles()`\| <br><br><br> <b>Table 2.
</b> Alignment offsets parameters by data processing method. \| Method
\| Channel \| Offset (s) \| Description \|
\|——————————–\|——————————–\|——————————–\|——————————–\| \| Typical \|
Temperature \| -0.5 \| Typical setting recommended by the manufacturer.
\| \| Typical CTM \| Temperature \| Estimated \| Use grid search to find
the offset parameter that maximizes the coefficient of determination
(r\<sup2</sup>) between first derivatives of temperature and
conductivity with respect to time for individual upcasts and downcasts,
to the nearest 0.01 s (Ullman and Hebert, 2014). \| \| TSA \|
Temperature \| Estimated \| Same as Typical CTM \| \| SPD \| Temperature
\| Estimated \| Same as Typical CTM \| <br><br><br>

<b>Table 3. </b> Conductivity cell thermal mass correction parameters by
data processing method. \| Method \| α \| β \| Description \|
\|——————————–\|——————————–\|——————————–\|——————————–\| \| Typical \|
0.04 \| 1/8 \| Typical setting recommended by the manufacturer. \| \|
Typical CTM \| 0.04 \| 1/8 \| Typical setting recommended by the
manufacturer. \| \| TSA \| Estimated \| Estimated \| Estimate optimal
conductivity cell thermal mass correction parameters (alpha and beta) by
using numerical optimization (Broyden-Fletcher-Goldfarb-Shanno with box
constraints) to find parameter values that minimize the area between
temperature-salinity curves from upcasts and downcasts (Garau et al.,
2011). Starting parameters based on initial grid search parameters α
(0.001, 0.01, 0.02, 0.04, 0.08, 0.12), β (1, 1/2, 1/4, 1/8, 1/12, 1/24).
Uses typical values (alpha = 0.04, beta = 0.125) if optimization does
not converge. \| \| SPD \| Estimated \| Estimated \| Same as TSA but
optimization minimizes the path distance of the salinity curve for
individual casts instead of the area between downcast and upcast T-S
curves. \| <br><br><br>

## References

Garau, B., Ruiz, S., Zhang, W. G., Pascual, A., Heslop, E., Kerfoot, J.,
& Tintoré, J. (2011). Thermal lag correction on slocum CTD glider data.
Journal of Atmospheric and Oceanic Technology, 28(9), 1065–1071.
<https://doi.org/10.1175/JTECH-D-10-05030.1>

Ullman, D. S., & Hebert, D. (2014). Processing of underway CTD data.
Journal of Atmospheric and Oceanic Technology, 31(4), 984–998.
<https://doi.org/10.1175/JTECH-D-13-00200.1>
