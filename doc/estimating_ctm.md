## Optimizing Cell Thermal Mass parameters

Conductivity cell thermal mass corrections correct for lags in
conductivity measurements due to the thermal inertia of the conductivity
cell. Conductivity measurements are temperature dependent but it takes
time for the conductivity cell to heat and cool as it passes through
temperature gradients. Because salinity is calculated from conductivity
and temperature, the lag in conductivity can cause erroneous spikes in
salinity around temperature gradients. The spikes are typically negative
(i.e., toward lower salinity) as the CTD passes from warm to cold, and
positive when passing from cold to warm. As such, upcasts and downcasts
in a thermally stratified water column tend to have spikes in opposite
directions around large temperature gradients.

Cell thermal mass corrections are performed using a recursive filter
(Lueck and Pickolo, 1990) that corrects conductivity measurements
sequentially based on changes in temperature between scans and the scan
frequency (4 Hz for bottom trawl survey data collected using an
SBE19plus). For the SBE19plus, the manufacturer recommends ‘typical’
cell thermal mass correction parameters α = 0.04 and τ = 8. However, the
optimal parameters for cell thermal mass correction can differ from the
optimal values depending on the magnitude of initial conductivity errors
that can vary depending on the steepness of temperature gradients and
instrument profiling rate (Mensah et al., 2009; Martini et al., 2019).
Rather than use a single set of thermal mass correction parameters, the
gapctd package uses the `ctm_adjust_ts_area` function to estimate cell
thermal mass correction parameters (α and τ) for each deployment based
on the method of Garau et al. (2011), who estimated parameters by
minimizing the area between temperature-salinity curves from autonomous
glider downcasts and upcasts.

## Load packages and data

Load the `gapctd` package and example upcast and downcast data from the
same haul in the eastern Bering Sea. these data were processed using the
[sbe19plus_v0 method](batch_processing_methods.md) then aligned using
[estimated temperature adjustments](estimating_alignment.md) (downcast
+0.73 seconds, upcast +0.77 seconds).

``` r
library(gapctd)
library(ggthemes)

align_down_oce <- oce::read.oce(system.file("./extdata/ex_data/ctm_downcast_ex.cnv", package = "gapctd"))
align_up_oce <- oce::read.oce(system.file("./extdata/ex_data/ctm_upcast_ex.cnv", package = "gapctd"))

no_ctm_df <- dplyr::bind_rows(data.frame(timeS = align_down_oce@data$timeS[align_down_oce@data$pressure > 4],
  temperature = align_down_oce@data$temperature[align_down_oce@data$pressure > 4], 
                                         pressure = align_down_oce@data$pressure[align_down_oce@data$pressure > 4],
                                         conductivity = align_down_oce@data$conductivity[align_down_oce@data$pressure > 4],
                                         salinity = align_down_oce@data$salinity[align_down_oce@data$pressure > 4],
                                         flag = align_down_oce@data$flag[align_down_oce@data$pressure > 4],
                                         direction = "down"),
                              data.frame(timeS = align_up_oce@data$timeS[align_up_oce@data$pressure > 4],
                                         temperature = align_up_oce@data$temperature[align_up_oce@data$pressure > 4], 
                                         pressure = align_up_oce@data$pressure[align_up_oce@data$pressure > 4],
                                         conductivity = align_up_oce@data$conductivity[align_up_oce@data$pressure > 4],
                                         salinity = align_up_oce@data$salinity[align_up_oce@data$pressure > 4],
                                         flag = align_up_oce@data$flag[align_up_oce@data$pressure > 4],
                                         direction = "up"))

no_ctm_binned_df <- no_ctm_df |>
  dplyr::filter(pressure > 4, !is.na(flag)) |>
  dplyr::mutate(pressure_bin = round(pressure)) |>
  dplyr::group_by(direction, pressure_bin) |>
  dplyr::summarise(salinity = mean(salinity),
                   temperature = mean(temperature), .groups = "keep")
```

Profiles (Figure 1) show a two-layer stratified water column.
Temperature and conductivity appear to be aligned, but downcast and
upcast profiles have salinity spikes in the opposite directions around
the pycnocline. As such a conductivity cell thermal mass correction is
warranted.

![](estimating_ctm_files/figure-markdown_github/unnamed-chunk-1-1.png)

<i>Figure 1. Temperature, conductivity, and salinity versus pressure
from a CTD profiled collected during AFSC’s 2021 eastern Bering Sea
continental shelf bottom trawl survey.</i>

## Estimating CTM parameters

The gapctd package uses the `ctm_adjust_tsarea` function to estimate
optimal parameters for cell thermal mass (CTM) correction by minimizing
the area between temperature-salinity curves. The optimization starts at
the manufacturer’s recommended ’typical parameters (α = 0.04, τ = 8) and
the scan interval (f_n) is set to 0.25 seconds for the 4 Hz sampling
rate of the SBE18 plus. Scans with pressure \< 4 db are not used for the
optimization due to instances of erroneously low surface salinity and
other artifacts while the instrument is being deployed. Numerical
optimization is conducted using the Broyden-Fletcher-Goldfarb-Shanno
algorithm (method = “BFGS”) with a convergence tolerance of 1e-5 and
maximum Newton steps = 500.

``` r
optim_results <- bbmle::mle2(minuslogl = gapctd:::ctm_adjust_tsarea,
                             start = list(alpha = 0.04,
                                          tau = 8),
                             data = list(down_temperature = align_down_oce@data$temperature[align_down_oce@data$pressure > 4],
                                         down_pressure = align_down_oce@data$pressure[align_down_oce@data$pressure > 4],
                                         down_conductivity = align_down_oce@data$conductivity[align_down_oce@data$pressure > 4],
                                         down_flag = align_down_oce@data$flag[align_down_oce@data$pressure > 4],
                                         up_temperature = align_up_oce@data$temperature[align_up_oce@data$pressure > 4],
                                         up_pressure = align_up_oce@data$pressure[align_up_oce@data$pressure > 4],
                                         up_conductivity = align_up_oce@data$conductivity[align_up_oce@data$pressure > 4],
                                         up_flag = align_up_oce@data$flag[align_up_oce@data$pressure > 4],
                                         obj_fn = "area",
                                         f_n = 0.25),
                             method = "L-BFGS-B",
                             lower = c(alpha = -10, tau = 0),
                             upper = c(alpha = 10, tau = 45),
                             control = list(maxit = 500, reltol = 1e-5, trace = 1))
```

    ## Warning in optim(par = c(alpha = 0.04, tau = 8), fn = function (p) : method L-
    ## BFGS-B uses 'factr' (and 'pgtol') instead of 'reltol' and 'abstol'

    ## iter   10 value 0.113029
    ## iter   20 value 0.111565
    ## iter   30 value 0.059414
    ## final  value 0.057839 
    ## converged

``` r
optim_results@coef
```

    ##      alpha        tau 
    ## -0.2847695  1.2091625

The algorithm converges and estimated parameters (α = -0.285, τ = 1.209)
differ from the ‘typical’ starting values, which suggests the
alternative parameters should be used. The package tries a few other
starting values if the optimization does not converge and uses the
manufacturer’s typical values if none of the optimization attempts
converge or if parameters estimates results in an optimized area between
T-S curves that is higher than the area from the typical values.

## Comparison with no CTM and CTM using typical parameters

Profiles with the smallest area between T-S curves are passed to later
stages of processing and eventually used to produce data products. It is
therefore instructive to compare areas between T-S for downcast and
upcast profiles without the correction (No CTM), CTM-corrected profiles
using typical values (Default CTM), and CTM-corrected profiles using the
optimal parameters (Optimized CTM).

``` r
ts_area_df <- data.frame(ctm = c("No CTM", "Default CTM", "Optimized CTM"),
           area = c(data.frame(temperature = c(no_ctm_binned_df$temperature[no_ctm_binned_df$direction == "down"],
                                               rev(no_ctm_binned_df$temperature[no_ctm_binned_df$direction == "up"]),
                                               no_ctm_binned_df$temperature[no_ctm_binned_df$direction == "down"][1]),
                               salinity = c(no_ctm_binned_df$salinity[no_ctm_binned_df$direction == "down"],
                                            rev(no_ctm_binned_df$salinity[no_ctm_binned_df$direction == "up"]),
                                            no_ctm_binned_df$salinity[no_ctm_binned_df$direction == "down"][1]),
                               direction = c(rep("down", nrow(no_ctm_binned_df)/2),
                                             rep("up", nrow(no_ctm_binned_df)/2),
                                             "down")) |>
                      sf::st_as_sf(coords = c("temperature", "salinity")) |>
                      dplyr::group_by(ID = 1) |>
                      summarise(do_union = FALSE) |>
                      sf::st_cast(to = "POLYGON") |>
                      sf::st_area(),
                    ctm_adjust_tsarea(alpha = 0.04,
                                      tau = 8,
                                      f_n = 0.25,
                                      down_temperature = no_ctm_df$temperature[no_ctm_df$direction == "down"],
                                      down_pressure = no_ctm_df$pressure[no_ctm_df$direction == "down"],
                                      down_conductivity = no_ctm_df$conductivity[no_ctm_df$direction == "down"],
                                      down_flag = no_ctm_df$flag[no_ctm_df$direction == "down"],
                                      up_temperature = no_ctm_df$temperature[no_ctm_df$direction == "up"],
                                      up_pressure = no_ctm_df$pressure[no_ctm_df$direction == "up"],
                                      up_conductivity = no_ctm_df$conductivity[no_ctm_df$direction == "up"],
                                      up_flag = no_ctm_df$flag[no_ctm_df$direction == "up"],
                                      obj_fn = "area"),
                    ctm_adjust_tsarea(alpha = optim_results@coef['alpha'],
                                      tau = optim_results@coef['tau'],
                                      f_n = 0.25,
                                      down_temperature = no_ctm_df$temperature[no_ctm_df$direction == "down"],
                                      down_pressure = no_ctm_df$pressure[no_ctm_df$direction == "down"],
                                      down_conductivity = no_ctm_df$conductivity[no_ctm_df$direction == "down"],
                                      down_flag = no_ctm_df$flag[no_ctm_df$direction == "down"],
                                      up_temperature = no_ctm_df$temperature[no_ctm_df$direction == "up"],
                                      up_pressure = no_ctm_df$pressure[no_ctm_df$direction == "up"],
                                      up_conductivity = no_ctm_df$conductivity[no_ctm_df$direction == "up"],
                                      up_flag = no_ctm_df$flag[no_ctm_df$direction == "up"],
                                      obj_fn = "area")),
           alpha = c(NA, 0.04, optim_results@coef['alpha']),
           tau = c(NA, 8, optim_results@coef['tau']))

ts_area_df
```

    ##             ctm       area      alpha      tau
    ## 1        No CTM 0.30091436         NA       NA
    ## 2   Default CTM 0.36575860  0.0400000 8.000000
    ## 3 Optimized CTM 0.05783882 -0.2847695 1.209162

Optimized CTM has a substantially smaller area between T-S curves than
both No CTM and Default CTM. Meanwhile, corrections using the the
typical values (Default CTM) result in a larger T-S area than the
profile without a correction (No CTM). The difference in area between
curves is evident in T-S plots (Figure 2) and salinity profiles (Figure
3) for No CTM, Default CTM, and Optimized CTM. In the Optimized CTM
profile, there are no salinity spikes around the pycnocline in the
upcast and the downcast spike is smaller than in the other profiles.

![](estimating_ctm_files/figure-markdown_github/unnamed-chunk-5-1.png)

<i>Figure 2. Temperature-salinity profiles and area between upcast and
downcast profiles. Panels show areas between T-S curves without cell
thermal mass correction (No CTM), with cell thermal mass correction
using default parameter values (Default CTM), and with cell thermal mass
correction using optimized parameters (Optimized CTM).</i>

![](estimating_ctm_files/figure-markdown_github/unnamed-chunk-6-1.png)

<i>Figure 3. Upcast and downcast profiles of salinity without cell
thermal mass correction (No CTM), with cell thermal mass correction
using default parameter values (Default CTM), and with cell thermal mass
correction using optimized parameters (Optimized CTM).</i>

## References

Garau, B., S. Ruiz, W. G. Zhang, A. Pascual, E. Heslop, J. Kerfoot, and
J. Tintoré, 2011: Thermal lag correction on slocum CTD glider data. J.
Atmos. Ocean. Technol., 28, 1065–1071,
<https://doi.org/10.1175/JTECH-D-10-05030.1>

Lueck, R. G., and J. J. Picklo, 1990: Thermal Inertia of Conductivity
Cells: Observations with a Sea-Bird Cell. J. Atmos. Ocean. Technol., 7,
756–768,
<https://doi.org/10.1175/1520-0426(1990)007%3C0756:TIOCCO%3E2.0.CO;2>

Martini, K. I., D. J. Murphy, R. W. Schmitt, and N. G. Larson, 2019:
Corrections for pumped SBE 41CP CTDs determined from stratified tank
experiments. J. Atmos. Ocean. Technol., 36, 733–744,
<https://doi.org/10.1175/JTECH-D-18-0050.1>

Mensah, V., M. Le Menn, and Y. Morel, 2009: Thermal mass correction for
the evaluation of salinity. J. Atmos. Ocean. Technol., 26, 665–672,
<https://doi.org/10.1175/2008JTECHO612.1>
