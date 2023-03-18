library(gapctd)

ex_haul <- readRDS(file = system.file("example_haul.rds", package = "gapctd"))
ex_oce <- read.oce(file = system.file("example_cast.cnv" , package = "gapctd"))

ex_haul <- ex_oce |>
  gapctd:::append_haul_data(haul_df = ex_haul) |>
  gapctd::median_filter(variables = c("temperature", "conductivity"),
                        window = c(5,5)) |>
  gapctd::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                         time_constant = c(0.5, 0.5, 1)) |>
  gapctd::align_var(variables = "temperature", 
                    offset = -0.5) |>
  gapctd:::assign_metadata_fields(cast_direction = "downcast") |>
  gapctd:::section_oce(by = "datetime",
                       cast_direction = "downcast") |>
  gapctd::conductivity_correction(alpha_C = 0.04, 
                                  beta_C = 1/8) |>
  gapctd::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
  gapctd::derive_eos() |>
  gapctd::bin_average(by = "depth", bin_width = 1)