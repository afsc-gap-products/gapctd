library(gapctd)

hex_out <- hex_to_cnv(hex_path = system.file("/extdata/example/ex_hex_file.hex", package = "gapctd"), 
                      output_path = here::here("data", "test.cnv"), 
                      sample_interval = 0.25,
                      output_channels = c("time_elapsed" = "timeS: Time, Elapsed [seconds]",
                                          "temperature" = "tv290C: Temperature [ITS-90, deg C]",
                                          "pressure" = "prdM: Pressure, Strain Gauge [db]",
                                          "conductivity" = "c0S/m: Conductivity [S/m]", 
                                          "flag" = "flag:  0.000e+00"),
                      output_sig_digits = c("time_elapsed" = 3,
                                            "temperature" = 4,
                                            "pressure" = 3,
                                            "conductivity" = 6, 
                                            "flag" = 1))