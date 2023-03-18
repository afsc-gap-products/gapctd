#' Get dynamic correction parameters from cnv file
#' 
#' Extract dynamic correction parameter processing text from cnv files.
#' 
#' @param cnv_path Path to a cnv file.
#' @return A data.frame containing the deployment ID (i.e., name of the original file), alignment offset for temperature (alignctd_adv_temperature), and conductivity cell thermal mass correction parameters (celltm_alpha, celltm_tau).

get_dynamic_correction_pars <- function(cnv_path) {
  
  oce_dat <- suppressWarnings(oce::read.oce(file = cnv_path))
  
  deploy_id <- oce_dat@metadata$header[grepl(pattern = "\\* FileName = ", 
                                             x = oce_dat@metadata$header)] |>
    gsub(pattern = "\\* FileName = ", 
         replacement = "") |>
    gsub(pattern = "\\.hex", 
         replacement = "") |>
    gsub(pattern = ".*\\\\", 
         replacement = "")
  
  celltm_alpha <- oce_dat@metadata$header[grepl(pattern = "celltm_alpha", x = oce_dat@metadata$header)] |>
    gsub(pattern = "# celltm_alpha = ", 
         replacement = "") |>
    gsub(pattern = "\\,.*", 
         replacement = "") |>
    as.numeric()
  
  celltm_tau <- oce_dat@metadata$header[grepl(pattern = "celltm_tau", 
                                              x = oce_dat@metadata$header)] |>
    gsub(pattern = "# celltm_tau = ", 
         replacement = "") |>
    gsub(pattern = "\\,.*", 
         replacement = "") |>
    as.numeric()
  
  alignctd_adv_temperature <- oce_dat@metadata$header[grepl(pattern = "alignctd_adv = tv290C", 
                                                            x = oce_dat@metadata$header)] |>
    gsub(pattern = "# alignctd_adv = tv290C ", 
         replacement = "") |>
    gsub(pattern = " ", 
         replacement = "") |>
    as.numeric()
  
  
  return(data.frame(deploy_id = deploy_id,
                    celltm_alpha = celltm_alpha,
                    celltm_tau = celltm_tau,
                    alignctd_adv_temperature = alignctd_adv_temperature))
}

