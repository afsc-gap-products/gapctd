#' Change AlignCTD parameters
#' 
#' Change SBE Data Processing alignment module parameters in a .psa file.
#' 
#' @param var Character vector indicating which variable channel should be aligned ("temperature", "conductivity", or "oxygen")
#' @param alignment Adjustment to channel alignment relative to pressure, in seconds. The 'typical' alignment for a pumped SBE19plus is 0.5 for temperature.
#' @param psa_path File path to the alignctd psa file. If not provided, attempts to use /psa_xmlcon/alignctd.psa.
#' @details This funtion modifies text in psa files that are used to run the AlignCTD module in SBE Data Processing. 
#' @export

change_psa_alignctd <- function(variable, alignment, psa_path = NULL) {
  
  variable <- tolower(variable)
  
  if(is.null(psa_path)) {
    psa_path <- here::here("psa_xmlcon", "alignctd.psa")
  }
  
  if(variable == "temperature") {
    align_psa_file <- readLines(psa_path)
    align_index <- which(grepl(pattern = "variable_name=\"Temperature\" />", align_psa_file))
  }
  
  if(variable == "conductivity") {
    align_psa_file <- readLines(psa_path)
    align_index <- which(grepl(pattern = "variable_name=\"Conductivity\" />", align_psa_file))
  }
  
  if(variable == "oxygen") {
    align_psa_file <- readLines(psa_path)
    align_index <- which(grepl(pattern = "variable_name=\"Oxygen\" />", align_psa_file))
  }
  
  string1 <- paste0(gsub(pattern = "\\value.*", "", align_psa_file[align_index]), "value")
  string2 <- paste0("=\"", format(alignment, nsmall = 3),  "000\" ")
  string3 <- paste0("variable_name", gsub(pattern = ".*variable_name", "", align_psa_file[align_index]))
  new_string <- paste0(string1, string2, string3)
  align_psa_file[align_index] <- new_string
  psa_con <- file(psa_path)
  writeLines(text = align_psa_file, con = psa_con)
  close(psa_con)
  
}

#' Change CellTM parameters
#' 
#' Change SBE Data Processing conductivity cell thermal inertia correction module parameters in a .psa file.
#' 
#' @param alpha Alpha parameter. 1L numeric vector.
#' @param tau_ctm Tau parameter (i.e., inv_beta) 1L numeric vector.
#' @param psa_path File path to cell thermal mass .psa file.
#' @details This funtion modifies text in psa files that are used to run the CellTM module in SBE Data Processing. 
#' @export

change_psa_celltm <- function(alpha, tau_ctm, psa_path = here::here("psa_xmlcon", "celltm.psa")) {
  
  celltm_psa_file <- readLines(psa_path)
  
  celltm_index <- which(grepl(pattern = "    <TA_Amplitude", celltm_psa_file))
  celltm_psa_file[celltm_index] <- paste0("    <TA_Amplitude value=\"", format(alpha, nsmall = 3), "000\" />")
  
  celltm_index <- which(grepl(pattern = "    <TA_TimeConstant", celltm_psa_file))
  celltm_psa_file[celltm_index] <- paste0("    <TA_TimeConstant value=\"", format(tau_ctm, nsmall = 1), "00000\" />")
  
  psa_con <- file(psa_path)
  writeLines(text = celltm_psa_file, con = psa_con)
  close(psa_con)
  
}
