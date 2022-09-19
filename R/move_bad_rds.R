#' Remove rds files based on QA/QC flags (R workflow)
#' 
#' Remove deployment rds files that contain bottom, upcast, and/or bottom oce objects based on QA/QC flags.
#' 
#' @param rds_dir_path File path to directory containing rds files to be evaluated.
#' @return Bad files moved to /bad_cnv/
#' @export

move_bad_rds <- function(rds_dir_path = here::here("output", "gapctd")) {
  
  rds_path <- list.files(rds_dir_path, pattern = ".rds", full.names = TRUE)
  
  n_moved <- 0
  
  for(hh in 1:length(rds_path)) {
    
    message("move_bad_rds: Checking ", rds_path[hh])
    eval_deployment <- readRDS(rds_path[hh])
    
    dc_flag <- TRUE
    if("downcast" %in% names(eval_deployment)) {
      dc_flag <- any(as.logical(eval_deployment$downcast@metadata$flags))
    }
    
    uc_flag <- TRUE
    if("upcast" %in% names(eval_deployment)) {
      uc_flag <- any(as.logical(eval_deployment$upcast@metadata$flags))
    }
    
    # Move bad rds file
    if(any(dc_flag, uc_flag)) {
      message(paste0("move_bad_rds: Moving ", rds_path[hh]))
      
      new_loc <- gsub(pattern = paste0("/output/", processing_method, "/"), 
                      replacement = "/bad_cnv/",
                      x = rds_path[hh])
      
      file.rename(from =  rds_path[hh], to = new_loc)
      
      n_moved <- n_moved + 1
    }
    
    # Remove only the bad profile
    if(dc_flag != uc_flag) {
      
      cond <- FALSE
      
      if(dc_flag & "downcast" %in% names(eval_deployment) & !uc_flag) {
        eval_deployment <- eval_deployment[-which(names(eval_deployment) == "downcast")]
        out_path <- gsub(pattern = "_raw.rds", replacement = "_uc_raw.rds", x = rds_path[hh])
        cond <- TRUE
      }
      
      if(uc_flag & "upcast" %in% names(eval_deployment) & !dc_flag) {
        eval_deployment <- eval_deployment[-which(names(eval_deployment) == "upcast")]
        out_path <- gsub(pattern = "_raw.rds", replacement = "_dc_raw.rds", x = rds_path[hh])
        cond <- TRUE
      }
      
      if(cond) {
        saveRDS(object = eval_deployment, file = out_path)
      }

      
    }
  }
  
  message(paste0("move_bad_rds: Moved ", n_moved, " files to /bad_cnv/"))
  
}