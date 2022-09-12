#' Function to move final .cnv files to bad_cnv and final_cnv directories (SBEDP workflow)
#' 
#' Moves binned .cnv files without data from the cnv folder to bad_cnv, and files with data to final_cnv.
#' 
#' @export

move_binned_cnv <- function() {
  bin_files <- list.files(path = paste0(getwd(), "/cnv/"), 
                          pattern = "_binavg", 
                          full.names = FALSE, 
                          include.dirs = TRUE)
  
  # Output data frame ----
  out_df <- data.frame(hex_file = character(length = length(bin_files)),
                       hex_id = character(length = length(bin_files)),
                       nvalues = numeric(length = length(bin_files)),
                       cnv_file = character(length = length(bin_files)))
  
  for(ii in 1:length(bin_files)) {
    
    # Find number of binned values in each cast ----
    sel_cnv <- read.delim(paste0(getwd(), "/cnv/", bin_files[ii]))
    nval_row <- sel_cnv[grepl("nvalues", sel_cnv[,1]),]
    nval_row <- gsub("[[:space:]]", "", nval_row) 
    nvals <- as.numeric(strsplit(nval_row, "=")[[1]][2])
    
    # Name of original .hex file ----
    hex_file <- sel_cnv[1,]
    hex_file <- gsub("[[:space:]]", "", hex_file) 
    hex_file <- strsplit(hex_file, "\\\\")
    hex_id <- hex_file[[1]][length(hex_file[[1]])] 
    hex_id <- strsplit(hex_id, ".hex")[[1]]
    
    hex_file <- paste0(hex_file[[1]][length(hex_file[[1]])-1], "\\", hex_file[[1]][length(hex_file[[1]])])
    
    out_df$hex_file[ii] <- hex_file
    out_df$hex_id[ii] <- hex_id
    out_df$nvalues[ii] <- nvals
    out_df$cnv_file[ii] <- bin_files[ii]
    
  }
  
  # Save csv with number of cast values ----
  write.csv(out_df, file = paste0(getwd(), "/output/cast_nvalues.csv"), row.names = FALSE)
  
  # Find and move cnv files for all casts with no values ----
  print("Moving bad cnv files")
  no_data_df <- subset(out_df, nvalues < 1)
  
  if(nrow(no_data_df) > 0) {
    for(ii in 1:nrow(no_data_df)) {
      file.rename(from = paste0(getwd(), "/cnv/", no_data_df$cnv_file[ii]), 
                  to = paste0(getwd(), "/bad_cnv/", no_data_df$cnv_file[ii]))
    }
  }
  
  # Find and move cnv files for all casts with no values ----
  print("Moving good cnv files")
  good_cnv_df <- subset(out_df, nvalues > 0)
  if(nrow(good_cnv_df) > 0) {
    for(ii in 1:nrow(good_cnv_df )) {
      file.rename(from = paste0(getwd(), "/cnv/", good_cnv_df $cnv_file[ii]), 
                  to = paste0(getwd(), "/final_cnv/", good_cnv_df $cnv_file[ii]))
    }
  }
}