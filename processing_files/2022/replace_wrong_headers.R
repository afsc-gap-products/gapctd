# Data from CTD #8102 were downloaded using the wrong config (.xmlcon) file
# This script replaces incorrect header lines from CTD #8106 with lines for the correct CTD (#8102)

# Before running this code, copy bad and good files to /data/ folder in the working directory

# Replace hex file lines
hex_files <- list.files(path = here::here("data", "202201_176_L3_bad"), recursive = TRUE, pattern = ".hex", full.names = TRUE)
hex_template <- readLines(here::here("data", "2022_07_21_176_0004.hex"))

for(ii in 1:length(hex_files)) {
  
  hex_bad <- readLines(hex_files[ii])
  
  hex_bad[3:5] <- hex_template[3:5]
  hex_bad[14:158] <- hex_template[14:158]
  hex_bad[217:355] <- hex_template[217:355]
  
  hex_path <- gsub(x = hex_files[ii], pattern = "202201_176_L3_bad", replacement = "202201_176_L3")
  
  dir.create(dirname(hex_path))
  
  hex_con <- file(hex_path)
  writeLines(text = hex_bad, con = hex_con)
  close(hex_con)
}


# Replace xml files lines
xml_files <- list.files(path = here::here("data", "202201_176_L3_bad"), recursive = TRUE, pattern = ".xml", full.names = TRUE)
xml_template <- readLines(here::here("data", "2022_07_21_176_0004.xml"))

for(ii in 1:length(xml_files)) {
  
  xml_bad <- readLines(xml_files[ii])
  
  xml_bad[1:119] <- xml_template[1:119]
  xml_bad[214:351] <- xml_template[214:351]
  
  xml_path <- gsub(x = xml_files[ii], pattern = "202201_176_L3_bad", replacement = "202201_176_L3")
  
  dir.create(dirname(xml_path))
  
  xml_con <- file(xml_path)
  writeLines(text = xml_bad, con = xml_con)
  close(xml_con)
}
