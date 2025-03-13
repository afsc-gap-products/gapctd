## This function adds .hex and .xmlcon file formats to the base R Filters matrix so those options appear when using choose.files() function in hex_to_cnv.R
## Created by: Nicole Charriere
## Contact: nicole.charriere@noaa.gov
## Created: 2025-02-24
## Modified: 2025-02-26
## Modified by: Nicole Charriere

## Change base R Filters matrix so that .hex and .xmlcon can be recognized by R as file formats
## Add .hex file to base R Filters matrix

Filters_baseR <- Filters
add_hex_filter <- c("HEX file (.hex)","*.hex")
Filters_hex <- rbind(Filters_baseR, add_hex_filter)

## Add .xmlcon file to newly created Filter_hex matrix

add_xmlcon_filter <- c("Sea-Bird Instrument Configuration (.xmlcon)", "*.xmlcon")
Filters_hex_xmlcon <- rbind(Filters_hex, add_xmlcon_filter)

## Cosmetic: Reorder row names in Filters_hex_xmlcon so "All Files" appears at bottom of final Filter matrix for traditional Windows file appearance

row.names(Filters_hex_xmlcon)
Filters_hex_xmlcon_reorder <- Filters_hex_xmlcon[c(1,2,3,4,5,6,7,8,9,10,11,13,14,12),]
row.names(Filters_hex_xmlcon_reorder)

##Test file selection capability (choose.files) with Filters_hex_xmlcon_reorder

#choose.files(default = "*.hex", caption = "Select .hex file",
#             multi = FALSE, filters = Filters_hex_xmlcon_reorder,
#             index = nrow(Filters_hex_xmlcon_reorder))

#choose.files(default = "*.xmlcon", caption = "Select .xmlcon file",
#             multi = FALSE, filters = Filters_hex_xmlcon_reorder,
#             index = nrow(Filters_hex_xmlcon_reorder))
