## SBE19plus batch processing steps

This document lists of batch processing steps for methods in gapctd that are implemented in `run_method`.

### sbe19plus_v0

Batch processing without using Align or Cell Thermal Mass Correction (CellTM) modules. Intended to be used before estimating Align and CTM parameters with `run_alignment_calcs` and `run_ctm_adjust_tsarea`.

<ol type = "1">
<li><b>Datcnv</b>: Convert hex files to cnv with the following channels as outputs: Temperature (T), Conductivity (C), Pressure</li>
<li><b>Wfilter</b>: Window filter T, C, pressure (median, 5 scan window)</li>
<li><b>Filter</b>: Low-pass filter (temperature = 0.5, salinity = 0.5, pressure = 1.0)</li>
<li><b>Loopedit</b>: Flag scans with ascent and descent speeds < 0.5 meters per second</li>
<li><b>Derive</b>: Derive EOS-80 PSS-78 salinity (PSU) and depth.</li>
<li><b>Section:</b> Use section to split upcasts and downcasts using a custom sectioning method based on on-bottom and haulback event flags from RACEBASE (with a 30 second buffer).</li>
<li><b>Binavg:</b> Calculate averages for all variables, by 1-m depth bin.
</ol>


### sbe19plus_v2

Batch processing using 'typical' values for a pumped SBE19plus V2.

<ol type = "1">
<li><b>Datcnv</b>: Convert hex files to cnv with the following channels as outputs: Temperature (T), Conductivity (C), Pressure</li>
<li><b>Wfilter</b>: Window filter T, C, pressure (median, 5 scan window)</li>
<li><b>Filter</b>: Low-pass filter (temperature = 0.5, salinity = 0.5, pressure = 1.0)</li>
<li><b>Alignctd</b>: Advance temperature relative to pressure by +0.5 seconds</li>
<li><b>Celltm</b>: Cell thermal mass correction using parameters (&alpha; = 0.04, 	&tau;<sub>CTM</sub> = 8)</li>
<li><b>Loopedit</b>: Flag scans with ascent and descent speeds < 0.5 meters per second</li>
<li><b>Derive</b>: Derive EOS-80 PSS-78 salinity (PSU) and depth.</li>
<li><b>Section:</b> Use section to split upcasts and downcasts using a custom sectioning method based on on-bottom and haulback event flags from RACEBASE (with a 30 second buffer).</li>
<li><b>Binavg:</b> Calculate averages for all variables, by 1-m depth bin.
</ol>
