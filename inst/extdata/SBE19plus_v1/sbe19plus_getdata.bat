@ SBE19plus V2 processing

@ Convert hex files
datcnv /i%1\data\*.hex /p%1\psa_xmlcon\DatCnv.psa /c%1\psa_xmlcon\%2 /o%1\cnv\ /a_raw
@ Filter
filter /i%1\cnv\*_raw.cnv /p%1\psa_xmlcon\Filter.psa /o%1\cnv\ /a_fil1 /c%1\psa_xmlcon\%2
@ Align
alignctd /i%1\cnv\*_fil1.cnv /p%1\psa_xmlcon\AlignCTD.psa /o%1\cnv\ /a_align /c%1\psa_xmlcon\%2
@ Derive
@ Derive /i%1\cnv\*_align.cnv /p%1\psa_xmlcon\Derive.psa /o%1\cnv\ /a_A80 /c%1\psa_xmlcon\%2
@ Plot
@ Seaplot /i%1\cnv\*_A80.cnv /p%1\psa_xmlcon\SeaPlot.psa /o%1\plots\
@ Cell Thermal Mass correction
Celltm /i%1\cnv\*_align.cnv /p%1\psa_xmlcon\CellTM.psa /o%1\cnv\ /a_tmcorrect /c%1\psa_xmlcon\%2
@ Loop Edit
loopedit  /i%1\cnv\*_tmcorrect.cnv /p%1\psa_xmlcon\LoopEdit.psa /o%1\cnv\ /a_loopedit /c%1\psa_xmlcon\%2
@ Window filter
Wfilter /i%1\cnv\*_loopedit.cnv /p%1\psa_xmlcon\W_Filter.psa /o%1\cnv\ /a_wfil /c%1\psa_xmlcon\%2
@ Derive
@ Derive /i%1\cnv\*_wfil.cnv /p%1\psa_xmlcon\Derive.psa /o%1\cnv\ /a_B80 /c%1\psa_xmlcon\%2
@ Plot
@ Seaplot /i%1\cnv\*_B80.cnv /p%1\psa_xmlcon\SeaPlot.psa /o%1\plots\
@ Plot
@ Seaplot /i%1\cnv\*_B80.cnv /p%1\psa_xmlcon\SeaPlot_TSPlot.psa /a_tsp /o%1\plots\