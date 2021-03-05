@SBE19 Seacat CTD processing

@ Convert hex files
datcnv /i%1\data\*.hex /p%1\psa_xmlcon\DatCnv.psa /c%1\psa_xmlcon\SBE19_2290.xmlcon /o%1\cnv\ /a_raw
@ Align CTD casts
alignctd /i%1\cnv\*_raw.cnv /p%1\psa_xmlcon\AlignCTD.psa /o%1\cnv\ /a_align /c%1\psa_xmlcon\SBE19_2290.xmlcon
@ Wild Edit
@ wildedit
@ Bandpass filter
@ filter
@ Remove loops
@ loopedit
@ Cell Thermal Mass correction
Celltm /i%1\cnv\*_align.cnv /p%1\psa_xmlcon\CellTM.psa /o%1\cnv\ /a_tmcorrect /c%1\psa_xmlcon\SBE19_2290.xmlcon
@ Bin cast data
@ Binavg
@ Derive EOS-80
@ Derive
@ Thermodynamic Equation of Seawater
@ DeriveTEOS10
@ Seaplot