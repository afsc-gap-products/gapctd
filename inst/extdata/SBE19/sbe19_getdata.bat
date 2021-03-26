@SBE19 Seacat CTD processing

@ Convert hex files
datcnv /i%1\data\*.hex /p%1\psa_xmlcon\DatCnv.psa /c%1\psa_xmlcon\%2 /o%1\cnv\ /a_raw
@ Window filter
Wfilter /i%1\cnv\*_raw.cnv /p%1\psa_xmlcon\W_Filter.psa /o%1\cnv\ /a_fil1 /c%1\psa_xmlcon\%2
@ Filter
filter /i%1\cnv\*_fil1.cnv /p%1\psa_xmlcon\Filter.psa /o%1\cnv\ /a_fil2 /c%1\psa_xmlcon\%2
@ Align CTD casts
alignctd /i%1\cnv\*_fil2.cnv /p%1\psa_xmlcon\AlignCTD.psa /o%1\cnv\ /a_align /c%1\psa_xmlcon\%2
@ Cell Thermal Mass correction
Celltm /i%1\cnv\*_align.cnv /p%1\psa_xmlcon\CellTM.psa /o%1\cnv\ /a_tmcorrect /c%1\psa_xmlcon\%2
@ Remove loops
@ loopedit /i%1\cnv\*_tmcorrect.cnv /p%1\psa_xmlcon\LoopEdit.psa /o%1\cnv\ /a_loopedit /c%1\psa_xmlcon\%2
@ Wild Edit
@ wildedit /i%1\cnv\*_loopedit.cnv /p%1\psa_xmlcon\WildEdit.psa /o%1\cnv\ /a_wildedit_filter /c%1\psa_xmlcon\%2
@ Split upcast and downcast
Split /i%1\cnv\*_tmcorrect.cnv /p%1\psa_xmlcon\Split.psa /o%1\cnv\ /a_split /c%1\psa_xmlc