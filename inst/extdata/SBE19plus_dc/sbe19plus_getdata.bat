@ SBE19plus V2 processing

@ Convert hex files
datcnv /i%1\data\*.hex /p%1\psa_xmlcon\DatCnv.psa /c%1\psa_xmlcon\%2 /o%1\cnv\ /a_raw
@ Window filter
Wfilter /i%1\cnv\*_raw.cnv /p%1\psa_xmlcon\W_Filter.psa /o%1\cnv\ /a_wfil /c%1\psa_xmlcon\%2
@ Filter
filter /i%1\cnv\*_wfil.cnv /p%1\psa_xmlcon\Filter.psa /o%1\cnv\ /a_fil1 /c%1\psa_xmlcon\%2