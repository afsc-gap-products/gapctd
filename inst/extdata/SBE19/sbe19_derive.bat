@ SBE19 Derive EOS80 and TEOS10

@ Derive EOS-80
Derive /i%1\cnv\*_split.cnv /p%1\psa_xmlcon\Derive.psa /o%1\cnv\ /a_EOS80 /c%1\psa_xmlcon\%2
@ Thermodynamic Equation of Seawater
DeriveTEOS10 /i%1\cnv\*_deriveEOS80.cnv /p%1\psa_xmlcon\DeriveTEOS10.psa /o%1\cnv\ /a_TEOS10 /c%1\psa_xmlcon\%2
@ Bin cast data
Binavg /i%1\cnv\*_TEOS10.cnv /p%1\psa_xmlcon\BinAvg.psa /o%1\cnv\ /a_binavg /c%1\psa_xmlcon\%2

@ Seaplot