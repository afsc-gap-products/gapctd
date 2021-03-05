@SBE19 Seacat CTD processing

@ set CTD_DIR = G:\RACE_CTD\data\2019\EBS\V162\*.hex

@ Convert hex files -- CHANGE PATH TO THE .XMLCON FILES
datcnv /iG:\RACE_CTD\data\2019\EBS\V162\*.hex /pG:\RACE_CTD\psa_files\SBE19\DatCnv.psa /cC:\Users\sean.rohan\Work\afsc\race_ctd\data\SBE19_2290.xmlcon /oC:\Users\sean.rohan\Work\afsc\race_ctd\data\test\ /a_raw
@ Align CTD casts
alignctd /iC:\Users\sean.rohan\Work\afsc\race_ctd\data\test\*_raw.cnv /pG:\RACE_CTD\psa_files\SBE19\AlignCTD.psa /oC:\Users\sean.rohan\Work\afsc\race_ctd\data\test\ /a_align /cC:\Users\sean.rohan\Work\afsc\race_ctd\data\SBE19_2290.xmlcon
@ Wild Edit
@ wildedit
@ Bandpass filter
@ filter
@ Remove loops
@ loopedit
@ Cell Thermal Mass correction
Celltm /iC:\Users\sean.rohan\Work\afsc\race_ctd\data\test\*_align.cnv /pG:\RACE_CTD\psa_files\SBE19\CellTM.psa /oC:\Users\sean.rohan\Work\afsc\race_ctd\data\test\ /a_tmcorrect /cC:\Users\sean.rohan\Work\afsc\race_ctd\data\SBE19_2290.xmlcon
@ Bin cast data
@ Binavg
@ Derive EOS-80
@ Derive
@ Thermodynamic Equation of Seawater
@ DeriveTEOS10
@ Seaplot