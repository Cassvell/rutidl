;Name:
;	event_case.pro
;purpose:
;	generate time label names for time series in day units 
;   
;author:
;   Pedro Corona Romero
;   Investigador 
;   Instituto de Geofísica, UNAM
;   Laboratorio Nacional de Clima Espacial
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data label
;
;calling sequence:
;   .r event_case
;   x = event_case(idate, time_window)
;parameters:
;   x:              Variable in which the xlabel array is stored
;   idate:          Initial time window. Must be set in [yyyy,mm,dd] format
;   time_window:    Number of daily files used. 
;
;dependencies:
;
;
;input files
;   non
;
;output files:
;   Array of strings for time labeling figures
;version
; Dec, 2022

FUNCTION event_case, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr	= date[0]
	mh	= date[1]
	dy 	= date[2]    

    TGM_n = STRING(yr, mh, dy, FORMAT='(I4,I02,I02)')    

    CASE TGM_n of
        '20030526' : TGM_n = 1
        '20031011' : TGM_n = 2
        '20031119' : TGM_n = 3
        '20040721' : TGM_n = 4
        '20040827' : TGM_n = 5
        '20041106' : TGM_n = 6
        '20050514' : TGM_n = 7
        '20050611' : TGM_n = 8
        '20050821' : TGM_n = 9
        '20050830' : TGM_n = 10
        '20060817' : TGM_n = 11
        '20061212' : TGM_n = 12        
        '20150315' : TGM_n = 13
        '20151005' : TGM_n = 14
        '20151217' : TGM_n = 15
        '20160304' : TGM_n = 16
        '20161011' : TGM_n = 17        
        '20170526' : TGM_n = 18
        '20170906' : TGM_n = 19
        '20180824' : TGM_n = 20
        '20230225' : TGM_n = 21
        '20230322' : TGM_n = 22
        '20230422' : TGM_n = 23
        '20240510' : TGM_n = 30
        ELSE: TGM_n = 'fuera de rango'
    ENDCASE 	

	
    RETURN, TGM_n	
	
END
