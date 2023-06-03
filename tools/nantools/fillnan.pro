;
;Name:
;	fillnan.pro
;purpose:
;	To fill the gaps within the data with interpolated values 
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data analysis
;
;calling sequence:
;   .r fillnan
;   x = fillnan(x)
;parameters:
;   
;
;dependencies:
;
;
;input files
;   x : time series to fill
;
;output files:
;   x with gap filled with interpolated vaules. It is recommended to use in case of low %NaN
;       within the data
;version
;   Dec, 2022

FUNCTION    fillnan, x
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
    x_tmp   = x
    x_exist = WHERE(finite(x_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    
    ; interpolate at the locations of the bad data using the good data    
    IF nbaddata GT 0 THEN x_tmp[baddata] = INTERPOL(x_tmp[x_exist], x_exist, baddata)
    x = x_tmp    
    x_tmp   = x
    
    RETURN, x    ; Return the time series with gaps filled
END
