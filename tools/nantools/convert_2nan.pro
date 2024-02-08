;Name:
;	convert_2nan
;purpose:
;	search for NAN values loc (indices) within timeseries and replace each next value for a NAN
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
;   .r convert_2nan
;   convert_2nan, time_series
;parameters:
;   time series
;
;dependencies:
;
;
;input files
;   any time series with existing NAN values
;	time_series[NAN] = NAN
;output files:
;   The same time series, with determined values considered as NaN
;	time_series[NAN+1] = NAN too (previously a valid Value)
;
;version
; Feb, 2024

PRO convert_2nan, series, direction
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
        @set_up_commons
        set_up	
    ; Find indices of NaN values
    nan_indices = WHERE(~finite(series))

	IF nan_indices[0] NE -1 THEN BEGIN
    ; Iterate over NaN indices
    	IF direction EQ 'r' THEN BEGIN
			FOR i = 0, N_ELEMENTS(nan_indices) - 1 DO BEGIN
				; Get the index of the next value after NaN
				nan_ext = nan_indices[i] + 1

				; Convert the next value after NaN to NaN
				series[nan_ext] = !Values.F_NAN
			ENDFOR
    	ENDIF 
    	IF direction EQ 'l' THEN BEGIN
			FOR i = 0, N_ELEMENTS(nan_indices) - 1 DO BEGIN
				; Get the index of the next value after NaN
				nan_ext = nan_indices[i] - 1

				; Convert the next value after NaN to NaN
				series[nan_ext] = !Values.F_NAN
			ENDFOR    		
    	ENDIF ELSE BEGIN
    		PRINT, "select a valid option, r/l"
    	ENDELSE
    
    ENDIF ELSE BEGIN
    	series = series
    	PRINT, 'No NAN Values'
    ENDELSE
    
END
