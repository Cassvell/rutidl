;Name:
;	add_nan
;purpose:
;	search for values like considered as NAN in timeseries and replace them by idl NaN values
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
;   .r add_nan
;   x = add_nan(x, nanVal, comp)
;parameters:
;   x:      time series
;   nanVal: set value to be considered as NaN
;   comp: string keyword indicating comparation around set value
;         options: 'equal'   for EQ
;                  'gequal'  for GE 
;                  'nequal'  for NE
;                  'lequal'  for LE
;                  'greater' for GT
;                  'lower'   for LT
;dependencies:
;
;
;input files
;   any time series with possible NAN values
;
;output files:
;   The same time series, with determined values considered as NaN
;version
; Dec, 2022

FUNCTION add_nan, x, nanVal, comp
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    

        FOR i=0, N_ELEMENTS(x)-1 DO BEGIN
            IF comp EQ 'equal' THEN BEGIN
                IF x[i] EQ nanVal THEN BEGIN
                    x[WHERE(x[*] EQ nanVal)] = !Values.F_NAN          
                ENDIF                                 
            ENDIF 
            
            IF comp EQ 'gequal' THEN BEGIN
                IF x[i] GE nanVal THEN BEGIN      
                    x[WHERE(x[*] GE nanVal)] = !Values.F_NAN
                ENDIF    
            ENDIF 
            
            IF comp EQ 'greater' THEN BEGIN
                IF x[i] GT nanVal THEN BEGIN      
                    x[WHERE(x[*] GT nanVal)] = !Values.F_NAN 
                ENDIF
            ENDIF 
                    
            IF comp EQ 'nequal' THEN BEGIN
                IF x[i] NE nanVal THEN BEGIN      
                    x[WHERE(x[*] NE nanVal)] = !Values.F_NAN 
                ENDIF    
            ENDIF
                    
            IF comp EQ 'lower' THEN BEGIN
                IF x[i] LT nanVal THEN BEGIN      
                    x[WHERE(x[*] LT nanVal)] = !Values.F_NAN
                ENDIF
            ENDIF
                     
            IF comp EQ 'lequal' THEN BEGIN
                IF x[i] LE nanVal THEN BEGIN      
                    x[WHERE(x[*] LE nanVal)] = !Values.F_NAN 
                ENDIF    
            ENDIF
                                                   
        ENDFOR
        
    RETURN, x
END
