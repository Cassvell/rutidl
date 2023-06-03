;
;Name:
;	nanpc.pro
;purpose:
;	Search and advice about percentage of NaN values within the time series 
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
;   .r nanpc
;   x = nanpc(x) 
;parameters:
;   x : time series to analyze for NaN values presence
;
;dependencies:
;
;
;input files
;   x :     time series to fill
;   setval: set value to get NaN values
;   comp: string keyword indicating comparation around set value
;         options: 'equal'   for EQ
;                  'gequal'  for GE 
;                  'nequal'  for NE
;                  'lequal'  for LE
;                  'greater' for GT
;                  'lower'   for LT 
;output files:
;   x with NaN values detected according to set values. Return a Message about NaN percentage
;       within the data
;version
;   Dec, 2022

FUNCTION    nanpc, x, setval, comp
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

; set values considered as NaN

    IF comp EQ 'equal' THEN BEGIN	
        i_nan = WHERE(x EQ setval, ncount)
    ENDIF
        
    IF comp EQ 'nequal' THEN BEGIN
        i_nan = WHERE(x NE setval, ncount)        
    ENDIF 
    
    IF comp EQ 'gequal' THEN BEGIN         
        i_nan = WHERE(x GE setval, ncount)
    ENDIF 
    
    IF comp EQ 'lequal' THEN BEGIN         
        i_nan = WHERE(x LE setval, ncount)    
    ENDIF 
    
    IF comp EQ 'greater' THEN BEGIN         
        i_nan = WHERE(x GT setval, ncount)    
    ENDIF 
    
    IF comp EQ 'lower' THEN BEGIN         
        i_nan = WHERE(x LT setval, ncount)
    ENDIF ELSE BEGIN
        i_nan = WHERE(x EQ setval, ncount)
    ENDELSE    
                          
    prcent_nan = FLOAT(ncount)*100.0
    PRINT,'porcentaje de valores NaN es de: ', prcent_nan/N_ELEMENTS(x),'%'
    
    RETURN, x    ; Return the time series with gaps filled
END
