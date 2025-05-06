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
    
    ; Input validation
    if (n_elements(comp) eq 0) then comp = 'equal'
    comp = strlowcase(comp)
    
    case comp of
        'equal':   mask = (x eq nanVal)
        'gequal':  mask = (x ge nanVal)
        'greater': mask = (x gt nanVal)
        'nequal':  mask = (x ne nanVal)
        'lower':   mask = (x lt nanVal)
        'lequal':  mask = (x le nanVal)
        else:      message, 'Invalid comparison operator', /info
    endcase
    
    if (total(mask) gt 0) then x[where(mask)] = !Values.F_NAN
    
    RETURN, x
END