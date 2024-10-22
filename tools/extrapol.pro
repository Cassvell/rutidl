;Name:
;	extrapol.pro
;purpose:
;   interpol a time series between points in order to increment sample resolution
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data processing
;
;calling sequence:
;   .r extrapol
;   new_timeseries = extrapol(timeseries, time_resolution)
;parameters:
;   time_resolution: n_elements(desired_new_time_series_resolution)
;
;dependencies:
;
;
;input files
;   time_series
;
;output files:
;   time_series with a higher time resolution
;
;version
;   oct, 2024
;
;note
;


function extrapol, x, new_timesample
@set_up_commons
set_up
On_error, 2
COMPILE_OPT idl2, HIDDEN
;############################################################################### 
; print, diono
 new_x = FLTARR(new_timesample)     	    
 tmp_x  = INTERPOL(x, new_timesample)
 new_x = tmp_x  
    return, new_x
end
