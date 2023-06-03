;Name:
;	xlabel.pro
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
;   .r xlabel
;   x = xlabel(idate, time_window)
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

FUNCTION xlabel, idate, time_window
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= idate[0]
	mh_i	= idate[1]
	dy_i 	= idate[2]    
    
	
     X_label   = STRARR(time_window+1)+' '
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        old_month = mh_i
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? STRING(tmp_day, $
                        FORMAT='(I02)') : STRING(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR 	
	
    RETURN, X_label	
	
END
