;Name:
;	month_name.pro
;purpose:
;	Convert numeric month format (01 to 12) to string name (January to December)
;   
;author:
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
;   .r month_name
;   x = month_name(month, language)
;parameters:
;   x:              Variable in which the xlabel array is stored
;   month:          Month to be convert. Must be set in [mm] format
;   language:       Set the language in which the month is gonna be set
;
;   current options:    'english'    
;                       'spanish'
;
;dependencies:
;
;
;input files
;   month component of idate vector
;
;output files:
;   string of named variable
;version
; Dec, 2022

FUNCTION month_name, month, language
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    
    date0 = STRING(month, FORMAT='(I02)')
    old_month = date0
    IF language EQ 'english' THEN BEGIN    
        CASE old_month of
            '01': old_month = 'January'
            '02': old_month ='February'
            '03': old_month ='March'
            '04': old_month ='April'
            '05': old_month ='May'
            '06': old_month ='June'
            '07': old_month ='July'
            '08': old_month ='August'
            '09': old_month ='September'
            '10': old_month ='October'
            '11': old_month ='November'
            '12': old_month ='December'
            ELSE: PRINT, 'fuera de rango'
        ENDCASE  

    ENDIF 
    
    IF language EQ 'spanish' THEN BEGIN
        CASE old_month of
            '01': old_month ='Enero'
            '02': old_month ='Febrero'
            '03': old_month ='Marzo'
            '04': old_month ='Abril'
            '05': old_month ='Mayo'
            '06': old_month ='Junio'
            '07': old_month ='Julio'
            '08': old_month ='Agosto'
            '09': old_month ='Septiembre'
            '10': old_month ='Octubre'
            '11': old_month ='Noviembre'
            '12': old_month ='Diciembre'
            ELSE: PRINT, 'fuera de rango'
        ENDCASE      
    ENDIF 

    RETURN, old_month

END
