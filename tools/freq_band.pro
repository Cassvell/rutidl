 ;Name:
;	freq_band.pro
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

FUNCTION freq_band, event_case, band_limit
	On_error, 2
	COMPILE_OPT idl2, HIDDEN   

    TGM_n = STRING(event_case, FORMAT='(I02)')
    freq = TGM_n
IF band_limit EQ 'passband_l' THEN BEGIN

    CASE freq of
        '01' : freq = 1.07e-5  ;23:08 hr
        '02' : freq = 9.5e-6  ;23:08 hr
        '03' : freq = 8.9e-6  ;23:08 hr listo
        '04' : freq = 1.05e-5 ;23:08 hr antes 1.05e-5 
        '05' : freq = 1e-5  ;23:08 hr
        '06' : freq = 1.07e-5    ;27:46 hr listo
        '07' : freq = 1.2e-5  ;23:08 hr listo
        '08' : freq = 8.9e-6 ;23:08 hr antes 1.1e-5 
        '09' : freq = 9.5e-6  ;23:08 hr antes 1.2
        '10' : freq = 1.2e-5  ;23:08 hr
        '11' : freq = 1.05e-5  ;23:08 hr
        '12' : freq = 1.2e-5  ;23:08 hr
        '13' : freq = 1.14e-5 ;24:21 hr listo
        '14' : freq = 9.5e-6  ;23:08 hr antes 1.2e-5
        '15' : freq = 9.7e-6  ;23:08 hr antes 9.5e-6 
        '16' : freq = 8.9e-6  ;23:08 hr
        '17' : freq = 8.9e-6  ;23:08 hr        
        '18' : freq = 8.9e-6  ;30:51 hr listo
        '19' : freq = 8.9e-6 ;23:08 hr listo
        '20' : freq = 8.9e-6  ;23:08 hr
        '21' : freq = 0.0000086808842 ;31:59
        '22' : freq = 0.0000081777351 ;33:58
        '23' : freq = 0.0000073394733 ;37:50
        ELSE: PRINT, 'fuera de rango'
    ENDCASE 
ENDIF     
    ;###############################################################################
IF band_limit EQ 'passband_u' THEN BEGIN   

    CASE freq of
        '01' : freq = 1.99e-5  ;23:08 hr
        '02' : freq = 1.8e-5  ;23:08 hr
        '03' : freq = 3.2e-5  ;23:08 hr listo
        '04' : freq = 2.42e-5  ;23:08 hr
        '05' : freq = 1.8e-5  ;23:08 hr
        '06' : freq = 2.5e-5    ;27:46 hr listo
        '07' : freq = 2.8e-5  ;23:08 hr listo
        '08' : freq = 2.7e-5  ;23:08 hr
        '09' : freq = 2.2e-5  ;23:08 hr
        '10' : freq = 2.4e-5  ;23:08 hr
        '11' : freq = 2.1e-5  ;23:08 hr
        '12' : freq = 2.3e-5  ;23:08 hr
        '13' : freq = 2.8e-5 ;24:21 hr listo
        '14' : freq = 2e-5  ;23:08 hr antes 2.15e-5
        '15' : freq = 2.1e-5  ;23:08 hr
        '16' : freq = 2.6e-5  ;23:08 hr
        '17' : freq = 2.5e-5  ;23:08 hr
        '18' : freq = 3.4e-5  ;30:51 hr listo 
        '19' : freq = 3.4e-5 ;23:08 hr listo
        '20' : freq = 2.4e-5  ;23:08 hr
        '21' : freq = 0.000025261966 ;10:59
        '22' : freq = 0.000021196104 ;13:06
        '23' : freq = 0.000020048564 ;13:51        
        ELSE: PRINT, 'fuera de rango'
    ENDCASE 
ENDIF     
    ;###############################################################################
    ; define high band frequencies
IF band_limit EQ 'highpass_l' THEN BEGIN    

    CASE freq of
        '01' : freq = 6.9e-5  ;23:08 hr
        '02' : freq = 6.9e-5  ;23:08 hr
        '03' : freq = 9.25e-5  ;23:08 hr listo
        '04' : freq = 6.9e-5  ;23:08 hr
        '05' : freq = 9.3e-5  ;23:08 hr
        '06' : freq = 6.9e-5   ;27:46 hr listo 
        '07' : freq = 6.9e-5  ;23:08 hr listo
        '08' : freq = 6.9e-5  ;23:08 hr
        '09' : freq = 6.9e-5  ;23:08 hr
        '10' : freq = 6.9e-5  ;23:08 hr
        '11' : freq = 6.9e-5  ;23:08 hr
        '12' : freq = 7.3e-5  ;23:08 hr
        '13' : freq = 6.9e-5 ;24:21 hr listo
        '14' : freq = 6.9e-5  ;23:08 hr
        '15' : freq = 6.9e-5  ;23:08 hr
        '16' : freq = 6.9e-5  ;23:08 hr
        '17' : freq = 6.9e-5  ;23:08 hr
        '18' : freq = 6.9e-5  ;30:51 hr listo
        '19' : freq = 6.9e-5 ;23:08 hr listo
        '20' : freq = 9.25e-5  ;23:08 hr
        '21' : freq = 0.000081112629 ;03:25
        '22' : freq = 0.000098254002 ;02:52
        '23' : freq = 0.000095262271 ;02:54         
        ELSE: PRINT, 'fuera de rango'
    ENDCASE
ENDIF

    RETURN, freq
END
