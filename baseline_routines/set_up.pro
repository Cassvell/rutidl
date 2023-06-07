;
;Name:
;	set_up.pro
;purpose:
;	Declares and defines the COMMON variables for
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   variable generator
;
;calling sequence:
;   .r H_filmaker
;   H_filmaker, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;
;
;dependencies:
;
;
;input files
;   geomagnetic field measurements from a certain observatory or geomagnetic station.
;
;output files:
;   .txt file of diurnal base line and SQ files in 24 h resolution based on two selected
;   Q days for each GS event.
;
;   imported to: 
;version
;   Dec, 2022
;   Feb, 2023
;
;note
;   For following analysis, this routine has to be run to create clean H obs data


PRO set_up
        On_error, 2
        COMPILE_OPT idl2, HIDDEN
                
    set_var = { MEGA_dir        : '/home/isaac/MEGAsync/datos/' ,$
                local_dir       : '/home/isaac/geomstorm/rutidl/'}
    
    ;print, set_var.local_dir
    RETURN
END
