;
;Name:
;	set_up.pro
;purpose:
;	set declared variables in set_up_commons.pro
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   common variables
;
;calling sequence:
;   @set_up_commons
;   set_up
;
;parameters:
;   not apply
;
;dependencies:
;
;
;input files
;   declared variables in set_up_commons
;
;output files:
;   common variables for geomstorm routines
;
;version
;   set_up june, 2023
;
;
;note
;   

PRO set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN    
    
    @set_up_commons
    
    
    stations    = ['coeneo', 'teoloyucan', 'tucson', 'bsl', 'iturbide']
    n           = N_ELEMENTS(stations)
    stat_code   = ['coe', 'teo', 'tuc', 'bsl', 'itu']
    
    node_gic    = ['lav', 'qro', 'maz']
    m           = N_ELEMENTS(node_gic)   
    set_var = {local_dir : '/home/isaac/geomstorm/rutidl/', $
               Mega_dir  : '/home/isaac/MEGAsync/datos/',$
               gic_dir   : '/home/isaac/MEGAsync/GICS/gic_rout/',$
               gms       : STRARR(n),$         ;geomagnetic station
               gms_code  : STRARR(n),$         ;geomagnetic station IAGA code    
               nod_gic   : STRARR(n)$       
                }
    set_var.gms     = stations[*]
    set_var.gms_code= stat_code[*]
    set_var.nod_gic= node_gic[*]


    RETURN
END
