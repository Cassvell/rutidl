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
    
    
    stations    = ['coeneo', 'teoloyucan', 'iturbide', 'juriquilla']
    n           = N_ELEMENTS(stations)
    stat_code   = ['coe', 'teo', 'itu', 'jur']
    
    intermagnet_station = ['beijing', 'boulder', 'brandon', 'bstenis', 'cocos', 'cheongyang', 'guimar', $
    					   'hartebeesthoek', 'islandpascua', 'kakioka', 'kakadu', 'keetmanshoop', $
    					   'pilar', 'sanjuan', 'tamanraset', 'tristandacunha', 'tucson', 'honolulu', $
                           'kanoya', 'jaipur', 'lanzhou', 'misalat', 'qsaybeh', 'alibag', 'iznik']

    m 			= N_ELEMENTS(intermagnet_station)
    
    intmag_code			= [ 'bmt', 'bou', 'brd', 'bsl', 'cki', 'cyg', 'gui', 'hbk', 'ipm', 'kak', $
    						'kdu', 'kmh', 'pil', 'sjg', 'tam', 'tdc', 'tuc', 'hon', 'kny', 'jai', $
                            'lzh', 'mlt', 'qsb', 'abg', 'izn']
    
    node_gic    = ['lav', 'qro', 'maz', 'rmy']
    l           = N_ELEMENTS(node_gic)   
    set_var = {local_dir : '/home/isaac/rutidl/', $
               Mega_dir  : '/home/isaac/datos/',$               
               google_dir  : '/home/isaac/google_sync/datos/',$
               gic_dir   : '/home/isaac/MEGAsync/GICS/gic_rout/',$
               gms       : STRARR(n),$         ;geomagnetic station
               gmsi		 : STRARR(m),$         ;geomagnetic station
               gmsi_code : STRARR(m),$         ;geomagnetic station IAGA code    
               gms_code  : STRARR(n),$         ;geomagnetic station REGMEX code    
               nod_gic   : STRARR(l)$          ;gic nodes detection
                }

    set_var.gms     	= stations[*]
    set_var.gms_code	= stat_code[*]
    set_var.gmsi 		= intermagnet_station[*]
    set_var.gmsi_code 	= intmag_code[*]
    set_var.nod_gic		= node_gic[*]


    RETURN
END
