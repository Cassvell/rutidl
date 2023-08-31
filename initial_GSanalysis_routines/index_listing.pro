;
;Name:
;	index_listing
;purpose:
;	Generating a list of events of interest
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   searching and listing info 
;
;calling sequence:
;   .r index_listing
;   list_"index", [yyyy, mm, dd], [yyyy, mm, dd]
;parameters:
;
;
;dependencies:
;
;
;input files
;   Kp, Dst, Kmex and dH indices
;
;output files:
;   txt file List containing the desired data
;Version
;Dec 2022

;###############################################################################
;###############################################################################
;###############################################################################
PRO list_kp, date_i, date_f
    
    On_error, 2
	COMPILE_OPT idl2, HIDDEN	

 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
        @set_up_commons
        set_up
        
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	

    tot_days= FINDGEN(file_number*24)/24.0  
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')    
;###############################################################################
;###############################################################################
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')   

    kp_max = FINDGEN(N_ELEMENTS(kp)/8)
    hr_max = INDGEN(N_ELEMENTS(kp)/8)
    
    time_w = TIMEGEN(N_ELEMENTS(file_number), final=JULDAY(mh_f, dy_f, yr_f), $
                start=JULDAY(mh_i, dy_i, yr_i) , units='D')
                
    caldat, time_w, m, d, y

    FOR i=0, N_ELEMENTS(kp_max)-1 DO BEGIN
        kp_max[i] = MAX(kp[i*8:(i+1)*8-1], j, /NAN)
        hr_max[i]= j
    ENDFOR


	PRINT, 'Enter threshold index for K local (kmex) equal or greater than 6: '
	threshold = ''
	READ, threshold, PROMPT = '> '
	                 
	idx = WHERE(kp_max GE threshold, count)
	IF count LE 0 THEN MESSAGE, 'ERROR'
    
    idx_kp  = kp_max[idx]
	year    = y[idx]
	month   = m[idx]
	day     = d[idx]
	hour    = hr_max[idx]

    Date    = STRING(year[0], mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
    outfile = '../rutidl/output/'+'kp_'+Date[0]+'TGM.txt'; defining the 
    ;output file names and path directories
       
    ;OPENW, LUN, outfile, /GET_LUN
    ;PRINTF, LUN, Year, Month, Day, Hour, Kp idx,  FORMAT = "(A, 2X, A, 2X, A, 2X, A, I5)"
 
    PRINT, '###################################################################'
    PRINT, 'lista de eventos de tormenta geomagnética '
    PRINT, '###################################################################'
    PRINT, '                                                                    '
    PRINT, 'Descripción: Identificación de la fecha y hora en formato '
    PRINT, 'fecha yy/mm/dd hh, día del año de cuando el índice Kp ascendió por'
    PRINT, 'encima de 7, significando un evento de tormenta geomagnética intensa.'
    PRINT, '                                                                    '
    
    PRINT, format='(2X, "Fecha", 4X, "Hora", 3X, "DOY", 2X, "Indice Kp")'
    PRINT, '---------------------------------'
       
    FOR i=0, N_ELEMENTS(idx)-1 DO BEGIN        
            IF idx_kp[i] NE 0 THEN BEGIN

                PRINT, year[i], month[i], day[i], hour[i], idx_kp[i], $
                             FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I02)'  
                ;PRINTF, lun, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_kp_tmp[i], $
                           ; FORMAT = '(I4, X, I02, X, I02, 2X, I02, 4X, I03, X, I02)'                      
        ENDIF        
    ENDFOR    
   ; CLOSE, LUN
    ;FREE_LUN, LUN
    PRINT, '                                                                   '
    PRINT, '###################################################################'
    PRINT, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    PRINT, 'límites de ventana del tiempo en formato DOY entorno a cada evento'

END     

PRO list_kmex, date_i, date_f

	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]	
;##############################################################################
 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
        @set_up_commons
        set_up
;###############################################################################
;define station data
	station_idx = ''
	PRINT, 'Enter GMS idx: 0:coe, 1:teo, 2:tuc, 3:bsl, 4:itu'
	READ, station_idx, PROMPT = '> '
    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu	
;###############################################################################
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k', station, FIX(station_idx))    
    k_mex   = add_nan(k_mex, 9.0, 'greater')                                                        ;built my self function to add nan to time series

    km_max = FINDGEN(N_ELEMENTS(k_mex)/8)
    hr_max = INDGEN(N_ELEMENTS(k_mex)/8)
    
    time_w = TIMEGEN(N_ELEMENTS(file_number), final=JULDAY(mh_f, dy_f, yr_f, 23), $
                start=JULDAY(mh_i, dy_i, yr_i, 0) , units='D')                
    CALDAT, time_w, m, d, y

    FOR i=0, N_ELEMENTS(km_max)-1 DO BEGIN
        km_max[i] = MAX(k_mex[i*8:(i+1)*8-1], j, /NAN)
        hr_max[i]= j
    ENDFOR

	PRINT, 'Enter threshold index for K local (kmex) equal or greater than 6: '
	threshold = ''
	READ, threshold, PROMPT = '> '
	             
	idx = WHERE(km_max GE FIX(threshold), count)
	IF count LE 0 THEN MESSAGE, 'ERROR'       

    y_idx        = y[idx]   
    m_idx        = m[idx]
    d_idx        = d[idx]
    k_mex_idx    = km_max[idx]
    hr_idx       = hr_max[idx]    
    
    ;outfile='../rutidl/output/'+'Kmex_list_TGM.txt'
     ;   OPENW, lun, outfile, /GET_LUN, /append
        
    PRINT, '###################################################################'
    PRINT, 'lista de eventos de tormenta geomagnética '
    PRINT, '###################################################################'
    PRINT, '                                                                    '
    PRINT, 'Descripción: Identificación de la fecha y hora en formato '
    PRINT, 'fecha yy/mm/dd hh, día del año de cuando el índice DH ascendió por'
    PRINT, 'encima de 6, significando un evento de tormenta geomagnética '
    PRINT, 'intensa.'
    PRINT, '                                                                    '    
    PRINT, format='(2X, "Fecha", 4X, "Hora", 2X, "Indice Kmex")'
    PRINT, '---------------------------------'
    PRINT, '                                                                    '
         
    FOR i=0, N_ELEMENTS(idx)-1 DO BEGIN
           IF d_idx[i] NE 0 THEN BEGIN

                print, y_idx[i], m_idx[i], d_idx[i], hr_idx[i], $
                k_mex_idx[i], $
                FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I03)' 
  
               ; PRINTF, lun, y_idx_tmp[i], m_idx_tmp[i], d_idx_tmp[i], $
               ; hr_idx_tmp[i], k_mex_idx_tmp[i], $ 
               ; FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I03)'                         
        ENDIF        
    ENDFOR
    ;CLOSE,lun
    ;FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
   PRINT, '                                                                    '
    PRINT, '###################################################################'
    PRINT, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    PRINT, 'límites de ventana del tiempo en formato DOY entorno a cada evento' 
END


PRO list_dst, date_i, date_f
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]	

 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
        @set_up_commons
        set_up	
;###############################################################################

        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1     
;###############################################################################
;Dst Data                       
    dst0 = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')                                
        
    dst0_min = FINDGEN(N_ELEMENTS(dst0)/24)
    hr_min= INDGEN(N_ELEMENTS(dst0)/24)

    time_w = TIMEGEN(N_ELEMENTS(file_number), final=JULDAY(mh_f, dy_f, yr_f, 23), $
                start=JULDAY(mh_i, dy_i, yr_i, 0) , units='D')
                
    CALDAT, time_w, m, d, y

    FOR i=0, N_ELEMENTS(dst0_min)-1 DO BEGIN
        dst0_min[i] = MIN(dst0[i*24:(i+1)*24-1], j, /NAN)
        hr_min[i]= j
    ENDFOR

	PRINT, 'Enter threshold index for dH local equal or lower than -30: '
	threshold = ''
	READ, threshold, PROMPT = '> '
	
    idx = WHERE(dst0_min LE threshold, count)
	IF count LE 0 THEN MESSAGE, 'ERROR' 
           	
    y_idx      = y[idx]   
    m_idx      = m[idx]
    d_idx      = d[idx]
    dst_idx    = dst0_min[idx]
    hr_idx     = hr_min[idx]
      
    Date    = STRING(yr_i)
    outfile = '../rutidl/output/'+'dst_'+Date[0]+'TGM.txt'   ; defining the output file
    ;names and path directories
   
 ;   OPENW, lun, Outfile, /GET_LUN
   
    PRINT, '###################################################################'
    PRINT, 'lista de eventos de tormenta geomagnética '
    PRINT, '###################################################################'
    PRINT, '                                                                    '
    PRINT, 'Descripción: Identificación de la fecha y hora en formato '
    PRINT, 'fecha yy/mm/dd hh, día del año de cuando el índice Dst descendió por'
    PRINT, 'debajo de -150 nT, lo que es un indicativo de la ocurrencia de un '
    PRINT, 'un evento de tormenta geomagnética intensa y de interés  '
    PRINT, '                                                                   '
    PRINT, FORMAT='("Fecha", 6X, "Hora", 8X, "Indice Dst")'
   
    FOR i=0, N_ELEMENTS(idx)-1 DO BEGIN

           IF d_idx[i] NE 0 THEN BEGIN

            ;idx_kp[idx] = idx_kp[i]

                PRINT, y_idx[i], m_idx[i], d_idx[i], hr_idx[i], dst_idx[i], $
                             FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 3X, I)'  
               ; PRINTF, lun, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_dst_tmp[i], $
                ;            FORMAT = '(I4, X, I02, X, I02, 2X, I02, 2X, I03, X, I4)'             
            ENDIF 
   
        ;PRINTF, lun, year[i], month[i], day[i], hour[i], doy[i], idx_kp[i], $
        ;FORMAT = '(I4, "/", I02, "/", I02, X, I02, 2X, I03, 4X, I)'       
    ENDFOR
    
  ;  CLOSE,lun
   ; FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
    PRINT, '                                                                    '
    PRINT, '###################################################################'
    PRINT, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    PRINT, 'límites de ventana del tiempo en formato DOY entorno a cada evento'     
END 

PRO list_dh, date_i, date_f

	On_error, 2
	COMPILE_OPT idl2, HIDDEN

 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
        @set_up_commons
        set_up
        
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]	
;##############################################################################
;###############################################################################
;define station data
	station_idx = ''
	PRINT, 'Enter GMS idx: 0:coe, 1:teo, 2:tuc, 3:bsl, 4:itu'
	READ, station_idx, PROMPT = '> '
    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu	
;###############################################################################
; reading data files
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1    
    
    H  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, FIX(station_idx))
 
    H = add_nan(H, 999999.0, 'equal')          
    time_w = TIMEGEN(N_ELEMENTS(file_number), final=JULDAY(mh_f, dy_f, yr_f, 23), $
                start=JULDAY(mh_i, dy_i, yr_i, 0) , units='D')
                
    CALDAT, time_w, m, d, y    
    
    H_min = FINDGEN(N_ELEMENTS(H)/24)
    hr_min= INDGEN(N_ELEMENTS(H)/24)

    FOR i=0, N_ELEMENTS(H_min)-1 DO BEGIN
        H_min[i] = MIN(H[i*24:(i+1)*24-1], j, /NAN)
        hr_min[i]= j
    ENDFOR

	PRINT, 'Enter threshold index for dH local equal or lower than -30: '
	threshold = ''
	READ, threshold, PROMPT = '> '

    idx = WHERE(H_min LE threshold, count)
	IF count LE 0 THEN MESSAGE, 'ERROR' 
           	
    y_idx    = y[idx]   
    m_idx    = m[idx]
    d_idx    = d[idx]
    H_idx    = H_min[idx]
    hr_idx   = hr_min[idx]
    
    outfile='/home/isaac/geomstorm/rutidl/output/DH_list_TGM.dat'
     ;   OPENW, lun, outfile, /GET_LUN, /append
        
    PRINT, '###################################################################'
    PRINT, 'lista de eventos de tormenta geomagnética '
    PRINT, '###################################################################'
    PRINT, '                                                                    '
    PRINT, 'Descripción: Identificación de la fecha y hora en formato '
    PRINT, 'fecha yy/mm/dd hh, día del año de cuando el índice DH descendió por'
    PRINT, 'debajo de -100 nT, significando un evento de tormenta geomagnética '
    PRINT, 'intensa.'
    PRINT, '                                                                    '    
    PRINT, format='(2X, "Fecha", 4X, "Hora", 2X, "Indice DH")'
    PRINT, '---------------------------------'
    PRINT, '                                                                    '
         
    FOR i=0, N_ELEMENTS(H_idx)-1 DO BEGIN
           IF d_idx[i] NE 0 THEN BEGIN

                PRINT, y_idx[i], m_idx[i], d_idx[i], hr_idx[i], $
                H_idx[i], FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, F6.1)' 
  
          ;      PRINTF, lun, y_idx_tmp[i], m_idx_tmp[i], d_idx_tmp[i], $
           ;     hr_idx_tmp[i], H_idx_tmp[i], FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, F6.1)'                         
        ENDIF        
    ENDFOR
    
   ; CLOSE,lun
   ; FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
    PRINT, '                                                                   '
    PRINT, '###################################################################'
    PRINT, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'

    PRINT, 'límites de ventana del tiempo en formato DOY entorno a cada evento' 
END
