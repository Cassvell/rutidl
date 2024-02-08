;
;Name:
;	index_plot.pro
;purpose:
;	plot kmex and kp data in line format
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
;   .r kmx_plot
;   kmx_plot, date_i, date_f
;parameters:
;   date(_i,_f): format = [yyyy,mm,dd]
;
;dependencies:
;
;
;input files
;   kmex data files
;
;output files:
;   kp and kmex in an IDL GUI window
;
;version
;   Dec, 2022
;

PRO index_plot, date_i, date_f
	On_error, 2
	compile_opt idl2, HIDDEN

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
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    tot_days= findgen(file_number*8)/8.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k', station, FIX(station_idx))    
    k_mex   = add_nan(k_mex, 9.0, 'greater')   

;###############################################################################
; Generate the time series DH and Dst                                
    H   = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, FIX(station_idx))
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')   

;###############################################################################                
;identifying NAN percentage values in the Time Series
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 100.0, 'greater')
;setting certain values as NaN        
    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 100.0, 'greater') 
    ;print, H    
    tot_days2       = FINDGEN(file_number*24)/24.
    
    print, FORMAT='(6X,"kp",10X,"k_mex")'
    print, MAX(kp), MAX(k_mex)

	print, '##########################################################'

    print, FORMAT='(6X,"Dst",10X,"dH")'
    print, MIN(dst), MIN(H)
    
    DEVICE, true=24, retain=2, decomposed=0
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT    
        
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')
    new_month = month_name(mh_f, 'english')
    IF mh_i NE mh_f THEN BEGIN
    	time_name = 'days of '+old_month+' and '+ new_month
    ENDIF ELSE BEGIN 
    	time_name = 'days of '+old_month
    ENDELSE
    
    WINDOW, 0,  XSIZE=1000, YSIZE=800, TITLE='kmex vx kp'

    PLOT, tot_days, kp, YRANGE=[0.0,9.0], CHARSIZE = 1.8, background=255, color=0, CHARTHICK=2.0,$
    XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label, THICK=3
    OPLOT, tot_days, k_mex, LINESTYLE=0, color=70, THICK=3  

	POLYFILL, [0.70,0.74,0.74,0.70], [0.80,0.80,0.806,0.806], color = 0, /NORMAL
	POLYFILL, [0.83,0.87,0.87,0.83], [0.80,0.80,0.806,0.806], color = 70, /NORMAL  
	
	XYOUTS, 0.745, 0.802 , /NORMAL, $
	'Kp,            Kmex', COLOR=0, $
	CHARSIZE = 1.8, $
	CHARTHICK=2.0


    IF MAX(dst) GT MAX(H) THEN up = MAX(dst) ELSE up = MAX(H)
    IF MIN(dst) LT MIN(H) THEN down = MIN(dst) ELSE down = MIN(H)
    
    WINDOW, 1,  XSIZE=1000, YSIZE=800, TITLE='Dst vs dH'

    PLOT, tot_days2, dst, YRANGE=[down, up], CHARSIZE = 1.8, background=255, color=0, CHARTHICK=2.0,$
    XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label, THICK=3
    
    OPLOT, tot_days2, H, LINESTYLE=0, color=250, THICK=3
	
	threshold = FLTARR(N_ELEMENTS(H))
	threshold[*] = -50	
	;print, 	threshold
	OPLOT, tot_days2, threshold, LINESTYLE=2, color=70, THICK=2

	POLYFILL, [0.70,0.74,0.74,0.70], [0.30,0.30,0.306,0.306], color = 0, /NORMAL
	POLYFILL, [0.83,0.87,0.87,0.83], [0.30,0.30,0.306,0.306], color = 250, /NORMAL  
	
	XYOUTS, 0.745, 0.302 , /NORMAL, $
	'Dst,            dH', COLOR=0, $
	CHARSIZE = 1.8, $
	CHARTHICK=2.0

END















