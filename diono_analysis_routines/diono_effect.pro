;Name:
;	diono_effect
;purpose:
;	print approximation of the Diono effect in ppc
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
;   .r diono_valid
;   diono_valid, [yyyy, mm, dd], [yyyy, mm, dd]
;parameters:
;   
;
;dependencies:
;
;
;input files
;   Dst, dH, Kp, Kmex, Ap, Amex, Newkmex, Bsq data
;
;output files:
;   geomagnetic index plot with local effects
;   prompt list with comparative analysis
;   .PNG figure imported to /output/eventos_tgm/diono_final_V6_yyyy-mm-dd.png
;
;VERSION
;   Dec, 2022


PRO diono_effect, date_i, date_f, JPEG = jpeg 
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

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
    idate0 = STRING(yr_i, mh_i, format='(I4,I02)')
   ; TGM_n  = event_case([yr_i,mh_i,dy_i])
;###############################################################################
	station_class = ''
	PRINT, 'Enter GMS class code: 		0:regmex or 1:intermagnet'
	READ, station_class, PROMPT = '> '
	
	CASE station_class of
		'0' : station_class = 'regmex'
		'1' : station_class = 'intermagnet'
		 ELSE : PRINT, 'non avaiable gms class'
	END
	PRINT, 'Enter GMS idx: If do not know the GMS idx, please run PRO gms code table'
	READ, station_idx, PROMPT = '> '
	
    IF station_class EQ 'regmex' THEN  BEGIN    
    station = set_var.gms[FIX(station_idx)]
    station_code = set_var.gms_code[FIX(station_idx)]  
    ENDIF 
    
    IF station_class EQ 'intermagnet' THEN  BEGIN 
    station = set_var.gmsi[FIX(station_idx)] 
    station_code = set_var.gmsi_code[FIX(station_idx)] 
    ENDIF
	PRINT,  'GMS selected: '+station+' IAGA code: '+station_code   	
	
;###############################################################################		
	TGM_n = ''
	PRINT, 'Enter GMS event: 	0:20'
	READ, TGM_n, PROMPT = '> ' 
	print, 'event: ', TGM_n
;###############################################################################
;Generate the time series

; define Dst and dH variables
    dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, FIX(station_idx))
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    dat   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'H')
    H = dat.H
;###############################################################################        
;identifying NAN percentage values in the Time Series
    dH = nanpc(dH, 999999.0, 'equal')
    dH = nanpc(dH, 100.0, 'greater')
;setting certain values as NaN        
  ;  dH      = add_nan(dH, 999999.0, 'equal')        
    dH      = add_nan(dH, 100.0, 'greater')
  ;  H       = add_nan(H, 99999.0, 'greater')                
                       
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo        
    dH = fillnan(dH)       
    H  = fillnan(H)
;############################################################################### 
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')
    
; compute frequencies 
    fn    = dionstr.fn

; compute diono variables    
    diono = dionstr.diono
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn
    dst_l = dionstr.p_a+dp2+ddyn  

;###############################################################################
    time = TIMEGEN(N_ELEMENTS(dst), START=julday(mh_i, dy_i, yr_i, 0), UNITS='Hours')       
    CALDAT, time, M, D, Y, hour

    doy = INTARR(N_ELEMENTS(hour))
    yy  = INTARR(N_ELEMENTS(hour))   
    ;print, N_ELEMENTS(Y)
    FOR i=0ll, N_ELEMENTS(hour)-1 DO BEGIN
    
        yy[i] = STRMID(STRING(Y[i], format='(I4)'),2,2)
       ; print, yy[i]
        doy[i]    = Date2DOY(STRING(yy[i], M[i], D[i], FORMAT='(I02,I02,I02)'))
    ENDFOR

;PRINT, doy       
;###############################################################################      
    data_path = set_var.Mega_dir+'/article_events/dst_lambda/'
    
    idate    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4,I02,I02)')
    fdate    = STRING(yr_f, mh_f, dy_f, FORMAT='(I4,I02,I02)')
        
    outfile= data_path+'tgmdata'+idate+'_'+fdate+'.dat'
    OPENW, lun, outfile, /GET_LUN

   ; PRINTF, lun, 'DOY', 'hora', 'Dst(l)', 'DH', FORMAT = '(3A,6A,6A,6A)'
    FOR i=0, N_ELEMENTS(dst_l)-1 DO BEGIN

      ;  print, doy[i], hour[i], dst[i], H[i], tec[i], med[i], FORMAT = '(I03, F5.1, I5, I5, F6.1, F5.2)'   
        PRINTF, lun, doy[i], hour[i], dst_l[i], DH[i], FORMAT = '(I03, F5.1, I5, F6.1)'                                
    ENDFOR
    
    close,lun
    FREE_LUN, lun    
                   
END
