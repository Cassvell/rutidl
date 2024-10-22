pro pws, date_i, date_f
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
    idate0 = STRING(yr_i, mh_i, format='(I4,I02)')
    TGM_n  = event_case([yr_i,mh_i,dy_i])
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
    ;###############################################################################
    ; Generate the time series variables 
    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), $
                    FINAL=JULDAY(mh_f, dy_f, yr_f, 23,59), UNITS='Minutes')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])	

    dat   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = dat.H
    H = fillnan(H)

    ;plot, date_time, H

    y = fft(H)
    n = n_elements(H)
    t = 60
    fk     = (1+FINDGEN(n))/(n*t)

    fny      = FLOAT(1.0/(2.0*t)) ; frecuencia de Nyquist


    fk     = (1+FINDGEN(n))/(n*t)
    hann_w = HANNING(n)
    
    w_ss = (TOTAL((hann_w)^2))/n
    ;print, w_ss
    y_w = FFT(hann_w*H)
    pws_w = (ABS(y_w[0:n/2])^2)/w_ss     

    DEVICE, true=24, retain=2, decomposed=0
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT 

    WINDOW,4,  XSIZE=600, YSIZE=600, TITLE='gammahat1'
    PLOT, fk, pws_w, XSTYLE=1, YSTYLE=2,background=255, color=0, CHARSIZE=2, /XLOG, /YLOG
end