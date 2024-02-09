FUNCTION day2day, X, component, date_i, date_f 
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
;###################################################################################################
;componente X
    X_hr = FINDGEN(N_ELEMENTS(X)/60)	;Mediana
    X_IQR = FINDGEN(N_ELEMENTS(X)/60)	;IQR de cada hora de datos
    
    FOR i=0, N_ELEMENTS(X_hr)-1 DO BEGIN
    	X_hr[i] = MEDIAN(X[i*60:(i+1)*60-1]) ;mediana
        QR1= cgPercentiles(X[i*60:(i+1)*60-1], Percentiles=[0.25])
        QR3= cgPercentiles(X[i*60:(i+1)*60-1], Percentiles=[0.75])
        X_IQR[i] = QR3-QR1        	
    ENDFOR

    time = FINDGEN(N_ELEMENTS(X))/1440.0      

 	xvar = HISTOGRAM(X_IQR, /NAN, LOCATION=loc)
    x_dist = GAUSSFIT(FINDGEN(N_ELEMENTS(X_IQR)), X_IQR, coeff, NTERMS=3)
    n = FINDGEN(N_ELEMENTS(xvar))    
	x_median = MEDIAN(X_IQR)
    z_scorex = x_median + ((STDDEV(X_IQR, /NAN)/3)*1.3490)

    threshold_X = z_scorex

;###############################################################################
;###############################################################################
;###############################################################################   
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
	
	;WINDOW, 5, XSIZE=1000, YSIZE=400, TITLE='X normal dist data'
    ;PLOT, loc, xvar
    ;OPLOT, [x_median, x_median], [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE = 2
    ;OPLOT, [z_scorex, z_scorex], [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE = 3

    X_24h = FINDGEN(N_ELEMENTS(X_IQR)/24)
    FOR i=0, N_ELEMENTS(X_24h)-1 DO BEGIN
    	X_24h[i] = MAX(X_IQR[i*24:(i+1)*24-1]) ;mediana	
		IF MEDIAN(X_IQR[i*24:(i+1)*24-1]) LT threshold_X THEN BEGIN
			X_24h[i] = MEDIAN(X_hr[i*24:(i+1)*24-1])
		ENDIF ELSE BEGIN
			X_24h[i] = !Values.F_NAN
		ENDELSE
		
    ENDFOR
	X_trend = INTERPOLATE(X_24h, time, CUBIC=-0.5,  /GRID, /MISSING)
    WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE=component +' raw data'
    PLOT, time, X, YRANGE=[MIN(X, /NAN), MAX(X, /NAN)], background=255, color=0, THICK=2, $
    XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
    
    OPLOT, FINDGEN(N_ELEMENTS(X_24h)), X_24h, PSYM=4, SYMSIZE=3, color=254, THICK=4
    OPLOT, time, X_trend, THICK=2, color=70
    
 ;   WINDOW, 0, XSIZE=1000, YSIZE=400, TITLE=component +' detrended'
 ;   plot, time, X-X_trend 	

; Call the procedure
    PRINT, "Press r/l for the direction in which value is converted into NAN"
    direction = ''
    READ, direction, PROMPT = '> '
	convert_2nan, X_24h, direction
    X_trend = INTERPOLATE(X_24h, time, CUBIC=-0.5,  /GRID, /MISSING)	
    
    WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE=component +' raw data'
    PLOT, time, X, YRANGE=[MIN(X, /NAN), MAX(X, /NAN)], background=255, color=0, THICK=2,$
    XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
    
    OPLOT, FINDGEN(N_ELEMENTS(X_24h)), X_24h, PSYM=4, SYMSIZE=3, color=254, THICK=4
    OPLOT, time, X_trend, color=70, THICK=2
    
    WINDOW, 0, XSIZE=1000, YSIZE=400, TITLE=component +' detrended'
    PLOT, time, X-X_trend, background=255, color=0, THICK=2,$
    XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label	    
;###################################################################################################
;###################################################################################################
	PRINT, "Press N in case of unsatisfactory results. To continue, press any other key"
	answer = ''
	READ, answer, PROMPT = '> '
;###################################################################################################
;###################################################################################################
	IF answer EQ 'n' OR answer EQ 'N' THEN BEGIN
	;in case of being necesary, begin a loop to redoo the interpolation until the result is satisfactory
		REPEAT BEGIN
		; Call the procedure to convert NaNs
		    PRINT, "Press r/l for the direction in which value is converted into NAN"
			direction = ''
			READ, direction, PROMPT = '> '
			convert_2nan, X_24h, direction
			WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE=component +' raw data'
			PLOT, time, X, YRANGE=[MIN(X, /NAN), MAX(X, /NAN)], background=255, color=0,$
   			XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
			
			OPLOT, FINDGEN(N_ELEMENTS(X_24h)), X_24h, PSYM=4, SYMSIZE=3, color=254, THICK=4
			OPLOT, time, X_trend, color=70, THICK=2
		
			WINDOW, 0, XSIZE=1000, YSIZE=400, TITLE=component +' detrended'
			PLOT, time, X-X_trend, background=255, color=0, THICK=2,$
  			XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label	
		 	
		 	WAIT, 0.5   
				; Wait for user input (Ctrl+S)
				key = ''
			PRINT, "press 's' when iteration is satisfied, to continue, pres any other key"
			READ, key, PROMPT = '> '	 
		ENDREP UNTIL key EQ 's'
	ENDIF
;end of the loop	
;###################################################################################################
;###################################################################################################
	X_24h = fillnan(X_24h)
	X_trend = INTERPOLATE(X_24h, time, CUBIC=-0.5,  /GRID, /MISSING)
    WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE=component +' raw data'
	PLOT, time, X, YRANGE=[MIN(X, /NAN), MAX(X, /NAN)], background=255, color=0, THICK=2,$
    XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
    
	OPLOT, FINDGEN(N_ELEMENTS(X_24h)), X_24h, PSYM=4, SYMSIZE=3, color=254, THICK=4
    OPLOT, time, X_trend, color=70, THICK=2
    
    WINDOW, 1, XSIZE=1000, YSIZE=400, TITLE=component +' detrended'
    PLOT, time, X-X_trend , background=255, THICK=2, color=0,$
    XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
;###################################################################################################
	X_det = X-X_trend 
	RETURN, X_det
END
