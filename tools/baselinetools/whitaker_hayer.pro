FUNCTION whitaker_hayer, x, threshold, component
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

    x_diff = TS_DIFF(x,1)
	xd_med = MEDIAN(x_diff)
	MAD    = MEDIAN(ABS(x_diff-xd_med))
	
	nfact  = 0.6745
	z  	   = nfact*(x_diff-xd_med)/MAD
	PRINT, "MAX Z value"
	PRINT, MAX(z)
	
	spikes = WHERE(abs(z) GT threshold)

	IF spikes[0] NE -1 THEN BEGIN
		time = FINDGEN(N_ELEMENTS(x_diff))
;###############################################################################   
		DEVICE, true=24, retain=2, decomposed=0
		TVLCT, R_bak, G_bak, B_bak, /GET        
		LOADCT, 39, /SILENT  
;###############################################################################  


		WINDOW, 0, XSIZE=1000, YSIZE=400, TITLE='diff'+ component
		PLOT, time, x_diff, YRANGE=[MIN(x_diff, /NAN),MAX(x_diff,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
		background=255, color=0, CHARTHICK=2.0, YTITLE = component+'diff mag/diffT   '+ component

		print, 'spikes num: ', N_ELEMENTS(spikes)
	 ; dif_Hdet[spikes] = !Values.F_NAN
		x_diff[spikes] = !Values.F_NAN
	;dif_Hdet = fillnan(dif_Hdet)	
		;H_det = fillnan(H_det)
		x[spikes] = !Values.F_NAN
		WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE='depicked '+component
		PLOT, time, x_diff, YRANGE=[MIN(x_diff, /NAN),MAX(x_diff,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
		background=255, color=0, CHARTHICK=2.0, YTITLE = component+'diff mag/diffT  '+ component	
		
		WINDOW, 1, XSIZE=1000, YSIZE=400, TITLE='depicked '+component
		PLOT, time, x, YRANGE=[MIN(x, /NAN),MAX(x,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
		background=255, color=0, CHARTHICK=2.0, YTITLE = component+'[nT]'	


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
		    PRINT, "Press value for threshold"
			threshold = ''
			READ, threshold, PROMPT = '> '
			
			x_diff = TS_DIFF(x,1)
			xd_med = MEDIAN(x_diff)
			MAD    = MEDIAN(ABS(x_diff-xd_med))
			
			nfact  = 0.6745
			z  	   = nfact*(x_diff-xd_med)/MAD			
			spikes = WHERE(abs(z) GT threshold)

			WINDOW, 0, XSIZE=800, YSIZE=400, TITLE='diff'
			PLOT, time, x_diff, YRANGE=[MIN(x_diff, /NAN),MAX(x_diff,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
			background=255, color=0, CHARTHICK=2.0, YTITLE = 'diff mag/diffT  '	+component

			x_diff[spikes] = !Values.F_NAN
			x[spikes] = !Values.F_NAN

			WINDOW, 2, XSIZE=800, YSIZE=400, TITLE='depicked' +component
			PLOT, time, x_diff, YRANGE=[MIN(x_diff, /NAN),MAX(x_diff,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
			background=255, color=0, CHARTHICK=2.0, YTITLE = 'diff mag/diffT  '	+component

			WINDOW, 1, XSIZE=800, YSIZE=400, TITLE='depicked '+component
			PLOT, time, x, YRANGE=[MIN(x, /NAN),MAX(x,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
			background=255, color=0, CHARTHICK=2.0, YTITLE = component+' [nT]'

		 	WAIT, 0.5   
				; Wait for user input (Ctrl+S)
				key = ''
			PRINT, "press 's' when iteration is satisfied, to continue, pres any other key"
			READ, key, PROMPT = '> '	 
		ENDREP UNTIL key EQ 's'
	ENDIF
;end of the loop	
		WINDOW, 2, XSIZE=800, YSIZE=400, TITLE='depicked' + component
		PLOT, time, x_diff, YRANGE=[MIN(x_diff, /NAN),MAX(x_diff,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
		background=255, color=0, CHARTHICK=2.0, YTITLE = 'diff mag/diffT  '	+component
;###################################################################################################
;###################################################################################################



	ENDIF ELSE BEGIN
		PRINT, 'no spikes detected in component'
		
	ENDELSE


	RETURN, spikes		
END


	

