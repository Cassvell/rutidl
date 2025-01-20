
;DATOS DE CAMPO MAGNETICO (TEOLOYUCAN?)
PRO wave_test, H, SQ, date_i, date_f, station_code, PS=ps

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
;###############################################################################
	@set_up_commons
	set_up	

bfield = H

arr=n_elements(bfield)     
times=findgen(arr)         ;arreglo de tiempo arbitrario

dt= 1.  ;resolucion de 1 minuto

;      wave = WAVELET(Y,DT)
; INPUTS:
;    Y = the time series of length N.
;  para el archivo: 2013-01-01-chan-0-3C298.dat
;  N= 44420
;    DT = amount of time between each Y value, i.e. the sampling time.
;  dt=0.0171661 (segundos) ; 4.76837e-06 (horas)


	pad = 1
	s0 =  dt     
	dj = 0.0625  
	j1 = 14./dj  
	mother = 'Morlet'


aa=bfield
time1 = findgen(n_elements(H))  
time2 = findgen(n_elements(SQ)) 
; Note: for accurate reconstruction and variance computation, set: 
; s0 = dt    for Morlet
; s0 = dt/4  for Paul
; (Most commonly, s0=2*dt
    wave = WAVELET(aa,dt,PERIOD=period,SCALE=scale,S0=s0, $
		COI=coi,DJ=dj,J=j1,MOTHER=mother,/RECON,/PAD,signif=signif)
        
;wave = WAVELET(bfield,dt,PERIOD=period,SCALE=scale,S0=s0, $
;		COI=coi,DJ=dj,J=j1,MOTHER=mother,/RECON,/PAD,signif=signif)

	power = (ABS(wave))^2  ; compute wavelet power spectrum
nscale = N_ELEMENTS(period)

;************************************************************************************
	n = N_ELEMENTS(bfield)
	sst = aa
	recon_sst = sst   ; save an extra copy, so we don't erase original sst

	global_ws = TOTAL(power,1)/n   ; global wavelet spectrum (GWS)
	J = N_ELEMENTS(scale) - 1

        SIGLVL=0.1  ;(siginficance level=.1 (90% confidence level))  ;1sigma=.683; 2sigma=.954 ; 3sigma=0.9973

; Significance levels, assuming the GWS as background spectrum:
	signif = WAVE_SIGNIF(sst,dt,scale,0, $
		GWS=global_ws,SIGLVL=SIGLVL,MOTHER=mother)
	signif = REBIN(TRANSPOSE(signif),n,J+1)  ; expand signif --> (J+1)x(N) array
	signif = power/signif   ; where ratio > 1, power is significant
	;PRINT, GWS
; GWS significance levels:
	dof = n - scale   ; the -scale corrects for padding at edges
	global_signif = WAVE_SIGNIF(sst,dt,scale,1, $
		LAG1=0.90,DOF=dof,MOTHER=mother,CDELTA=Cdelta,PSI0=psi0)

; check total variance (Parseval's theorem) [Eqn(14)]
	scale_avg = REBIN(TRANSPOSE(scale),n,J+1)  ; expand scale-->(J+1)x(N) array
	power_norm = power/scale_avg
	variance = (MOMENT(sst))[1]
	recon_variance = dj*dt/(Cdelta*n)*TOTAL(power_norm)  ; [Eqn(14)]
	
	IF (N_ELEMENTS(recon_sst) GT 1) THEN BEGIN
		recon_variance = (MOMENT(recon_sst))[1]
; RMS of Reconstruction [Eqn(11)]
	rms_error = SQRT(TOTAL((sst - recon_sst)^2)/n)
	
; Scale-average 
	avg = WHERE((scale GE 0.5) AND (scale LT 3.3))
	scale_avg = dj*dt/Cdelta*TOTAL(power_norm[*,avg],2)  ; [Eqn(24)]
	scaleavg_signif = WAVE_SIGNIF(sst,dt,scale,2, $
;		GWS=global_ws,SIGLVL=SIGLVL,DOF=[0.3,50.0],MOTHER=mother)
		GWS=global_ws,SIGLVL=SIGLVL,DOF=[.5,3.3],MOTHER=mother)

     endif

;==============================================================================
;==============================================================================
;SMOOTHING
;time average WS




    wave2 = WAVELET(SQ,dt,PERIOD=period,SCALE=scale2,S0=s0, $
    COI=coi1,DJ=dj,J=j1,MOTHER=mother,/RECON,/PAD,signif=signif)            
    
    ; print, 'size of power: ', size(power)
    ;print, scale1 - scale2
    wave_coherency, wave2,time2,scale2,wave,time1,scale, COI1=coi1, DT=dt,DJ=dj, WAVE_COHER=wave_coher,WAVE_PHASE=wave_phase, $
    TIME_OUT=time_out,SCALE_OUT=scale_out,COI_OUT=coi_out, GLOBAL_COHER=global_coher,GLOBAL_PHASE=global_phase, $
    CROSS_WAVELET=cross_wavelet,POWER1=power1,POWER2=power2, NOSMOOTH=nosmooth, VERBOSE=verbose

    n = 11
    semblance = cos(wave_phase)^n


    ddyn = wave_coher * semblance
;==============================================================================
    
    print, 'size of coherence', size(wave_coher)
    print, 'size of local phase: ', size(wave_phase)
    print, 'size of ddyn: ', size(ddyn)
    print, min(wave_phase), max(wave_phase)
;==============================================================================
    IF keyword_set(ps) THEN BEGIN
    
        path = set_var.local_dir+'output/wavelet/'+station_code+'/evento13/'	
            test = FILE_TEST(path, /DIRECTORY) 
            IF test EQ 0 THEN BEGIN
                FILE_MKDIR, path
                PRINT, 'PATH directory '+path
                PRINT, 'created'
            ENDIF ELSE BEGIN
                PRINT, ''
                
            ENDELSE    
            make_psfig, power, period, coi, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path,  station_code   
            make_psfig2, real_part(cross_wavelet), period, coi_out, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code	
            make_psfig3, ddyn, period, coi_out, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code
        ENDIF

END

PRO make_psfig, power, period, coi, date_i, date_f, path, station_code	
        @set_up_commons
        set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])  

;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
	
    psfile =  path+'power_'+Date+'.scaled.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

 cgLOADCT,40

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

period2 = FIX(ALOG(period)/ALOG(2))
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
;minPower =  min(power)
;maxPower =  max(power)

minPower =  1e-11;min(power)
maxPower =  8e5;max(power)

nLevels = 48
levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower

; Generate tick names based on levels
      
CGCONTOUR,power,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.1, .36, .92, .92],$
	YSTYLE=5,C_COLORS=colors, XMINOR=8,YTICKFORMAT='exponent',$ 
	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1;,  xTITLE = 'Time [days]'

    ;ticknames: in, optional, type=string
    ;A string array of names or values for the color bar tick marks. 
    nColors = !D.TABLE_SIZE

    title = Textoidl('Power [nT^{2} Hz^{-1}]')
    tickNames = STRING(levels, FORMAT='(E12.2)')
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title


    l1 = 1600.0
    l2 = 1100.0
    l3 = 675.0
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l1,l1], color='yellow', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l2,l2], color='yellow', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l3,l3], color='yellow', thick=2

    print, 'frequency ranges of significant Ddyn [Hz]:'
    print,  string(1.0/l1, 1.0/l2, 1.0/l3,  FORMAT='(E12.5, X, E12.5, X, E12.5)')

    print, 'period ranges of significant Ddyn [Hz]:'
    print,  string(l1/60.0, l2/60.0, l3/60.0, FORMAT='(F7.1, X, F7.1, X, F7.1)')

    p2 = 1400.0
    p1 = 1100.0
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [p1,p1], color='red', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [p2,p2], color='red', thick=2

    print, 'frequency ranges of Ddyn peak energy:'
    print,  string(1.0/p2, 1.0/p1, FORMAT='(E12.5, X, E12.5)')
    print, 'period ranges of Ddyn peak energy:'
    print,  string(p2/60.0, p1/60.0, FORMAT='(F7.1, X, F7.1)')
; Check that colors and levels are properly defined before passing to cgColorbar

;print, 'pico de potencia, rango de frecuencia, rango de periodo'

    freq_series = 1/(period*60)
    print, n_elements(freq_series), n_elements(period2)
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

	CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 720, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 240, PSYM=8, COLOR='white', thick=4
  	CGPLOTS, max(date_time), 60, PSYM=8, COLOR='white', thick=4

   CGTEXT, MAX(date_time), 2880  , ' 48',$
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65;, ORIENTATION=90   

   CGTEXT, MAX(date_time), 1440  , ' 24', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
   
   CGTEXT, MAX(date_time), 720  , ' 12', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
   
   CGTEXT, MAX(date_time), 240  , '  4', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
   
	CGTEXT, MAX(date_time), 60  , '  1', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65    
;##################################################

	x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='gray', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='gray', /FILL


        CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
                         COLOR='white', $
                         XSTYLE=1,$ 
                         XMINOR=8,$
                         XTICKS=file_number,$
                         ;xTITLE = 'Time [days]',$ 
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5,$
                         XTICKFORMAT='(A1)'
                                           
        CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)    
                         COLOR='white', $
                         XSTYLE=1,$
                         XTICKS=file_number,$
                         XMINOR=8,$
                         XTICKFORMAT=['LABEL_DATE'],$
                         XTICKUNITS=['day']                         

        cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
                         YTITLE = '', $
                         ystyle=1,$  
                         COLOR='black', $                
                         /ylog,$
                         CHARSIZE = 1.65,$
                         CHARTHICK=1.5

                        
        cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
                         /ylog,$                          
                         COLOR='black', $
                         YTICKFORMAT='(A1)',$ 
                         ystyle=5, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                                            
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT LAYER

cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .92, .92],$
color = 'white',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
;print, n_elements(period2), n_elements(date_time)
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT TICK LAYERS         

    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='white', $                
    /ylog,$
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='white', $
    YTICKFORMAT='(A1)',$ 
    ystyle=5, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5
;###############################################################################   
;###############################################################################
;###############################################################################
;###############################################################################                          
;###############################################################################   
   class = gms_class(station_code)
   info = stationlist(class, station_code)
   title = STRING(STRUPCASE(station_code), info.mlat, info.hem, ", UTC: ", info.utc,' h', $
   FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, A, " ", I02, A)')
  
  
  x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.93   
   XYOUTS, X, y, title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65     

   x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.18   

   xtitle = 'Time [UT h]'

   XYOUTS, X, y, xtitle, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4  

;###############################################################################
;###############################################################################

   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.025   
   CGTEXT, x, y,'Freq [Hz]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90

   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.985   
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90
   print, '/min power: ', min(power), '     max power: ',max(power)

;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END 


PRO make_psfig2, ddyn, period, coi, date_i, date_f, path, station_code	
        @set_up_commons
        set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])  

;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
	
    psfile =  path+'xwt_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

 cgLOADCT,40

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

period2 = FIX(ALOG(period)/ALOG(2))
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
;minPower =  min(power)
;maxPower =  max(power)

minPower =  min(ddyn)
maxPower =  max(ddyn)

nLevels = 48
levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower


; Generate tick names based on levels
      
    cgCONTOUR,ddyn,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.1, .36, .92, .92],$
	YSTYLE=5,C_COLORS=colors, XMINOR=8,	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1;,  xTITLE = 'Time [days]'

    nColors = !D.TABLE_SIZE

    title = Textoidl('crosswavelet [nT]')
    tickNames = STRING(levels, FORMAT='(F12.2)')
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title


    ;###############################################################################
    ;###############################################################################
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

	CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 720, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 240, PSYM=8, COLOR='red', thick=4
  	CGPLOTS, max(date_time), 60, PSYM=8, COLOR='red', thick=4

    CGTEXT, MAX(date_time), 2880  , ' 48',$
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65;, ORIENTATION=90   

    CGTEXT, MAX(date_time), 1440  , ' 24', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 720  , ' 12', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 240  , '  4', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
        CGTEXT, MAX(date_time), 60  , '  1', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65    
    ;##################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .92, .92],$
    color = 'black',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
    ;print, n_elements(period2), n_elements(date_time)
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='gray', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='gray', /FILL
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT TICK LAYERS         
    
    freq_series = 1/(period*60)
    print, n_elements(freq_series), n_elements(period2)
    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='black', $                
    /ylog,$
    ;YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='black', $
    YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5
    ;###############################################################################   
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################                          
    ;###############################################################################   
    class = gms_class(station_code)
    info = stationlist(class, station_code)
    title = STRING(STRUPCASE(station_code), info.mlat, info.hem, ", UTC: ", info.utc,' h', $
    FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, A, " ", I02, A)')


    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.93   
    XYOUTS, X, y, title, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65     

    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.18   

    xtitle = 'Time [UT h]'

    XYOUTS, X, y, xtitle, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4  

    ;###############################################################################
    ;###############################################################################

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.025   
    CGTEXT, x, y,'Freq [Hz]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.985   
    CGTEXT, x, y,'Period [h]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90
;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END 


PRO make_psfig3, ddyn, period, coi, date_i, date_f, path, station_code	
        @set_up_commons
        set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])  

;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
	
    psfile =  path+'ddyn_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

 cgLOADCT,40

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

period2 = FIX(ALOG(period)/ALOG(2))
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
;minPower =  min(power)
;maxPower =  max(power)

minPower =  min(ddyn)
maxPower =  max(ddyn)
print, 'min Ddyn: ', minPower
print, 'max Ddyn: ', maxPower

nLevels = 48
levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower


; Generate tick names based on levels
      
    cgCONTOUR,ddyn,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.1, .36, .92, .92],$
	YSTYLE=5,C_COLORS=colors, XMINOR=8,	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1;,  xTITLE = 'Time [days]'

    nColors = !D.TABLE_SIZE

    title = Textoidl('Semblance [nT]')
    tickNames = STRING(levels, FORMAT='(F12.2)')
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title


    ;###############################################################################
    ;###############################################################################
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

	CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 720, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 240, PSYM=8, COLOR='red', thick=4
  	CGPLOTS, max(date_time), 60, PSYM=8, COLOR='red', thick=4

    CGTEXT, MAX(date_time), 2880  , ' 48',$
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65;, ORIENTATION=90   

    CGTEXT, MAX(date_time), 1440  , ' 24', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 720  , ' 12', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 240  , '  4', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
        CGTEXT, MAX(date_time), 60  , '  1', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65    
    ;##################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .92, .92],$
    color = 'black',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
    ;print, n_elements(period2), n_elements(date_time)
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='gray', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='gray', /FILL
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT TICK LAYERS         
    
    freq_series = 1/(period*60)
    print, n_elements(freq_series), n_elements(period2)
    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='black', $                
    /ylog,$
    ;YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='black', $
    YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5
    ;###############################################################################   
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################                          
    ;###############################################################################   
    class = gms_class(station_code)
    info = stationlist(class, station_code)
    title = STRING(STRUPCASE(station_code), info.mlat, info.hem, ", UTC: ", info.utc,' h', $
    FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, A, " ", I02, A)')


    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.93   
    XYOUTS, X, y, title, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65     

    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.18   

    xtitle = 'Time [UT h]'

    XYOUTS, X, y, xtitle, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4  

    ;###############################################################################
    ;###############################################################################

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.025   
    CGTEXT, x, y,'Freq [Hz]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.985   
    CGTEXT, x, y,'Period [h]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90
;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END