
;DATOS DE CAMPO MAGNETICO (TEOLOYUCAN?)
PRO wave_test, H, date_i, date_f, station_code, PS=ps

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
;se lee el archivo de datos
;readcol,'teo20030526_20030608.dat',bfield,format='f' 
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


;==============================================================================
;==============================================================================
    IF keyword_set(ps) THEN BEGIN
    
    path = set_var.local_dir+'output/wavelet/'+station_code+'/'	
    	test = FILE_TEST(path, /DIRECTORY) 
		IF test EQ 0 THEN BEGIN
			FILE_MKDIR, path
			PRINT, 'PATH directory '+path
			PRINT, 'created'
		ENDIF ELSE BEGIN
			PRINT, ''
			
		ENDELSE    
    	make_psfig, power, wave, times, period, coi, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path,  station_code   
		make_psfig2, power, wave, times, period, coi, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code	
    ENDIF

;WINDOW,0, XSIZE=1000, YSIZE=500

;LOADCT,23
;!P.POSITION=[.09, .1, .93, .95]


period2 = FIX(ALOG(period)/ALOG(2))
;ndays = N_ELEMENTS(times)/(60*48)
;cgCONTOUR,power,times,period, $
     ;  XSTYLE=1,YTITLE='Period [minutes]', title='', xtickformat='xticks',$
    ;   ystyle=1,C_COLORS=colors, ytickformat='exponent', XTICKS=ndays,$   ;*** Large-->Small period
   ;    /YTYPE, LEVELS=levels,xrange=[min(times),max(times)],$    ;*** make y-axis logarithmic
  ;     yrange=[min(period),max(period)],$
 ;      NLEVELS=24,/FILL
;CGPLOTS,times,coi,COLOR='gray',NOCLIP=0,THICK=2
; cone-of-influence, anything "below" is dubious
;	x = [times[0],times,MAX(times)]
;	y = [MAX(period),coi,MAX(period)]

;	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='gray', /FILL
;	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='gray', /FILL

fk     = (1+FINDGEN(n))/(n*60)
y  = fft(bfield)
fny = FLOAT(1.0/(2.0*60))
pws = (ABS(y))^2

WINDOW,1, XSIZE=500, YSIZE=500, TITLE='global_ws'
PLOT, period, global_ws, /XLOG, /YLOG,  XRANGE=[MIN(period), MAX(period)], YRANGE=[MIN(global_ws), MAX(global_ws)]
OPLOT, period, pws, linestyle=1
;OPLOT, fk, global_ws

WINDOW,3, XSIZE=500, YSIZE=500, TITLE='signif vs WTP'
PLOT, fk, power, /XLOG, /YLOG,  XRANGE=[MIN(fk), fny], YRANGE=[MIN(pws), MAX(pws)]
OPLOT, fk, pws, linestyle=1
OPLOT, fk, 1/global_ws, linestyle=2
;OPLOT, fk, signif, linestyle=3

PRINT, 'Variance: ', recon_variance
;print, period
;loadct,0
;AXIS, YAXIS=1, YRANGE=[min(1./period),max(1./period)],ystyle=1,/ylog,color=0,$
;      ytitle='Frequency [Hz]',ytickformat='exponent',charsize=1.7


;PRINT, period
END

PRO make_psfig, power, wave, times, period, coi, date_i, date_f, path, station_code	
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
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    ;path = '../rutidl/output/article1events/diono_ev/'
    ;path = set_var.local_dir+'/output/wavelet/'+station_code	
    psfile =  path+'power_'+Date+'.uncut.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7
    old_month = month_name(mh_i, 'english') 
 ;   	print, TGM_n
    time_title = ' UT [days]'
   ; IF TGM_n NE 'fuera de rango' THEN BEGIN    
    	window_title = 'Event '+ STRING(TGM_n, FORMAT='(I2)')
    
    ;ENDIF ELSE BEGIN
	;    window_title = 'Quiet Period, '+ $
    ;            STRING(old_month, yr_i, FORMAT='(A, X, I4)')           
    ;ENDELSE                    
    periodo = 'Period [h]'   
   
;###############################################################################               
    freqs = [1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/3600] 
    
    ;i = WHERE(f_k GE freqs[0]);, THICK=1 
    ;fny=WHERE(f_k EQ fn)

	hr_36 = 1.0/(36.0*3600.0)
	hr_7 = 1.0/(7.0*3600.0)
	threshold_rn = 1.0/(1300.0)
	hr_4 = 1.0/(4.0*3600.0)
	
    ;j = WHERE(f_k GE threshold_rn)
    ;k = WHERE(f_k GE freqs[4])
    ;m = WHERE(f_k GE hr_36)
    ;n = WHERE(f_k GE hr_7)
	;limP2 = MIN([j])
	;limP  = MIN([k])
	;limP1  = MIN([n])
	;limP0 = MIN([m])

 ;###############################################################################  

LOADCT,23

;!P.POSITION=[.1, .55, .92, .9]

period_m = [(6e2), (6e3), (6e4), (6e5)]
period_h = [10/60., 720/60., 1440/60., 2880/60.]

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

period2 = FIX(ALOG(period)/ALOG(2))
CGCONTOUR,power,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.11, .25, .92, .91],$
      ; YSTYLE=5,C_COLORS=colors, XTICKS=file_number, XMINOR=8,YTICKFORMAT='exponent',$   ;*** Large-->Small period
	YSTYLE=5,C_COLORS=colors, XMINOR=8,YTICKFORMAT='exponent',$ 
      ;/YTYPE, LEVELS=levels, XRANGE=[times[0],times[N_ELEMENTS(times)-1]],$    ;*** make y-axis logarithmic
	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=24,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1,  xTITLE = 'Time [days]'
      
	CGPLOTS, max(date_time), 2880, PSYM=4, COLOR='white'
	CGPLOTS, max(date_time), 1440, PSYM=4, COLOR='white'
	CGPLOTS, max(date_time), 720, PSYM=4, COLOR='white'
	CGPLOTS, max(date_time), 240, PSYM=4, COLOR='white'
  
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
   CGTEXT, MIN(date_time), 950, '1.7E-04', $
   COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2
   
   CGTEXT, MIN(date_time), 95  , '1.7E-03',$
   COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2

; cone-of-influence, anything "below" is dubious
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

        cgAxis,YAxis=0, yrange=[30,4000], $
                         YTITLE = '', $
                         ystyle=1,$  
                         ;TEXT_COLOR = 'black',$
                         COLOR='white', $                
                         YTICKV=1/period_m,$                         
                         YTICKN=STRING(1/period_m, FORMAT='(E7.1)'),$
                         YTICKFORMAT='(A1)',$ 
                         /ylog,$
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5
                        
        cgAxis, YAxis=1, yrange=[30,4000], $
                         /ylog,$                          
                         COLOR='white', $
                         YTICKFORMAT='(A1)',$ 
                         ystyle=1, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                                            
;###############################################################################
;###############################################################################
;###############################################################################

;###############################################################################

   
 ;  CGTEXT, x, 10   , '1.7E-03', $
;   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2
   
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
   y = 0.02   
;   XYOUTS, X, y, 'Time [days]', /NORMAL, $
;   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4  

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

    cgPS_Close, density = 300, width = 1600 ;, /PNG  
    RETURN  
END 

PRO make_psfig2, power, wave, times, period, coi, date_i, date_f, path, station_code
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
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)

    psfile =  path+'wave_'+Date+'.uncut.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7
    old_month = month_name(mh_i, 'english') 
 ;   	print, TGM_n
   ; time_title = ' UT [days]'
   ; IF TGM_n NE 'fuera de rango' THEN BEGIN    
  ;  	window_title = 'Event '+ STRING(TGM_n, FORMAT='(I2)')
    
    ;ENDIF ELSE BEGIN
	;    window_title = 'Quiet Period, '+ $
    ;            STRING(old_month, yr_i, FORMAT='(A, X, I4)')           
    ;ENDELSE                    
    periodo = 'Period [h]'   
   
;###############################################################################               
    freqs = [1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/3600] 
    
    ;i = WHERE(f_k GE freqs[0]);, THICK=1 
    ;fny=WHERE(f_k EQ fn)

	hr_36 = 1.0/(36.0*3600.0)
	hr_7 = 1.0/(7.0*3600.0)
	threshold_rn = 1.0/(1300.0)
	hr_4 = 1.0/(4.0*3600.0)
	
    ;j = WHERE(f_k GE threshold_rn)
    ;k = WHERE(f_k GE freqs[4])
    ;m = WHERE(f_k GE hr_36)
    ;n = WHERE(f_k GE hr_7)
	;limP2 = MIN([j])
	;limP  = MIN([k])
	;limP1  = MIN([n])
	;limP0 = MIN([m])
 ;###############################################################################  

LOADCT,23
period_m = [(6e2), (6e3), (6e4), (6e5)]
;period_m = [(6e5), (6e4), (6e3), (6e2)]
period_h = [10/60., 720/60., 1440/60., 2880/60.]

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])		

;period2 = FIX(ALOG(period)/ALOG(2))
;ytickv = 2.^(period2(UNIQ(period2)))
	cgCONTOUR,wave,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.11, .25, .92, .91],$
      ; YSTYLE=5,C_COLORS=colors, XTICKS=file_number, XMINOR=8,YTICKFORMAT='exponent',$   ;*** Large-->Small period
	YSTYLE=5,C_COLORS=colors, XMINOR=8,YTICKFORMAT='exponent',$ 
      ;/YTYPE, LEVELS=levels, XRANGE=[times[0],times[N_ELEMENTS(times)-1]],$    ;*** make y-axis logarithmic
	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=24,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1, xTITLE = 'Time [days]'

	CGPLOTS, MAX(date_time), 2880, PSYM=4, COLOR='white'
	CGPLOTS, MAX(date_time), 1440, PSYM=4, COLOR='white'
	CGPLOTS, MAX(date_time), 720, PSYM=4, COLOR='white'
	CGPLOTS, MAX(date_time), 240, PSYM=4, COLOR='white'

    x = MAX(date_time)+(MAX(date_time)*0.02) 
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

; cone-of-influence, anything "below" is dubious
	x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='dark gray', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='dark gray', /FILL

        CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
                         COLOR='black', $
                         XSTYLE=1,$ 
                         XMINOR=8,$
                         XTICKS=file_number,$
                      ;   xTITLE = 'Time [days]',$
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5,$
                         XTICKFORMAT='(A1)'
                                           
        CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                        
                         COLOR='black', $
                         XTICKS=file_number,$
                         XMINOR=8,$
                         XTICKFORMAT='(A1)'

        cgAxis,YAxis=0, yrange=[30,4000], $
                         YTITLE = '', $
                         ystyle=1,$                  
                         COLOR='black', $                         
                         YTICKV=1/period_m,$                         
                         YTICKN=STRING(1/period_m, FORMAT='(E7.1)'),$
                         ;YTICKFORMAT='(E7.1)',$ 
                         /ylog,$
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5

        cgAxis, YAxis=1, yrange=[30,4000], $
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
;###############################################################################      
   class = gms_class(station_code)
   info = stationlist(class, station_code)
   title = STRING(STRUPCASE(station_code), info.mlat, info.hem, ", UTC: ", info.utc,' h', $
   FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, A, " ", I02, A)')    

   x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.93   
   XYOUTS, X, y, title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65    

   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.025   
   XYOUTS, x, y,'Freq [Hz]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90

   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.985   
   XYOUTS, x, y,'Period [h]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################   

    cgPS_Close, density = 300, width = 1600 ;, /PNG  
    RETURN  
END 
