;****************************************************** WAVE_COHERENCY
;+
;   WAVE_COHERENCY
;
; PURPOSE:   Compute the wavelet coherency between two time series.
;
;
; CALLING SEQUENCE:
;
;     WAVE_COHERENCY, $
;         wave1,time1,scale1, $
;         wave2,time2,scale2, $
;         WAVE_COHER=wave_coher,WAVE_PHASE=wave_phase, $
;         TIME_OUT=time_out,SCALE_OUT=scale_out
;
;
; INPUTS:
;
;    WAVE1 = wavelet power spectrum for time series #1
;    TIME1 = a vector of times for time series #1
;    SCALE1 = a vector of scales for time series #1
;    WAVE2 = wavelet power spectrum for time series #2
;    TIME2 = a vector of times for time series #2
;    SCALE2 = a vector of scales for time series #2
;
;
; OPTIONAL KEYWORD INPUTS:
;
;    DT = amount of time between each Y value, i.e. the sampling time.
;         If not input, then calculated from TIME1(1)-TIME1(0)
;
;    DJ = the spacing between discrete scales.
;         If not input, then calculated from SCALE1
;
;   VERBOSE = if set, then print out the scales and system time
;
;   NOSMOOTH = if set, then just compute the GLOBAL_COHER, GLOBAL_PHASE,
;              and the unsmoothed CROSS_WAVELET and return
;
;
; OPTIONAL KEYWORD OUTPUTS:
;
;   WAVE_COHER = the wavelet coherency, as a function of
;       TIME_OUT and SCALE_OUT
;
;   TIME_OUT = the time vector, given by the overlap of TIME1 and TIME2
;
;   SCALE_OUT = the scale vector of scale indices, given by the overlap
;               of SCALE1 and SCALE2
;
;   COI_OUT = the vector of the cone-of-influence
;
;	GLOBAL_COHER = the global (or mean) coherence averaged over all times.
;
;   GLOBAL_PHASE = the global (or mean) phase averaged over all times
;
;	CROSS_WAVELET = the cross wavelet between the time series
;
;   POWER1 = the wavelet power spectrum; should be the same as WAVE1
;            if TIME1 and TIME2 are identical, otherwise it is only the
;            overlapping portion. If NOSMOOTH is set,
;            then this is unsmoothed, otherwise it is smoothed.
;
;   POWER2 = same as POWER1 but for time series #2
;
;
;----------------------------------------------------------------------------
; Copyright (C) 1998-2005, Christopher Torrence
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or
; implied warranties whatsoever.
;
; Reference: Torrence, C. and P. J. Webster, 1999: Interdecadal changes in the
;            ENSO-monsoon system. <I>J. Climate</I>, 12, 2679-2690.
;
; Please send a copy of any publications to C. Torrence:
;  Dr. Christopher Torrence
;  Research Systems, Inc.
;  4990 Pearl East Circle
;  Boulder, CO 80301, USA
;  E-mail: chris[AT]rsinc[DOT]com
;----------------------------------------------------------------------------
;-



;****************************************************************** WAVELET
PRO wave_coherency, $
	wave1,time1,scale1,wave2,time2,scale2, $   ;*** required inputs
	COI1=coi1, $
	DT=dt,DJ=dj, $
	WAVE_COHER=wave_coher,WAVE_PHASE=wave_phase, $
	TIME_OUT=time_out,SCALE_OUT=scale_out,COI_OUT=coi_out, $
	GLOBAL_COHER=global_coher,GLOBAL_PHASE=global_phase, $
	CROSS_WAVELET=cross_wavelet,POWER1=power1,POWER2=power2, $
	NOSMOOTH=nosmooth, $
	VERBOSE=verbose

    ON_ERROR, 2
    COMPILE_OPT idl2, HIDDEN

	verbose = KEYWORD_SET(verbose)

;*** find overlapping times
	time_start = MIN(time1) > MIN(time2)
	time_end = MAX(time1) < MAX(time2)
	time1_start = MIN(WHERE((time1 GE time_start)))
	time1_end = MAX(WHERE((time1 LE time_end)))
	time2_start = MIN(WHERE((time2 GE time_start)))
	time2_end = MAX(WHERE((time2 LE time_end)))

;*** find overlapping scales
	scale_start = MIN(scale1) > MIN(scale2)
	scale_end = MAX(scale1) < MAX(scale2)
	scale1_start = MIN(WHERE((scale1 GE scale_start)))
	scale1_end = MAX(WHERE((scale1 LE scale_end)))
	scale2_start = MIN(WHERE((scale2 GE scale_start)))
	scale2_end = MAX(WHERE((scale2 LE scale_end)))

;*** cross wavelet & individual wavelet power
	cross_wavelet = wave1(time1_start:time1_end,scale1_start:scale1_end)*$
		CONJ(wave2(time2_start:time2_end,scale2_start:scale2_end))
	power1 = ABS(wave1(time1_start:time1_end,scale1_start:scale1_end))^2
	power2 = ABS(wave2(time2_start:time2_end,scale2_start:scale2_end))^2

	IF (N_ELEMENTS(dt) LE 0) THEN dt = time1(1) - time1(0)
	ntime = time1_end - time1_start + 1
	nj = scale1_end - scale1_start + 1
	IF (N_ELEMENTS(dj) LE 0) THEN dj = ALOG(scale1(1)/scale1(0))/ALOG(2)
	scale = scale1(scale1_start:scale1_end)
	IF (verbose) THEN PRINT,dt,ntime,dj,nj
	time_out = time1(time1_start:time1_end)
	scale_out = scale1(scale1_start:scale1_end)
	IF (N_ELEMENTS(coi1) EQ N_ELEMENTS(time1)) THEN $
		coi_out = coi1(time1_start:time1_end)

; calculate global coherency before doing local smoothing
	global1 = TOTAL(power1,1)
	global2 = TOTAL(power2,1)
	global_cross = TOTAL(cross_wavelet,1)
	global_coher = ABS(global_cross)^2/(global1*global2)
	global_phase = 180./!PI*ATAN(IMAGINARY(global_cross),FLOAT(global_cross))

	IF KEYWORD_SET(nosmooth) THEN RETURN

	FOR j=0,nj-1 DO BEGIN ;*** time-smoothing
		st1 = SYSTIME(1)
		nt = LONG(4L*scale(j)/dt)/2L*4 + 1L
		time_wavelet = (FINDGEN(nt) - nt/2)*dt/scale(j)
		wave_function = EXP(-time_wavelet^2/2.)   ;*** Morlet
		wave_function = FLOAT(wave_function/TOTAL(wave_function)) ; normalize
		nz = nt/2
		zeros = COMPLEX(FLTARR(nz),FLTARR(nz))
		cross_wave_slice = [zeros,cross_wavelet(*,j),zeros]
		cross_wave_slice = CONVOL(cross_wave_slice,wave_function)
		cross_wavelet(*,j) = cross_wave_slice(nz:ntime+nz-1)
		zeros = FLOAT(zeros)
		power_slice = [zeros,power1(*,j),zeros]
		power_slice = CONVOL(power_slice,wave_function)
		power1(*,j) = power_slice(nz:ntime + nz - 1)
		power_slice = [zeros,power2(*,j),zeros]
		power_slice = CONVOL(power_slice,wave_function)
		power2(*,j) = power_slice(nz:ntime + nz - 1)
		IF (verbose) THEN PRINT,j,scale(j),SYSTIME(1)-st1;,FORMAT='(I4,$)'
	ENDFOR  ;*** time-smoothing

;*** normalize by scale
	scales = REBIN(TRANSPOSE(scale),ntime,nj)
	cross_wavelet = TEMPORARY(cross_wavelet)/scales
	power1 = TEMPORARY(power1)/scales
	power2 = TEMPORARY(power2)/scales

	nweights = FIX(0.6/dj/2 + 0.5)*2 - 1   ; closest (smaller) odd integer
	weights = REPLICATE(1.,nweights)
	weights = weights/TOTAL(weights) ; normalize
	FOR i=0,ntime-1 DO BEGIN ;*** scale-smoothing
		cross_wavelet(i,*) = CONVOL((cross_wavelet(i,*))(*),weights)
		power1(i,*) = CONVOL((power1(i,*))(*),weights)
		power2(i,*) = CONVOL((power2(i,*))(*),weights)
	ENDFOR ;*** scale-smoothing

	wave_phase = 180./!PI*ATAN(IMAGINARY(cross_wavelet),FLOAT(cross_wavelet))
	wave_coher = (ABS(cross_wavelet)^2)/(power1*power2 > 1E-9) ;Amplitude
;	wave_phase = wave_phase + 360.*(wave_phase LT 0.)
END


PRO make_psfig, wave, times, period, date_i, date_f, path, station_code	
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
	
    psfile =  path+'power_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

 cgLOADCT,23

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

period2 = FIX(ALOG(period)/ALOG(2))
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
minPower =  min(power)
maxPower =  max(power)
nLevels = 24
levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower
print, 'max power: ',max(power), '/min power: ', min(power)
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

    title = Textoidl('Power [nT^{2}]')
    tickNames = STRING(levels, FORMAT='(E12.2)')
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    ticklen=0.15,  title=title


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


;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END 


