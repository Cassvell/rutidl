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

    ;ON_ERROR, 2
    COMPILE_OPT idl2, HIDDEN

	verbose = KEYWORD_SET(verbose)

;*** find overlapping times
	time_start = MIN(time1) > MIN(time2)
	time_end = MAX(time1) < MAX(time2)
	time1_start = MIN(WHERE((time1 GE time_start)))
	time1_end = MAX(WHERE((time1 LE time_end)))
	time2_start = MIN(WHERE((time2 GE time_start)))
	time2_end = MAX(WHERE((time2 LE time_end)))
	scale = scale1
;*** find overlapping scales
	scale_start = MIN(scale1) > MIN(scale2)
	scale_end = MAX(scale1) < MAX(scale2)
	scale1_start = MIN(WHERE((scale1 GE scale_start)))
	scale1_end = MAX(WHERE((scale1 LE scale_end)))
	scale2_start = MIN(WHERE((scale2 GE scale_start)))
	scale2_end = MAX(WHERE((scale2 LE scale_end)))

;*** cross wavelet & individual wavelet power
	cross_wavelet = wave1[time1_start:time1_end,scale1_start:scale1_end]*$
		CONJ(wave2[time2_start:time2_end,scale2_start:scale2_end])
	power1 = ABS(wave1[time1_start:time1_end,scale1_start:scale1_end])^2
	power2 = ABS(wave2[time2_start:time2_end,scale2_start:scale2_end])^2

	IF (N_ELEMENTS(dt) LE 0) THEN dt = time1(1) - time1(0)
	ntime = time1_end - time1_start + 1
	nj = scale1_end - scale1_start + 1
	IF (N_ELEMENTS(dj) LE 0) THEN dj = ALOG(scale1(1)/scale1(0))/ALOG(2)
	scale = scale1[scale1_start:scale1_end]
	IF (verbose) THEN PRINT,dt,ntime,dj,nj
	time_out = time1[time1_start:time1_end]
	scale_out = scale1[scale1_start:scale1_end]
	IF (N_ELEMENTS(coi1) EQ N_ELEMENTS(time1)) THEN $
		coi_out = coi1[time1_start:time1_end]

; calculate global coherency before doing local smoothing
	global1 = TOTAL(power1,1)
	global2 = TOTAL(power2,1)
	global_cross = TOTAL(cross_wavelet,1)
	global_coher = ABS(global_cross)^2/(global1*global2)
	global_phase = 180./!PI*ATAN(IMAGINARY(global_cross),FLOAT(global_cross))

	IF KEYWORD_SET(nosmooth) THEN RETURN

	FOR j=0,nj-1 DO BEGIN ;*** time-smoothing
		st1 = SYSTIME(1)
		nt = LONG(4L*scale[j]/dt)/2L*4 + 1L
		time_wavelet = (FINDGEN(nt) - nt/2)*dt/scale[j]
		wave_function = EXP(-time_wavelet^2/2.)   ;*** Morlet
		wave_function = FLOAT(wave_function/TOTAL(wave_function)) ; normalize
		nz = nt/2
		zeros = COMPLEX(FLTARR(nz),FLTARR(nz))
		cross_wave_slice = [zeros,cross_wavelet[*,j],zeros]
		cross_wave_slice = CONVOL(cross_wave_slice,wave_function)
		cross_wavelet[*,j] = cross_wave_slice[nz:ntime+nz-1]
		zeros = FLOAT(zeros)
		power_slice = [zeros,power1[*,j],zeros]
		power_slice = CONVOL(power_slice,wave_function)
		power1[*,j] = power_slice[nz:ntime + nz - 1]
		power_slice = [zeros,power2[*,j],zeros]
		power_slice = CONVOL(power_slice,wave_function)
		power2[*,j] = power_slice[nz:ntime + nz - 1]
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
	
	IF (nweights GT 1) THEN BEGIN
		pad_length = FLOOR(nweights / 2)
		pad = REPLICATE(0.0, pad_length)  ; For real arrays
		; If arrays are complex, use COMPLEX padding:
		; pad = COMPLEX(REPLICATE(0.0, pad_length), REPLICATE(0.0, pad_length))

		FOR i = 0, nj - 1 DO BEGIN
		    ; Pad the columns
		    padded_cross_wavelet = [pad, cross_wavelet[*, i], pad]
		    padded_power1 = [pad, power1[*, i], pad]
		    padded_power2 = [pad, power2[*, i], pad]

		    ; Apply convolution
		    convolved_cross_wavelet = CONVOL(padded_cross_wavelet, weights)
		    convolved_power1 = CONVOL(padded_power1, weights)
		    convolved_power2 = CONVOL(padded_power2, weights)

		    ; Remove padding
		    cross_wavelet[*, i] = convolved_cross_wavelet[pad_length:N_ELEMENTS(convolved_cross_wavelet) - pad_length - 1]
		    power1[*, i] = convolved_power1[pad_length:N_ELEMENTS(convolved_power1) - pad_length - 1]
		    power2[*, i] = convolved_power2[pad_length:N_ELEMENTS(convolved_power2) - pad_length - 1]
		ENDFOR
	ENDIF

	wave_phase = 180./!PI*ATAN(IMAGINARY(cross_wavelet),FLOAT(cross_wavelet))
	wave_coher = (ABS(cross_wavelet)^2)/(power1*power2 > 1E-9) ;Amplitude
	wave_phase = wave_phase + 360.*(wave_phase LT 0.)
	;print, 'local phase: ', wave_phase
	;print, '#####################################################################'
	;print, 'Amplitude: ', wave_coher
END

pro diono_xwt, date_i, date_f, station_code, PS=ps, Bsq=Bsq 
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

    IF station_code EQ '' THEN BEGIN 
    
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
    ;###############################################################################
    ;###############################################################################  
        IF station_class EQ 'regmex' THEN  BEGIN    
        station = set_var.gms[FIX(station_idx)]
        station_code = set_var.gms_code[FIX(station_idx)]  
        ENDIF 
        
        IF station_class EQ 'intermagnet' THEN  BEGIN 
        station = set_var.gmsi[FIX(station_idx)] 
        station_code = set_var.gmsi_code[FIX(station_idx)] 
        ENDIF
        PRINT,  'GMS selected: '+station+' IAGA code: '+station_code   
        ENDIF
    ;###############################################################################	
;###############################################################################  
    	
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
;###############################################################################
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_n = event_case([yr_i,mh_i,dy_i])  
;###############################################################################   
    time= findgen(file_number*1440)/1440.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')

    data   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = data.H
    SQ = data.SQ
    idx = sym_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH = idx.symH

    idx2 = sym0_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH0 = idx2.symH0  
    
    symH0 = fillnan(symH0)
    H = fillnan(H)

    old_month = month_name(mh_i, 'english')
    new_month = month_name(mh_f, 'english')
    IF mh_i NE mh_f THEN BEGIN
    	time_name = 'days of '+old_month+' and '+ new_month
    ENDIF ELSE BEGIN 
    	time_name = 'days of '+old_month
    ENDELSE
    set_plot, 'x'   
       time = FINDGEN(N_ELEMENTS(H))/1440.0       

    ; define frequencies
    l            = 28.06  
    mlat         = l*!pi
    ld           = cos(mlat/180)
    p_a          = symH*ld
    baseline     = p_a;Bsq + p_a Desde la actualización, Bsq ya está restado de H
    Bdiono       = H-baseline
    ; Bdiono2	     = H-p_a+Bsq	; Al no tener Bsq desde el inicio, aquí se suma
    n            = N_ELEMENTS(Bdiono) 

    time_res    = 'm'
    time        = 0.


    CASE time_res of 
        'h'     : time = 3600.0
        'm'     : time = 60.0
    ENDCASE

    fny      = FLOAT(1.0/(2.0*time)) ; frecuencia de Nyquist
    y        = FFT(Bdiono)            ; Compute Fast Fourie Transform from diono time series
    ;###############################################################################



    ;###############################################################################    
    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
                    FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])	

    class = gms_class(station_code)
    info = stationlist(class, station_code)

    print, 'UTC: ', info.utc

    CALDAT, date_time, mh, dy, yr, hr, min 
    
    ;DEVICE, true=24, retain=2, decomposed=0
    ;TVLCT, R_bak, G_bak, B_bak, /GET     
    ;LOADCT, 39
    ;WINDOW, 1, XSIZE=800, YSIZE=500, TITLE='GS'

    ;plot, date_time, H, background=255, color=0, XMINOR=8, XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	;XTICKINTERVAL = 1,  xTITLE = 'Time [days]'
    ;oplot,date_time, symH0, color=120, thick = 2
    ;oplot,date_time, Bdiono, color=250, thick = 2
    ;oplot,date_time, SQ, color=70, thick = 2

    ;###############################################################################
    ;###############################################################################
    ;###############################################################################                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ###############################################################################
    ;###############################################################################                         


; INPUTS:
;
;    WAVE1 = wavelet power spectrum for time series #1
;    TIME1 = a vector of times for time series #1
;    SCALE1 = a vector of scales for time series #1
;    WAVE2 = wavelet power spectrum for time series #2
;    TIME2 = a vector of times for time series #2
;    SCALE2 = a vector of scales for time series #2    

    time1 = findgen(n_elements(Bdiono))  
    time2 = findgen(n_elements(SQ))  
    
    dt= 1.  ;resolucion de 1 minuto

;      wave = WAVELET(Y,DT)
; INPUTS:
;    Y = the time series of length N.
;  para el archivo: 2013-01-01-chan-0-3C298.dat
;  N= 44420
;    DT = amount of time between each Y value, i.e. the sampling time.
;  dt=0.0171661 (segundos) ; 4.76837e-06 (horas)

    dt = 1
	pad = 1
	s0 =  dt     
	dj = 0.0625  
	j1 = 14./dj  
    n = N_ELEMENTS(Bdiono)
    mother = 'Morlet'

    wave1 = WAVELET(Bdiono,dt,PERIOD=period,SCALE=scale,S0=s0, $
		COI=coi1,DJ=dj,J=j1,MOTHER=mother,/RECON,/PAD,signif=signif)

	
    wave2 = WAVELET(SQ,dt,PERIOD=period,SCALE=scale,S0=s0, $
    COI=coi1,DJ=dj,J=j1,MOTHER=mother,/RECON,/PAD,signif=signif)            
	
	;print, scale1 - scale2
    wave_coherency, wave1,time1,scale,wave2,time2,scale, COI1=coi1, DT=dt,DJ=dj, WAVE_COHER=wave_coher,WAVE_PHASE=wave_phase, $
	TIME_OUT=time_out,SCALE_OUT=scale_out,COI_OUT=coi_out, GLOBAL_COHER=global_coher,GLOBAL_PHASE=global_phase, $
	CROSS_WAVELET=cross_wavelet,POWER1=power1,POWER2=power2, NOSMOOTH=nosmooth, VERBOSE=verbose
	
	;print, 'local phase: ', wave_phase
	print, '#####################################################################'
	;print, 'Amplitude: ', 
	
	;compute semblance
	;semblance = 

	

END

