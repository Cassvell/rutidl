
;DATOS DE CAMPO MAGNETICO (TEOLOYUCAN?)
PRO wave_test, H_loc, H, SQ, asymH, date_i, date_f, station_code, PS=ps

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

    n = 7
    semblance = cos(wave_phase)^n

    
    ddyn = wave_coher * semblance
    
;==============================================================================
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
            make_psfig_composed, asymH, H_loc, power, cross_wavelet, ddyn, period, coi, date_i, date_f, path, station_code  
            make_psfig1, power, period, coi, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path,  station_code   
            make_psfig2, real_part(cross_wavelet), period, coi_out, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code	
            make_psfig3, ddyn, period, coi_out, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code

        ENDIF

END

