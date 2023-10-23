
;DATOS DE CAMPO MAGNETICO (TEOLOYUCAN?)

;se lee el archivo de datos
readcol,'teo20030526_20030608.dat',bfield,format='f' 

arr=n_elements(bfield)     
times=findgen(arr)         ;arreglo de tiempo arbitrario

dt= 1.  ;resolucion de 1 minuto


erase
WINDOW,0, XSIZE=1000, YSIZE=500

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

; GWS significance levels:
	dof = n - scale   ; the -scale corrects for padding at edges
	global_signif = WAVE_SIGNIF(sst,dt,scale,1, $
		LAG1=0.90,DOF=dof,MOTHER=mother,CDELTA=Cdelta,PSI0=psi0)

; check total variance (Parseval's theorem) [Eqn(14)]
	scale_avg = REBIN(TRANSPOSE(scale),n,J+1)  ; expand scale-->(J+1)x(N) array
	power_norm = power/scale_avg
	variance = (MOMENT(sst))(1)
	recon_variance = dj*dt/(Cdelta*n)*TOTAL(power_norm)  ; [Eqn(14)]
	
	IF (N_ELEMENTS(recon_sst) GT 1) THEN BEGIN
		recon_variance = (MOMENT(recon_sst))(1)
; RMS of Reconstruction [Eqn(11)]
	rms_error = SQRT(TOTAL((sst - recon_sst)^2)/n)
	
; Scale-average 
	avg = WHERE((scale GE 0.5) AND (scale LT 3.3))
	scale_avg = dj*dt/Cdelta*TOTAL(power_norm(*,avg),2)  ; [Eqn(24)]
	scaleavg_signif = WAVE_SIGNIF(sst,dt,scale,2, $
;		GWS=global_ws,SIGLVL=SIGLVL,DOF=[0.3,50.0],MOTHER=mother)
		GWS=global_ws,SIGLVL=SIGLVL,DOF=[.5,3.3],MOTHER=mother)

     endif

;==============================================================================


LOADCT,23

!P.POSITION=[.09, .1, .93, .95]


period2 = FIX(ALOG(period)/ALOG(2))

cgCONTOUR,power,times,period, $
       XSTYLE=1,YTITLE='Period [minutes]', title='MAGNETIC FIELD DATA, teo20030526',$; xtickformat='xticks',$
       ystyle=1,C_COLORS=colors,ytickformat='exponent',$   ;*** Large-->Small period
       /YTYPE, LEVELS=levels,xrange=[min(times),max(times)],$    ;*** make y-axis logarithmic
       yrange=[min(period),max(period)],$
       NLEVELS=24,/FILL



;loadct,0
;AXIS, YAXIS=1, YRANGE=[min(1./period),max(1./period)],ystyle=1,/ylog,color=0,$
;      ytitle='Frequency [Hz]',ytickformat='exponent',charsize=1.7






end
