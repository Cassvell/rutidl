;Name:
;	PWS_windowed.pro
;purpose:
;   plot geomagnetic and ionospheric response due to Dst and Ddyn magnetic trace
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
;   .r iono_resp
;   iono_resp, idate, fdate
;parameters:
;   idate/fdate: format ([yyyy,mm,dd])
;
;dependencies:
;
;
;input files
;   Dst files, H obs, Bsq baseline, TEC data files
;
;output files:
;   .PNG figure
;   imported to /output/eventos_tgm/iono_resp_V9_yyyy-mm-dd.png
;
;version
;   Dec, 2022
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;
PRO PWS_windowed, date_i, date_f, PS=ps;, Bsq=Bsq 
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
;###############################################################################	
;###############################################################################     	
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
;###############################################################################
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_n = event_case([yr_i,mh_i,dy_i])  
;###############################################################################   
    time= findgen(file_number*1440)/1440.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################
; Generate the time series variables 
; define H variables                  
  ;  dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
  ;  dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    data   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = data.H
    sym = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'sym')


; define Bsq 
   ; Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx, 'min')    

; Generate the time variables to plot TEC time series         
;###############################################################################
;identifying NAN percentage values in the Time Series 
;Identifying the NAN values        
;    tec = add_nan(tec, 999.0, 'equal')            
;    med = add_nan(med, 999.0, 'equal')                
    H = add_nan(H, 999999.0, 'equal')
    H = add_nan(H, 99999.0, 'equal')                   
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
    H = fillnan(H)
;   print, H
; define frequencies
    l            = 28.06  
    mlat         = l*!pi
    ld           = cos(mlat/180)
    p_a          = sym*ld
    baseline     = p_a;Bsq + p_a Desde la actualización, Bsq ya está restado de H
    Bdiono       = H-baseline
    ;Bdiono2	     = H-p_a+Bsq	; Al no tener Bsq desde el inicio, aquí se suma
    n            = N_ELEMENTS(Bdiono) 
    
    time_res    = 'm'
    time        = 0.

    
    CASE time_res of 
        'h'     : time = 3600.0
        'm'     : time = 60.0
    ENDCASE

    fny      = FLOAT(1.0/(2.0*time)) ; frecuencia de Nyquist
    y        = FFT(Bdiono)            ; Compute Fast Fourie Transform from diono time series
	;y2 		 = FFT(Bdiono2)
    power_s = ABS(y[0:n/2])^2
   ; pws_s   = SMOOTH(pws, 1)

    fk     = (1+FINDGEN(n))/(n*time)
    hann_w = HANNING(n)
    
    w_ss = (TOTAL((hann_w)^2))/n
    ;print, w_ss
    y_w = FFT(hann_w*Bdiono)
    pws_w = (ABS(y_w[0:n/2])^2)/w_ss 

   ; y_w2 = FFT(hann_w*Bdiono2)
   ; pws_w2 = (ABS(y_w2[0:n/2])^2)/w_ss 
;###############################################################################
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/(0.5*3600.0)]
    hr_4 = 1.0/(4.0*3600.0)

    periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0, 0.3]
	threshold_rn = 1.0/(1300.0)
	hr_36 = 1.0/(36.0*3600.0)
	hr_7 = 1.0/(7.0*3600.0)
    j = WHERE(fk GE threshold_rn)
    k = WHERE(fk GE freqs[5])
    m = WHERE(fk GE hr_36)
    n = WHERE(fk GE hr_7)
	limP2 = MIN([j])
	limP  = MIN([k])
	limP1  = MIN([n])
	limP0 = MIN([m])
    passband_l = 7.34E-06
 ;1.0/(36.0*3600.0)
    passband_u = 2.0048564e-05
 ;1.0/(18.0*3600.0)
    highpass_l = 9.5262271e-05;8.11E-05
; 1.0/(3.0*3600.0)

    
    i = WHERE(fk GE freqs[0])
    fn=WHERE(fk EQ fny)

	pws_w = pws_w/SQRT(TOTAL(pws_w^2))	;se normaliza el espectro de potencia
	;PRINT, SQRT(MEDIAN(pws_w[limP0:limP1]))
;	pws_w2 = pws_w2/SQRT(TOTAL(pws_w2^2))	;se normaliza el espectro de potencia
    ysup = MAX(pws_w[MIN(i):fn])
    yinf = MIN(pws_w[MIN(i):fn])
;###############################################################################
;###############################################################################
pw_mod = pw_parameter([yr_i,mh_i,dy_i])
;###############################################################################
;LS-log curve fitting, CURVA 
c = 7.62127
N = c+0.25068;cte Euler
a = pw_mod.a1 
logX = (-0.57721466/alog10(10))-ALOG10(2)
p_law = pws_powerlaw(fk[limP0:limP], pws_w[limP0:limP], a, N, 0.05)
a_err1 = a+SQRT(p_law.a_2err)
a_err2 = a-SQRT(p_law.a_2err)
P = p_law.P

P = P*SQRT(TOTAL(pws_w[limP0:limP1]^2)) ;P*SQRT(TOTAL(pws_w[limP0:limP1]^2))
a_l = p_law.a_l
N_l = p_law.N_l
a_lw = p_law.a_lw
N_lw = p_law.N_lw
PRINT, ''
PRINT, 'a law, least square result (LSR), (LSR weighted): '
PRINT, a, ABS(a_l), ABS(a_lw)
PRINT, ''
PRINT, 'N law, least square result ALOG(LSR), ALOG(LSR weighted), : '
PRINT, N, ABS(N_l), EXP(ABS(N_l))
;P_95 = p_law.P_lim*SQRT(TOTAL(pws_w[0:limP]^2))
err1 = pws_powerlaw(fk[limP0:limP1], pws_w[limP0:limP1], a_err1, N, 0.05)
err2 = pws_powerlaw(fk[limP0:limP1], pws_w[limP0:limP1], a_err2, N, 0.05)

P_err1 = err1.P
P_err1 = P_err1*SQRT(TOTAL(pws_w[limP0:limP1]^2))

P_err2 = err2.P
P_err2 = P_err2*SQRT(TOTAL(pws_w[limP0:limP1]^2))

;###############################################################################
;###############################################################################
;###############################################################################
;lineas de tolerancia
;usar el error^2 de alfa
a_err = SQRT(0.0706783)   
;###############################################################################
;LS-log curve fitting, CURVA 2
c2 = 12.4164
N2 = c2+0.25068;cte Euler

a2 = pw_mod.a2 

;logP2 = ALOG10(N2) -a2*(ALOG10(fk[limP:limP2]))
p_law2 = pws_powerlaw(fk[limP:limP2], pws_w[limP:limP2], a2, N2, 0.05)
P2 = p_law2.P*SQRT(TOTAL(pws_w[limP:limP2]^2))
a_l2 = p_law2.a_l
N_l2 = p_law2.N_l
a_lw2 = p_law2.a_lw
N_lw2 = p_law2.N_lw
a2_err1 = a2+SQRT(p_law2.a_2err)
a2_err2 = a2-SQRT(p_law2.a_2err)
err2_1 = pws_powerlaw(fk[limP:limP2], pws_w[limP:limP2], a2_err1, N, 0.05)
err2_2 = pws_powerlaw(fk[limP:limP2], pws_w[limP:limP2], a2_err2, N, 0.05)

P2_err1 = err2_1.P
P2_err1 = P2_err1*SQRT(TOTAL(pws_w[limP:limP2]^2))

P2_err2 = err2_2.P
P2_err2 = P2_err2*SQRT(TOTAL(pws_w[limP:limP2]^2))
;P2_95 = p_law2.P_lim*SQRT(TOTAL(pws_w[limP:limP2]^2))
PRINT, ''
PRINT, 'a2 law, least square result (LSR), LSR weighted: '
PRINT, a2, ABS(a_l2), ABS(a_lw2)
PRINT, ''
PRINT, 'N, least square result ALOG(LSR), (LSR weighted): '
PRINT, N2, ABS(N_l2), (ABS(N_lw2))



;###############################################################################
;###############################################################################
    
    IF keyword_set(ps) THEN BEGIN
    	path = set_var.local_dir+'output/pws/'+station_code+'/'
    	test = FILE_TEST(path, /DIRECTORY) 
		IF test EQ 0 THEN BEGIN
			FILE_MKDIR, path
			PRINT, 'PATH directory '+path
			PRINT, 'created'
		ENDIF ELSE BEGIN
			PRINT, ''
			
		ENDELSE
    	make_psfig, fk, fny, pws_w, P ,P2, P_err1, P_err2, P2_err1, P2_err2, a, a2, $
    	[yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path    
    ENDIF

;f_k, fn, pws, P, P2, P_err1, P_err2, P2_err1, P2_err2, a1, a2, $
;				date_i, date_f, path

    DEVICE, true=24, retain=2, decomposed=0
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT 
    OMAX = 0
    OMIN = 0
    Hist = HISTOGRAM(H, NBINS=1000, LOCATIONS=binvals,  MIN=MIN(H), MAX=MAX(H), OMAX=OMAX, OMIN=OMIN)
WINDOW,1,  XSIZE=600, YSIZE=600, TITLE='Dist H'
    plot, binvals, Hist, XRANGE=[OMIN,OMAX], background=255, color=0, CHARSIZE=2

;PDF = p_law.PDF
;PDF2 = p_law2.PDF
;WINDOW,0,  XSIZE=600, YSIZE=600, TITLE='Dist Chi'
;PLOT, fk, PDF, /XLOG, /YLOG, XSTYLE=1, YSTYLE=1, background=255, color=0, CHARSIZE=2    

;WINDOW,3,  XSIZE=600, YSIZE=600, TITLE='Dist Chi2'
;PLOT, fk, PDF2, /XLOG, /YLOG, XSTYLE=1, YSTYLE=1,background=255, color=0 , CHARSIZE=2

gammahat  = 2*pws_w[limP0:limP]/P 
gammahat1 = 2*pws_w[limP0:limP]/P_err1 
gammahat2 = 2*pws_w[limP0:limP]/P_err2

WINDOW,4,  XSIZE=600, YSIZE=600, TITLE='gammahat1'
PLOT, fk[limP0:limP1], gammahat, XSTYLE=1, YSTYLE=2,background=255, color=0, CHARSIZE=2, /XLOG, /YLOG
ERRPLOT, fk[limP0:limP1], gammahat2, gammahat1, color=0, THICK=2
OPLOT, fk[limP0:limP1], gammahat, PSYM=4, color=70, THICK=4

OPLOT, [fk[limP0],fk[limP1]], [1,1], color=240
OPLOT, [fk[limP0],fk[limP1]], [1+SQRT(a_err),1+SQRT(a_err)], color=240, linestyle=2
OPLOT, [fk[limP0],fk[limP1]], [1-SQRT(a_err),1-SQRT(a_err)], color=240, linestyle=2
    
gammahat2 = 2*pws_w[limP:limP2]/P2 
WINDOW,5,  XSIZE=600, YSIZE=600, TITLE='gammahat2'
PLOT, fk[limP:limP2], gammahat2, XSTYLE=1, YSTYLE=1,background=255, color=0, /XLOG, /YLOG, CHARSIZE=2
OPLOT, fk[limP:limP2], gammahat2, PSYM=4, color=70, THICK=2

OPLOT, [fk[limP],fk[limP2]], [1,1], color=240, THICK=2
OPLOT, [fk[limP],fk[limP2]], [1+SQRT(a_err),1+SQRT(a_err)], color=240, linestyle=2, THICK=2
OPLOT, [fk[limP],fk[limP2]], [1-SQRT(a_err),1-SQRT(a_err)], color=240, linestyle=2, THICK=2
;###############################################################################    
    freqs = [1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/3600] 
    
    i = WHERE(fk GE freqs[0])
    fn=WHERE(fk EQ fny)
    
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ;ysup = MAX(pws[MIN(i):fny])+1
   ;: yinf = MIN(pws[MIN(i):fny]);-0.0001
    ysup = MAX(pws_w)+1
    yinf = MIN(pws_w);-0.0001
                   
    periods = [48.0, 24.0, 12.0, 6.0, 3.0, 1.0]

WINDOW,2,  XSIZE=600, YSIZE=600, TITLE='LS fit PSD'
    PLOT, fk, pws_w, /XLOG, /YLOG, XRANGE = [freqs[0], fny], yrange=[yinf, ysup],$
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=1, BACKGROUND=255, COLOR=0
    
	OPLOT, fk[limP0:limP1], P_err1, LINESTYLE=2, COLOR=75, THICK=2 
    OPLOT, fk[limP0:limP1], P_err2, LINESTYLE=2, COLOR=75, THICK=2

    OPLOT, fk[limP0:limP1], P, LINESTYLE=0, COLOR=250, THICK=2
    OPLOT, fk[limP:limP2], P2, LINESTYLE=0, COLOR=75, THICK=2	
        AXIS, XAXIS = 0, XRANGE=[freqs[0], fny], $
                         /XLOG,$                          
                         COLOR=0, $
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], fny], $;.0/(!X.CRANGE), $
                         /XLOG,$                          
                         COLOR=0, $
                         XTICKS=7,$
                      ;   XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(F4.1)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.4,$
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                         COLOR=0, $
                         /ylog,$
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                         /ylog,$                          
                         COLOR=0, $
                         YTICKFORMAT='(A1)',$ 
                         ystyle=1, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
	;WINDOW, 3, XSIZE=600, YSIZE=600, TITLE='fit PSD'
	;PLOT, fk[limP0:limP], P_err2, /XLOG, /YLOG, $
	;LINESTYLE=0, XSTYLE=1, YSTYLE=1,  COLOR=0, BACKGROUND=255  , CHARSIZE=2                      
	;OPLOT, fk[limP0:limP], P_err1, color=250
;	WINDOW, 4, XSIZE=600, YSIZE=600, TITLE='fit PSD2'
;	PLOT, fk[limP:limP2], gamma_hat2, /XLOG, /YLOG, $
;	LINESTYLE=0, XSTYLE=1, YSTYLE=1,  COLOR=0, BACKGROUND=255

;    Hist2 = HISTOGRAM(gamma_hat, NBINS=10000, LOCATIONS=binvals,  MIN=MIN(gamma_hat), $
;    MAX=MAX(gamma_hat), OMAX=OMAX, OMIN=OMIN)
    
	gama = TeXtoIDL('\gamma') 
;	WINDOW,0,  XSIZE=600, YSIZE=600, TITLE='Dist'
;    plot, binvals, Hist, XRANGE=[OMIN,OMAX], background=255, color=0, title=gama, CHARSIZE=2, /XLOG

	alfa = TeXtoIDL('\alpha')	
END

PRO make_psfig, f_k, fn, pws, P, P2, P_err1, P_err2, P2_err1, P2_err2, a1, a2, $
				date_i, date_f, path
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
    psfile =  path+'diono_PWS_powerL_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=8, YSize=16

    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english') 
    	print, TGM_n
    time_title = ' UT [days]'
   ; IF TGM_n NE 'fuera de rango' THEN BEGIN    
    	window_title = 'Event-'+ STRING(TGM_n, FORMAT='(I2)')+', '+ $
                STRING(old_month, yr_i, FORMAT='(A, X, I4)') 
    
    ;ENDIF ELSE BEGIN
	;    window_title = 'Quiet Period, '+ $
    ;            STRING(old_month, yr_i, FORMAT='(A, X, I4)')           
    ;ENDELSE                    
    periodo = 'Period [h]'   
        
;###############################################################################               
    freqs = [1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/3600] 
    
    i = WHERE(f_k GE freqs[0]);, THICK=1 
    fny=WHERE(f_k EQ fn)

	hr_36 = 1.0/(36.0*3600.0)
	hr_7 = 1.0/(7.0*3600.0)
	threshold_rn = 1.0/(1300.0)
	hr_4 = 1.0/(4.0*3600.0)
	
    j = WHERE(f_k GE threshold_rn)
    k = WHERE(f_k GE freqs[4])
    m = WHERE(f_k GE hr_36)
    n = WHERE(f_k GE hr_7)
	limP2 = MIN([j])
	limP  = MIN([k])
	limP1  = MIN([n])
	limP0 = MIN([m])
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ;ysup = MAX(pws[MIN(i):fny])+1
   ;: yinf = MIN(pws[MIN(i):fny]);-0.0001
    ysup = MAX(pws)+1
    yinf = MIN(pws);-0.0001
                   
    periods = [48, 24, 12, 6, 3, 1]
   
    cgPLOT, f_k, pws, /XLOG, /YLOG, XRANGE = [freqs[0], 2.7e-3], POSITION=[0.07,0.52,0.9,0.92],$
    YRANGE=[yinf, ysup], BACKGROUND = blanco, COLOR='black', $
    CHARSIZE = 1.4, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=1, /NODATA
    
  ;  IF TGM_n NE 'fuera de rango' THEN BEGIN
    cgPolygon, [f_k[limP0], f_k[limP1] ,f_k[limP1], f_k[limP0]], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], Color='grey', /FILL

    cgPolygon, [f_k[limP], f_k[limP2] ,f_k[limP2], f_k[limP]], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], Color='light grey', /FILL
                                             
    ;ENDIF

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.97   
   XYOUTS, X, y, window_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.8, CHARTHICK=3 

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.94   
   XYOUTS, X, y, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65 

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.492   
   XYOUTS, X, y, 'Frequence [Hz]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4

   CGTEXT, 0.19, 0.7, 'a1= '+STRING(a1, FORMAT='(F4.1)'), /Normal, $
   Alignment=0.5, Charsize=1.4, CHARTHICK= 5, COLOR='red' 


   CGTEXT, X+0.05, 0.89, 'a2= '+STRING(a2, FORMAT='(F4.1)'), /Normal, $
   Alignment=0.5, Charsize=1.4, CHARTHICK= 5 , COLOR='red' 


	sq = TeXtoIDL('H_{SQ}')
   CGTEXT, 0.8, 0.79, 'sin '+sq, /Normal, $
   Alignment=0.5, Charsize=2, CHARTHICK= 5, COLOR='blue' 

    cgOPLOT, f_k[limP:limP2], pws[limP:limP2], PSYM=4, Color='yellow', THICK=1
    
    cgOPLOT, f_k, pws, COLOR='black', THICK=1 
	cgOPLOT, f_k[limP0:limP1], P_err1, LINESTYLE=2, Color='red', THICK=1 
    cgOPLOT, f_k[limP0:limP1], P_err2, LINESTYLE=2, Color='red', THICK=1    
    cgOPLOT, f_k[limP0:limP1], pws[limP0:limP1], PSYM=4, Color='yellow', THICK=1
    
    cgOPLOT, f_k[limP0:limP1], P, COLOR='red', THICK=3

 	 
	cgOPLOT, f_k[limP:limP2], P2_err1, LINESTYLE=2, Color='red', THICK=1 
    cgOPLOT, f_k[limP:limP2], P2_err2, LINESTYLE=2, Color='red', THICK=1    
    cgOPLOT, f_k[limP:limP2], P2, COLOR='red', THICK=3
    
    
   
        AXIS, XAXIS = 0, XRANGE=[freqs[0], 2.7e-3], $
                         /XLOG,$
                         XSTYLE=1,$
                         ;xTITLE = 'Frequence [Hz]',$
                        ; COLOR=negro, $
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], 2.7e-3], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(I2)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.4,$
                       ;  COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                        ; COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                        ; COLOR=negro, $
                         /ylog,$
                         YTICKFORMAT='(A1)',$ 
                         ystyle=1, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
;############################################################################### 
  
;###############################################################################                          
;###############################################################################                     
;second panel legend                     
                
;###############################################################################                                                            

;###############################################################################  
 ;   CGTEXT, 0.93, 0.73, '(a)', /Normal, $
 ;   Alignment=0.5, Charsize=1.6, CHARTHICK= 5   
   
;   XYOuts, 0.93, 0.53, '(b)', /Normal, $
 ;  Alignment=0.5, Charsize=1.6, CHARTHICK= 5  
    
   
;   XYOuts, 0.11, 0.14, '(d)', /Normal, $
 ;  Alignment=0.5, Charsize=2.4, CHARTHICK= 5   
   
   ;XYOuts, 0.93, 0.13, '(e)', /Normal, $
  ; Alignment=0.5, Charsize=1.6, CHARTHICK= 5      
;###############################################################################   

    cgPS_Close, density = 300, width = 1600 ;, /PNG  
    RETURN  
END 

