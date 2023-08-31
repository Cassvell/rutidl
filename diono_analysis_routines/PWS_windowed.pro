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
PRO PWS_windowed, date_i, date_f, PS=ps 
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
        
	station_idx = ''
	PRINT, 'Enter GMS idx: 0:coe, 1:teo, 2:tuc, 3:bsl, 4:itu'
	READ, station_idx, PROMPT = '> '
	
    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu	
    	
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
    H   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx, 'min')
    sym = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'sym')


; define Bsq 
    Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx, 'min')    

; Generate the time variables to plot TEC time series         
;###############################################################################
;identifying NAN percentage values in the Time Series
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 100.0, 'greater')    
;Identifying the NAN values        
;    tec = add_nan(tec, 999.0, 'equal')            
;    med = add_nan(med, 999.0, 'equal')                
    H = add_nan(H, 999999.0, 'equal')
    H = add_nan(H, 99999.0, 'equal')                   
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
    H = fillnan(H)

; define frequencies
    l            = 28.06  
    mlat         = l*!pi
    ld           = cos(mlat/180)
    p_a          = sym*ld
    baseline     = Bsq + p_a             
    Bdiono       = H-baseline
    n            = N_ELEMENTS(Bdiono) 
    
    time_res    = 'm'
    time        = 0.

    
    CASE time_res of 
        'h'     : time = 3600.0
        'm'     : time = 60.0
    ENDCASE

    fny      = FLOAT(1.0/(2.0*time)) ; frecuencia de Nyquist
    y        = FFT(Bdiono)            ; Compute Fast Fourie Transform from diono time series

    power_s = ABS(y[0:n/2])^2
   ; pws_s   = SMOOTH(pws, 1)

    fk     = (1+FINDGEN(n))/(n*time)
    hann_w = HANNING(n)
    
    w_ss = (TOTAL((hann_w)^2))/n
    ;print, w_ss
    y_w = FFT(hann_w*Bdiono)
  
    pws_w = (ABS(y_w[0:n/2])^2)/w_ss        
;###############################################################################
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/(0.5*3600.0)]
    

    periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0, 0.3]

    passband_l = 7.34E-06
 ;1.0/(36.0*3600.0)
    passband_u = 2.0048564e-05
 ;1.0/(18.0*3600.0)
    highpass_l = 9.5262271e-05;8.11E-05
; 1.0/(3.0*3600.0)

    
    i = WHERE(fk GE freqs[0])
    fn=WHERE(fk EQ fny)

	pws_w = pws_w/SQRT(TOTAL(pws_w^2))	;se normaliza el espectro de potencia

    ysup = MAX(pws_w[MIN(i):fn])
    yinf = MIN(pws_w[MIN(i):fn])
;###############################################################################
	;WINDOW, 2, XSIZE=1200, YSIZE=800, TITLE='PWS diono'
    PLOT, fk, pws_w, /XLOG, /YLOG, XRANGE = [freqs[0], fny], yrange=[yinf, ysup],$
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=1

    
    POLYFILL, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=255

    POLYFILL, [highpass_l, freqs[6], freqs[6], highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=255
    OPLOT, fk, pws_w, THICK=1 
            
        AXIS, XAXIS = 0, XRANGE=[freqs[0], fny], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], fny], $;.0/(!X.CRANGE), $
                         /XLOG,$
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
                         COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                         /ylog,$
                         YTICKFORMAT='(A1)',$ 
                         ystyle=1, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                         
    PRINT, 'Press click to get PWS value in a certain point until right mouse button is pressed'
  ;  WHILE (!MOUSE.button NE 4) DO BEGIN  ; repeat printing H trend value until right mouse button is pressed
;  FOR i = 0, 4 DO BEGIN; (!MOUSE.button NE 4) THEN BEGIN
  ;    CURSOR, x, y, /WAIT, /DATA
    ;  PRINT, x 
                 
;   ENDFOR      
  ;  ENDWHILE       
    IF keyword_set(ps) THEN BEGIN
    make_psfig, fk, fny, pws_w, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]    
    ENDIF
    
END

PRO make_psfig, f_k, fn, pws, date_i, date_f

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

    ;path = '../rutidl/output/article1events/diono_ev/'
    path = '../rutidl/output/article2/'
    psfile =  path+'diono_PWS_'+Date+'.eps'    
   ; LOADCT, 39
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=16

    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english') 
    
    time_title = ' UT [days]'
    window_title = 'Event-'+ STRING(TGM_n, FORMAT='(I2)')+', '+ $
                STRING(old_month, yr_i, FORMAT='(A, X, I4)')    
    periodo = 'Period [h]'                     
;###############################################################################               
    freqs = [1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/3600]
    
    i = WHERE(f_k GE freqs[0])
    fny=WHERE(f_k EQ fn)
    
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ;ysup = MAX(pws[MIN(i):fny])+1
   ;: yinf = MIN(pws[MIN(i):fny]);-0.0001
    ysup = MAX(pws)+1
    yinf = MIN(pws);-0.0001
                   
    periods = [48.0, 24.0, 12.0, 6.0, 3.0, 1.0]
    
    cgPLOT, f_k, pws, /XLOG, /YLOG, XRANGE = [freqs[0], fn], POSITION=[0.07,0.5,0.95,0.9],$
    YRANGE=[yinf, ysup], BACKGROUND = blanco, COLOR='black', $
    CHARSIZE = 1.4, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=1, /NODATA

    passband_l = freq_band(TGM_n, 'passband_l')
    passband_u = freq_band(TGM_n, 'passband_u')
    highpass_l = freq_band(TGM_n, 'highpass_l')
    
    cgPolygon, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='yellow', /FILL

    cgPolygon, [highpass_l, 1.0/(1800.0), 1.0/1800.0, highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='yellow', /FILL
                                             
              
    cgOPLOT, f_k, pws, COLOR='black', THICK=1  
;###############################################################################    
        AXIS, XAXIS = 0, XRANGE=[freqs[0], fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                        ; COLOR=negro, $
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], fn], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(F4.1)'),$
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
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.94, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, CHARTHICK=1.5   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.02, y, 'Spectral component [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90, CHARTHICK=1.5        
                     
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.97   
   XYOUTS, X, y, window_title, /NORMAL, $
   ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5      
                                                           
;###############################################################################    
    i = WHERE(f_k GE freqs[0])
    fny=WHERE(f_k EQ freqs[5])
    
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ysup = 10e-2
    yinf = 10e-7      
    cgPLOT, f_k, pws, POSITION=[0.07,0.1,0.45,0.4], /XLOG, /YLOG, XRANGE = [freqs[0], freqs[5]], $
    YRANGE=[yinf, ysup], XSTYLE=5, YSTYLE=5, /NOERASE                                                                


    
    cgPolygon, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='yellow', /FILL

    cgPolygon, [highpass_l, 1.0/(3600.0), 1.0/3600.0, highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='yellow', /FILL
                            
    cgOPLOT, f_k, pws, COLOR='black', THICK=1  
                            
        AXIS, XAXIS = 0, XRANGE=[freqs[0], freqs[5]], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                        ; COLOR=negro, $
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], freqs[5]], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(F4.1)'),$
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
  
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.43, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, CHARTHICK=1.5                           
;###############################################################################
    freqs = [1.0/3600.0, 1.0/1800.0, 1.0/(0.25*3600.0), $
              1.0/(0.10*3600.0)]
    
    i = WHERE(f_k GE freqs[0])
    fny=WHERE(f_k EQ fn)
    
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ysup = 10e-4
   ; yinf = 10e-7
   ; ysup = MAX(pws)+1
    yinf = MIN(pws);-0.0001
                   
    periods = [60.0, 30.0, 15.0, 6.0]
    
    cgPLOT, f_k, pws, POSITION=[0.55,0.1,0.95,0.4], /XLOG, /YLOG, XRANGE = [freqs[0], fn], $
    YRANGE=[yinf, ysup], XSTYLE=5, YSTYLE=5, /NOERASE  

    cgPolygon, [1.0/(3600.0), freqs[1], freqs[1], 1.0/3600.0], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='yellow', /FILL

    cgOPLOT, f_k, pws, COLOR='black', THICK=1  
    
        AXIS, XAXIS = 0, XRANGE=[freqs[0], fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                        ; COLOR=negro, $
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], fn], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(F4.1)'),$
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

    periodo = 'Period [m]'                          
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.43, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, CHARTHICK=1.5                           
;###############################################################################                     
;second panel legend                     
                
;###############################################################################                                                            
 ;   CGTEXT, 0.93, 0.73, '(a)', /Normal, $
 ;   Alignment=0.5, Charsize=1.6, CHARTHICK= 5   
   
;   XYOuts, 0.93, 0.53, '(b)', /Normal, $
 ;  Alignment=0.5, Charsize=1.6, CHARTHICK= 5  
   
;   XYOuts, 0.93, 0.33, '(c)', /Normal, $
  ; Alignment=0.5, Charsize=1.6, CHARTHICK= 5  
   
;   XYOuts, 0.11, 0.14, '(d)', /Normal, $
 ;  Alignment=0.5, Charsize=2.4, CHARTHICK= 5   
   
   ;XYOuts, 0.93, 0.13, '(e)', /Normal, $
  ; Alignment=0.5, Charsize=1.6, CHARTHICK= 5      
;###############################################################################   

    cgPS_Close, density = 300, width = 1600 ;, /PNG  
    RETURN  
END 

