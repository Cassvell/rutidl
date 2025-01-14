;Name:
;	iono_respV2.pro
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
;   iono_resp, idate, fdate, PNG='png', PS='ps'
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
;   apr, 2023
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;

PRO iono_respV2, date_i, date_f, PNG = png, PS=ps 

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
;###############################################################################
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_n = event_case([yr_i,mh_i,dy_i])  
;###############################################################################   
    time= findgen(file_number*1440)/1440.0
    ;time_h = findgen(file_number*24)/24.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################
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
; Generate the time series variables 
; define H variables                  
    dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code)
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    dat   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = dat.H

;###############################################################################      
; IP data
; Generate the time variables to plot TEC time series         
    tec         = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'tec')
    med         = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'med') 
    tec_resp    = tec-med          
;###############################################################################
;identifying NAN percentage values in the Time Series
   ; H = nanpc(H, 999999.0, 'equal')
   ; H = nanpc(H, 100.0, 'greater')    
;Identifying the NAN values        
    tec = add_nan(tec, 999.0, 'equal')            
    med = add_nan(med, 999.0, 'equal')                

    dH = add_nan(dH, 100.0, 'greater')
;    H = add_nan(H, 99999.0, 'equal')                   
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
    H = fillnan(H)
 ;   PRINT, H;N_ELEMENTS(H), N_ELEMENTS(Bsq)
;###############################################################################    
    new_dst = FLTARR(N_ELEMENTS(time))     	    
    tmp_dst  = INTERPOL(dst, N_ELEMENTS(time))
    new_dst = tmp_dst 
    
    new_H = FLTARR(N_ELEMENTS(time))     	    
    tmp_H  = INTERPOL(H, N_ELEMENTS(time))
    new_H = tmp_H       
;###############################################################################      
    new_dH = FLTARR(N_ELEMENTS(time))     	    
    tmp_dH  = INTERPOL(dH, N_ELEMENTS(time))
    new_dH = tmp_dH 
    
    new_tecresp = FLTARR(N_ELEMENTS(time))     	    
    tmp_tecresp  = INTERPOL(tec_resp, N_ELEMENTS(time))
    new_tecresp = tmp_tecresp 
    new_tecresp = add_nan(new_tecresp, 0.0, 'equal')             
;###############################################################################
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')
 ;   PRINT, Bsq
; compute frequencies 
    f_k   = dionstr.f_k
    fn    = dionstr.fn

; compute and define Power Spectrum
    pws = dionstr.pws
	pws = pws/SQRT(TOTAL(pws^2))
; compute diono variables    
    diono = dionstr.diono
  ;  PRINT, dst
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn
;############################################################################### 
    i_diff = diono
   ; print, diono
    new_idiff = FLTARR(N_ELEMENTS(time))     	    
    tmp_idiff  = INTERPOL(i_diff, N_ELEMENTS(time))
    new_idiff = tmp_idiff      
;###############################################################################      
    new_ddyn = FLTARR(N_ELEMENTS(time))     	    
    tmp_ddyn  = INTERPOL(ddyn, N_ELEMENTS(time))
    new_ddyn = tmp_ddyn           
;############################################################################### 
    new_dp2 = FLTARR(N_ELEMENTS(time))     	    
    tmp_dp2  = INTERPOL(dp2, N_ELEMENTS(time))
    new_dp2 = tmp_dp2
;###############################################################################    
    dst_min = MIN(dst,i)    
    t0 = 0
    
    CASE TGM_n of
        1   :   t0 = 18
        2   :   t0 = 38
        3   :   t0 = 14
        4   :   t0 = 124
        5   :   t0 = 36
        6   :   t0 = 20
        7   :   t0 = 11
        8   :   t0 = 18
        9   :   t0 = 14
        10  :   t0 = 10
        11  :   t0 = 17
        12  :   t0 = 18
        13  :   t0 = 17
        14  :   t0 = 26
        15  :   t0 = 35
        16  :   t0 = 30
        17  :   t0 = 22
        18  :   t0 = 18
        19  :   t0 = 5
        20  :   t0 = 19        
        ELSE: PRINT, 'evento no disponible'   
    ENDCASE
    tf = 0
    CASE TGM_n of
        1   :   tf = 183
        2   :   tf = 186
        3   :   tf = 52
        4   :   tf = 36
        5   :   tf = 70
        6   :   tf = 66
        7   :   tf = 60
        8   :   tf = 123
        9   :   tf = 38
        10  :   tf = 146
        11  :   tf = 82
        12  :   tf = 34
        13  :   tf = 71
        14  :   tf = 60
        15  :   tf = 42
        16  :   tf = 120
        17  :   tf = 144
        18  :   tf = 19
        19  :   tf = 34
        20  :   tf = 22        
        ELSE: PRINT, 'evento no disponible'   
    ENDCASE
    print, i
    SG_beg = i-t0
    IF tf NE 0 THEN BEGIN
    SG_end = i+tf
    ENDIF ELSE BEGIN
    SG_end = N_ELEMENTS(dH)-10
    ENDELSE 
;###############################################################################    
    med_ddyn = MEDIAN(new_ddyn)
   ; std_ddyn = STDDEV(new_ddyn, /NAN)
    
    IQR = PERCENTILES(new_ddyn, CONFLIMIT=0.5) 
    IQR_n = (IQR[1]-IQR[0])*1
    
    ddyn_out = WHERE(new_ddyn GE med_ddyn+IQR_n OR new_ddyn LE med_ddyn-IQR_n)
    ddyn_in  = WHERE(new_ddyn LE med_ddyn+IQR_n AND new_ddyn GE med_ddyn-IQR_n)
    
    ddyn_diff_out = new_ddyn
    ddyn_diff_out[ddyn_in]=!Values.F_NAN
    
    ddyn_diff_in  = new_ddyn
    ddyn_diff_in[ddyn_out]=!Values.F_NAN    
;###############################################################################
    med_dp2 = MEDIAN(new_dp2)                       
;###############################################################################
; define device and color parameters 
;###############################################################################
    
  ;  f_s = 2.77777777e-4;f_k[1]-f_k[0]
  ;  print, f_s
  ;  f_r = f_k*((2*!PI)/f_s)
  ;  print, f_r
;   PLOT, f_r, ATAN(pws), THICK=2, YSTYLE=1, XSTYLE=1, /XLOG

;        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
;                         XTICKS=file_number, $
 ;                        XTITLE=time_title, $                         
 ;                        XMINOR=8, $
  ;                       XTICKNAME=X_label, $
  ;                       COLOR=negro, $
   ;                     CHARSIZE = 1.6,$
    ;                     CHARTHICK=1.8
                             
     ;   AXIS, YAXIS = 0, YTITLE = 'Thermal pressure [nPa]', $
      ;                   ystyle=1,$                          
                        ; COLOR=negro, $
                         ;/ylog,$
       ;                  CHARSIZE = 1.6,$
        ;                 CHARTHICK=1.8

    IF keyword_set(ps) THEN BEGIN
    make_psfig, f_k, fn, pws, new_dst, new_dH, new_idiff, new_ddyn, new_dp2, time, new_tecresp,$
        SG_beg, SG_end, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]    
    ENDIF

    IF keyword_set(png) THEN BEGIN        
    make_pngfig, f_k, fn, pws, new_Bz, new_Ey, new_vp, new_pdyn, new_idiff, new_ddyn, new_dp2, time, $
        SG_beg, SG_end, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]               
    ENDIF 


END

PRO make_psfig, f_k, fn, pws, new_dst, new_dH, new_idiff, new_ddyn, new_dp2, time, new_tecresp,$
        SG_beg, SG_end, date_i, date_f

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
    path = '../rutidl/output/article2/ev_art1/'
    psfile =  path+'iono_PI_'+Date+'.eps'    
   ; LOADCT, 39
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10

    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english') 
    
    time_title = ' UT [days]'
    window_title = 'Event '+ STRING(TGM_n, FORMAT='(I2)')+', '+ $
                STRING(old_month, yr_i, FORMAT='(A, X, I4)')    
    periodo = 'Period [h]'        
;###############################################################################               
    cgPLOT, f_k, pws, /XLOG, /YLOG, POSITION=[0.1,0.11,0.95,0.89],$
    BACKGROUND = 'white', COLOR='black', $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA    

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, $
   ALIGNMENT=0.5, CHARSIZE=2.8, CHARTHICK=1.5               
;###############################################################################               
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0)]
    
    i = WHERE(f_k GE freqs[0])
    fny=WHERE(f_k EQ fn)
    
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ysup = MAX(pws[MIN(i):fny])+1
    yinf = MIN(pws[MIN(i):fny]);-0.0001
               
    periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0]
    
    cgPLOT, f_k, pws, /XLOG, /YLOG, XRANGE = [freqs[1], fn], POSITION=[0.08,0.11,0.4,0.89],$
    YRANGE=[yinf, ysup], BACKGROUND = 'white', COLOR='black', $
    CHARSIZE = 1.4, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=2, /NODATA,$
    /NOERASE

    passband_l = freq_band(TGM_n, 'passband_l')
    passband_u = freq_band(TGM_n, 'passband_u')
    highpass_l = freq_band(TGM_n, 'highpass_l')
    
    cgPolygon, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='BLK2', /FILL

    cgPolygon, [highpass_l, fn, fn, highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='BLK2', /FILL
                                             
              
    cgOPLOT, f_k, pws, COLOR='black', THICK=5   
;###############################################################################    
        AXIS, XAXIS = 0, XRANGE=[freqs[1], fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = '',$
                        ; COLOR=negro, $
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[1], fn], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(F4.1)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.2,$
                       ;  COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                        ; COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                        ; COLOR=negro, $
                         /ylog,$
                         YTICKFORMAT='(A1)',$ 
                         ystyle=1, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5

   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.922, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5   
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.065, 'Frequence [Hz]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5      
   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.025, y, 'Power Spectral Density', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=1.5
   
   XYOUTS, 0.18, .3, 'Ddyn', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5     
   
   XYOUTS, 0.378, .775, 'DP2', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5    
;###############################################################################       
     dH = TeXtoIDL('\DeltaH') 
;###############################################################################   [0.5,0.7,0.95,0.9]   
      up = MAX(new_dH)
     down=MIN(new_dH)

     CGPLOT, time, new_dH, XTICKS=file_number, XMINOR=8, BACKGROUND = 'white', $
     COLOR='black', CHARSIZE = 0.9, CHARTHICK=chr_thick1, $
     POSITION=[0.5,0.71,0.95,0.89], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, THICK=2, /NODATA       
     
     d_H = TeXtoIDL('\DeltaH')
     
     CGOPLOT, time, new_dst, COLOR='GRN5', THICK=4      

     CGOPLOT, time, new_dH, COLOR='black', THICK=2   
         print, SG_beg
    cgOPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
    
    cgOPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'      
        AXIS, XAXIS = 0, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                        ; COLOR=negro, $
                         CHARSIZE = 3.0 , $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5 
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                        ; COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = 'G. Indices [nT]', $                          
                         ;COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6 
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                        ; COLOR=negro, $                                                                      
                         YSTYLE=2, $       
                         YTICKFORMAT='(A1)',$
                         CHARSIZE = 1.4 ,$
                         CHARTHICK=1.6                                                                                            
;###############################################################################                                 
;############################################################################### 
     up_diono=max(new_idiff)
     down_diono=min(new_idiff)
    ; print,  new_idiff
     cgPLOT, time, new_idiff, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR='black', CHARSIZE = 0.6, CHARTHICK=chr_thick1, $
     POSITION=[0.5,0.51,0.95,0.68], XSTYLE = 5, XRANGE=[0, file_number], ySTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down_diono,up_diono], /NOERASE,$
     THICK=2, /NODATA   

    cgOPLOT, time, new_idiff, THICK=1, LINESTYLE=0, COLOR='black'     
    cgOPLOT, time[SG_beg*60:SG_end*60], new_idiff[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR='black' 
  

     
    cgOPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
    
    cgOPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'  

    ppi = TexToIDL('D_{I}')        
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='', $                         
                         XMINOR=8, $
                         XTICKFORMAT='(A1)',$
               ;          COLOR=negro, $
                         CHARSIZE = 1.6, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$                     
                      ;   COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $
                         YTITLE = ppi+' [nT]', $                          
                   ;      COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $      
                     ;    COLOR=negro, $
                         YSTYLE=2, $       
                         YTICKFORMAT='(A1)',$
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6                
;###############################################################################                
    IF max(new_ddyn) GT max(new_dp2) THEN up = max(new_ddyn) ELSE up = max(new_dp2)
    IF min(new_ddyn) LT min(new_dp2) THEN down = min(new_ddyn) ELSE down = min(new_dp2)
;###############################################################################
   ; IF upddyn GT updp2 THEN up = upddyn ELSE up=updp2 
    ;IF downddyn LT downdp2 THEN down = downddyn ELSE down=downdp2 
                               
     cgPLOT, time, new_ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR='black', CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.5,0.31,0.95,0.48], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA
    
    cgOPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
    
    cgOPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'

    cgOPLOT, time, new_ddyn, COLOR='black' , LINESTYLE=0, THICK=1
    cgOPLOT, time[SG_beg*60:SG_end*60], new_ddyn[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR='black'    
  
;###############################################################################     
    cgOPLOT, time, new_dp2, COLOR='red', THICK=1
    cgOPLOT, time[SG_beg*60:SG_end*60], new_dp2[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR='red'       
    
    cgOPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0.], LINESTYLE=1, THICK=4,COLOR='black'
    med_dp2 = MEDIAN(new_dp2)   
 

        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='', $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6
                         
                                                 
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 0.8, $                                                
                    ;     COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, yrange=[down,up], $ 
                         ystyle=2, $  
                         YTITLE = 'I. Currents [nT]', $                          
                    ;     COLOR=negro, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=2, $        
                         YTICKFORMAT='(A1)',$
                  ;       COLOR=negro, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6          
;###############################################################################                                                                     
;###############################################################################
    up      = MAX(new_tecresp)
    down    = MIN(new_tecresp)
     cgPLOT, time, new_tecresp, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR='black', CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.5,0.11,0.95,0.28], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA
    dtec = TeXtoIDL('\Delta TEC') 
    cgOPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
    
    cgOPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'

    cgOPLOT, time, new_tecresp, COLOR='red' , LINESTYLE=0, THICK=1
    cgOPLOT, time[SG_beg*60:SG_end*60], new_tecresp[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR='red'    
  ;
  ;print, new_tecresp
;###############################################################################     
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $       
                       ;  XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6
                         
                                                 
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 0.8, $                                                
                    ;     COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, yrange=[down,up], $ 
                         ystyle=1, $  
                         YTITLE = dtec+' [TECu]', $                          
                    ;     COLOR=negro, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=2, $        
                         YTICKFORMAT='(A1)',$
                  ;       COLOR=negro, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6       
;###############################################################################
;first panel legend 
        cgPolygon, [0.90,0.93,0.93,0.90], [0.804,0.804,0.807,0.807], color = 'black', /NORMAL, /FILL    
        cgPolygon, [0.90,0.93,0.93,0.90], [0.767,0.767,0.770,0.770], color = 'GRN5', /NORMAL , /FILL  
        
        XYOUTS, 0.87, 0.8 , /NORMAL, d_H, CHARSIZE = 1.2, CHARTHICK=chr_thick1                 
                
        XYOUTS, 0.87, 0.762 , /NORMAL, 'Dst', CHARSIZE = 1.2, CHARTHICK=chr_thick1  
;###############################################################################                     
;second panel legend                   
        cgPolygon, [0.91,0.94,0.94,0.91], [0.461,0.461,0.464,0.464], color = 'black', /NORMAL, /FILL    
        cgPolygon, [0.91,0.94,0.94,0.91], [0.424,0.424,0.427,0.427], color = 'red', /NORMAL , /FILL  
        
        XYOUTS, 0.87, 0.456 , /NORMAL, 'Ddyn', CHARSIZE = 1.2, CHARTHICK=chr_thick1                 
                
        XYOUTS, 0.87, 0.42 , /NORMAL, 'DP2', CHARSIZE = 1.2, CHARTHICK=chr_thick1     
                
;###############################################################################                                                            
  !P.Font = 1
  XYOuts, 0.53, 0.735, '(a)', /Normal, $
    Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font= 3 
   
   XYOuts, 0.91, 0.54, '(b)', /Normal, $
   Alignment=0.5, Charsize=3.2, CHARTHICK= 10, font=1  
   
   XYOuts, 0.53, 0.335, '(d)', /Normal, $
   Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font=1  
   
   XYOuts, 0.11, 0.82, '(c)', /Normal, $
   Alignment=0.5, Charsize=3.8, CHARTHICK= 10;, font=1  
   
   XYOuts, 0.91, 0.235, '(e)', /Normal, $
   Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font=1    
   !P.Font = 0
;###############################################################################   

    cgPS_Close, density = 300, width = 1600;, /PNG  
    RETURN  
END 

PRO make_pngfig, f_k, fn, pws, new_Bz, new_Ey, new_vp, new_pdyn, new_idiff, new_ddyn, new_dp2, time, $
    SG_beg, SG_end, date_i, date_f

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
    time_title = ' UT [days]'
    window_title = 'SGS-'+ STRING(TGM_n, FORMAT='(I02)')
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)') 
                
        Device_bak2 = !D.Name         
        SET_PLOT, 'Z'      
        
        Xsize=fix(1600)
        Ysize=1000
        DEVICE, SET_RESOLUTION = [Xsize,Ysize],Set_Pixel_Depth=24, DECOMPOSED=1  
        DEVICE, z_buffer=4
        DEVICE, set_character_size = [10, 12] 
        
        chr_size1 = 0.9
        chr_thick1= 1.5
        space     = 0.015
        rojo      = 248
        amarillo  = 190
        verde     = 150
        negro     = 0
        azul      = 70
        blanco    = 255
        gris      = 110
        morado    = 16
        naranja  = 220
                
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT

    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')
;Function to convert month from mm format to string              
;###############################################################################   
;############################################################################### 
    time_title = ' UT ['+textoidl("days")+' of '+old_month+'].'
    window_title = 'SGS'+ STRING(TGM_n, FORMAT='(I2)')+', '+ $
                    STRING(old_month, yr_i, FORMAT='(A, X, I4)')
    
    periodo = 'Period [h]'        
;###############################################################################               
    PLOT, f_k, pws, /XLOG, /YLOG, POSITION=[0.1,0.1,0.95,0.9],$
    BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA    

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5               
;###############################################################################               
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0)]
    
    i = WHERE(f_k GE freqs[0])
    fny=WHERE(f_k EQ fn)
    
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ysup = MAX(pws[MIN(i):fny])+1
    yinf = MIN(pws[MIN(i):fny]);-0.0001
               
    periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0]
    
    PLOT, f_k, pws, /XLOG, /YLOG, XRANGE = [freqs[0], fn], POSITION=[0.07,0.1,0.42,0.9],$
    YRANGE=[yinf, ysup], BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = 1.4, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=2, /NODATA,$
    /NOERASE

    passband_l = freq_band(TGM_n, 'passband_l')
    passband_u = freq_band(TGM_n, 'passband_u')
    highpass_l = freq_band(TGM_n, 'highpass_l')
    
    POLYFILL, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=amarillo

    POLYFILL, [highpass_l, fn, fn, highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=amarillo
    OPLOT, f_k, pws, COLOR=negro, THICK=5   
;###############################################################################    
        AXIS, XAXIS = 0, XRANGE=[freqs[0], fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                         COLOR=negro, $
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
                         COLOR=negro, $
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
                         COLOR=negro, $
                         /ylog,$
                         ystyle=1, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5

   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.94, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, CHARTHICK=1.5   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.02, y, 'Power Spectral density [nT]', /NORMAL, CHARSIZE=2, ALIGNMENT=0.5, $
   ORIENTATION=90, CHARTHICK=1.5        
;###############################################################################       
     dH = TeXtoIDL('\DeltaH') 
;###############################################################################      
     up_B = MAX(new_Bz+5) 
    down_B = MIN(new_Bz-5) 

    up_E = MAX(new_Ey+5) 
    down_E = MIN(new_Ey-5)
        
    PLOT, time, new_Bz, XTICKS=file_number, xminor=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.55,0.7,0.95,0.9], $
     XSTYLE = 5, XRANGE=[0, file_number],  XTICKNAME=REPLICATE(' ', file_number+1), YSTYLE = 5,$
     YRANGE=[down_B, up_B], THICK=1, /NOERASE  

 	OPLOT, time[SG_beg*60:SG_end*60], new_Bz[SG_beg*60:SG_end*60], LINESTYLE=0, THICK=3, COLOR=negro  
    
    OPLOT, time, new_Ey, LINESTYLE=0, THICK=1, COLOR=azul
    OPLOT, time[SG_beg*60:SG_end*60], new_Ey[SG_beg*60:SG_end*60], LINESTYLE=0, THICK=3, COLOR=azul 
    
       
    OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro 
               
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         ;XTITLE='UT [days]', $                           
                         XMINOR=8, $
                         XTICKFORMAT='(A1)',$ 
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down_B, up_B], $
                         YSTYLE=1, $  
                         YTITLE = 'Bz [nT] & Ey [mV]', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 1.4;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down_E,up_E], $
                         YSTYLE=1, $  
                         ;YTITLE = '', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.4, $
                         CHARTHICK=1.5                          
;###############################################################################
;###############################################################################    
    up_p    = MAX(new_pdyn)
    down_p  = MIN(new_pdyn) 
    
    up_v    = MAX(new_vp)
    down_v  = MIN(new_vp)

     PLOT, time, new_vp, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.55,0.5,0.95,0.69], $
     XSTYLE = 5, XRANGE=[0, file_number], YRANGE=[down_v,up_v], $
     XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 5, /NOERASE, THICK=3, /NODATA
     
    OPLOT, time, new_vp, LINESTYLE=0, THICK=1, COLOR=azul 
    OPLOT, time[SG_beg*60:SG_end*60], new_vp[SG_beg*60:SG_end*60], LINESTYLE=0, THICK=3, COLOR=azul     

     PLOT, time, new_pdyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.55,0.5,0.95,0.69], $
     XSTYLE = 5, XRANGE=[0, file_number], YRANGE=[down_p,up_p], $
     XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 5, /NOERASE, THICK=3, /NODATA
    
    OPLOT, time, new_pdyn, LINESTYLE=0, THICK=1, COLOR=negro 
    OPLOT, time[SG_beg*60:SG_end*60], new_pdyn[SG_beg*60:SG_end*60], LINESTYLE=0, THICK=3, COLOR=negro
    
  
    OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
      
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         ;XTITLE='UT [days]', $                           
                         XMINOR=8, $
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down_v, up_v], $
                         YSTYLE=1, $  
                         YTITLE = 'V [m s!U-1!N]', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 1.4;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down_p,up_p], $
                         YSTYLE=1, $  
                         YTITLE = 'P [nPa]', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.4, $
                         CHARTHICK=1.5                               
;###############################################################################                                  
;############################################################################### 
     up_diono=max(new_idiff)
     down_diono=min(new_idiff)          
     PLOT, time, new_idiff, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = 0.6, CHARTHICK=chr_thick1, $
     POSITION=[0.55,0.3,0.95,0.49], XSTYLE = 5, XRANGE=[0, file_number], ySTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down_diono,up_diono], /NOERASE,$
     THICK=2, /NODATA   

    OPLOT, time, new_idiff, THICK=1, LINESTYLE=0, COLOR=negro     
    OPLOT, time[SG_beg*60:SG_end*60], new_idiff[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=negro 
     
    OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro  

    ppi = TexToIDL('D_{I}')        
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='', $                         
                         XMINOR=8, $
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 1.6, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$                     
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $
                         YTITLE = ppi+' [nT]', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $      
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.6                
;###############################################################################                
    IF max(new_ddyn) GT max(new_dp2) THEN up = max(new_ddyn) ELSE up = max(new_dp2)
    IF min(new_ddyn) LT min(new_dp2) THEN down = min(new_ddyn) ELSE down = min(new_dp2)
;###############################################################################
   ; IF upddyn GT updp2 THEN up = upddyn ELSE up=updp2 
    ;IF downddyn LT downdp2 THEN down = downddyn ELSE down=downdp2 
                               
     PLOT, time, new_ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.55,0.1,0.95,0.29], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA
    
    OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro

    OPLOT, time, new_ddyn, COLOR=negro, LINESTYLE=0, THICK=1
    OPLOT, time[SG_beg*60:SG_end*60], new_ddyn[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=negro    
  
;###############################################################################     
    OPLOT, time, new_dp2, COLOR=rojo, THICK=1
    OPLOT, time[SG_beg*60:SG_end*60], new_dp2[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=rojo       
    
    OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0], LINESTYLE=1, THICK=4,COLOR=negro
;###############################################################################
    med_dp2 = MEDIAN(new_dp2)     
;###############################################################################

        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 1.6, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6
                         
                                                 
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 0.8, $                                                
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, yrange=[down,up], $ 
                         ystyle=2, $  
                         YTITLE = 'DP2 & Ddyn [nT]', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=2, $ 
                         COLOR=negro, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.6                                                      
;###############################################################################      

   XYOuts, 0.93, 0.85, '(a)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.4, CHARTHICK= 3   
   
   XYOuts, 0.93, 0.65, '(b)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.4, CHARTHICK= 3  
   
   XYOuts, 0.93, 0.45, '(c)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.4, CHARTHICK= 3  
   
   XYOuts, 0.11, 0.14, '(d)', /Normal, $
   color=negro, Alignment=0.5, Charsize=2.4, CHARTHICK= 3   
   
   XYOuts, 0.93, 0.25, '(e)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.4, CHARTHICK= 3      
;###############################################################################   
 ;  y = (0.66 - 0.34) / 2. + 0.34 
 ;  XYOUTS, 0.51, y, 'DP2 and Ddyn [nT]', /NORMAL, $
 ;  COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5   


 ;  y = (0.9 - 0.73) / 2. + 0.73 
 ;  XYOUTS, 0.51, y, ppi+' [nT]', /NORMAL, $
 ;  COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5                
;###############################################################################                      
;second panel legend
     ;   POLYFILL, [0.88,0.91,0.91,0.88], [0.405,0.405,0.407,0.407], color = rojo, /NORMAL
    ;    POLYFILL, [0.88,0.91,0.91,0.88], [0.375,0.375,0.377,0.377], color = negro, /NORMAL        

  ;      XYOUTS, 0.91, 0.4 , /NORMAL, $
  ;              ' DP2', COLOR=negro, $
   ;             CHARSIZE = chr_size1, $
    ;            CHARTHICK=chr_thick1 

     ;   XYOUTS, 0.91, 0.37 , /NORMAL, $
      ;          ' Ddyn', COLOR=negro, $
       ;         CHARSIZE = chr_size1, $
        ;        CHARTHICK=chr_thick1                                    
;###############################################################################
; saving png
;###############################################################################     
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak2  
    path = '../rutidl/output/article1events/diono_ev/'

    WRITE_PNG, path+'iono_resp_'+Date+'.png', Image, R_bak, G_bak, B_bak

    print, '        Saving: '+path+'iono_resp_'+Date+'.png'
    print, ''
        RETURN 	
END	
