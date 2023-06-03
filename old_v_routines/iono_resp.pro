;Name:
;	iono_resp.pro
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

PRO iono_resp, date_i, date_f, JPEG = jpeg 

	On_error, 2
	COMPILE_OPT idl2, HIDDEN

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
    dst_days= findgen(file_number*24)/24.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################
; Generate the time series variables 
; define H variables                  
    dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    H   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
    k_mex   = add_nan(k_mex, 9.0, 'greater') 
    k_days  = FINDGEN(file_number*8)/8. 
; define Bsq 
    Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])    

; Generate the time variables to plot TEC time series         
    tec  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'tec')
    med  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'med')

;###############################################################################
;identifying NAN percentage values in the Time Series
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 100.0, 'greater')    
;Identifying the NAN values        
    tec = add_nan(tec, 999.0, 'equal')            
    med = add_nan(med, 999.0, 'equal')                
    H = add_nan(H, 999999.0, 'equal')
    H = add_nan(H, 99999.0, 'equal')                   
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
    H = fillnan(H)
;###############################################################################      
    tec_days= findgen(file_number*12)/12.0                         
    tec_diff = tec-med  
 
    new_tecdays = findgen(file_number*1440)/1440.0 ;se genera un arreglo de tiempo con 
;    muestreo cada 15 min. para mejorar la resolución de las gráficas    
    
    new_tecdiff = FLTARR(N_ELEMENTS(new_tecdays))     	    
    tmp_tecdif  = INTERPOL(tec_diff, N_ELEMENTS(new_tecdays))
    new_tecdiff = tmp_tecdif
;###############################################################################
    new_dstdays = findgen(file_number*1440)/1440.0 ;se genera un arreglo de tiempo con 
;    muestreo cada 15 min. para mejorar la resolución de las gráficas    
    
    new_dst = FLTARR(N_ELEMENTS(new_dstdays))     	    
    tmp_dst  = INTERPOL(dst, N_ELEMENTS(new_dstdays))
    new_dst = tmp_dst 
    
    new_H = FLTARR(N_ELEMENTS(new_dstdays))     	    
    tmp_H  = INTERPOL(H, N_ELEMENTS(new_dstdays))
    new_H = tmp_H     
;###############################################################################
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, Bsq, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')
    
; compute frequencies 
    f_k   = dionstr.f_k
    fn    = dionstr.fn

; compute and define Power Spectrum
    pws = dionstr.pws

; compute diono variables    
    diono = dionstr.diono
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn
;###############################################################################
    i_diff = diono
    new_idiff = FLTARR(N_ELEMENTS(new_dstdays))     	    
    tmp_idiff  = INTERPOL(i_diff, N_ELEMENTS(new_dstdays))
    new_idiff = tmp_idiff      
;###############################################################################      
    new_ddyn = FLTARR(N_ELEMENTS(new_dstdays))     	    
    tmp_ddyn  = INTERPOL(ddyn, N_ELEMENTS(new_dstdays))
    new_ddyn = tmp_ddyn           
;############################################################################### 
    new_dp2 = FLTARR(N_ELEMENTS(new_dstdays))     	    
    tmp_dp2  = INTERPOL(dp2, N_ELEMENTS(new_dstdays))
    new_dp2 = tmp_dp2         
;###############################################################################
; define device and color parameters 
;###############################################################################      
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
        spam_i = idate0
    case spam_i of
        '200311' : spam_i = 1000
        '200411' : spam_i = 50
        '200505' : spam_i = 0
        '201503' : spam_i = 0
        '201705' : spam_i = 0
        '201709' : spam_i = 0
        else: print, 'fuera de rango'
    endcase 

        spam_f = idate0
    case spam_f of
        '200311' : spam_f = 2800
        '200411' : spam_f = 4400
        '200505' : spam_f = 0
        '201503' : spam_f = 0
        '201705' : spam_f = 0
        '201709' : spam_f = 0
        else: print, 'fuera de rango'
    endcase    
;############################################################################### 
       days = intarr(file_number+1)
       for i=0, n_elements(days)-1 do begin
            days[i] = dy_i+i
       endfor
       days = days*24/24. 
       day_time = findgen(24)   
;############################################################################### 
    time_title = ' UT ['+textoidl("days")+' of '+old_month+'].'
    window_title = 'SGS'+ STRING(TGM_n, FORMAT='(I01)')+', '+ $
                    STRING(old_month, yr_i, FORMAT='(A, X, I4)')
    
    periodo = 'Period [h]'        
;###############################################################################               
    PLOT, f_k, pws, /XLOG, /YLOG, POSITION=[0.07,0.1,0.95,0.9],$
    BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA    

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5               
;###############################################################################           
    ysup = MAX(pws)+10
    yinf = MIN(pws)-0.0001
    
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0)]
               
    periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0]
    
    PLOT, f_k, pws, /XLOG, /YLOG, XRANGE = [freqs[0], fn], POSITION=[0.07,0.1,0.45,0.9],$
    YRANGE=[yinf, ysup], BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA,$
    /NOERASE

    passband_l = freq_band(TGM_n, 'passband_l')
    passband_u = freq_band(TGM_n, 'passband_u')
    highpass_l = freq_band(TGM_n, 'highpass_l')
    POLYFILL, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=amarillo

    POLYFILL, [highpass_l, fn ,fn, highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=amarillo
    OPLOT, f_k, pws, COLOR=negro, THICK=5    
;###############################################################################    
        AXIS, XAXIS = 0, XRANGE=[freqs[0], fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                         COLOR=negro, $
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], fn], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(F4.1)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.2,$
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                         COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                         COLOR=negro, $
                         /ylog,$
                         ystyle=1, $
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5

   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.928, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.02, y, 'Spectral component [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5     
;###############################################################################
    med_ddyn = MEDIAN(new_ddyn)
    std_ddyn = STDDEV(new_ddyn, /NAN)
    
    ddyn_out = WHERE(new_ddyn GE med_ddyn+std_ddyn OR new_ddyn LE med_ddyn-std_ddyn)
    ddyn_in  = WHERE(new_ddyn LE med_ddyn+std_ddyn AND new_ddyn GE med_ddyn-std_ddyn)
    
    ddyn_diff_out = new_ddyn
    ddyn_diff_out[ddyn_in]=!Values.F_NAN
    
    ddyn_diff_in  = new_ddyn
    ddyn_diff_in[ddyn_out]=!Values.F_NAN
 
     upddyn     = max(ddyn)
     downddyn   = min(ddyn)
     
     updp2     = max(dp2)
     downdp2   = min(dp2)     
;###############################################################################
    ddyn_i = idate0
CASE ddyn_i of
    '200311' : ddyn_i = ddyn_out[0]
    '200411' : ddyn_i = ddyn_out[0]
    '200505' : ddyn_i = ddyn_out[500]
    '201503' : ddyn_i = ddyn_out[400]
    '201705' : ddyn_i = ddyn_out[0]
    '201709' : ddyn_i = ddyn_out[400]
    ELSE: PRINT, 'fuera de rango'
ENDCASE 

    ddyn_si = idate0
CASE ddyn_si of
    '200311' : ddyn_si = -200
    '200411' : ddyn_si = -130
    '200505' : ddyn_si = -160
    '201503' : ddyn_si = -650
    '201705' : ddyn_si = -130
    '201709' : ddyn_si = -350
    ELSE: PRINT, 'fuera de rango'
ENDCASE 

    ddyn_sf = idate0
CASE ddyn_sf of
    '200311' : ddyn_sf = -800
    '200411' : ddyn_sf = 600
    '200505' : ddyn_sf = 5370
    '201503' : ddyn_sf = 4780
    '201705' : ddyn_sf = 1800
    '201709' : ddyn_sf = 4170
    ELSE: PRINT, 'fuera de rango'
ENDCASE       
;###############################################################################   
     dH = TeXtoIDL('\DeltaH') 
;###############################################################################      
    med_tec = MEDIAN(new_tecdiff)
    std_tec = stddev(new_tecdiff)
    
    index_out = WHERE(new_tecdiff GE med_tec+std_tec OR new_tecdiff LE med_tec-std_tec)
    index_in  = WHERE(new_tecdiff LE med_tec+std_tec AND new_tecdiff GE med_tec-std_tec)
    tec_diff_out = new_tecdiff
    tec_diff_out[index_in]=!Values.F_NAN
    
    tec_diff_in  = new_tecdiff
    tec_diff_in[index_out]=!Values.F_NAN

    sup = med_tec+std_tec
    inf = med_tec-std_tec
    
    l_sup = fltarr(n_elements(tec_diff))
    l_sup[*] = sup

    l_inf = fltarr(n_elements(tec_diff))
    l_inf[*] = inf  
     
    up_tecdiff      = MAX(tec_diff)
    down_tecdiff    = MIN(tec_diff) 
    PLOT, tec_days, tec_diff, XTICKS=file_number, xminor=8, BACKGROUND = blanco,$ 
     CHARSIZE = chr_size1, CHARTHICK=chr_thick1, POSITION=[0.55,0.1,0.95,0.27], $
     XSTYLE = 5, XRANGE=[0, file_number], XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 6,$
     /NOERASE, YRANGE=[down_tecdiff, up_tecdiff], /NODATA
    
    ;POLYFILL, [new_dstdays[ddyn_i+ddyn_si], new_dstdays[ddyn_i+spam_f+ddyn_sf],$
    ;          new_dstdays[ddyn_i+spam_f+ddyn_sf], new_dstdays[ddyn_i+ddyn_si]], $
    ;          [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
    ;          COLOR=amarillo  

     OPLOT, tec_days, tec_diff, LINESTYLE=0, THICK=4, COLOR=morado  
             
     OPLOT, tec_days, l_sup, LINESTYLE=2, THICK=2, COLOR=morado
     OPLOT, tec_days, l_inf, LINESTYLE=2, THICK=2, COLOR=morado
                        
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                         
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-0.25), $
                         XTICKS=file_number, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $                                            
                         XMINOR=8, $ 
                         CHARSIZE = 0.9, $                       
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5

        AXIS, YAXIS = 0, YRANGE=[down_tecdiff, up_tecdiff], $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, YRANGE=[down_tecdiff, up_tecdiff], $
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5                                       
;############################################################################### 
     up_diono=max(diono)
     down_diono=min(diono)          
     PLOT, dst_days, diono, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = 0.6, CHARTHICK=chr_thick1, $
     POSITION=[0.55,0.73,0.95,0.9], XSTYLE = 5, XRANGE=[0, file_number], ySTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down_diono,up_diono], /NOERASE,$
     THICK=4, /NODATA   

    ;POLYFILL, [new_dstdays[ddyn_i+ddyn_si], new_dstdays[ddyn_i+spam_f+ddyn_sf],$
    ;          new_dstdays[ddyn_i+spam_f+ddyn_sf], new_dstdays[ddyn_i+ddyn_si]], $
    ;          [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
    ;          COLOR=amarillo   
     
     OPLOT, dst_days, diono, THICK=4, LINESTYLE=0, COLOR=negro   
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                         
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-0.25), $
                         XTICKS=file_number, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $                                            
                         XMINOR=8, $ 
                         CHARSIZE = 0.9, $                       
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
    ppi = TexToIDL('D_{I}')
        AXIS, YAXIS = 0, YRANGE=[down_diono,up_diono], $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.1,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, YRANGE=[down_diono,up_diono], $      
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.1,$
                         CHARTHICK=1.5                   
;###############################################################################                
    if max(ddyn) gt max(dp2) then up = max(ddyn) else up = max(dp2)
    if min(ddyn) lt min(dp2) then down = min(ddyn) else down = min(dp2)
;###############################################################################
    IF upddyn GT updp2 THEN up = upddyn ELSE up=updp2 
    IF downddyn LT downdp2 THEN down = downddyn ELSE down=downdp2 
                               
     PLOT, dst_days, ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.55,0.34,0.95,0.66], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA
    
      ;  OPLOT, new_dstdays[ddyn_i+ddyn_si:ddyn_i+spam_f+ddyn_sf], $
      ;  ddyn_diff_out[ddyn_i+ddyn_si:ddyn_i+spam_f+ddyn_sf], $
      ;  COLOR=negro, LINESTYLE=0, THICK=5   
                    
      ;  OPLOT, new_dstdays[ddyn_i+ddyn_si:ddyn_i+spam_f+ddyn_sf], $
      ;  ddyn_diff_in[ddyn_i+ddyn_si:ddyn_i+spam_f+ddyn_sf], $
      ;  COLOR=negro, LINESTYLE=0, THICK=5            
;###############################################################################
        OPLOT, new_dstdays, new_ddyn, COLOR=negro, LINESTYLE=3
       ; OPLOT, new_dstdays, ddyn_diff_out, COLOR=negro, LINESTYLE=0, THICK=5
;###############################################################################
    med_dp2 = MEDIAN(new_dp2)
    std_dp2 = stddev(new_dp2, /NAN)
    
    dp2_out = WHERE(new_dp2 GE med_dp2+std_dp2 OR new_dp2 LE med_dp2-std_dp2)
    dp2_in  = WHERE(new_dp2 LE med_dp2+std_dp2 AND new_dp2 GE med_dp2-std_dp2)
    
    dp2_diff_out = new_dp2
    dp2_diff_out[dp2_in]=!Values.F_NAN
    
    dp2_diff_in  = new_dp2
    dp2_diff_in[dp2_out]=!Values.F_NAN     
;###############################################################################
    dp2_i = idate0
CASE dp2_i of
    '200311' : dp2_i = dp2_out[6]
    '200411' : dp2_i = dp2_out[50]
    '200505' : dp2_i = dp2_out[100]
    '201503' : dp2_i = dp2_out[500]
    '201705' : dp2_i = dp2_out[100]
    '201709' : dp2_i = dp2_out[500]
    ELSE: print, 'fuera de rango'
ENDCASE  

    dp2_si = idate0
CASE dp2_si of
    '200311' : dp2_si = 0
    '200411' : dp2_si = -100
    '200505' : dp2_si = -100
    '201503' : dp2_si = -730
    '201705' : dp2_si = -50
    '201709' : dp2_si = 0
    ELSE: print, 'fuera de rango'
ENDCASE 

    dp2_sf = idate0
CASE dp2_sf of
    '200311' : dp2_sf = 20
    '200411' : dp2_sf = 100
    '200505' : dp2_sf = 350
    '201503' : dp2_sf = 550
    '201705' : dp2_sf = 900
    '201709' : dp2_sf = 990
    ELSE: print, 'fuera de rango'
ENDCASE       
;###############################################################################     
        oplot, dst_days, dp2, color=rojo
        oplot, new_dstdays[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], $
        new_dp2[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], color=rojo, $
        linestyle=0, thick=4      
        
        oplot, new_dstdays[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], $
        new_dp2[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], color=rojo, $
        linestyle=0, thick=4          
;############################################################################### 
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                         
                         IF TGM_n EQ 3 THEN Ltime = 0.25
                         IF TGM_n EQ 5 THEN Ltime = 0.25
                         IF TGM_n EQ 6 THEN Ltime = 0.25
                         IF TGM_n EQ 1 THEN Ltime = 5./24.
                         IF TGM_n EQ 2 THEN Ltime = 5./24.
                         IF TGM_n EQ 4 THEN Ltime = 5./24.                                                  
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-Ltime), $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $  
                         CHARSIZE = 0.8, $                                                
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5

        AXIS, YAXIS = 0, yrange=[down,up], $ 
                         ystyle=2, $  
                         YTITLE = '', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.1,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=2, $ 
                         COLOR=negro, $
                         CHARSIZE = 1.1,$
                         CHARTHICK=1.5      
;###############################################################################    
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.921, 'LT', /Normal, $
   color=negro, Alignment=0.5, Charsize=0.9, CHARTHICK=1.5     

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.679, 'LT', /Normal, $
   color=negro, Alignment=0.5, Charsize=0.9, CHARTHICK=1.5     
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.291, 'LT', /Normal, $
   color=negro, Alignment=0.5, Charsize=0.9, CHARTHICK=1.5   
   

   XYOuts, 0.93, 0.75, '(a)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.4, CHARTHICK= 3   
   
   XYOuts, 0.11, 0.14, '(b)', /Normal, $
   color=negro, Alignment=0.5, Charsize=2.4, CHARTHICK= 3  
   
   XYOuts, 0.93, 0.63, '(c)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.4, CHARTHICK= 3  
   
   XYOuts, 0.93, 0.24, '(d)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.4, CHARTHICK= 3     
;###############################################################################   
   y = (0.66 - 0.34) / 2. + 0.34 
   XYOUTS, 0.51, y, 'DP2 and Ddyn [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5   

   y = (0.9 - 0.73) / 2. + 0.73 
   XYOUTS, 0.51, y, ppi+' [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5   

   y = (0.27 - 0.1) / 2. + 0.1 
   XYOUTS, 0.51, y, 'TEC-<TEC> [TECu]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5                
;###############################################################################                      
;second panel legend
        POLYFILL, [0.88,0.91,0.91,0.88], [0.405,0.405,0.407,0.407], color = rojo, /NORMAL
        POLYFILL, [0.88,0.91,0.91,0.88], [0.375,0.375,0.377,0.377], color = negro, /NORMAL        

        XYOUTS, 0.91, 0.4 , /NORMAL, $
                ' DP2', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1 

        XYOUTS, 0.91, 0.37 , /NORMAL, $
                ' Ddyn', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1                                    
;###############################################################################
; saving png
;###############################################################################     
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak2  
    path = '../rutidl/output/eventos_tgm/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = R_bak[image]
                true_image[1,*,*] = G_bak[image]
                true_image[2,*,*] = B_bak[image]
                write_jpeg, path+'iono_resp_V10_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'iono_resp_V10_'+Date+'.png', Image, R_bak, G_bak, B_bak
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'iono_resp_V9_'+Date+'.png'
                print, ''
        ENDIF
        RETURN 	
end	
