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
;   iono_resp, idate, fdate, PNG = 'png', PS = 'ps'
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
;   imported to /output/eventos_tgm/iono_resp_V1_yyyy-mm-dd.png
;   .PS figure
;   imported to /output/eventos_tgm/iono_resp_V2_yyyy-mm-dd.ps
;version
;   apr, 2023
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;

PRO iono_respV3, date_i, date_f, PNG = png, PS = ps

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
    time= findgen(file_number*1440)/1440.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################
; Generate the time series variables 
; define H variables                  
    dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    H   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    sym = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'sym')
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
    k_mex   = add_nan(k_mex, 9.0, 'greater') 
    k_days  = FINDGEN(file_number*8)/8. 
; define Bsq 
    Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])    

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
   ; PRINT, N_ELEMENTS(H), N_ELEMENTS(Bsq)
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
   ; tec_days= findgen(file_number*12)/12.0                         
   ; tec_diff = tec-med  
 
 ;   new_tecdays = findgen(file_number*1440)/1440.0 ;se genera un arreglo de tiempo con 
;    muestreo cada 15 min. para mejorar la resolución de las gráficas    
    
 ;   new_tecdiff = FLTARR(N_ELEMENTS(new_tecdays))     	    
 ;   tmp_tecdif  = INTERPOL(tec_diff, N_ELEMENTS(new_tecdays))
 ;   new_tecdiff = tmp_tecdif
;###############################################################################
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, Bsq, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')
    ;PRINT, Bsq
; compute frequencies 
    f_k   = dionstr.f_k
    fn    = dionstr.fn

; compute and define Power Spectrum
    pws = dionstr.pws

; compute diono variables    
    diono = dionstr.diono
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn
    ;print, dp2
;############################################################################### 
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

    SG_beg = i-t0
    IF tf NE 0 THEN BEGIN
    SG_end = i+tf
    ENDIF ELSE BEGIN
    SG_end = N_ELEMENTS(dH)-10
    ENDELSE 
;###############################################################################
    med_ddyn = MEDIAN(new_ddyn)
   ; std_ddyn = STDDEV(new_ddyn, /NAN)
    
    IQR = PERCENTILES(ddyn, CONFLIMIT=0.5) 
    IQR_n = (IQR[1]-IQR[0])*1
    
    ddyn_out = WHERE(new_ddyn GE med_ddyn+IQR_n OR new_ddyn LE med_ddyn-IQR_n)
    ddyn_in  = WHERE(new_ddyn LE med_ddyn+IQR_n AND new_ddyn GE med_ddyn-IQR_n)
    
    ddyn_diff_out = new_ddyn
    ddyn_diff_out[ddyn_in]=!Values.F_NAN
    
    ddyn_diff_in  = new_ddyn
    ddyn_diff_in[ddyn_out]=!Values.F_NAN    
;###############################################################################
    med_dp2 = MEDIAN(dp2)    
;###############################################################################   
     dH = TeXtoIDL('\DeltaH')                                   
;############################################################################### 
    IF keyword_set(png) THEN BEGIN
        
    make_pngfig, new_idiff, time, new_ddyn, new_dp2, SG_beg, SG_end, $
        [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]               
    ENDIF 
    
    IF keyword_set(ps) THEN BEGIN    
        make_psfig, new_idiff, time, new_ddyn, new_dp2, SG_beg, SG_end, $
        [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]
    ENDIF
    RETURN

END

PRO make_psfig, new_idiff, time, new_ddyn, new_dp2, SG_beg, SG_end,  date_i, date_f
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')      	
    LOADCT, 39;, /SILENT
    TGM_n = event_case([yr_i,mh_i,dy_i])  
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')
    path = '../rutidl/output/article1events/diono_ev/'
    psfile =  path+'iono_PI_V2_'+Date+'.ps'    
;############################################################################### 
    time_title = ' UT [days]'
    window_title = 'SGS-'+ STRING(TGM_n, FORMAT='(I02)') 
;###############################################################################        
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=3., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10
    
     up_diono=max(new_idiff)
     down_diono=min(new_idiff)          
     cgPLOT, time, new_idiff, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR='black', CHARSIZE = 0.6, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.52,0.9,0.91], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down_diono,up_diono],$
     THICK=2, /NODATA   

    ;POLYFILL, [new_dstdays[ddyn_i+ddyn_si], new_dstdays[ddyn_i+spam_f+ddyn_sf],$
    ;          new_dstdays[ddyn_i+spam_f+ddyn_sf], new_dstdays[ddyn_i+ddyn_si]], $
    ;          [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
    ;          COLOR=amarillo   
    
    cgOPLOT, time, new_idiff, THICK=1, LINESTYLE=0, COLOR='black'
     
    cgOPLOT, time[SG_beg*60:SG_end*60], new_idiff[SG_beg*60:SG_end*60], THICK=5, LINESTYLE=0, COLOR='black'
    
  ;  OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0], LINESTYLE=1, THICK=4,COLOR=negro 
          
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
                         COLOR=negro, $
                         CHARSIZE = 3.0 , $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5 
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$                     
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=2.0

        AXIS, YAXIS = 0, YRANGE=[down_diono,up_diono], $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, YRANGE=[down_diono,up_diono], $      
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                  
;###############################################################################                
    IF MAX(new_ddyn) GT MAX(new_dp2) THEN up = MAX(new_ddyn) ELSE up = MAX(new_dp2)
    IF MIN(new_ddyn) LT MIN(new_dp2) THEN down = MIN(new_ddyn) ELSE down = MIN(new_dp2)
;###############################################################################
                               
     cgPLOT, time, new_ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR='black', CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.13,0.9,0.51], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA             
;###############################################################################
    cgOPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
    
    cgOPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'

    cgOPLOT, time, new_ddyn, COLOR='black', LINESTYLE=0, THICK=1
    cgOPLOT, time[SG_beg*60:SG_end*60], new_ddyn[SG_beg*60:SG_end*60], THICK=5, LINESTYLE=0, COLOR='black'    
;###############################################################################
   ; std_dp2 = stddev(dp2, /NAN)
    
    
   ; dp2_diff_out = dp2
   ; dp2_diff_out[dp2_in]=!Values.F_NAN
    
   ; dp2_diff_in  = dp2
   ; dp2_diff_in[dp2_out]=!Values.F_NAN     
;###############################################################################  
;###############################################################################     
    cgOPLOT, time, new_dp2, COLOR='red', THICK=1
    cgOPLOT, time[SG_beg*60:SG_end*60], new_dp2[SG_beg*60:SG_end*60], THICK=5, LINESTYLE=0, COLOR='red'       
    
    cgOPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0], LINESTYLE=1, THICK=4,COLOR='black'            
;############################################################################### 
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         CHARSIZE = 3.0 , $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5 
                         
                                                 
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 3.0, $                                                
                         COLOR=black, $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5

        AXIS, YAXIS = 0, yrange=[down,up], $ 
                         ystyle=2, $  
                         YTITLE = '', $                          
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=2, $ 
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                                                           
;###############################################################################      
   x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, COLOR=negro, ALIGNMENT=0.5, CHARSIZE=3.0, CHARTHICK=3.5    
;###############################################################################   
    y = (0.45 - 0.20) / 2. + 0.20 
    XYOUTS, 0.02, y, 'DP2 & Ddyn [nT]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=3.5  

    ppi = TexToIDL('D_{I}')
    y = (0.80 - 0.55) / 2. + 0.55
    XYOUTS, 0.02, y, ppi+' [nT]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=3.5                                              
;###############################################################################
; saving png
    cgPS_Close, density = 300, width = 1000   
    RETURN
END

PRO make_pngfig, new_idiff, time, new_ddyn, new_dp2, SG_beg, SG_end,  date_i, date_f
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
;###############################################################################
    med_ddyn = MEDIAN(new_ddyn)
   ; std_ddyn = STDDEV(new_ddyn, /NAN)
    
    IQR = PERCENTILES(new_ddyn, CONFLIMIT=0.5) 
    IQR_n = (IQR[1]-IQR[0])*1
    
  ;  ddyn_out = WHERE(new_ddyn GE med_ddyn+IQR_n OR new_ddyn LE med_ddyn-IQR_n)
  ;  ddyn_in  = WHERE(new_ddyn LE med_ddyn+IQR_n AND new_ddyn GE med_ddyn-IQR_n)
    
  ;  ddyn_diff_out = new_ddyn
  ;  ddyn_diff_out[ddyn_in]=!Values.F_NAN
    
  ;  ddyn_diff_in  = new_ddyn
  ;  ddyn_diff_in[ddyn_out]=!Values.F_NAN    
;###############################################################################
  ;  med_dp2 = MEDIAN(new_dp2)    
;###############################################################################   
     dH = TeXtoIDL('\DeltaH')                                   
;############################################################################### 
;IF keyword_set(png) THEN BEGIN
     
        Device_bak2 = !D.Name         
        SET_PLOT, 'Z'      
        
        Xsize=fix(1600)
        Ysize=1000
        DEVICE, SET_RESOLUTION = [Xsize,Ysize],Set_Pixel_Depth=24, DECOMPOSED=1  
        DEVICE, z_buffer=4
        DEVICE, set_character_size = [10, 12] 

;###############################################################################
; define device and color parameters         
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

     up_diono=max(new_idiff)
     down_diono=min(new_idiff)          
     PLOT, time, new_idiff, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = 0.6, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.52,0.9,0.9], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down_diono,up_diono],$
     THICK=2, /NODATA     
    
    OPLOT, time, new_idiff, THICK=1, LINESTYLE=0, COLOR=negro
     
    OPLOT, time[SG_beg*60:SG_end*60], new_idiff[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=negro
    
  ;  OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0], LINESTYLE=1, THICK=4,COLOR=negro 
          
    OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro    
         
        AXIS, XAXIS = 0, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 3.0 , $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5 
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$                     
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=2.0

        AXIS, YAXIS = 0, YRANGE=[down_diono,up_diono], $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, YRANGE=[down_diono,up_diono], $      
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                  
;###############################################################################                
    IF max(new_ddyn) GT max(new_dp2) THEN up = max(new_ddyn) ELSE up = max(new_dp2)
    IF min(new_ddyn) LT min(new_dp2) THEN down = min(new_ddyn) ELSE down = min(new_dp2)
;###############################################################################
                               
     PLOT, time, new_ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.12,0.9,0.5], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA             
;###############################################################################
    OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro

    OPLOT, time, new_ddyn, COLOR=negro, LINESTYLE=0, THICK=1
    OPLOT, time[SG_beg*60:SG_end*60], new_ddyn[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=negro    
;###############################################################################
   ; std_dp2 = stddev(dp2, /NAN)
    
    
   ; dp2_diff_out = dp2
   ; dp2_diff_out[dp2_in]=!Values.F_NAN
    
   ; dp2_diff_in  = dp2
   ; dp2_diff_in[dp2_out]=!Values.F_NAN     
;###############################################################################  
;###############################################################################     
    OPLOT, time, new_dp2, COLOR=rojo, THICK=1
    OPLOT, time[SG_beg*60:SG_end*60], new_dp2[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=rojo       
    
    OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0], LINESTYLE=1, THICK=4,COLOR=negro            
;############################################################################### 
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 3.0 , $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5 
                         
                                                 
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 3.0, $                                                
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5

        AXIS, YAXIS = 0, yrange=[down,up], $ 
                         ystyle=2, $  
                         YTITLE = '', $                          
                         COLOR=negro, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=2, $ 
                         COLOR=negro, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                                                           
;###############################################################################      
   x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, COLOR=negro, ALIGNMENT=0.5, CHARSIZE=3.0, CHARTHICK=3.5    
;###############################################################################   
    y = (0.45 - 0.20) / 2. + 0.20 
    XYOUTS, 0.02, y, 'DP2 & Ddyn [nT]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=3.5  

    ppi = TexToIDL('D_{I}')
    y = (0.80 - 0.55) / 2. + 0.55
    XYOUTS, 0.02, y, ppi+' [nT]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=3.5                                              
;###############################################################################
; saving png
;###############################################################################     
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
        
    SET_PLOT, Device_bak2  
    path = '../rutidl/output/article1events/diono_ev/'
    PRINT, '        Setting PNG as default file type.'
                WRITE_PNG, path+'iono_PI_V1_'+Date+'.png', Image, R_bak, G_bak, B_bak


    PRINT, '        Saving: '+path+'iono_PI_V1_'+Date+'.png'
    PRINT, ''
RETURN
END
