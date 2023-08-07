;Name:
;	iono_respPWS.pro
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

PRO iono_resp_pws, date_i, date_f, JPEG = jpeg 

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
	@set_up_commons
	set_up
        
	station_idx = ''
	PRINT, 'Enter GMS idx: 0:coe, 1:teo, 2:tuc, 3:bsl, 4:itu'
	READ, station_idx, PROMPT = '> '
	
    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu		
; Generate the time series variables 
; define H variables                  
 ;   dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    ;dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    H   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx, 'min')
    sym = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'sym')
; define K variables   
  ;  kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
  ;  k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
  ;  k_mex   = add_nan(k_mex, 9.0, 'greater') 
  ;  k_days  = FINDGEN(file_number*8)/8. 
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
   ; PRINT, N_ELEMENTS(H), N_ELEMENTS(Bsq)
;###############################################################################
    new_dstdays = findgen(file_number*1440)/1440.0 ;se genera un arreglo de tiempo con 
;    muestreo cada 15 min. para mejorar la resolución de las gráficas    
    
;    new_dst = FLTARR(N_ELEMENTS(new_dstdays))     	    
;    tmp_dst  = INTERPOL(dst, N_ELEMENTS(new_dstdays))
;    new_dst = tmp_dst 
    
;    new_H = FLTARR(N_ELEMENTS(new_dstdays))     	    
;    tmp_H  = INTERPOL(H, N_ELEMENTS(new_dstdays))
;    new_H = tmp_H       
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
    dionstr = gen_diono(sym, H, Bsq, 28.06, 'm', TGM_n, DIG_FILTER = 'dig_filter')
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
 ;   new_idiff = FLTARR(N_ELEMENTS(new_dstdays))     	    
  ;  tmp_idiff  = INTERPOL(i_diff, N_ELEMENTS(new_dstdays))
  ;  new_idiff = tmp_idiff      
;###############################################################################      
  ;  new_ddyn = FLTARR(N_ELEMENTS(new_dstdays))     	    
  ;  tmp_ddyn  = INTERPOL(ddyn, N_ELEMENTS(new_dstdays))
  ;  new_ddyn = tmp_ddyn           
;############################################################################### 
   ; new_dp2 = FLTARR(N_ELEMENTS(new_dstdays))     	    
   ; tmp_dp2  = INTERPOL(dp2, N_ELEMENTS(new_dstdays))
   ; new_dp2 = tmp_dp2               
;###############################################################################
; define device and color parameters 
;###############################################################################      
        Device_bak2 = !D.Name         
        SET_PLOT, 'Z'      
        
        Xsize=fix(800)
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

;###############################################################################
    med_ddyn = MEDIAN(ddyn)
   ; std_ddyn = STDDEV(new_ddyn, /NAN)
    
    IQR = PERCENTILES(ddyn, CONFLIMIT=0.5) 
    IQR_n = (IQR[1]-IQR[0])*1
    
    ddyn_out = WHERE(ddyn GE med_ddyn+IQR_n OR ddyn LE med_ddyn-IQR_n)
    ddyn_in  = WHERE(ddyn LE med_ddyn+IQR_n AND ddyn GE med_ddyn-IQR_n)
    
    ddyn_diff_out = ddyn
    ddyn_diff_out[ddyn_in]=!Values.F_NAN
    
    ddyn_diff_in  = ddyn
    ddyn_diff_in[ddyn_out]=!Values.F_NAN    
;###############################################################################    
;###############################################################################   
     dH = TeXtoIDL('\DeltaH') 
;###############################################################################      
 
    sym_min = MIN(sym,i)    
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
        21  :   t0 = 19
        22  :   t0 = 19
        23  :   t0 = 19
        ELSE: PRINT, 'evento no disponible'   
    ENDCASE
    tf = 0
    CASE TGM_n of
        1   :   tf = 0
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
        17  :   tf = 0
        18  :   tf = 19
        19  :   tf = 34
        20  :   tf = 22  
        21  :   t0 = 19
        22  :   t0 = 19
        23  :   t0 = 19              
        ELSE: PRINT, 'evento no disponible'   
    ENDCASE

    SG_beg = i-t0
    IF tf NE 0 THEN BEGIN
    SG_end = i+tf
    ENDIF ELSE BEGIN
    SG_end = N_ELEMENTS(H)-10
    ENDELSE
                                  
;############################################################################### 
     up_diono=max(diono)
     down_diono=min(diono)          
     PLOT, time, diono, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = 0.6, CHARTHICK=chr_thick1, $
     POSITION=[0.15,0.8,0.85,0.95], XSTYLE = 5, XRANGE=[0, file_number], ySTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down_diono,up_diono], THICK=2, /NODATA   

    OPLOT, time, diono, THICK=1, LINESTYLE=0, COLOR=negro     
    ;OPLOT, time[SG_beg*60:SG_end*60], new_idiff[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=negro 
     
   ; OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
   ; [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
   ; THICK=2, COLOR=negro
    
  ;  OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
  ;  [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
   ; THICK=2, COLOR=negro    
         
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='', $                         
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
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.6,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $      
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.6,$
                         CHARTHICK=1.6                  
;###############################################################################                
;###############################################################################               
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0)]
    
    i = WHERE(f_k GE freqs[0])
    fny=WHERE(f_k EQ fn)
    
    ;PRINT, MIN(i), MIN(f_k), freqs[0]
    ysup = MAX(pws[MIN(i):fny])+1
    yinf = MIN(pws[MIN(i):fny]);-0.0001
               
    periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0]
    
    PLOT, f_k, pws, /XLOG, /YLOG, XRANGE = [freqs[0], fn], POSITION=[0.3,0.35,0.7,0.65],$
    YRANGE=[yinf, ysup], BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=2, /NOERASE, /NODATA

    passband_l = freq_band(TGM_n, 'passband_l')
    passband_u = freq_band(TGM_n, 'passband_u')
    highpass_l = freq_band(TGM_n, 'highpass_l')
    
    POLYFILL, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=amarillo

    POLYFILL, [highpass_l, (1.0/(0.5*3600.0)), (1.0/(0.5*3600.0)), highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR=amarillo
    OPLOT, f_k, pws, COLOR=negro, THICK=5   
;###############################################################################    
        AXIS, XAXIS = 0, XRANGE=[freqs[0], fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frequence [Hz]',$
                         COLOR=negro, $
                         CHARSIZE = 1.6, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[0], fn], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(FIX(periods), FORMAT='(I4)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.6,$
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                         COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.6,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                         COLOR=negro, $
                         /ylog,$
                         ystyle=1, $
                         CHARSIZE = 1.6,$
                         CHARTHICK=1.5

   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.69, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, CHARTHICK=1.5   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.15, y, 'Spectral component [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=1.5     
;###############################################################################
    IF max(ddyn) GT max(dp2) THEN up = max(ddyn) ELSE up = max(dp2)
    IF min(ddyn) LT min(dp2) THEN down = min(ddyn) ELSE down = min(dp2)
;###############################################################################                            
     PLOT, time, ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.15,0.1,0.85,0.25], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA             
;###############################################################################
 ;   OPLOT, [time[SG_beg*60],time[SG_beg*60]], $ ;referencia para el inicio de la Tormenta
 ;   [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
  ;  THICK=2, COLOR=negro
    
   ; OPLOT, [time[SG_end*60],time[SG_end*60]], $ ;referencia para el final de la Tormenta
   ; [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
   ; THICK=2, COLOR=negro

    OPLOT, time, ddyn, COLOR=negro, LINESTYLE=0, THICK=1
 ;   OPLOT, time[SG_beg*60:SG_end*60], new_ddyn[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=negro    
  
;###############################################################################     
    OPLOT, time, dp2, COLOR=rojo, THICK=1
 ;   OPLOT, time[SG_beg*60:SG_end*60], new_dp2[SG_beg*60:SG_end*60], THICK=3, LINESTYLE=0, COLOR=rojo       
    
    OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0], LINESTYLE=1, THICK=4,COLOR=negro            
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
                         YTITLE = '', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.6,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=2, $ 
                         COLOR=negro, $
                         CHARSIZE = 1.6,$
                         CHARTHICK=1.6                                                             
;###############################################################################         
;###############################################################################   
    y = (0.25 - 0.10) / 2. + 0.10 
    XYOUTS, 0.04, y, 'DP2 and Ddyn [nT]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=1.8   

    ppi = TexToIDL('D_{I}')
    y = (0.95 - 0.80) / 2. + 0.8
    XYOUTS, 0.04, y, ppi+' [nT]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=1.8   
             
;###############################################################################                      
;second panel legend       
                               
;###############################################################################
; saving png
;###############################################################################     
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak2  
    path = '/home/isaac/geomstorm/rutidl/output/article2/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = R_bak[image]
                true_image[1,*,*] = G_bak[image]
                true_image[2,*,*] = B_bak[image]
                write_jpeg, path+'iono_resp_V3_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN PRINT, '        Setting PNG as default file type.'
                WRITE_PNG, path+'iono_resp_V3_'+Date+'.png', Image, R_bak, G_bak, B_bak
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'iono_resp_V3_'+Date+'.png'
                print, ''
        ENDIF
        RETURN 	
END	
