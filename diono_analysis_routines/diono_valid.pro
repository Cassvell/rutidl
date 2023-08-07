;Name:
;	diono_valid
;purpose:
;	plot and print approximation to DH from adding DP2 and Ddyn to Dst/Kp
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
;   .r diono_valid
;   diono_valid, [yyyy, mm, dd], [yyyy, mm, dd]
;parameters:
;   
;
;dependencies:
;
;
;input files
;   Dst, dH, Kp, Kmex, Ap, Amex, Newkmex, Bsq data
;
;output files:
;   geomagnetic index plot with local effects
;   prompt list with comparative analysis
;   .PNG figure imported to /output/eventos_tgm/diono_final_V6_yyyy-mm-dd.png
;
;VERSION
;   Dec, 2022

PRO diono_valid, date_i, date_f, PNG = png, PS=ps 
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

    tot_days= FINDGEN(file_number*24)/24.0  
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')    
;###############################################################################
    idate0 = STRING(yr_i, mh_i, format='(I4,I02)')
    TGM_n  = event_case([yr_i,mh_i,dy_i])
;###############################################################################
	station_idx = ''
	PRINT, 'Enter GMS idx: 0:coe, 1:teo, 2:tuc, 3:bsl, 4:itu'
	READ, station_idx, PROMPT = '> '

    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)] 
; Generate the time series variables 
;Generate the time series

; define Dst and dH variables
    dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, FIX(station_idx))
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    H   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, FIX(station_idx), 'H')
    
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
    k_mex   = add_nan(k_mex, 9.0, 'greater') 
    k_days  = FINDGEN(file_number*8)/8. 
; define Bsq 
    Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx, 'H')
    
; define DK effect
    new_kmex1   = new_kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'new_kmex1')
    new_kmex2   = new_kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'new_kmex2')
     ;PRINT, new_kmex1
     ;PRINT, new_kmex2
     
;###############################################################################        
;identifying NAN percentage values in the Time Series
    dH = nanpc(dH, 999999.0, 'equal')
    dH = nanpc(dH, 100.0, 'greater')
;setting certain values as NaN        
    dH      = add_nan(dH, 999999.0, 'equal')        
    dH      = add_nan(dH, 100.0, 'greater')
    H       = add_nan(H, 99999.0, 'greater')             
    k_mex   = add_nan(k_mex, 9.0, 'greater')     
                       
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo        
    dH = fillnan(dH)       
    H  = fillnan(H)
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
   ; SG_end = N_ELEMENTS(dH)-10
    ENDELSE
;###############################################################################

IF keyword_set(png) THEN BEGIN
    makefig_png, tot_days, dH, dst, kp, k_mex, k_days, Bsq, H, new_kmex1, new_kmex2, $
    [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], SG_beg, SG_end
ENDIF

IF keyword_set(ps) THEN BEGIN
    makefig_ps, tot_days, dH, dst, kp, k_mex, k_days, Bsq, H, new_kmex1, new_kmex2, $
    [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], SG_beg, SG_end
ENDIF

END

PRO makefig_ps, tot_days, dH, dst, kp, k_mex, k_days, Bsq, H, new_kmex1,new_kmex2,$
    date_i, date_f, SG_beg, SG_end
    
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	

    tot_days= FINDGEN(file_number*24)/24.0  
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')    
;###############################################################################
    idate0 = STRING(yr_i, mh_i, format='(I4,I02)')
    LOADCT, 39;, /SILENT
    TGM_n = event_case([yr_i,mh_i,dy_i])  
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')
    path = '../rutidl/output/article2/'
    psfile =  path+'diono_valid_V4_'+Date+'.eps'    
;###############################################################################

;###############################################################################        
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=3., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10


    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')                        ;Function to convert month from mm format to string                           
;############################################################################### 
    time_title = ' UT [days]'
    window_title = 'Event'+ STRING(TGM_n, FORMAT='(I01)')+', '+ $
                    STRING(old_month, yr_i, FORMAT='(A, X, I4)') 

     IF MAX(dH) GT MAX(dst) THEN up = MAX(dH) ELSE up = MAX(dst)
     IF MIN(dH) LT MIN(dst) THEN down=MIN(dH) ELSE down=MIN(dst)
                             
    ; up = MAX(dH)
    ; down=MIN(dH)

     CGPLOT, tot_days, dH, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR='black', CHARSIZE = 0.9, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.53,0.9,0.91], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], THICK=4
;###############################################################################        
     d_H = TeXtoIDL('\DeltaH') 
     dst_l = TexToIDL('Dst_\lambda')

;############################################################################### 
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, Bsq, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')
    
; compute frequencies 
    fn    = dionstr.fn

; compute diono variables    
    diono = dionstr.diono
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn
     
     Bsq_med     = MEDIAN(Bsq)
     Bsq0        = Bsq-Bsq_med
     dp2_effect  = dionstr.p_a+dp2
     ddyn_effect = dionstr.p_a+ddyn
     diono_effect= dionstr.p_a+dp2+ddyn
     diono_effect_tot=dionstr.p_a+diono
     
     CGOPLOT, tot_days, dst, COLOR='green', THICK=4  
     CGOPLOT, tot_days[SG_beg:SG_end], diono_effect[SG_beg:SG_end], COLOR='red', THICK=4         
    ; OPLOT, tot_days, diono_effect_tot, COLOR=azul, THICK=4
    window_title = 'Event-'+ STRING(TGM_n, FORMAT='(I2)')+', '+ $
                STRING(old_month, yr_i, FORMAT='(A, X, I4)')
    
    CGOPLOT, [tot_days[SG_beg],tot_days[SG_beg]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
    
    CGOPLOT, [tot_days[SG_end],tot_days[SG_end]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'

    
     
     dist_func0  = ABS(dH[SG_beg:SG_end]-dst[SG_beg:SG_end])
     tot_diff0   = TOTAL(dist_func0)
     avr_diff0   = tot_diff0 / (N_ELEMENTS(dH[SG_beg:SG_end])) 
     
     dist_func1  = ABS(dH[SG_beg:SG_end]-dp2_effect[SG_beg:SG_end])
     tot_diff1   = TOTAL(dist_func1)
     avr_diff1   = tot_diff1 / (N_ELEMENTS(dH[SG_beg:SG_end]))   

     dist_func2  = ABS(dH[SG_beg:SG_end]-ddyn_effect[SG_beg:SG_end])
     tot_diff2   = TOTAL(dist_func2)
     avr_diff2   = tot_diff2 / (N_ELEMENTS(dH[SG_beg:SG_end]))          

     dist_func3  = ABS(dH[SG_beg:SG_end]-diono_effect[SG_beg:SG_end])
     tot_diff3   = TOTAL(dist_func3)
     avr_diff3   = tot_diff3 / (N_ELEMENTS(dH[SG_beg:SG_end])) 

     dist_func4  = ABS(dH[SG_beg:SG_end]-diono_effect_tot[SG_beg:SG_end])
     tot_diff4   = TOTAL(dist_func4)
     avr_diff4   = tot_diff4 / (N_ELEMENTS(dH[SG_beg:SG_end]))

     PRINT, '###################################################################'
     PRINT, 'funcion distancia d(DH,Dst): ', tot_diff0
     PRINT, '<Distancia total>', avr_diff0    
     PRINT, '###################################################################'          
     PRINT, 'funcion distancia total d(DH,Dst+DP2): ', tot_diff1
     PRINT, '<Distancia total>: ', avr_diff1      
     PRINT, '###################################################################'         
     PRINT, 'funcion distancia d(DH,Dst+Ddyn) en la ventana de t: ', tot_diff2
     PRINT, '<Distancia total>', avr_diff2     
     PRINT, '###################################################################'
     PRINT, 'funcion distancia d(DH,Dst+PI) en la ventana de t: ', tot_diff3
     PRINT, '<Distancia total>', avr_diff3             
     PRINT, '###################################################################'
     PRINT, 'funcion distancia d(DH,Dst+diono) en la ventana de t: ', tot_diff4
     PRINT, '<Distancia total>', avr_diff4             
     PRINT, '###################################################################'     
                    
;###############################################################################                         
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
                         YTITLE = '', $                          
                         ;COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                        ; COLOR=negro, $                                                                      
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                                                                                        
;###############################################################################    
     CGPLOT, k_days, k_mex, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR='black', CHARSIZE = 0.9, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.13,0.9,0.51], XSTYLE = 5, XRANGE=[0, file_number], ySTYLE = 5,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[0,9], THICK=4, /NOERASE
     
     CGOPLOT, k_days, kp, COLOR='green', LINESTYLE=0, THICK=4
     A = FINDGEN(17) * (!PI*2/16.)
     USERSYM, COS(A), SIN(A), /FILL     
  

    CGOPLOT, [tot_days[SG_beg],tot_days[SG_beg]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
    
    CGOPLOT, [tot_days[SG_end],tot_days[SG_end]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR='black'
     
   ;  IF SG_beg/3. 
     GS_ini = ROUND(SG_beg/3.)
     GS_end = ROUND(SG_end/3.)
     
     k_w = kp[GS_ini:GS_end]
     km_w= k_mex[GS_ini:GS_end]
     
     km1_w = new_kmex1[GS_ini:GS_end]
     km2_w = new_kmex2[GS_ini:GS_end]

     k_days_w = k_days[GS_ini:GS_end]  
       
     CGOPLOT, k_days_w, k_w, COLOR='black', PSYM=8, THICK=4, symsize=1 
          
FOR i=0, N_ELEMENTS(k_w)-1 DO BEGIN                
     IF k_w[i] GT km_w[i] THEN BEGIN        
        ERRPLOT,k_days_w[i], km1_w[i], k_w[i], THICK=4
        ERRPLOT, k_days_w[i], k_w[i], km2_w[i], LINESTYLE=0, THICK=1
    ENDIF  
        IF k_w[i] LT km_w[i] THEN BEGIN
        ERRPLOT,k_days_w[i], km1_w[i], k_w[i], LINESTYLE=0, THICK=1
        ERRPLOT, k_days_w[i], k_w[i], km2_w[i], THICK=4
    ENDIF
ENDFOR                  
;###############################################################################                         
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         ;COLOR=negro, $
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

        AXIS, YAXIS = 0, YRANGE=[0,9], $
                         YTITLE = '', $                          
                        ; COLOR=negro, $
                         YSTYLE=1, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, YRANGE=[0,9], $
                        ; COLOR=negro, $                     
                         YSTYLE=1, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                                                 
;###############################################################################
   x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, ALIGNMENT=0.5, CHARSIZE=3.0, CHARTHICK=3.5    

 ;  x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
 ;  XYOUTS, X, 0.928, 'LT', /NORMAL, color=negro, Alignment=0.5, Charsize=1.3, CHARTHICK=1.5    

 ;  x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
 ;  XYOUTS, X, 0.475, 'LT', /NORMAL, color=negro, Alignment=0.5, Charsize=1.3, CHARTHICK=1.5   
;############################################################################### 
  ; XYOUTS, 0.14, 0.57, '(a)', /NORMAL, color=negro, Alignment=0.5, Charsize=1.8, CHARTHICK= 4    
   
  ; XYOUTS, 0.14, 0.41, '(b)', /NORMAL, color=negro, Alignment=0.5, Charsize=1.8, CHARTHICK= 4          
;###############################################################################                     
;first panel legend                   
        cgPolygon, [0.79,0.82,0.82,0.79], [0.654,0.654,0.657,0.657], color = 'black', /NORMAL, /FILL
        cgPolygon, [0.79,0.82,0.82,0.79], [0.624,0.624,0.627,0.627], color = 'red', /NORMAL  , /FILL      
        cgPolygon, [0.79,0.82,0.82,0.79], [0.594,0.594,0.597,0.597], color = 'green', /NORMAL , /FILL  
        
        XYOUTS, 0.825, 0.65 , /NORMAL, d_H, CHARSIZE = 2.4, CHARTHICK=chr_thick1 
                
        XYOUTS, 0.825, 0.62 , /NORMAL, dst_l, CHARSIZE = 2.4, CHARTHICK=chr_thick1   
                
        XYOUTS, 0.825, 0.59 , /NORMAL, 'Dst', CHARSIZE = 2.4, CHARTHICK=chr_thick1     
                
;second panel legend  
        cgPolygon, [0.79,0.82,0.82,0.79], [0.424,0.424,0.427,0.427], color = 'black', /NORMAL, /FILL
        cgPolygon, [0.79,0.82,0.82,0.79], [0.394,0.394,0.397,0.397], color = 'green', /NORMAL , /FILL 

        XYOUTS, 0.825, 0.42 , /NORMAL, 'Kmex', CHARSIZE = 2.4, CHARTHICK=chr_thick1   
                
        XYOUTS, 0.825, 0.39 , /NORMAL, 'Kp', CHARSIZE = 2.4, CHARTHICK=chr_thick1   
;###############################################################################                
   y = (0.9 - 0.55) / 2. + 0.55 
   XYOUTS, 0.02, y, '[nT]', /NORMAL, $
   ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=3.5                       
;###############################################################################
; saving png
    cgPS_Close, density = 300, width = 1000   
    RETURN    
END


PRO makefig_png, tot_days, dH, dst, kp, k_mex, k_days, Bsq, H, new_kmex1,new_kmex2,$
    date_i, date_f, SG_beg, SG_end
    
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	

    tot_days= FINDGEN(file_number*24)/24.0  
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')    
;###############################################################################
    idate0 = STRING(yr_i, mh_i, format='(I4,I02)')
    TGM_n  = event_case([yr_i,mh_i,dy_i])
;###############################################################################
; define device and color parameters 
        Device_bak2 = !D.Name         
        SET_PLOT, 'Z'      
        
        Xsize=FIX(1600)
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
    old_month = month_name(mh_i, 'english')                        ;Function to convert month from mm format to string
                         
;############################################################################### 
       days = intarr(file_number+1)
       FOR i=0, N_ELEMENTS(days)-1 DO BEGIN
            days[i] = dy_i+i
       ENDFOR
       days = days*24/24. 
       day_time = findgen(24)   
;############################################################################### 
    time_title = ' UT [days]'
    window_title = 'SGS'+ STRING(TGM_n, FORMAT='(I01)')+', '+ $
                    STRING(old_month, yr_i, FORMAT='(A, X, I4)') 

                                  
     up = MAX(dH)
     down=MIN(dH)

     PLOT, tot_days, dH, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = 0.9, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.52,0.9,0.9], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], THICK=4
;###############################################################################        
     d_H = TeXtoIDL('\DeltaH') 
     dst_l = TexToIDL('Dst_\lambda')

;############################################################################### 
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, Bsq, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')
    
; compute frequencies 
    fn    = dionstr.fn

; compute diono variables    
    diono = dionstr.diono
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn
     
     Bsq_med     = MEDIAN(Bsq)
     Bsq0        = Bsq-Bsq_med
     dp2_effect  = dionstr.p_a+dp2
     ddyn_effect = dionstr.p_a+ddyn
     diono_effect= dionstr.p_a+dp2+ddyn
     diono_effect_tot=dionstr.p_a+diono
     
     OPLOT, tot_days, dst, COLOR=verde, THICK=4  
     OPLOT, tot_days[SG_beg:SG_end], diono_effect[SG_beg:SG_end], COLOR=rojo, THICK=4         
    ; OPLOT, tot_days, diono_effect_tot, COLOR=azul, THICK=4
    window_title = 'SGS'+ STRING(TGM_n, FORMAT='(I2)')+', '+ $
                STRING(old_month, yr_i, FORMAT='(A, X, I4)')
    
    OPLOT, [tot_days[SG_beg],tot_days[SG_beg]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [tot_days[SG_end],tot_days[SG_end]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro

    
     
     dist_func0  = ABS(dH[SG_beg:SG_end]-dst[SG_beg:SG_end])
     tot_diff0   = TOTAL(dist_func0)
     avr_diff0   = tot_diff0 / (N_ELEMENTS(dH[SG_beg:SG_end])) 
     
     dist_func1  = ABS(dH[SG_beg:SG_end]-dp2_effect[SG_beg:SG_end])
     tot_diff1   = TOTAL(dist_func1)
     avr_diff1   = tot_diff1 / (N_ELEMENTS(dH[SG_beg:SG_end]))   

     dist_func2  = ABS(dH[SG_beg:SG_end]-ddyn_effect[SG_beg:SG_end])
     tot_diff2   = TOTAL(dist_func2)
     avr_diff2   = tot_diff2 / (N_ELEMENTS(dH[SG_beg:SG_end]))          

     dist_func3  = ABS(dH[SG_beg:SG_end]-diono_effect[SG_beg:SG_end])
     tot_diff3   = TOTAL(dist_func3)
     avr_diff3   = tot_diff3 / (N_ELEMENTS(dH[SG_beg:SG_end])) 

     dist_func4  = ABS(dH[SG_beg:SG_end]-diono_effect_tot[SG_beg:SG_end])
     tot_diff4   = TOTAL(dist_func4)
     avr_diff4   = tot_diff4 / (N_ELEMENTS(dH[SG_beg:SG_end]))

     PRINT, '###################################################################'
     PRINT, 'funcion distancia d(DH,Dst): ', tot_diff0
     PRINT, '<Distancia total>', avr_diff0    
     PRINT, '###################################################################'          
     PRINT, 'funcion distancia total d(DH,Dst+DP2): ', tot_diff1
     PRINT, '<Distancia total>: ', avr_diff1      
     PRINT, '###################################################################'         
     PRINT, 'funcion distancia d(DH,Dst+Ddyn) en la ventana de t: ', tot_diff2
     PRINT, '<Distancia total>', avr_diff2     
     PRINT, '###################################################################'
     PRINT, 'funcion distancia d(DH,Dst+PI) en la ventana de t: ', tot_diff3
     PRINT, '<Distancia total>', avr_diff3             
     PRINT, '###################################################################'
     PRINT, 'funcion distancia d(DH,Dst+diono) en la ventana de t: ', tot_diff4
     PRINT, '<Distancia total>', avr_diff4             
     PRINT, '###################################################################'     
                    
;###############################################################################                         
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
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                         COLOR=negro, $                                                                      
                         YSTYLE=2, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                                                                                        
;###############################################################################    
     PLOT, k_days, k_mex, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = 0.9, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.12,0.9,0.5], XSTYLE = 5, XRANGE=[0, file_number], ySTYLE = 5,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[0,9], THICK=4, /NOERASE
     
     OPLOT, k_days, kp, COLOR=verde, LINESTYLE=0, THICK=4
     A = FINDGEN(17) * (!PI*2/16.)
     USERSYM, COS(A), SIN(A), /FILL     
  

    OPLOT, [tot_days[SG_beg],tot_days[SG_beg]], $ ;referencia para el inicio de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    
    OPLOT, [tot_days[SG_end],tot_days[SG_end]], $ ;referencia para el final de la Tormenta
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
     
   ;  IF SG_beg/3. 
     GS_ini = ROUND(SG_beg/3.)
     GS_end = ROUND(SG_end/3.)
     
     k_w = kp[GS_ini:GS_end]
     km_w= k_mex[GS_ini:GS_end]
     
     km1_w = new_kmex1[GS_ini:GS_end]
     km2_w = new_kmex2[GS_ini:GS_end]

     k_days_w = k_days[GS_ini:GS_end]  
       
     OPLOT, k_days_w, k_w, COLOR=negro, PSYM=8, THICK=4, symsize=1 
          
FOR i=0, N_ELEMENTS(k_w)-1 DO BEGIN                
     IF k_w[i] GT km_w[i] THEN BEGIN        
        ERRPLOT,k_days_w[i], km1_w[i], k_w[i], COLOR=negro, THICK=3
        ERRPLOT, k_days_w[i], k_w[i], km2_w[i], COLOR=negro, LINESTYLE=0
    ENDIF  
        IF k_w[i] LT km_w[i] THEN BEGIN
        ERRPLOT,k_days_w[i], km1_w[i], k_w[i], COLOR=negro, LINESTYLE=0
        ERRPLOT, k_days_w[i], k_w[i], km2_w[i], COLOR=negro, THICK=3
    ENDIF
ENDFOR                  
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
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[0,9], $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         YSTYLE=1, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5 
                        
        AXIS, YAXIS = 1, YRANGE=[0,9], $
                         COLOR=negro, $                     
                         YSTYLE=1, $
                         CHARSIZE = 2.8 ,$
                         CHARTHICK=3.5                                                 
;###############################################################################
   x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, COLOR=negro, ALIGNMENT=0.5, CHARSIZE=3.0, CHARTHICK=3.5    

 ;  x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
 ;  XYOUTS, X, 0.928, 'LT', /NORMAL, color=negro, Alignment=0.5, Charsize=1.3, CHARTHICK=1.5    

 ;  x = (!X.WINDOW[1] - !X.WINDOW[0]) / 2. + !X.WINDOW[0]
 ;  XYOUTS, X, 0.475, 'LT', /NORMAL, color=negro, Alignment=0.5, Charsize=1.3, CHARTHICK=1.5   
;############################################################################### 
  ; XYOUTS, 0.14, 0.57, '(a)', /NORMAL, color=negro, Alignment=0.5, Charsize=1.8, CHARTHICK= 4    
   
  ; XYOUTS, 0.14, 0.41, '(b)', /NORMAL, color=negro, Alignment=0.5, Charsize=1.8, CHARTHICK= 4          
;###############################################################################                     
;first panel legend                   
        POLYFILL, [0.79,0.82,0.82,0.79], [0.654,0.654,0.657,0.657], color = negro, /NORMAL
        POLYFILL, [0.79,0.82,0.82,0.79], [0.624,0.624,0.627,0.627], color = rojo, /NORMAL        
        POLYFILL, [0.79,0.82,0.82,0.79], [0.594,0.594,0.597,0.597], color = verde, /NORMAL   
        
        XYOUTS, 0.825, 0.65 , /NORMAL, d_H, COLOR=negro, CHARSIZE = 2.4, CHARTHICK=chr_thick1 
                
        XYOUTS, 0.825, 0.62 , /NORMAL, dst_l, COLOR=negro, CHARSIZE = 2.4, CHARTHICK=chr_thick1   
                
        XYOUTS, 0.825, 0.59 , /NORMAL, 'Dst', COLOR=negro, CHARSIZE = 2.4, CHARTHICK=chr_thick1     
                
;second panel legend  
        POLYFILL, [0.79,0.82,0.82,0.79], [0.424,0.424,0.427,0.427], color = negro, /NORMAL
        POLYFILL, [0.79,0.82,0.82,0.79], [0.394,0.394,0.397,0.397], color = verde, /NORMAL  

        XYOUTS, 0.825, 0.42 , /NORMAL, 'Kmex', COLOR=negro, CHARSIZE = 2.4, CHARTHICK=chr_thick1   
                
        XYOUTS, 0.825, 0.39 , /NORMAL, 'Kp', COLOR=negro, CHARSIZE = 2.4, CHARTHICK=chr_thick1   
;###############################################################################                
   y = (0.9 - 0.55) / 2. + 0.55 
   XYOUTS, 0.02, y, '[nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=3.5                       
;###############################################################################
; saving png
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak2  
    path = '../rutidl/output/article1events/diono_ev/dH_approx/'
    PRINT, '        Setting PNG as default file type.'
    WRITE_PNG, path+'diono_valid_V4_'+Date+'.png', Image, R_bak, G_bak, B_bak
    print, '        Saving: '+path+'diono_valid_V4_'+Date+'.png'
    print, ''
        RETURN 	
END	
