;Name: kdiono_effect.pro
;purpose:
;	plot the effects of DP2 and Ddyn on Kp index in graphic bar 
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
;   .r kdiono_effect
;   kdiono_effect, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;  
;
;dependencies:
;   Instituto de Geofísica, Unidad Michoacan
;
;input files
;   Dst Index, DH index, Kp Index, Kmex index, ksigma[K1,K2], Bsq data
;
;output files:
;   DP2 and Ddyn effects (ksigma[K1,K2]) on Kp. 
;   Import a .PNG figure on the Dir output/eventos_tgm/
;   named k_vs_diono_Vn_yyyy-mm-dd.png
;
;version
;   Dec, 2022
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;       3. having the new kmex data files(new_km.pro routine)
;
;
PRO kdiono_effect, r_dst, r_kp, DOY, date_i, date_f, JPEG = jpeg 
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
;###############################################################################
    idate0 = STRING(yr_i, mh_i, FORMAT='(I4,I02)')
    TGM_n = event_case([yr_i,mh_i,dy_i]) 
;###############################################################################  
    tiempo = TIMEGEN(t, START=julday(d_dst.month[0], d_dst.day[0],  $
                     d_dst.year[0], d_dst.hour[0]), UNITS='Hours')  
                     
    Date    = STRING(year[0], mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################
; define DH variables
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    k_days = FINDGEN(file_number*8)/8.0 

;Generate the time series
; define Dst and dH variables
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
    
; define DK effect
    new_kmex1   = new_kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'new_kmex1')
    new_kmex2   = new_kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'new_kmex2')    
;calcular el porcentaje de valores NaN en la serie de tiempo
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 99999.0, 'equal')                       
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
    H = fillnan(H)     
;###############################################################################
    new_dstdays = FINDGEN(file_number*1440)/1440.0 ;se genera un arreglo de tiempo con 
;muestreo cada 15 min. para mejorar la resolución de las gráficas    
    
    new_dst = FLTARR(N_ELEMENTS(new_dstdays))     	        
;###############################################################################
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, Bsq, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')
    
; compute frequencies 
    fn    = dionstr.fn

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
        Device_bak2 = !D.Name         
        SET_PLOT, 'Z'      
        
        Xsize=FIX(1600)
        Ysize=1000
        DEVICE, SET_RESOLUTION = [Xsize,Ysize],Set_Pixel_Depth=24, DECOMPOSED=1  
        DEVICE, z_buffer=4
        DEVICE, set_character_size = [10, 12] 
        
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        amarillo  = 190
        verde     = 150
        negro     = 0
        azul      = 90
        blanco    = 255
        gris      = 110
        morado    = 16
        naranja  = 220
                
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT
    
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')                        ;Function to convert month from mm format to string            
;###############################################################################
    spam_i = idate0
CASE spam_i of
    '200311' : spam_i = 1000
    '200411' : spam_i = 50
    '200505' : spam_i = 100
    '201503' : spam_i = 0
    '201705' : spam_i = 0
    '201709' : spam_i = 0
    ELSE: PRINT, 'fuera de rango'
ENDCASE 
;###############################################################################
    spam_f = idate0
CASE spam_f of
    '200311' : spam_f = 2800
    '200411' : spam_f = 4400
    '200505' : spam_f = 0
    '201503' : spam_f = 0
    '201705' : spam_f = 0
    '201709' : spam_f = 0
    ELSE: PRINT, 'fuera de rango'
ENDCASE    
;###############################################################################
       days = intarr(tw+1)
       FOR i=0, n_elements(days)-1 DO BEGIN
            days[i] = dy_i+i
       ENDFOR
       days = days*24/24. 
       day_time = findgen(24)   
;############################################################################### 
    time_title = ' Tiempo Universal ['+textoidl("dias")+' de '+old_month+'].'
    window_title = 'TGM'+ STRING(TGM_n, FORMAT='(I01)')+', '+ $
                    STRING(old_month, yr_i, FORMAT='(A, X, I4)')
;###############################################################################
    med_ddyn = MEDIAN(new_ddyn)
    std_ddyn = stddev(new_ddyn, /NAN)
    
    ddyn_out = WHERE(new_ddyn GE med_ddyn+std_ddyn OR new_ddyn LE med_ddyn-std_ddyn)
    ddyn_in  = WHERE(new_ddyn LE med_ddyn+std_ddyn AND new_ddyn GE med_ddyn-std_ddyn)
    
    ddyn_diff_out = new_ddyn
    ddyn_diff_out[ddyn_in]=!Values.F_NAN
    
    ddyn_diff_in  = new_ddyn
    ddyn_diff_in[ddyn_out]=!Values.F_NAN
     
     updp2     = max(diono-ddyn)
     downdp2   = min(diono-ddyn)     
                               
     PLOT, tot_days, dp2, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.58,0.95,0.9], XSTYLE = 5, XRANGE=[0, tw], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', tw+1), YRANGE=[downdp2,updp2], /NODATA          
;###############################################################################
    med_dp2 = MEDIAN(new_dp2)
    std_dp2 = STDDEV(new_dp2, /NAN)
    
    dp2_out = WHERE(new_dp2 GE med_dp2+std_dp2 OR new_dp2 LE med_dp2-std_dp2)
    dp2_in  = WHERE(new_dp2 LE med_dp2+std_dp2 AND new_dp2 GE med_dp2-std_dp2)
    
    dp2_diff_out = new_dp2
    dp2_diff_out[dp2_in]=!Values.F_NAN
    
    dp2_diff_in  = new_dp2
    dp2_diff_in[dp2_out]=!Values.F_NAN     
;###############################################################################
    dp2_i = idate0
case dp2_i of
    '200311' : dp2_i = dp2_out[6]
    '200411' : dp2_i = dp2_out[50]
    '200505' : dp2_i = dp2_out[100]
    '201503' : dp2_i = dp2_out[500]
    '201705' : dp2_i = dp2_out[100]
    '201709' : dp2_i = dp2_out[500]
    else: print, 'fuera de rango'
endcase  

    dp2_si = idate0
case dp2_si of
    '200311' : dp2_si = 0
    '200411' : dp2_si = -100
    '200505' : dp2_si = -100
    '201503' : dp2_si = -730
    '201705' : dp2_si = -50
    '201709' : dp2_si = 0
    else: print, 'fuera de rango'
endcase 

    dp2_sf = idate0
case dp2_sf of
    '200311' : dp2_sf = 20
    '200411' : dp2_sf = 100
    '200505' : dp2_sf = 350
    '201503' : dp2_sf = 550
    '201705' : dp2_sf = 900
    '201709' : dp2_sf = 990
    else: print, 'fuera de rango'
endcase 
;############################################################################### 
        OPLOT, new_dstdays ,new_idiff-new_ddyn, COLOR=negro, LINESTYLE=0, THICK=4          
        OPLOT, new_dstdays, new_dp2, COLOR=rojo

        OPLOT, new_dstdays[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], $
        new_dp2[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], COLOR=rojo, $
        LINESTYLE=0, THICK=4      
        
        OPLOT, new_dstdays[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], $
        new_dp2[dp2_i+spam_i+dp2_si:dp2_i+spam_f+dp2_sf], COLOR=rojo, $
        LINESTYLE=0, THICK=4            
;############################################################################### 
        AXIS, XAXIS = 0, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-0.25), $
                         XTICKS=tw, $
                         XMINOR=8, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $  
                         CHARSIZE = 0.9, $                                                
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[downdp2,updp2], $ 
                         YSTYLE=2, $  
                         YTITLE = '[nT]', $                          
                         COLOR=negro, $
                         CHARSIZE = 0.9;, $
                        
        AXIS, YAXIS = 1, YRANGE=[downdp2,updp2], $ 
                         YSTYLE=2, $                           
                         COLOR=negro, $
                         CHARSIZE = 0.9;, $      
;###############################################################################        
    PLOT, k_days, k_mex, PSYM=6, /NODATA, MAX_VALUE=9., XTICKS=file_number, XMINOR=8, $
                    TITLE = '', SUBTITLE = '', XTITLE = 's', YTITLE = 's', $
                    BACKGROUND = blanco, COLOR=negro, YRANGE=[0,9], YTICKS=9, $
                    YMINOR=0, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                    POSITION=[0.1,0.1,0.95,0.51], XSTYLE = 5, YSTYLE = 5,$
                    XTICKNAME=REPLICATE(' ', file_number+1), XRANGE=[0, file_number], $
                    /NOERASE
        j = N_ELEMENTS(k_mex)
        FOR i = 0, j-1 DO BEGIN
                IF k_mex[i] LE 9 THEN BEGIN
                                        color = 0
                                        step  = (k_mex[i] EQ 0) ? 0.1 : 0.
                                        CASE 1 OF
                                                k_mex[i] EQ 4 : COLOR = azul
                                                k_mex[i] GE 4 : COLOR = azul
                                                ELSE       : COLOR = azul
                                        ENDCASE
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], [0,0,k_mex[i]$
                                        +step,k_mex[i]+step], COLOR=color
                                      ENDIF $
                                      ELSE BEGIN                                        
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], $
                                        [0,0,9.,9.], color=morado, /LINE_FILL, $
                                        ORIENTATION=45., LINESTYLE = 0
                                                                                  
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], $
                                        [0,0,9.,9.],COLOR=morado, /LINE_FILL, $
                                         ORIENTATION=-45., LINESTYLE = 0
                                      ENDELSE
        ENDFOR

        XYOUTS, 0.01, 0.031 , /NORMAL, $
                'Codigo de Color:        Kp,             Kp y Kmex,           Kmex,                datos no disponibles.', COLOR=negro, $
                CHARSIZE = 0.9, $   
                CHARTHICK=chr_thick1
        POLYFILL, [0.09,0.12,0.12,0.09], [0.025,0.025,0.045,0.045], COLOR = verde, /NORMAL
        POLYFILL, [0.17,0.20,0.20,0.17], [0.025,0.025,0.045,0.045], COLOR = amarillo, /NORMAL
        POLYFILL, [0.27,0.3,0.3,0.27], [0.025,0.025,0.045,0.045], COLOR = azul, /NORMAL
        POLYFILL, [0.52,0.55,0.55,0.52], [0.025,0.025,0.045,0.045], COLOR = morado, /NORMAL, /LINE_FILL, ORIENTATION=45., LINESTYLE = 0
        POLYFILL, [0.52,0.55,0.55,0.52], [0.025,0.025,0.045,0.045], COLOR = morado, /NORMAL, /LINE_FILL, ORIENTATION=-45., LINESTYLE = 0  
;###############################################################################                           
    PLOT, k_days, Kp, PSYM=6, /NODATA, MAX_VALUE=9., XTICKS=file_number, XMINOR=8, $
                    TITLE = '', SUBTITLE = '', XTITLE = 's', YTITLE = 's', $
                    BACKGROUND = blanco, COLOR=negro, YRANGE=[0,9], YTICKS=9, $
                    YMINOR=0, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                    POSITION=[0.1,0.1,0.95,0.51]  , XSTYLE = 5, YSTYLE = 5,$
                    XTICKNAME=REPLICATE(' ', file_number+1), XRANGE=[0, file_number],$
                    /NOERASE
        j = N_ELEMENTS(Kp)
        FOR i = 0, j-1 DO BEGIN
                IF Kp[i] LE 9 THEN BEGIN
                                        color = 0
                                        step  = (Kp[i] EQ 0) ? 0.1 : 0.
                                        CASE 1 OF
                                                Kp[i] EQ 4 : COLOR = verde
                                                Kp[i] GE 4 : COLOR = verde
                                                ELSE       : COLOR = verde
                                        ENDCASE
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], [0,0,Kp[i]$
                                        +step,Kp[i]+step], COLOR=color
                                      ENDIF $
                                      ELSE BEGIN                                        
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], $
                                        [0,0,9.,9.], color=morado, /LINE_FILL, $
                                        ORIENTATION=45., LINESTYLE = 0
                                                                                  
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], $
                                        [0,0,9.,9.],COLOR=morado, /LINE_FILL, $
                                         ORIENTATION=-45., LINESTYLE = 0
                                      ENDELSE
        ENDFOR
        
        FOR i = 0, j-1 DO BEGIN
                IF k_mex[i] LT Kp[i] THEN BEGIN
                                        color = 0
                                        step  = (k_mex[i] EQ 0) ? 0.1 : 0.
                                        CASE 1 OF
                                                k_mex[i] EQ 4 : COLOR = azul
                                                k_mex[i] GE 4 : COLOR = azul
                                                ELSE       : COLOR = azul
                                        ENDCASE
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], [0,0,k_mex[i]$
                                        +step,k_mex[i]+step], COLOR=color
                                      ENDIF 
                                      IF  k_mex[i] EQ Kp[i] THEN BEGIN
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], [0,0,k_mex[i]$
                                        +step,k_mex[i]+step], COLOR=amarillo
                                        ENDIF                                      
        ENDFOR        
 
 FOR i = 0, file_number-1 DO BEGIN
                OPLOT, [i,i], [0.,9.], LINESTYLE=1, COLOR=negro
 ENDFOR

        FOR i=5, 8, 1 DO OPLOT, [0,file_number], [i,i], LINESTYLE=1, COLOR=negro
;###############################################################################
FOR i=0, N_ELEMENTS(Kp)-1 DO BEGIN         
     IF Kp[i] GT k_mex[i] THEN BEGIN
        ERRPLOT,k_days[i]+1.5/24, new_kmex1[i], Kp[i], COLOR=negro, THICK=3
        ERRPLOT, k_days[i]+1.5/24, Kp[i], new_kmex2[i], COLOR=negro, LINESTYLE=0
    ENDIF  
        IF Kp[i] LT k_mex[i] THEN BEGIN
        ERRPLOT,k_days[i]+1.5/24, new_kmex1[i], Kp[i], COLOR=negro, LINESTYLE=0
        ERRPLOT, k_days[i]+1.5/24, Kp[i], new_kmex2[i], COLOR=negro, THICK=3
    ENDIF
ENDFOR     
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTITLE=time_title, $ 
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=0.9, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-0.25), $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $ 
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[0,9], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTITLE = 'indices K', $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.01

        AXIS, YAXIS = 1, YRANGE=[0,9], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTICKNAME=[' ', ' ', ' ', ' ', ' ', 'G1', 'G2', 'G3', 'G4', 'G5'], $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.01;, $
;###############################################################################    
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.95   
   XYOUTS, X, y, window_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65  

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.921, 'Tiempo Local', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=0.9  

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.53, 'Tiempo Local', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=0.9   
;###############################################################################                     
;first panel legend                   
        POLYFILL, [0.79,0.82,0.82,0.79], [0.654,0.654,0.657,0.657], color = rojo, /NORMAL
        POLYFILL, [0.79,0.82,0.82,0.79], [0.624,0.624,0.627,0.627], color = negro, /NORMAL        

        XYOUTS, 0.825, 0.65 , /NORMAL, $
                'DP2', COLOR=negro, $
                CHARSIZE = 1.4, $
                CHARTHICK=chr_thick1 
                
     D1 = TexToIDL('P_{PI}')
        XYOUTS, 0.825, 0.62 , /NORMAL, $
               D1+'-Ddyn', COLOR=negro, $
                CHARSIZE = 1.4, $
                CHARTHICK=chr_thick1                                  
;###############################################################################
; saving png     
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
        
    SET_PLOT, Device_bak2  
    path = '../rutidl/output/eventos_tgm/'
        IF keyword_set(jpeg) THEN BEGIN
                info = SIZE(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = R_bak[image]
                true_image[1,*,*] = G_bak[image]
                true_image[2,*,*] = B_bak[image]
                write_jpeg, path+'k_vs_diono_V4_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN PRINT, '        Setting PNG as default file type.'
                WRITE_PNG, path+'k_vs_diono_V4_'+Date+'.png', Image, R_bak, G_bak, B_bak
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                PRINT, '        Saving: '+path+'k_vs_diono_V4_'+Date+'.png'
                PRINT, ''
        ENDIF
        RETURN 	
END	
