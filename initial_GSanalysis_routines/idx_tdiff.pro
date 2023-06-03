PRO idx_tdiff, date_i, date_f
	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]     
;###############################################################################    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    tot_days= FINDGEN(file_number*24)/24.0    
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
    
; Generate the time series variables 
; define H variables                  
    H  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])    
    
;identifying NAN percentage values in the Time Series
    PRINT, ''
    PRINT, ''  
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 100.0, 'greater')

; set values as NaN         
    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 100.0, 'greater')    
;###############################################################################     
    time_w = TIMEGEN(N_ELEMENTS(file_number), final=JULDAY(mh_f, dy_f, yr_f, 23), $
                start=JULDAY(mh_i, dy_i, yr_i, 0) , units='H')

    CALDAT, time_w, m, d, y, hr

    h_min =  MIN(H, j)
    dst_min =  MIN(dst, k)
    PRINT, ''
    PRINT, ''    
    PRINT, "fecha y hora del mínimo en dH [mm/dd hr]"
    PRINT, STRING(y[j], m[j], d[j], hr[j], ':00:00', FORMAT='(I04,"/",I02,"/",I02,2X,I02,A)')
    PRINT, '    '
    PRINT, "fecha y hora del mínimo en Dst [mm/dd hr]"
    PRINT, STRING(y[j], m[j], d[k], hr[k], ':00:00', FORMAT='(I04,"/",I02,"/",I02,2X,I02,A)') 
    PRINT, ''
    PRINT, ''         
   ; PRINT, "periodo de desarrollo hasta el mínimo"  
   ; FOR i = 0, 24 DO BEGIN
        ;PRINT, m[(j+36)-i], d[(j+36)-i], hr[(j+36)-i], H[(j+36)-i], FORMAT='(I02,"/",I02,2X,I02,3X,I04)'
   ;     PRINT, m[(j)-i], d[(j)-i], hr[(j)-i], dst[(j)-i], FORMAT='(I02,"/",I02,2X,I02,3X,I04)'
    ;ENDFOR
;##############################################################################    
    TGM_n = event_case([yr_i, mh_i,dy_i])        
    t0  = 0   
    CASE TGM_n of
        3   :   t0 = 8
        11  :   t0 = 12
        14  :   t0 = 19
        15  :   t0 = 20
        16  :   t0 = 6
        18  :   t0 = 10
        20  :   t0 = 14        
        ELSE: PRINT, 'evento no disponible'   
    ENDCASE

;###############################################################################
; define device and color parameters       
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        Xsize=fix(1200)
        Ysize=600
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=0     
        DEVICE, set_character_size = [10, 12]
        DEVICE, DECOMPOSED=1     
        chr_size1 = 1.5
        chr_thick1= 1.5
        space     = 0.015
        rojo      = 248
        amarillo  = 200
        verde     = 150
        negro     = 0
        azul      = 70
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
;###############################################################################
; Time label    
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')                        ;Function to convert month from mm format to string      
;###############################################################################                              
    dstdt = TS_DIFF(dst,1)
    dhdt  = TS_DIFF(H,1)
    
    window_title = 'SGS '+ STRING(old_month, yr_i, FORMAT='(A, X, I4)')
    PRINT, old_month
;###############################################################################
; Plot data 
    IF MAX(dstdt) GT MAX(dhdt) THEN up = MAX(dstdt) ELSE up = MAX(dhdt)
    IF MIN(dstdt) LT MIN(dhdt) THEN down=MIN(dstdt) ELSE down=MIN(dhdt)
  
     dH = TeXtoIDL('\DeltaH\DeltaT')    
     dDst=TeXtoIDL('dst\DeltaT')
     
    PLOT, tot_days, dstdt, XTICKS=file_number, XMINOR = 8, POSITION=[0.1,0.1,0.9,0.9],$
    XTICKFORMAT='LABEL_DATE', XSTYLE = 5, YSTYLE=5, YRANGE=[down-10, up+10],$
    TITLE = window_title, BACKGROUND = blanco, COLOR=negro, XRANGE=[0,file_number],$
	YTITLE = dH, THICK=2, CHARSIZE=1.6, /NODATA

    IF k GT j THEN l = k ELSE l = j

    POLYFILL, [tot_days[l-t0],tot_days[l],tot_days[l],tot_days[l-t0]], $
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=amarillo, /LINE_FILL, ORIENTATION=45, LINESTYLE=1, THICK=1, $
              SPACING=0.1
              
    POLYFILL, [tot_days[l-t0],tot_days[l],tot_days[l],tot_days[l-t0]], $
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=amarillo, /LINE_FILL, ORIENTATION=-45, LINESTYLE=1, THICK=1, $
              SPACING=0.1

    IF k NE j THEN BEGIN
        OPLOT, [tot_days[j], tot_days[j]], [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=4, THICK=4,$
        COLOR=azul
        
        OPLOT, [tot_days[k], tot_days[k]], [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=4, THICK=4,$
        COLOR=negro
    ENDIF

    OPLOT, tot_days, dstdt, THICK=4, COLOR=negro, LINESTYLE=0
    OPLOT, tot_days, dhdt, THICK=4, COLOR=azul,LINESTYLE=0
    
                                
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTITLE='UT [H]', $  
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number], $      
                         XTICKS=file_number, $
                         XMINOR=8, $ 
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 0.9, $                         
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down-5,up+5], $
                         YTITLE = dDst +', ' + dH, $
                         YSTYLE=1, $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 1.1;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down-5,up+5], $       
                         COLOR=negro, $
                         YSTYLE=1, $
                         CHARSIZE = 1.1, $
                         CHARTHICK=1.5
;###############################################################################
;panel legends
        POLYFILL, [0.77,0.8,0.8,0.77], [0.264,0.264,0.267,0.267], COLOR = azul, /NORMAL
        POLYFILL, [0.77,0.8,0.8,0.77], [0.234,0.234,0.237,0.237], COLOR = negro, /NORMAL                
                
        XYOUTS, 0.81, 0.26 , /NORMAL, $
                dH, COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
        XYOUTS, 0.81, 0.23 , /NORMAL, $
                dDst, COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1                    
                
;############################################################################### 
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   
;###############################################################################
; open the post stript device
    path = '../rutidl/output/article1events/idx_tdiff/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'idx_tdiffV2'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'idx_tdiffV2'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'idx_tdiffV2'+Date+'.png'
                print, ''
        ENDIF
        RETURN                  	
    
                    
END    
