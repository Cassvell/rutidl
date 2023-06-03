;
;Name:
;	gen_obs.pro
;purpose:
;	this routine will call read a certain number of files containing 
;   interplanetary data and geomagnetic data measurements
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   plot data
;
;calling sequence:
;   .r gen_obs
;   gen_obs, initialdate([yyyy,mm,dd]), finaldate([yyyy,mm,dd])
;parameters:
;
;
;dependencies:
;
;
;input files
;   (currently)
;   Ey data
;   dynamic preassure data
;   Dst and DH 
;   Kp and Kmex (Ap and Amex)
;   TEC data
;
;output files:
;   .PNG figure   
;   Figure imported into the 
;   output/eventos_tgm/
;   figure named: 'mag_tec_V3_yyyy-mm-dd.png'
;   
;version
;   Dec, 2022

PRO gen_obs, date_i, date_f
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
    tot_days= findgen(file_number*24)/24.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
    
;###############################################################################
; Generate the time series variables 
; define H variables                  
    H  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
    k_mex   = add_nan(k_mex, 9.0, 'greater') 
    k_days  = FINDGEN(file_number*8)/8. 
 
 ; Generate the variables TEC time series         
    tec  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'tec')
    med  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'med')

; Define the IP time series 
    p_dyn = ip_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'p_dyn')
    Ey = ip_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'E')                      	                         
;###############################################################################        
;identifying NAN percentage values in the Time Series
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 100.0, 'greater')

; set values as NaN         
    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 100.0, 'greater')        
     
    p_dyn = add_nan(p_dyn, 999999.0, 'equal') 
    Ey    = add_nan(Ey, 999999.0, 'equal')                                      
;############################################################################### 
    tec_days= findgen(file_number*12)/12.0  
    tec_diff = tec-med    
;###############################################################################
; define device and color parameters       
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        Xsize=fix(800)
        Ysize=1000
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=0     
        DEVICE, set_character_size = [10, 12]
        DEVICE, DECOMPOSED=1     
        chr_size1 = 1.5
        chr_thick1= 1.5
        space     = 0.015
        rojo      = 248
        amarillo  = 220
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
idate0 = string(yr_i, mh_i, format='(I4,I02)')
TGM_n = idate0
case TGM_n of
    '200311' : TGM_n = 1
    '200411' : TGM_n = 2
    '200505' : TGM_n = 3
    '201503' : TGM_n = 4
    '201705' : TGM_n = 5
    '201709' : TGM_n = 6
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
    if max(dst) gt max(H) then up = max(dst) else up = max(H)
    if min(dst) lt min(H) then down = min(dst) else down = min(H)
    
    window_title = 'SGS'+ string(TGM_n, format='(I01)')+', '+ $
                string(old_month, yr_i, format='(A, X, I4)')
;###############################################################################
; Plot data
    up_E     = max(Ey)
    down_E   = min(Ey)
    
    up_p    = max(p_dyn)
    down_p  = min(p_dyn) 
        
    plot, tot_days, Ey, XTICKS=file_number, xminor=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.1,0.76,0.9,0.93], $
     XSTYLE = 5, XRANGE=[0, file_number],  XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 6,$
     YRANGE=[down_E,up_E], THICK=4  

 	
    plot, tot_days, p_dyn, XTICKS=file_number, xminor=8, BACKGROUND = blanco, COLOR=azul,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.1,0.76,0.9,0.93], $
     XSTYLE = 5, XRANGE=[0, file_number], YRANGE=[down_p,up_p], $
     XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 6, /noerase, THICK=4 ;, SUBTITLE = time_title 

        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='UT [days]', $                           
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $  
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down_E, up_E], $
                         ystyle=2, $  
                         YTITLE = 'Ey [mV/m]', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 0.9;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down_p,up_p], $
                         ystyle=2, $  
                         YTITLE = 'P [nPa]', $                          
                         COLOR=azul, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=3 
;###############################################################################
    up      = MAX(H+10)
    down    = MIN(H-10)    
    plot, tot_days, H, XTICKS=file_number, xminor = 8, POSITION=[0.1,0.52,0.9,0.69],$
    XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=5, YRANGE=[down, up],$
    title = '', BACKGROUND = blanco, COLOR=negro, XRANGE=[0, file_number],$
	ytitle = '',  XTICKNAME=REPLICATE(' ', file_number+1), /noerase, THICK=4

    oplot, tot_days, dst, COLOR=verde, linestyle=0, THICK=4   
     dH = TeXtoIDL('\DeltaH') 
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTITLE='UT [days]', $  
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $  
                         CHARSIZE = 0.9, $                         
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = dH+' and Dst [nT]', $
                         ystyle=1, $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 0.9;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $       
                         COLOR=negro, $
                         ystyle=1, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5
;###############################################################################   
    plot, k_days, k_mex, XTICKS=file_number, xminor = 8, POSITION=[0.1,0.28,0.9,0.45],$
    XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=5, YRANGE=[0,9],$
    BACKGROUND = blanco, COLOR=negro, XRANGE=[0, file_number],$
	ytitle = '',  XTICKNAME=REPLICATE(' ', file_number+1), /noerase, THICK=4

     OPLOT, k_days, kp, COLOR=verde, LINESTYLE=0, THICK=4

        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9 , $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
                         IF TGM_n EQ 3 THEN Ltime = 0.25
                         IF TGM_n EQ 5 THEN Ltime = 0.25
                         IF TGM_n EQ 6 THEN Ltime = 0.25
                         IF TGM_n EQ 1 THEN Ltime = 5./24.
                         IF TGM_n EQ 2 THEN Ltime = 5./24.
                         IF TGM_n EQ 4 THEN Ltime = 5./24.                                                  
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-Ltime), $
                         XTICKS=file_number, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $                                            
                         XMINOR=8, $ 
                         CHARSIZE = 0.9 , $                       
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[0,9], $
                         YTITLE = 'Kp and Kmex', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         YSTYLE=1, $
                         CHARSIZE = 0.9 ;, $
                        
        AXIS, YAXIS = 1, YRANGE=[0,9], $        
                         COLOR=negro, $                         
                         YSTYLE=1, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 0.9 ;, $  
;###############################################################################
    IF MAX(tec) GT MAX(med) THEN up_tecdiff = MAX(tec) ELSE up_tecdiff = MAX(med)
    IF MIN(tec) LT MIN(med) THEN down_tecdiff = MIN(tec) ELSE down_tecdiff = MIN(med)
        
    plot, tec_days, tec, XTICKS=file_number, xminor=8, BACKGROUND = blanco, $
     CHARSIZE = chr_size1, CHARTHICK=chr_thick1, POSITION=[0.1,0.04,0.9,0.21], $
     XSTYLE = 5, XRANGE=[0, file_number], XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 6,$
     /noerase, YRANGE=[down_tecdiff, up_tecdiff], THICK=4, COLOR=rojo

        oplot, tec_days, med, color=negro, linestyle=0, THICK=4
    
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTITLE='UT [days]', $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 0.9, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=(!X.CRANGE+dy_i-0.25), $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKV=FIX(days), $       
                         XTICKN=STRING(days, FORMAT='(I02)'), $  
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$                         
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down_tecdiff, up_tecdiff], $
                         ystyle=2, $  
                         YTITLE = 'TEC and <TEC> [TECu]', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 0.9;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down_tecdiff, up_tecdiff], $
                         ystyle=2, $                    
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5   
;###############################################################################
;second panel legend                   
        POLYFILL, [0.77,0.8,0.8,0.77], [0.564,0.564,0.567,0.567], color = negro, /NORMAL
        POLYFILL, [0.77,0.8,0.8,0.77], [0.534,0.534,0.537,0.537], color = verde, /NORMAL                
                
        XYOUTS, 0.81, 0.56 , /NORMAL, $
                dH, COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
        XYOUTS, 0.81, 0.53 , /NORMAL, $
                'Dst', COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1     

;third panel legend  
        POLYFILL, [0.77,0.8,0.8,0.77], [0.424,0.424,0.427,0.427], color = negro, /NORMAL
        POLYFILL, [0.77,0.8,0.8,0.77], [0.394,0.394,0.397,0.397], color = verde, /NORMAL  

                XYOUTS, 0.81, 0.42 , /NORMAL, $
                'Kmex', COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
        XYOUTS, 0.81, 0.39 , /NORMAL, $
                'Kp', COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1  
                
;fourth panel legend  
        POLYFILL, [0.77,0.8,0.8,0.77], [0.184,0.184,0.187,0.187], color = negro, /NORMAL
        POLYFILL, [0.77,0.8,0.8,0.77], [0.154,0.154,0.157,0.157], color = rojo, /NORMAL  

                XYOUTS, 0.81, 0.18 , /NORMAL, $
                '<TEC>', COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
        XYOUTS, 0.81, 0.15 , /NORMAL, $
                'TEC', COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
;###############################################################################
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.97
   XYOuts, X, y, window_title, /Normal, color=negro, Alignment=0.5,$
   Charsize=2, CHARTHICK = 1.5  
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.949, 'LT', /Normal, $
   color=negro, Alignment=0.5, Charsize=0.8, CHARTHICK = 1.5   

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.71, 'LT', /Normal, $
   color=negro, Alignment=0.5, Charsize=0.8, CHARTHICK = 1.5     
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.468, 'LT', /Normal, $
   color=negro, Alignment=0.5, Charsize=0.8, CHARTHICK = 1.5     
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.231, 'LT', /Normal, $
   color=negro, Alignment=0.5, Charsize=0.8, CHARTHICK = 1.5              
;###############################################################################
   XYOuts, 0.14, 0.9, '(a)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3    
      
   XYOuts, 0.14, 0.55, '(b)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3 
   
   XYOuts, 0.14, 0.42, '(c)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3    
      
   XYOuts, 0.14, 0.17, '(d)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3    
;############################################################################### 
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   
;###############################################################################
; open the post stript device
    path = '../rutidl/output/eventos_tgm/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'mag_tec_V3_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'mag_tec_V3_'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'mag_tec_V3_'+Date+'.png'
                print, ''
        ENDIF
        RETURN 	                    
END
