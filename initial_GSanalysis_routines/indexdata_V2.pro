;Name:
;	indexdata_V2
;purpose:
;	leer datos de índices geomagnéticos y de contenido total de electrones para graficarlos 
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   plotting
;
;calling sequence:
;   .r indexdata_V2.pro
;   indexdata_V2, initialdate[yyyy,mm,dd], finaldate[yyyy,mm,dd]
;parameters:
;
;
;dependencies:
;
;
;input files
;   kp data, dst data, kmex data, dh data, TEC data
;
;output files:
;   figure .PNG imported to /output/geom_index/idx_V2_yyyy-mm-dd.png
;
;version
;   Dec, 2022
;

PRO indexdata_v2, date_i, date_f

	On_error, 2
	compile_opt idl2, HIDDEN

;##############################################################################
; defining time window
;############################################################################## 
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]

    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
    fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1    
    days_dst       = FINDGEN(file_number*24)/24.
;###############################################################################   
    tec_days= FINDGEN(file_number*12)/12.0            
    k_days= FINDGEN(file_number*8)/8.0       

; Generate the time series variables 
; define H variables                  
    H  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k')    
    k_mex   = add_nan(k_mex, 9.0, 'greater') 

; Generate the time variables to plot TEC time series         
 ;   tec  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'tec')
 ;   med  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'med')

;setting certain values as NaN        
    H      = add_nan(dH, 999999.0, 'equal')        
    H      = add_nan(dH, 100.0, 'greater') 

;Identifying the NAN values        
    tec = add_nan(tec, 999.0, 'equal')            
    med = add_nan(tec, 999.0, 'equal')                                         
;###############################################################################
; initiate the figure Device, defining colors and figure dim
;###############################################################################
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        IF file_number GT 7 THEN tmp_spam = 1.5
        IF file_number GT 15 THEN tmp_spam = 2.
        
        Xsize=fix(800*tmp_spam)
        Ysize=1200
        ;DEVICE, decompose=0
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]
             
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        amarillo  = 198
        verde     = 160
        negro     = 0
        azul      = 80
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT  
;###############################################################################
; Create a time label based on the DOY initial and DOY final inputs
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = mh_i   
;###############################################################################
; plot the Dst time series for each event

window_title = 'from '+string(yr_i, mh_i, dy_i, $
                FORMAT='(I4, "/", I02, "/", I02)')+' to '$
                +string(yr_f, mh_f, dy_f, $
                FORMAT='(I4, "/", I02, "/", I02)')

time_title = 'Time [UT]' 
                
MAG_source = 'Source: International Service of Geomagnetic Indices'  

    if max(H) gt max(dst) then up0 = max(H) else up0 = max(dst)
    if min(H) lt min(dst) then down0 = min(H) else down0 = min(dst)
            
    plot, days_dst, H, XTICKS=file_number, xminor = 8, POSITION=[0.07,0.66,0.95,0.96],$
    XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=6, YRANGE=[down0,up0],$
    title = window_title, BACKGROUND = blanco, COLOR=negro, XRANGE=[0, file_number],$
	ytitle = 'Indice DST [nT]',  XTICKNAME=REPLICATE(' ', file_number+1)

    oplot, days_dst, dst, COLOR=azul
  
    if up0-down0 gt 300 then begin
        for i = -600., 100., 100. do oplot, [0,file_number], [i,i], linestyle=1, COLOR=negro
    endif else begin
        for i = -600., 100., 50. do oplot, [0,file_number], [i,i], linestyle=1, COLOR=negro
    endelse
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKFORMAT='(A1)',$
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
;                         CHARTHICK=chr_thick1, $
                         XTICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=REPLICATE(' ', file_number+1), $
                         COLOR=negro, $
                         XTICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down0,up0], $
                         YTITLE = 'Dst and DH [nT]', $                          
                         COLOR=negro, $
                         ystyle=2, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

                        
        AXIS, YAXIS = 1, YRANGE=[down0,up0], $
                         ystyle=2, $
                         COLOR=negro, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $    

;###############################################################################     
    plot, k_days, Kp, psym=6, /NoDATA, MAX_VALUE=9., XTICKS=file_number, xminor=8, $
                    TITLE = plot_title, SUBTITLE = '', XTITLE = 's', YTITLE = 's', $
                    BACKGROUND = blanco, COLOR=negro, YRANGE=[0,9], YTICKS=9, $
                    YMINOR=0, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                    POSITION=[0.07,0.36,0.95,0.64], XSTYLE = 5, ystyle = 5,$
                    XTICKNAME=REPLICATE(' ', file_number+1), XRANGE=[0, file_number], /NOERASE

        j = N_ELEMENTS(Kp)
        FOR i = 0, j-1 DO BEGIN
                IF Kp[i] LE 9 THEN BEGIN
                                        color = 0
                                        step  = (Kp[i] EQ 0) ? 0.1 : 0.
                                        CASE 1 OF
                                                Kp[i] EQ 4 : color = amarillo
                                                Kp[i] GE 4 : color = rojo
                                                ELSE       : color = verde
                                        ENDCASE
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], [0,0,Kp[i]$
                                        +step,Kp[i]+step], color=color
                                      ENDIF $
                                      ELSE BEGIN                                        
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], $
                                        [0,0,9.,9.], color=morado, /LINE_FILL, $
                                        ORIENTATION=45., linestyle = 0                                         
                                         
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+k_days[i], $
                                        [0,0,9.,9.],color=morado, /LINE_FILL, $
                                         ORIENTATION=-45., linestyle = 0
                                      ENDELSE
        ENDFOR

 FOR i = 0, file_number-1 DO BEGIN
                OPLOT, [i,i], [0.,90.], linestyle=1, COLOR=negro
        ENDFOR

        FOR i=5, 8, 1 DO OPLOT, [0,file_number], [i,i], linestyle=1, COLOR=negro

        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.7, $
                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=REPLICATE(' ', file_number+1), $
                         COLOR=negro, $
                         TICKLEN=0.04


        AXIS, YAXIS = 0, YRANGE=[0,9], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTITLE = 'Kp index', $
                         COLOR=negro, $
                         CHARSIZE = 0.7, $
                         CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[0,9], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTICKNAME=[' ', ' ', ' ', ' ', ' ', 'G1', 'G2', 'G3', 'G4', 'G5'], $
                         COLOR=negro, $
                         CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1;, $

        XYOUTS, 0.01, 0.070 , /NORMAL, $
                MAG_source, COLOR=negro, $
                CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1

;###############################################################################
    up      = max(tec)
    down    = min(tec)
    
    plot, tec_days, tec, XTICKS=file_number, xminor = 8, POSITION=[0.07,0.1,0.95,0.34],$
    XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=6, YRANGE=[down, up],$
    BACKGROUND = blanco, COLOR=negro, XRANGE=[0, file_number], $
	XTICKNAME=REPLICATE(' ', file_number+1), /noerase

    oplot, tec_days, med, COLOR=rojo
    
    if up-down gt 120 then begin
        for i = -100., 260., 50. do oplot, [0,file_number], [i,i], linestyle=1, COLOR=negro
    endif else begin
        for i = -100., 260., 10. do oplot, [0,file_number], [i,i], linestyle=1, COLOR=negro
    endelse 
         
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKFORMAT='(A1)',$
                         xtitle='time [UT]', $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKFORMAT='(A1)',$                         
                         ;XTICKNAME=REPLICATE(' ', file_number+1), $
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = 'TEC [TECu]', $                          
                         COLOR=negro, $
                         ystyle=2, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                         COLOR=negro, $
                         ystyle=2, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $    
;###############################################################################
    if file_number gt 15 then begin
    
        XYOUTS, 0.01, 0.165 , /NORMAL, $
                'Color Code:        quiet,            disturbed,           storm,                   data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
       
        POLYFILL, [0.07,0.10,0.10,0.07], [0.165,0.165,0.195,0.195], color = verde, /NORMAL
        POLYFILL, [0.15,0.18,0.18,0.15], [0.165,0.165,0.195,0.195], color = amarillo, /NORMAL
        POLYFILL, [0.25,0.28,0.28,0.25], [0.165,0.165,0.195,0.195], color = rojo, /NORMAL
        POLYFILL, [0.37,0.40,0.40,0.37], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        POLYFILL, [0.37,0.40,0.40,0.37], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0
    
    endif else begin
        XYOUTS, 0.01, 0.165 , /NORMAL, $
                'Color Code:       quiet,          disturbed,           storm,                 data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
      
        POLYFILL, [0.09,0.12,0.12,0.09], [0.165,0.165,0.195,0.195], color = verde, /NORMAL
        POLYFILL, [0.19,0.22,0.22,0.19], [0.165,0.165,0.195,0.195], color = amarillo, /NORMAL
        POLYFILL, [0.32,0.35,0.35,0.32], [0.165,0.165,0.195,0.195], color = rojo, /NORMAL
        POLYFILL, [0.47,0.50,0.50,0.47], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        POLYFILL, [0.47,0.50,0.50,0.47], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0
    endelse
;first panel legend
        POLYFILL, [0.77,0.80,0.80,0.77], [0.674,0.674,0.676,0.676], color = negro, /NORMAL
        POLYFILL, [0.86,0.89,0.89,0.86], [0.674,0.674,0.676,0.676], color = azul, /NORMAL        
    if file_number gt 7 then begin
        XYOUTS, 0.777, 0.670 , /NORMAL, $
                '    Dst,          DH', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1         
    endif else begin
        XYOUTS, 0.764, 0.671 , /NORMAL, $
                '    Dst,      DH', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1     
    endelse

;second panel legend
        POLYFILL, [0.77,0.80,0.80,0.77], [0.62,0.62,0.622,0.622], color = negro, /NORMAL
        POLYFILL, [0.86,0.89,0.89,0.86], [0.62,0.62,0.622,0.622], color = verde, /NORMAL        
    if file_number gt 7 then begin
        XYOUTS, 0.772, 0.617 , /NORMAL, $
                '     Ap,           Amex', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1         
    endif else begin
        XYOUTS, 0.772, 0.617 , /NORMAL, $
                '    Ap,      Amex', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1     
    endelse

;third panel legend

        POLYFILL, [0.70,0.73,0.73,0.70], [0.304,0.304,0.306,0.306], color = negro, /NORMAL
        POLYFILL, [0.83,0.86,0.86,0.83], [0.304,0.304,0.306,0.306], color = rojo, /NORMAL        
    if file_number gt 7 then begin
        XYOUTS, 0.732, 0.302 , /NORMAL, $
                'Tec obs,             Tec med', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1         
    endif else begin
        XYOUTS, 0.731,   0.302 , /NORMAL, $
                'Tec obs,     Tec med', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1     
    endelse
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; open the post stript device
    path = '../rutidl/output/geom_index/' 
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'idx_V2'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'idx_V2'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'mag_idx_'+Date+'_V2.png'
                print, ''
        ENDIF
        RETURN
END
