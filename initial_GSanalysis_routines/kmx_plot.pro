;
;Name:
;	kmx_plot.pro
;purpose:
;	plot km data in graphic bar format 
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
;   .r kmx_plot
;   kmx_plot, date_i, date_f
;parameters:
;   date(_i,_f): format = [yyyy,mm,dd]
;
;dependencies:
;
;
;input files
;   kmex data files
;
;output files:
;   graphic bar plot of kmex. Import a .PNG figure to output/kplots dir
;
;version
;   Dec, 2022
;

PRO kmx_plot, date_i, date_f
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]    
;##############################################################################
; reading data files
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
    k_mex   = add_nan(k_mex, 9.0, 'greater') 
    
    time = FINDGEN(file_number *8)/8.0     
    
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
               
        Xsize=fix(1200)
        Ysize=400
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;definición de color
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-        
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        naranja   = 220
        amarillo  = 198
        verde     = 160
        negro     = 0
        gris_o    = 100
        blanco    = 255
        gris      = 130
        morado    = 248
                     
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-    
     X_label   = STRARR(file_number+1)+' '
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        old_month = mh_i
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR        
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; PLOTTING
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
time_title = 'Time window begin: '+string(yr_i, mh_i, dy_i, $
                FORMAT='(I4, "/", I02, "/", I02)')+' 00:00 UTC'
      str = TeXtoIDL('\DeltaH, K_{mex}')          
MAG_source = 'Source: International Service of Geomagnetic Indices'                
    
    plot, time, k_mex, psym=6, /NoDATA, MAX_VALUE=9., XTICKS=file_number, xminor=8, $
                    TITLE = str, SUBTITLE = time_title, XTITLE = 's', YTITLE = 's', $
                    BACKGROUND = blanco, COLOR=negro, YRANGE=[0,9], YTICKS=9, $
                    YMINOR=0, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                    POSITION=[0.07,0.35,0.95,0.9], XSTYLE = 5, ystyle = 5,$
                    XTICKNAME=REPLICATE(' ', file_number+1), XRANGE=[0, file_number]

        j = N_ELEMENTS(k_mex)
        FOR i = 0, j-1 DO BEGIN
                IF k_mex[i] LE 9 THEN BEGIN
                                        color = 0
                                        step  = (k_mex[i] EQ 0) ? 0.1 : 0.
                                        CASE 1 OF
                                                k_mex[i] EQ 4 : color = amarillo
                                                k_mex[i] GE 4 : color = rojo
                                                ELSE       : color = verde
                                        ENDCASE
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+time[i], [0,0,k_mex[i]$
                                        +step,k_mex[i]+step], color=color
                                      ENDIF $
                                      ELSE BEGIN
                                        LOADCT, 0, /SILENT
                                        
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+time[i], $
                                        [0,0,9.,9.], color=morado, /LINE_FILL, $
                                        ORIENTATION=45., linestyle = 0
                                         
                                         
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+time[i], $
                                        [0,0,9.,9.],color=morado, /LINE_FILL, $
                                         ORIENTATION=-45., linestyle = 0
                                      ENDELSE
        ENDFOR
 
 FOR i = 0, file_number-1 DO BEGIN
                OPLOT, [i,i], [0.,9.], linestyle=1, COLOR=negro
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
                         YTITLE = 'Kmex index', $
                         COLOR=negro, $
                         CHARSIZE = 0.7, $
                         CHARTHICK=chr_thick1;, $

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

        XYOUTS, 0.01, 0.165 , /NORMAL, $
                'Color Code:       quiet,       disturbed,         storm,        intense storm,        data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
        POLYFILL, [0.09,0.12,0.12,0.09], [0.165,0.165,0.195,0.195], color = verde, /NORMAL
        POLYFILL, [0.17,0.20,0.20,0.17], [0.165,0.165,0.195,0.195], color = amarillo, /NORMAL
        POLYFILL, [0.29,0.32,0.32,0.29], [0.165,0.165,0.195,0.195], color = naranja, /NORMAL
        POLYFILL, [0.38,0.41,0.41,0.38], [0.165,0.165,0.195,0.195], color = rojo, /NORMAL
        POLYFILL, [0.52,0.55,0.55,0.52], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        POLYFILL, [0.52,0.55,0.55,0.52], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0

    Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    SET_PLOT, Device_bak
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; open the post stript device
    path = '../rutidl/output/kplots/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'test_Km_'+fecha+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'test_Km_'+fecha+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'test_Km_'+fecha
                print, ''
        ENDIF
        RETURN 
END
