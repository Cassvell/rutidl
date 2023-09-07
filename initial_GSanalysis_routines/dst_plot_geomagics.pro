;Name:
;	dh_dst_plot.pro
;purpose:
;	this routine will call read a certain number of files containing 
;   Dst and DH data to plot
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
;   .r dH_plot
;   dH_plot, [yyyy, mm, dd], [yyyy, mm, dd]
;parameters:
;
;
;dependencies:
;
;
;input files
;   Dst and dH data
;
;output files:
;   Dst and dH time series plot in .PNG format
;   import to output/new_events/gmindex_yyyy-mm-dd.png 
;
;version
; Dec, 2022

PRO dst_plot_geomagics, date_i, date_f
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
        @set_up_commons
        set_up
        
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]      
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
    tot_days       = FINDGEN(file_number*24)/24.

	dst = dst_array([2023,9,1], [2023,9,7], 'dst')
    
        
;print, time
        ;time          = findgen(file_number*8)/DOUBLE(8)+JULDAY(initial_month,initial_day,initial_year)
        
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        IF file_number GT 7 THEN tmp_spam = 1.5
        IF file_number GT 15 THEN tmp_spam = 2.
        
        Xsize=fix(800*tmp_spam)
        Ysize=400
        ;DEVICE, decompose=0
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]
        ;window, 1, xsize=Xsize, ysize=Ysize
        
        
        time          = (findgen(file_number*24)+0.5)/24.0;+JULDAY(initial_month,initial_day,initial_year)
        ;print, time

        ;oplot, time+JULDAY(initial_month,initial_day,initial_year), k_mex_data, psym=6

;goto, jump1


        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        amarillo  = 198
        verde     = 160
        negro     = 0
        blanco    = 255
        gris      = 150
        morado    = 16
        
        
        TVLCT, R_bak, G_bak, B_bak, /GET
        
        LOADCT, 39, /SILENT

;!P.FONT = 0

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
                ;print, X_label[i]

        ENDFOR

        y_maximum = 100.;MAX(dh_data[WHERE(dh_data LT 999999.)])
        y_minimum = -400.;MIN(dh_data[WHERE(dh_data LT 999999.)])


        plot, time, dst, psym=6, /NoDATA, BACKGROUND = blanco, COLOR=negro, MIN_VALUE=y_minimum, MAX_VALUE=y_maximum, $
                                YRANGE=[y_minimum,y_maximum], YSTYLE=5, YTICKS=5, YMINOR=2, $
                                TITLE=Plot_title, $
                                SUBTITLE= Time_title,$
                                YTITLE = K_axis_title, $
                                XRANGE=[0,file_number], XSTYLE=5, XTICKS=file_number, XMINOR=4, $; XTICKUNITS = 'Days', $
                                XTITLE = Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                                XTICKNAME=REPLICATE(' ', file_number+1), $
                                CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                                POSITION=[0.10,0.35,0.95,0.9];, font=1
                               

        OPLOT, [0,file_number], [1.,1.]*25., thick=5, linestyle=1, color=azul
        OPLOT, [0,file_number], [1.,1.]*(-25.), thick=5, linestyle=1, color=azul
        OPLOT, [0,file_number], [1.,1.]*50., thick=5, linestyle=1, color=amarillo
        OPLOT, [0,file_number], [1.,1.]*(-50.), thick=5, linestyle=1, color=amarillo
        OPLOT, [0,file_number], [1.,1.]*75., thick=5, linestyle=1, color=rojo
        OPLOT, [0,file_number], [1.,1.]*(-100.), thick=5, linestyle=1, color=rojo
        OPLOT, [0,file_number], [1.,1.]*(-250.), thick=5, linestyle=1, color=morado

        ;j = N_ELEMENTS(k_mex_data)-1
        ;WHILE k_mex_data[j] GT 9 AND j GT 0 DO j--
        
        oplot, time, dst, thick=4, color=negro, MIN_VALUE=y_minimum, MAX_VALUE=y_maximum;, linestyle=0
        

        FOR i = 0, file_number-1 DO BEGIN
                OPLOT, [i,i], [y_minimum,y_maximum], linestyle=2, COLOR=negro
                ;print, [i,i], [0.,9.]
        ENDFOR

        FOR i=0, 3 DO OPLOT, [0,file_number], [-100.,-100.]*i, linestyle=2, COLOR=negro
        ;OPLOT, [0,file_number], [6.,6.], linestyle=1, COLOR=negro

        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=4, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=4, $
                         XTICKNAME=REPLICATE(' ', file_number+1), $
                         COLOR=negro, $
                         TICKLEN=0.04


        AXIS, YAXIS = 0, YRANGE=[y_minimum,y_maximum], $
                         YTICKS=5, $
                         YMINOR=2, $
                         YTITLE = K_axis_title, $
                         COLOR=negro, $
                         CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[y_minimum,y_maximum], $
                         YTICKS=5, $
                         YMINOR=2, $
                         YTICKNAME=[' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '], $
                         COLOR=negro, $
                         CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1;, $

	LANCE_banner  = 'http://wdc.kugi.kyoto-u.ac.jp/dst_realtime/'
        XYOUTS, 0.01, 0.015 , /NORMAL, $
                LANCE_banner, COLOR=negro, $
                CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1
tmp_string    = 'by World Data Center for Geomagnetism, Kyoto'
MAG_banner    = 'Dst: '+tmp_string
        XYOUTS, 0.01, 0.070 , /NORMAL, $
                MAG_banner, COLOR=negro, $
                CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1

        XYOUTS, 0.01, 0.165 , /NORMAL, $
                'Color Code:        weak,        moderate,        intense,          extreme,        data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
        PLOTS, [0.14,0.18], [1.,1.]*0.18, thick=5, linestyle=1, color = azul, /NORMAL
        PLOTS, [0.27,0.31], [1.,1.]*0.18, thick=5, linestyle=1, color = amarillo, /NORMAL
        PLOTS, [0.44,0.48], [1.,1.]*0.18, thick=5, linestyle=1, color = rojo, /NORMAL
        PLOTS, [0.60,0.64], [1.,1.]*0.18, thick=5, linestyle=1, color = morado, /NORMAL
        PLOTS, [0.76,0.775], [1.,1.]*0.18, thick=5, linestyle=0, color = negro, /NORMAL
        PLOTS, [0.785,0.80], [1.,1.]*0.18, thick=5, linestyle=0, color = negro, /NORMAL
        ;POLYFILL, [0.15,0.18,0.18,0.15], [0.165,0.165,0.195,0.195], color = verde, /NORMAL
        ;POLYFILL, [0.27,0.30,0.30,0.27], [0.165,0.165,0.195,0.195], color = amarillo, /NORMAL
        ;POLYFILL, [0.44,0.47,0.47,0.44], [0.165,0.165,0.195,0.195], color = rojo, /NORMAL
        ;POLYFILL, [0.58,0.63,0.63,0.58], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        ;POLYFILL, [0.58,0.63,0.63,0.58], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0
        Julian = SYSTIME(/JULIAN, /UTC) 
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_hour    = 0
        tmp_minute  = 0
        CALDAT, SYSTIME(/JULIAN, /UTC), tmp_month, tmp_day, tmp_year, tmp_hour, tmp_minute
        UPDATE_banner = 'Updated: '+STRING(tmp_year,tmp_month,tmp_day, tmp_hour,tmp_minute, FORMAT='(I4,"/",I02,"/",I02,"-",I02,":",I02," UTC")')
        XYOUTS, 0.65, 0.015 , /NORMAL, $
                UPDATE_banner, COLOR=negro, $
                CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1


        Image=TVRD()                           ; reads Z buffer !!
        TVLCT, reds, greens, blues, /get
        

        TVLCT, R_bak, G_bak, B_bak

		;DEVICE, /CLOSE

        
        SET_PLOT, Device_bak
        

        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, file_dir+file_name+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, '../rutidl/output/article2/dst_lance.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '
                print, ''
        ENDIF

;print, file_number
;print, k_mex_data

        RETURN          	
END
