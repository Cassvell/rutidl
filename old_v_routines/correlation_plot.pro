;+
; NAME:
;       ????????????????
;
;
; PURPOSE:
;
;       ????????????????
;
; AUTHOR:
;
;       Pedro Corona Romero
;       Instituto de Geofisica, UNAM
;       UNAM Campus Morelia, Antigua Carretera a Patzcuaron,
;       Exhacienda de San Jose del Cerrito, Morelia, Michoacan, Mexico
;       piter.cr@gmail.com
;       06.iv.mmxxii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;
;       ?????????????????????
;
;       Description:
;       ???????????????????????
;
;
; PARAMETERS:
;       ???????    : ?????????????
;
; KEYWORD PARAMETERS:
;
;       /????????? : ?????????????
;
; DEPENDENCIAS:
;       ?????????? : ????????????
;
; ARCHIVOS ANALIZADOS:
;       ??????????
;
; ARCHIVOS DE SALIDA:
;
; HISTORIA:
;






PRO correlation_plot;, idate, fdate

        input_dir  = '~/rutidl/output/'
        path       = input_dir
        
        data_files         = FILE_SEARCH(input_dir+'tgmdata'+'?????????????????'+'.txt')
        print, data_files 
        files_lines_number = FILE_LINES(data_files)
        MX_latitude        = 28.06*!Pi/180.
        corellation_index  = FLTARR(N_ELEMENTS(data_files));<<<<<<<<<<<<<<<<preguntar
        
        limits = [0, -20, -40, -60, -80, -100, -125, -150, -200]
        
;print, data_files

        dat_str0 = { doy: INTARR(MAX(files_lines_number)), $
                    hour: FLTARR(MAX(files_lines_number)), $
                     dst: INTARR(MAX(files_lines_number)), $
                     dh: FLTARR(MAX(files_lines_number)), $
                     number_of_lines: 0 , $
                     correlation: FLTARR(N_ELEMENTS(limits)), $
                     correlation_points : INTARR(N_ELEMENTS(limits))}
        
        data = REPLICATE(dat_str0, N_ELEMENTS(data_files))
        ;data[*].number_of_lines = files_lines_number
;print, data[*].number_of_lines
        data[*].number_of_lines = files_lines_number-1
        data[*].correlation[*]  = 999.
;print, data[*].number_of_lines
;Return

        FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
                data_strings  = STRARR(files_lines_number[event])
                
                OPENR, file_lun, data_files[event], /GET_LUN
                READF, file_lun, data_strings;, FORMAT = '(A)'
                FREE_LUN, file_lun
;print, data_strings
;return
                dat_str1 = { doy:0, hour: 0., dst: 0, dh: 0 }
                tmp_data = REPLICATE(dat_str1, data[event].number_of_lines)
                
                READS, data_strings[1:*], tmp_data, $
                FORMAT='(I03,X,F4.1,X,I04,X,F6.1)'
                
               ; print, dst
                data[event].doy[*] = 999
;print, data[event].number_of_lines, N_ELEMENTS(tmp_data[*].doy)
;RETURN
                data[event].doy[0:data[event].number_of_lines-1]=tmp_data[*].doy
                
                data[event].hour[*] = 999.
                data[event].hour[0:data[event].number_of_lines-1]=tmp_data[*].hour
                
                data[event].dst[*] = 999.
                data[event].dst[0:data[event].number_of_lines-1]=tmp_data[*].dst*cos(MX_latitude)
                
                data[event].dh[*] = 999.
                data[event].dh[0:data[event].number_of_lines-1]=tmp_data[*].dh
                
                tmp_index = WHERE(data[event].dh[*] GE 999.)
                data[event].dst[tmp_index] = 999.
        ENDFOR


        FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
                FOR i=0, N_ELEMENTS(limits)-1 DO BEGIN
                        IF i EQ 0 THEN index_tmp = WHERE(data[event].dst[*] GE limits[i] AND data[event].dst[*] LT 200 ) $
                                  ELSE index_tmp = WHERE(data[event].dst[*] GE limits[i] AND data[event].dst[*] LT limits[i-1] )
                        IF N_ELEMENTS(index_tmp) GT 2 THEN data[event].correlation[i] = CORRELATE(data[event].dst[index_tmp], data[event].dh[index_tmp])
                        data[event].correlation_points[i] = N_ELEMENTS(index_tmp)
                ENDFOR
        ENDFOR
        
        means=FLTARR(N_ELEMENTS(limits))
        devs =FLTARR(N_ELEMENTS(limits))
        maxs =FLTARR(N_ELEMENTS(limits))
        mins =FLTARR(N_ELEMENTS(limits))
        ;print, means
        for i=0, N_ELEMENTS(limits)-1 DO BEGIN
                ;print, n_elements(limits), limits
               ; print, data[*].correlation[i]
                index_tmp = WHERE(data[*].correlation[i] LT 100)
               ; print, index_tmp
                ;print, data[index_tmp].correlation[i]
                means[i] = Mean( data[index_tmp].correlation[i]^2 )
                devs[i]  = STDDEV( data[index_tmp].correlation[i]^2, /NAN)
                ;maxs[i]  = MAX( data[index_tmp].correlation[i] )
                ;mins[i]  = MIN( data[index_tmp].correlation[i] )
                print, means[i], devs[i];, maxs[i], mins[i]
        ENDFOR

;###############################################################################          
; define device and color parameters 
;###############################################################################      
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        
        Xsize=fix(1600*tmp_spam)
        Ysize=600
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]
             
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        amarillo  = 220
        verde     = 130
        negro     = 0
        azul      = 60
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 0, /SILENT

    ;path = '../rutidl/output/eventos_tgm/'

        event = 0
        PLOT, limits[WHERE(data[event].correlation_points[*] GT 3)]+0.5*(limits[1]+limits[0]), $
        data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
        MAX=1., XRANGE=[10.,-210], YRANGE=[0., 1.], BACKGROUND = blanco, $
        Xtitle='Dst', Ytitle='R!U2!N', Xstyle=5, Ystyle=5, /NODATA, color=negro,$
        charsize=1.5, POSITION=[0.13,0.1,0.9,0.9], TITLE='Correlacion entre indice Dst y H'


        POLYFILL, [10.,-210.,-210.,10.], [0.,0.,0.5,0.5], color=amarillo

        AXIS, Xaxis=0, Xtitle='Dst', charsize=1.5, color=negro, XRANGE=[10.,-210], Xstyle=1
        AXIS, Xaxis=1, color=negro, XTICKNAME=REPLICATE( ' ', 8 ), XRANGE=[10.,-210]
        
        AXIS, Yaxis=0, Ytitle='R!U2!N', charsize=1.5, color=negro, Ystyle=1
        AXIS, yaxis=1, color=negro, YTICKNAME=REPLICATE( ' ', 7 )

        
    symbols=[1,2,3,4,5,6]
    colors=[1, 1, 1, 1, 1, 1]*gris
    
        FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
                oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
                data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
                psym=symbols[event], color=colors[event], thick=4, symsize=1.5
                
                ;oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
                ;data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
                ;color=colors[event]                
        ENDFOR
        print, !Y.CRANGE

        x_correlation = (!X.CRANGE[1]-!X.CRANGE[0])*FINDGEN(101)/100+!X.CRANGE[0]
        y_correlation = INTERPOL(means[*],limits[*], x_correlation)
        oplot, x_correlation[*], y_correlation[*], color=negro, thick=4

        A = FINDGEN(17) * (!PI*2/16.)
        ERRPLOT, limits[*], means[*]+devs[*], means[*]-devs[*], color=negro
        USERSYM, COS(A), SIN(A), /FILL
        oplot, limits[*], means[*], color=blanco, PSYM = 8, symsize=2
        oplot, limits[*], means[*], color=negro, PSYM = 8, symsize=1
        USERSYM, COS(A), SIN(A)
        oplot, limits[*], means[*], color=negro, PSYM = 8, symsize=2, thick=4
        
        

    Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   


;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; open the post stript device
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'correlation_plotV3.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'correlation_plotV3.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'correlation_plotV3.png'
                print, ''
        ENDIF
        RETURN 	


RETURN
END




