; NAME: corr_plot.pro
;
; PURPOSE:
;       Compute correlation between geomagnetic indeces Dst and DH for n Geomagnetic storm
;       events.
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
;       .r corr_plot
;       corr_plot
;
;       Description:
;       ???????????????????????
;
;
; PARAMETERS:
;       Dst index    : DH index
;
; KEYWORD PARAMETERS:
;
;       /????????? : ?????????????
;
; DEPENDENCIAS:a
;       ?????????? : ????????????
;
; ARCHIVOS ANALIZADOS:
;       ??????????
;
; ARCHIVOS DE SALIDA:
;       figura .PNG
; HISTORIA:
;       versión 1.3
;###############################################################################
;###############################################################################0.5, .1, 0.01, 0.35
FUNCTION svdfit_funct, X, m
   RETURN,[ [((x+1)/(x-1) + x + 1)] ]
END
;###############################################################################
;###############################################################################
PRO corr_plot;, idate, fdate

        input_dir  = '/home/isaac/geomstorm/datos/tgm/article_events/'
        path       = input_dir				

       ; header = 1      ; Defining number of lines of the header 

       ; idate = string(iyr, imh, idy, format = '(I4, I02, I02)')
      ;  fdate = string(fyr, fmh, fdy, format = '(I4, I02, I02)')
                
        
        data_files         = FILE_SEARCH(input_dir+'tgmdata'+'?????????????????'+'.txt')
        ;print,  data_files
        files_lines_number = FILE_LINES(data_files)
        MX_latitude        = 28.06*!Pi/180.
       ; corellation_index  = FLTARR(N_ELEMENTS(data_files));<<<<<<<<<<<<<<<<preguntar
        
        limits = [0, -10, -20, -30, -40, -50, -60, -70, -80, -90, -100, -110, -120, -140, -160, -200]
        
;print, data_files

        dat_str0 = { doy: INTARR(MAX(files_lines_number)), $
                    hour: FLTARR(MAX(files_lines_number)), $
                     dst: INTARR(MAX(files_lines_number)), $
                     dh: FLTARR(MAX(files_lines_number)), $
                     number_of_lines: 0 , $
                     correlation: FLTARR(N_ELEMENTS(limits)), $
                     correlation_points : INTARR(N_ELEMENTS(limits))}
        
        data = REPLICATE(dat_str0, N_ELEMENTS(data_files))
        data[*].number_of_lines = files_lines_number
;print, data[*].number_of_lines
        data[*].number_of_lines = files_lines_number-1
        data[*].correlation[*]  = 999.
;print, data[*].number_of_lines
;Return
         dh = FLTARR(1)
         
         dst = FLTARR(1)
;###############################################################################
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
                
                dh  = [dh, tmp_data[*].dh]
                dst = [dst, tmp_data[*].dst*cos(MX_latitude)]
               ; PRINT, correlate(dh, dst)^2
                tmp_index = WHERE(data[event].dh[*] GE 999.)
                data[event].dst[tmp_index] = 999.                
        ENDFOR
;###############################################################################
    correlation = FLTARR(N_ELEMENTS(data_files))
        FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
            correlation[event] = CORRELATE(data[event].dst[0:data[event].number_of_lines-1], $
            data[event].dh[0:data[event].number_of_lines-1])
            
         ;   PRINT, N_ELEMENTS(data[*].dst[0:data[event].number_of_lines-1])
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
   ; print, N_ELEMENTS(limits)            
        for i=0, N_ELEMENTS(limits)-1 DO BEGIN
                ;print, n_elements(limits), limits
               ; print, data[*].correlation[i]
                index_tmp = WHERE(data[*].correlation[i] LT 100)
               ; print, N_ELEMENTS(index_tmp)
              ;  print, data[index_tmp].correlation[i]^2
                means[i] = Mean( data[index_tmp].correlation[i]^2 )
                devs[i]  = STDDEV( data[index_tmp].correlation[i]^2, /NAN)
                ;maxs[i]  = MAX( data[index_tmp].correlation[i] )
                ;mins[i]  = MIN( data[index_tmp].correlation[i] )
               ; print, means[i], devs[i];, maxs[i], mins[i]
        ENDFOR
    
;###############################################################################          
; define device and color parameters       
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        Xsize=fix(1600)
        Ysize=600
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]

        amarillo  = 220
        gris      = 130
        blanco    = 255
        negro     = 0
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 0, /SILENT
    H = TeXtoIDL('\DeltaH')
        event = 0
        PLOT, limits[WHERE(data[event].correlation_points[*] GT 3)]+0.5*(limits[1]+limits[0]), $
        data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
        MAX=1., XRANGE=[10.,-210], YRANGE=[0, 1.0], BACKGROUND = blanco, $
        Xtitle='Dst', Ytitle='R!U2!N', Xstyle=5, Ystyle=5, /NODATA, color=negro,$
        CHARSIZE=1.9, POSITION=[0.08,0.12,0.9,0.9], TITLE='Correlation between Dst & '+ H, $
        CHARTHICK=2.0


        POLYFILL, [10.,-210.,-210.,10.], [0.,0.,0.5,0.5], color=amarillo

        AXIS, Xaxis=0, Xtitle='Dst [nT]', CHARSIZE=1.8, color=negro, XRANGE=[10.,-210], Xstyle=1, $
        CHARTHICK=3
        AXIS, Xaxis=1, color=negro, XTICKNAME=REPLICATE( ' ', 8 ), XRANGE=[10.,-210], $
        CHARTHICK=3
        
        AXIS, Yaxis=0, Ytitle='R!U2!N', CHARSIZE=1.8, color=negro, Ystyle=1, $
        CHARTHICK=3
        AXIS, yaxis=1, color=negro, YTICKNAME=REPLICATE( ' ', 7 ), $
        CHARTHICK=3

        
    symbols =[4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4]
    colors  =[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]*gris

        FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
               ; oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
               ; data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
               ; psym=symbols[event], color=colors[event], thick=4, symsize=1.5
                
                ;oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
                ;data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
                ;color=colors[event]  
                ;PRINT, data[event].correlation_points[*]              
        ENDFOR
      ;  print, !Y.CRANGE
        
        x_correlation = (!X.CRANGE[1]-!X.CRANGE[0])*FINDGEN(101)/100+!X.CRANGE[0]
        y_correlation = INTERPOL(means[*],limits[*], x_correlation)
       ; oplot, x_correlation[*], y_correlation[*], color=negro, thick=4

        xvec = (FINDGEN(16)*(-20))
        ;print, xvec
        
        ;print, N_ELEMENTS(means[*])
        yvec      = FLTARR(N_ELEMENTS(x_correlation[*]))
        yvec2      = FLTARR(N_ELEMENTS(x_correlation[*]))  
        ;X = FINDGEN(16)*(-10) + 10.
        
        ;Se declara la función:
     ;   func = svdfit_funct(xvec, 0.5, .1, 0.01, 0.35, .01, 0.1)

              
        fit = POLY_FIT(xvec, means[*], 6, MEASURE_ERRORS=devs[*], YFIT=yvec)
        line_fit = INTERPOL(yvec, N_ELEMENTS(x_correlation[*]))      
        
        ;M=[1.0, 1.0, 1.0]
        M=[1.0]
        
        fit2 = SVDFIT(xvec, means[*], A=M, MEASURE_ERRORS=devs[*], FUNCTION_NAME='svdfit_funct', YFIT=yvec2)
        line_fit2 = INTERPOL(yvec2, N_ELEMENTS(x_correlation[*]), /QUADRATIC, /SPLINE) 
               
        OPLOT, x_correlation[*], line_fit, color=negro, thick=2
       ; OPLOT, x_correlation[*], line_fit2, color=negro, thick=4, LINESTYLE=2        
        print, '################################################################'
        print, fit
        print, '################################################################'
      ;  print, N_ELEMENTS(x_correlation[*]), N_ELEMENTS(line_fit2)                
        print, '################################################################'
              
        
        A = FINDGEN(17) * (!PI*2/16.)
        ERRPLOT, limits[0:12]+5, means[0:12]+devs[0:12], means[0:12]-devs[0:12], color=negro
        ERRPLOT, limits[13:14]+10, means[13:14]+devs[13:14], means[13:14]-devs[13:14], color=negro
        ERRPLOT, limits[15]+20, means[15]+devs[15], means[15]-devs[15], color=negro         
               
        USERSYM, COS(A), SIN(A), /FILL             
                
        oplot, limits[0:12]+5, means[0:12], color=negro, PSYM = 8, symsize=1
        oplot, limits[13:14]+10, means[13:14], color=negro, PSYM = 8, symsize=1
        PLOTS, limits[15]+20, means[15], color=negro, PSYM = 8, symsize=1                                   
;###############################################################################       
    Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   
;###############################################################################
; open the post stript device
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'correlation_case.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'correlation_case.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'correlation_case.png'
                print, ''
        ENDIF
        RETURN 	


RETURN
END




