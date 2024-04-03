;+ 
; NAME:
;       scatter_plot.pro
;
;
; PURPOSE:
;
;       statistic data analysis
;
; AUTHOR:
;
;       Pedro Corona Romero
;       C. Isaac Castellanos Velazco
;       Instituto de Geofisica, UNAM
;       UNAM Campus Morelia, Antigua Carretera a Patzcuaron,
;       Exhacienda de San Jose del Cerrito, Morelia, Michoacan, Mexico
;       piter.cr@gmail.com
;       ccastellanos@igeofisica.unam.mx
;       06.iv.mmxxii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;       .r scatter_plot
;       scatter_plot
;
;       Description:
;       ???????????????????????
;
;
; PARAMETERS:
;       Dst index    : dH index
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
; ARCHIVOS DE SALIDA: grafica.png de dispersión
;
; HISTORIA:
;   versión 1.1, agosto 2022






PRO scatter_plot, PNG=png, PS=ps
	@set_up_commons
	set_up
        input_dir  = set_var.Mega_dir+'/article_events/dst_lambda/'
        path       = set_var.local_dir+'output/'

	;iyr	= idate[0]
	;imh	= idate[1]
	;idy = idate[2]
	
	;fyr	= fdate[0]
	;fmh	= fdate[1]
	;fdy = fdate[2]					

       ; header = 1      ; Defining number of lines of the header 

       ; idate = string(iyr, imh, idy, format = '(I4, I02, I02)')
      ;  fdate = string(fyr, fmh, fdy, format = '(I4, I02, I02)')
                
        
        data_files         = FILE_SEARCH(input_dir+'tgmdata'+'?????????????????'+'.dat')
         
        files_lines_number = FILE_LINES(data_files)
        MX_latitude        = 28.06*!Pi/180.
       ; corellation_index  = FLTARR(N_ELEMENTS(data_files));<<<<<<<<<<<<<<<<preguntar
        
        limits = [0, -40, -50, -60, -80, -100, -120, -200];[-50, -200]
        
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
                FORMAT='(I03,X,F4.1,X,I04,F6.1)'
                
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
                              
                dh  = [dh, tmp_data[*].dh]
                dst = [dst, tmp_data[*].dst*cos(MX_latitude)]
               ; PRINT, correlate(dh, dst)^2
                tmp_index = WHERE(data[event].dh[*] GE 999.)
                data[event].dst[tmp_index] = 999.

        ENDFOR

        PRINT, 'Correlación global'
        PRINT, CORRELATE(dh, dst)^2
        
        idx = WHERE(dst GE -20 AND dst LT 60)      
        j =   WHERE(dst LT -20 AND dst GE -250)

        idx2 = WHERE(dst GE -50 AND dst LT 60)      
        j2 =   WHERE(dst LT -50 AND dst GE -250)
 
        idx3 = WHERE(dst GE -100 AND dst LT 60)      
        j3 =   WHERE(dst LT -100 AND dst GE -250)
                
        corr1 = CORRELATE(dh[idx], dst[idx])^2
        corr2 =  CORRELATE(dh[j], dst[j])^2
        PRINT, 'límite, -20 nT'
        PRINT, corr1
        PRINT, corr2

        corr1 = CORRELATE(dh[idx2], dst[idx2])^2
        corr2 =  CORRELATE(dh[j2], dst[j2])^2
        PRINT, 'límite, -50 nT'
        PRINT, corr1
        PRINT, corr2

        corr1 = CORRELATE(dh[idx3], dst[idx3])^2
        corr2 =  CORRELATE(dh[j3], dst[j3])^2
        PRINT, 'límite, -100 nT'
        PRINT, corr1
        PRINT, corr2
        
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

  ;  PRINT,  '    corr_med', '    corr_std'      
        means=FLTARR(N_ELEMENTS(limits))
        devs =FLTARR(N_ELEMENTS(limits))
        maxs =FLTARR(N_ELEMENTS(limits))
        mins =FLTARR(N_ELEMENTS(limits))
        ;PRINT, means
        for i=0, N_ELEMENTS(limits)-1 DO BEGIN
                ;PRINT, n_elements(limits), limits
            ;    PRINT, data[*].correlation[i]
                index_tmp = WHERE(data[*].correlation[i] LT 100)
               ; PRINT, index_tmp
                ;PRINT, data[index_tmp].correlation[i]
               ; means[i] = Mean( data[index_tmp].correlation[i]^2 )
                ;devs[i]  = STDDEV( data[index_tmp].correlation[i]^2, /NAN)
                ;maxs[i]  = MAX( data[index_tmp].correlation[i] )
                ;mins[i]  = MIN( data[index_tmp].correlation[i] )
               ; PRINT, means[i];, devs[i];, maxs[i], mins[i]
                PRINT, ''
               ; PRINT, data[index_tmp].correlation[i]^2;, data[index_tmp].doy
        ENDFOR
    IF keyword_set(png) THEN BEGIN
    makefig_png, dst, dh, med, desv, path
    ENDIF

    IF keyword_set(ps) THEN BEGIN
    makefig_ps, dst, dh, med, corr1, corr2, desv, path
    ENDIF
     
RETURN
END


PRO makefig_ps, dst, dh, med, R1, R2, desv, path

 ;   LOADCT, 0, /SILENT

    ;path = '../rutidl/output/eventos_tgm/'
  ;  H = TeXtoIDL('elta')


psfile = path+'dispersion_general_dst.eps'
    
cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=3., font=0, /encapsulated, $
     XSize=6, YSize=6, /LANDSCAPE    
        event = 0
   up0  =  100
   down0=-250        
   slope = findgen(601)-500
   x1= slope
   y1= slope
   cgplot, x1,y1, xstyle=5, ystyle=5, color='black', background='white', YRANGE=[down0,up0], $
   XRANGE=[down0,up0], CHARSIZE = 1.7, CHARTHICK=1.2, Xtitle='', Ytitle='H',$
   POSITION=[0.16,0.12,0.96,0.96], TITLE='', /NODATA                 
    
    
  ;  POLYFILL, [!X.CRANGE[0], 0, 0, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], 0, 0], COLOR=verde
    
   ; POLYFILL, [!X.CRANGE[0], -20, -75, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -75, -75], $
   ; COLOR=amarillo, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=0.03, SPACING=0.3
    
   ; POLYFILL, [!X.CRANGE[0], -75, -75, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -75, -75], $
   ; COLOR=amarillo, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=0.03, SPACING=0.3                

;    cgPolygon, [!X.CRANGE[0], -50, -50, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -50, -50], $
 ;   Color = cgColor("GRN2"),  /FILL

    cgPolygon, [!X.CRANGE[0], -100, -100, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -100, -100], $
    Color = cgColor("GRN3"), /FILL
    
        AXIS, Xaxis=0, Xtitle= '', CHARSIZE=1.6, XRANGE=[down0,up0], Xstyle=1, $
        CHARTHICK=2
        AXIS, Xaxis=1, XTICKNAME=REPLICATE( ' ', 8 ), XRANGE=[down0,up0], $
        CHARTHICK=2
        
        AXIS, Yaxis=0, Ytitle='', CHARSIZE=1.6, color=negro, Ystyle=1, $
        CHARTHICK=2
        AXIS, yaxis=1, YTICKNAME=REPLICATE( ' ', 7 ), $
        CHARTHICK=2

 ;   symbols =[1,2,4,5,6,7]
 ;   colors  =[azul,rojo,amarillo,negro,naranja,verde]
    
     ;   FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
     ;           oplot, data[event].dst[*], data[event].dh[*], $
     ;           psym=symbols[event], color=colors[event], thick=1, symsize=1
                
              ;  oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
              ;  data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
              ;  color=colors[event]                
     ;   ENDFOR

   cgOPLOT, dst, dh, PSYM=4, Color = cgColor("Charcoal"), SYMSIZE=0.5        
        
    cgOPLOT, x1, y1, THICK=1.0, COLOR='black' 
;###############################################################################                             
;###############################################################################
    med = texToidl('R^2 = ')
        XYOUTS, 0.2, 0.75 , /NORMAL, $
             ;   med, COLOR=negro, $
                STRING(med, R1, FORMAT='(A, F4.2)') , $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
    desv = texToidl('(100 \leq dst \leq -100)')                
        XYOUTS, 0.2, 0.8 , /NORMAL, $
                 desv, $
                ;desv+' '+corr_std, COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1 
;###############################################################################
    med = texToidl('R^2 = ')
        XYOUTS, 0.63, 0.25 , /NORMAL, $
             ;   med, COLOR=negro, $
                STRING(med, R2, FORMAT='(A, F4.2)') , $
                CHARSIZE = 1.3, $
                CHARTHICK=chr_thick1   
                
    desv = texToidl('(-100 \leq dst \leq -250)')                
        XYOUTS, 0.63, 0.20 , /NORMAL, $
                desv , $
                ;desv+' '+corr_std, COLOR=negro, $
                CHARSIZE = 1.3, $
                CHARTHICK=chr_thick1
                
    d_H = TeXtoIDL('\DeltaH [nT]')
    dt = TeXtoIDL('\DeltaT')
    dstlbd = TeXtoIDL('Dst_\lambda [nT]')

    
    y = (.75 - 0.25) / 2. + 0.25
    XYOUTS, 0.04, y, d_H, /NORMAL, $
    ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=1.8                   
                
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.02, dstlbd, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, CHARTHICK=1.5         
           
  ; pic = cgSnapshot(/PNG, FILENAME='test_1')             
   cgPS_Close, density = 300, height = 600;, /PNG, IM_options='-trim -bordercolor white -border 50', /DELETE_PS

                
    
    
RETURN                
END


PRO makefig_png, dst, dh, med, desv, path

;###############################################################################          
; define device and color parameters 
;###############################################################################      
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        Xsize=fix(600)
        Ysize=600
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]
             
        chr_size1 = 0.9
        chr_thick1= 1.1
        space     = 0.015
        rojo      = 248
        amarillo  = 200
        naranja   = 220
        verde     = 150
        negro     = 0
        azul      = 100
        blanco    = 255
        gris      = 90
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 0, /SILENT

    ;path = '../rutidl/output/eventos_tgm/'
    H = TeXtoIDL('\DeltaH')
    dt = TeXtoIDL('\DeltaT')
    dstlbd = TeXtoIDL('Dst(\lambda)')
        event = 0
   up0  =  100
   down0=-250        
   slope = findgen(601)-500
   x1= slope
   y1= slope
   plot, x1,y1, xstyle=5, ystyle=5, color=negro, background=blanco, YRANGE=[down0,up0], $
   XRANGE=[down0,up0], CHARSIZE = 1.7, CHARTHICK=1.2, Xtitle='Dst', Ytitle='H',$
   POSITION=[0.17,0.1,0.9,0.9], TITLE='GS Events: dispersion study', /NODATA                 
    
    
  ;  POLYFILL, [!X.CRANGE[0], 0, 0, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], 0, 0], COLOR=verde
    
   ; POLYFILL, [!X.CRANGE[0], -20, -75, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -75, -75], $
   ; COLOR=amarillo, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=0.03, SPACING=0.3
    
    POLYFILL, [!X.CRANGE[0], -75, -75, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -75, -75], $
    COLOR='yellow', /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=0.03, SPACING=0.3                

  ;  POLYFILL, [!X.CRANGE[0], -50, -50, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -50, -50], $
  ;  COLOR=amarillo

    POLYFILL, [!X.CRANGE[0], -100, -100, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -100, -100], $
    COLOR=naranja
    
        AXIS, Xaxis=0, Xtitle= 'Dst [nT]', CHARSIZE=1.1, color=negro, XRANGE=[down0,up0], Xstyle=1, $
        CHARTHICK=2
        AXIS, Xaxis=1, color=negro, XTICKNAME=REPLICATE( ' ', 8 ), XRANGE=[down0,up0], $
        CHARTHICK=2
        
        AXIS, Yaxis=0, Ytitle=H+' [nT]', CHARSIZE=1.1, color=negro, Ystyle=1, $
        CHARTHICK=2
        AXIS, yaxis=1, color=negro, YTICKNAME=REPLICATE( ' ', 7 ), $
        CHARTHICK=2

 ;   symbols =[1,2,4,5,6,7]
 ;   colors  =[azul,rojo,amarillo,negro,naranja,verde]
    
     ;   FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
     ;           oplot, data[event].dst[*], data[event].dh[*], $
     ;           psym=symbols[event], color=colors[event], thick=1, symsize=1
                
              ;  oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
              ;  data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
              ;  color=colors[event]                
     ;   ENDFOR

    OPLOT, dst, dh, PSYM=4, COLOR=gris, SYMSIZE=0.5        
        
    OPLOT, x1, y1, THICK=1.0, COLOR=negro 
;###############################################################################                             
;###############################################################################
    med = texToidl('R^2 = ')
        XYOUTS, 0.2, 0.79 , /NORMAL, $
             ;   med, COLOR=negro, $
                string(med, 0.77, FORMAT='(A, F4.2)') , COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
    desv = texToidl('(100 \leq dst \leq -100)')                
        XYOUTS, 0.2, 0.84 , /NORMAL, $
                 desv, COLOR=negro, $
                ;desv+' '+corr_std, COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1 
;###############################################################################
    med = texToidl('R^2 = ')
        XYOUTS, 0.2, 0.65 , /NORMAL, $
             ;   med, COLOR=negro, $
                string(med, 0.42, FORMAT='(A, F4.2)') , COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
    desv = texToidl('(-100 \leq dst \leq -250)')                
        XYOUTS, 0.2, 0.70 , /NORMAL, $
                desv , COLOR=negro, $
                ;desv+' '+corr_std, COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1 

    Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   


;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; open the post stript device
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        PRINT, '        Setting PNG as default file type.'
        WRITE_PNG, path+'dispersion_general_dst.png', Image, reds,greens,blues
                PRINT, '        Saving: '+path+'dispersion_general_dst_ld.png'
                PRINT, ''
    RETURN
END
