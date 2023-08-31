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

PRO dh_dst_plot, date_i, date_f
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
    Date = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT  = '(I4,"-",I02,"-",I02,"_",I4,"-",I02,"-",I02)')    
;define station data
	station_idx = ''
	PRINT, 'Enter GMS idx: 0:coe, 1:teo, 2:tuc, 3:bsl, 4:itu'
	READ, station_idx, PROMPT = '> '
    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu	    
;###############################################################################
; Generate the time series DH and Dst                                
    H   = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, FIX(station_idx))
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')    
;###############################################################################                
;identifying NAN percentage values in the Time Series
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 100.0, 'greater')
;setting certain values as NaN        
    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 100.0, 'greater') 
        
    time_w = TIMEGEN(N_ELEMENTS(file_number), final=JULDAY(mh_f, dy_f, yr_f, 23), $
                start=JULDAY(mh_i, dy_i, yr_i, 0) , units='H')

    CALDAT, time_w, m, d, y, hr

    ;h_min =  MIN(H, j)
    dst_min =  MIN(dst, j)
        
    PRINT, "fecha y hora del mínimo [mm/dd hr]"
    PRINT, STRING(y[j], m[j], d[j], hr[j], ':00:00', FORMAT='(I04,"/",I02,"/",I02,2X,I02,A)')
    
    PRINT, "periodo de desarrollo hasta el mínimo"  
    FOR i = 0, 24 DO BEGIN
        ;PRINT, m[(j+36)-i], d[(j+36)-i], hr[(j+36)-i], H[(j+36)-i], FORMAT='(I02,"/",I02,2X,I02,3X,I04)'
        PRINT, m[(j)-i], d[(j)-i], hr[(j)-i], dst[(j)-i], FORMAT='(I02,"/",I02,2X,I02,3X,I04)'
    ENDFOR                                                                        
;###############################################################################
; define device and color parameters       
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        Xsize=fix(1200)
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
    IF MAX(dst) GT MAX(H) THEN up = MAX(dst) ELSE up = MAX(H)
    IF MIN(dst) LT MIN(H) THEN down = MIN(dst) ELSE down = MIN(H)
    
    window_title = 'SGS '+ STRING(old_month, yr_i, FORMAT='(A, X, I4)')
    PRINT, old_month
;###############################################################################
; Plot data     
     dH = TeXtoIDL('\DeltaH')    
    
    PLOT, tot_days, dst, XTICKS=file_number, XMINOR = 8, POSITION=[0.1,0.1,0.9,0.9],$
    XTICKFORMAT='LABEL_DATE', XSTYLE = 5, YSTYLE=5, YRANGE=[down-10, up+10],$
    TITLE = window_title, BACKGROUND = blanco, COLOR=negro, XRANGE=[0, file_number],$
	YTITLE = dH, THICK=2, CHARSIZE=1.6

    OPLOT, tot_days, H, COLOR=rojo, LINESTYLE=0, THICK=4    
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTITLE='Tiempo Universal [dias]', $  
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

        AXIS, YAXIS = 0, YRANGE=[down-10,up+10], $
                         YTITLE = dH+' y Dst [nT]', $
                         YSTYLE=1, $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 0.9;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down-10,up+10], $       
                         COLOR=negro, $
                         YSTYLE=1, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5
;###############################################################################
;second panel legend                   
        POLYFILL, [0.77,0.8,0.8,0.77], [0.264,0.264,0.267,0.267], COLOR = rojo, /NORMAL
        POLYFILL, [0.77,0.8,0.8,0.77], [0.234,0.234,0.237,0.237], COLOR = negro, /NORMAL                
                
        XYOUTS, 0.81, 0.26 , /NORMAL, $
                dH, COLOR=negro, $
                CHARSIZE = 1.2, $
                CHARTHICK=chr_thick1   
                
        XYOUTS, 0.81, 0.23 , /NORMAL, $
                'Dst', COLOR=negro, $
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
    path = '../rutidl/output/new_events/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'gmindex'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'gmindex'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'gmindex'+Date+'.png'
                print, ''
        ENDIF
        RETURN                  	
END
