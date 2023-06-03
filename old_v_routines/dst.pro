FUNCTION dst_data, date

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    
	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, format = '(I4,"-",I02,"-",I02)')
		file_name = '../master_thesis/datos/dst/daily/dst_'+date+'.txt'
		;print, date
        header = 1             ; Defining number of lines of the header 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'		
;	    IF opened_files NE N_ELEMENTS(file) THEN BEGIN
;	        file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_P.dat'
;	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    ;IF opened_files NE N_ELEMENTS(file) THEN BEGIN
    	   ;     file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_Q.dat'
	      ;      file = FILE_SEARCH(file_name, COUNT=opened_files)    	        
    	 ;       IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  
    	;    ENDIF    	    	    
	  ;  ENDIF

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
        second : 0, DOY : 0, Dst: 0}

		r_dst = REPLICATE(DataStruct, number_of_lines-header)	        
        
		READS, data[header:number_of_lines-1], r_dst, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,X,I2,I04,I5)'
		RETURN, r_dst
END

PRO dst, date_i, date_f

	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
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
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = strarr(file_number)
        string_date     = strarr(file_number)
       
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')
                data_file_name[i] = '../rutidl/dst/daily/'+'dst_'+string_date[i]+'.txt'
        ENDFOR
        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.dst_index.',A,' impossible to plot all data.')"              
        ENDIF

        dst    = FLTARR(file_number*24)                               
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i] = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')                
                        dat = dst_data([tmp_year, tmp_month, tmp_day])
                        
                        dst[i*24:(i+1)*24-1] = dat.Dst[*]                                                
                                                                                              
                ENDIF ELSE BEGIN
                         dst[i*24:(i+1)*24-1] = 999999.0                      
                ENDELSE                
        ENDFOR

        time = FINDGEN(file_number * 24)/24.0
        path = '../rutidl/output/eventos_tgm/'

        Device_bak = !D.Name 
        SET_PLOT, 'Z'

        
        Xsize=fix(800)
        Ysize=550
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

     X_label   = STRARR(file_number+1)+' '
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        old_month = mh_i
        ;print, old_month
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : string(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR        

case old_month of
    1: old_month = 'Enero'
    2: old_month ='Febrero'
    3: old_month ='Marzo'
    4: old_month ='Abril'
    5: old_month ='Mayo'
    6: old_month ='Junio'
    7: old_month ='Julio'
    8: old_month ='Agosto'
    9: old_month ='Septiembre'
    10:old_month ='Octubre'
    11:old_month ='Noviembre'
    12:old_month ='Diciembre'
    else: print, 'fuera de rango'
endcase                 
;############################################################################### 
    time_title = ' Tiempo Universal [dias]'
    window_title =  STRING(old_month, yr_i, FORMAT='(A, X, I4)')                                               
;###############################################################################                                    
     up = max(dst)
     down=min(dst)
    ; print, up, down
     PLOT, time, dst, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, $
     COLOR=negro, CHARSIZE = 0.9, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.15,0.9,0.9], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], THICK=4,/NODATA
;FASE INICIAL         
    POLYFILL, [time[38],time[46],time[46],time[38]], $
    [!Y.CRANGE[0],!Y.CRANGE[0],!Y.CRANGE[1],!Y.CRANGE[1]], COLOR=verde, $
    /LINE_FILL, LINESTYLE=1, SPACING=0.01
    
    POLYFILL, [time[38],time[46],time[46],time[38]], $
    [!Y.CRANGE[0],!Y.CRANGE[0],!Y.CRANGE[1],!Y.CRANGE[1]], COLOR=verde, $
    /LINE_FILL, LINESTYLE=1, SPACING=0.01, ORIENTATION=90    
;FASE PRINCIPAL    
    POLYFILL, [time[46],time[55],time[55],time[46]], $
    [!Y.CRANGE[0],!Y.CRANGE[0],!Y.CRANGE[1],!Y.CRANGE[1]], COLOR=rojo, $
    /LINE_FILL, LINESTYLE=1, SPACING=0.01
    
    POLYFILL, [time[46],time[55],time[55],time[46]], $
    [!Y.CRANGE[0],!Y.CRANGE[0],!Y.CRANGE[1],!Y.CRANGE[1]], COLOR=rojo, $
    /LINE_FILL, LINESTYLE=1, SPACING=0.01, ORIENTATION=90 

;FASE DE RECUPERACIÓN    
    POLYFILL, [time[55],time[144],time[144],time[55]], $
    [!Y.CRANGE[0],!Y.CRANGE[0],!Y.CRANGE[1],!Y.CRANGE[1]], COLOR=amarillo, $
    /LINE_FILL, LINESTYLE=1, SPACING=0.01
    
    POLYFILL, [time[55],time[144],time[144],time[55]], $
    [!Y.CRANGE[0],!Y.CRANGE[0],!Y.CRANGE[1],!Y.CRANGE[1]], COLOR=amarillo, $
    /LINE_FILL, LINESTYLE=1, SPACING=0.01, ORIENTATION=90                        
;###############################################################################  
    OPLOT, time, dst, THICK=4, LINESTYLE=0, COLOR=negro
;###############################################################################  
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                        ; XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 1.2 , $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number], $
                         XTICKFORMAT="(A1)", $                                                    
                         XMINOR=8, $ 
                         CHARSIZE = 1.2 , $                       
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                     ;    YTITLE = 'Dst [nT]', $                          
                         COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                         COLOR=negro, $                                                                      
                         YSTYLE=2, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5    
;###############################################################################                         
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.95   
   XYOUTS, X, y, 'Mayo y Junio, 2017', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.85, CHARTHICK=1.5    
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.05 
   XYOUTS, X, y, time_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5    
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.03, y, 'Dst [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5                                  
;###############################################################################
; saving png
;###############################################################################     
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak 
    path = '../rutidl/output/eventos_tgm/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = R_bak[image]
                true_image[1,*,*] = G_bak[image]
                true_image[2,*,*] = B_bak[image]
                write_jpeg, path+'dst_ex'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'dst_ex'+Date+'.png', Image, R_bak, G_bak, B_bak
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'dst_ex'+Date+'.png'
                print, ''
        ENDIF
        RETURN 	                                    
END













