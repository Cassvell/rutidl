FUNCTION coe, date
	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
;reading data files
        date = string(year, month, day, format = '(I4, I02, I02)')
        path='/home/c-isaac/geomstorm/datos'		
		file_name = path+'/coeneo/aux_'+date+'.clean.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;###############################################################################
;extracting data and denfining an structure data
        DStruct = {year : 0, month : 0, day : 0, hr : 0, min : 0, seg : 0., doy : 0., $
                   D : 0., H :0., Z : 0., F : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,X,F6,X,I03,F12,F10,F10,F10)'		
		RETURN, teo_mag		
END

pro a_fourier, date_i, date_f, JPEG = jpeg 

	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	

    tot_days= findgen(file_number*1440)/1440.0  
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')    
;###############################################################################
idate0 = string(yr_i, mh_i, format='(I4,I02)') 
;###############################################################################
; define DH variables
;###############################################################################        
        data_path='/home/c-isaac/geomstorm/datos' 
        data_file_name_h  = strarr(file_number)                                   
                                              
        string_date        = strarr(file_number)
        string_date_2        = strarr(file_number)        
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                string_date_2[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')   
                		           
                data_file_name_h[i]  = data_path+'/coeneo/'+'aux_'+string_date[i]+'.clean.dat'                
		                        		        
		        file = FILE_SEARCH(data_file_name_h[i], COUNT=opened_files)        
                            
        ENDFOR

        exist_data_file_h   = FILE_TEST(data_file_name_h)
        capable_to_plot_h   = N_ELEMENTS(where(exist_data_file_h EQ 1))

     
        IF capable_to_plot_h NE N_ELEMENTS(data_file_name_h) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.dh_index.',A,' impossible to plot all data.')"              
        ENDIF
;###############################################################################
; Generate the time variables to plot D time series  
        D    = FLTARR(file_number*1440)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_h)-1 DO BEGIN
                IF exist_data_file_h[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_h = coe([tmp_year, tmp_month, tmp_day])
                        
                        D[i*1440:(i+1)*1440-1] = d_h.D[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        D[i*1440:(i+1)*1440-1] = 999999.0
                ENDELSE                
        ENDFOR
;###############################################################################
; Generate the time variables to plot D time series  
        Z    = FLTARR(file_number*1440)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_h)-1 DO BEGIN
                IF exist_data_file_h[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_h = coe([tmp_year, tmp_month, tmp_day])
                        
                        Z[i*1440:(i+1)*1440-1] = d_h.Z[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        Z[i*1440:(i+1)*1440-1] = 999999.0
                ENDELSE                
        ENDFOR
;###############################################################################        
;identifying NAN values in the Time Series
        i_nan1 = where(D eq 999999.0, ncount)
    ;    i_nan2 = where(dH gt 100.0, n2count)
        
        prcent_nan = FLOAT(ncount)*100.0
        print,'porcentaje de valores NaN:', prcent_nan/n_elements(D),'%'        
;############################################################################### 
        for i=0, n_elements(D)-1 do begin
            if D[i] eq 9999.0 then begin
                D[where(D[*] eq 9999.0)] = !Values.F_NAN          
            endif
        endfor
        
        for i=0, n_elements(D)-1 do begin
            if D[i] EQ 99999.0 then begin
                D[where(D[*] EQ 99999.0)] = !Values.F_NAN          
            endif
        endfor  
                      
    ;implementar una función de interpolación en caso de que el porcentaje de 
    ;nan sea muy bajo
       
    D_tmp   = D
    D_exist = where(finite(D_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then D_tmp[baddata] = interpol(D_tmp[D_exist], D_exist, baddata)
    D = D_tmp   
;############################################################################### 
        for i=0, n_elements(Z)-1 do begin
            if Z[i] eq 999999.0 then begin
                Z[where(Z[*] eq 999999.0)] = !Values.F_NAN          
            endif
        endfor
        
        for i=0, n_elements(Z)-1 do begin
            if Z[i] EQ 99999.0 then begin
                Z[where(Z[*] EQ 99999.0)] = !Values.F_NAN          
            endif
        endfor  
                      
    ;implementar una función de interpolación en caso de que el porcentaje de 
    ;nan sea muy bajo
       
    Z_tmp   = Z
    Z_exist = where(finite(Z_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then Z_tmp[baddata] = interpol(Z_tmp[Z_exist], Z_exist, baddata)
    Z = Z_tmp         
;###############################################################################  

n       = n_elements(D) 
time    = 3600.0

fn      = FLOAT(1.0/(2.0*time)) ; frecuencia de Nyquist

y_D     = FFT(D)
y_Z     = FFT(Z)         

pws_D     = abs(y_D[0:n/2])^2
pws_Ds   = smooth(pws_D, 1)

pws_Z     = abs(y_Z[0:n/2])^2
pws_Zs   = smooth(pws_Z, 1)

f_k     = (1+findgen(n))/(n*time)
print, 'Nyquist freq: ', fn, 'Hz'         
;###############################################################################
; define device and color parameters 
        Device_bak2 = !D.Name         
        SET_PLOT, 'Z'      
        
        Xsize=fix(1600)
        Ysize=1000
        DEVICE, SET_RESOLUTION = [Xsize,Ysize],Set_Pixel_Depth=24, DECOMPOSED=1  
        DEVICE, z_buffer=4
        DEVICE, set_character_size = [10, 12] 
        
        chr_size1 = 0.9
        chr_thick1= 1.5
        space     = 0.015
        rojo      = 248
        amarillo  = 190
        verde     = 150
        negro     = 0
        azul      = 70
        blanco    = 255
        gris      = 110
        morado    = 16
        naranja  = 220
                
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT
     X_label   = STRARR(file_number+1)+' '
        months    = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic']
        old_month = mh_i
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, $
                        FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
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
       days = intarr(file_number+1)
       for i=0, n_elements(days)-1 do begin
            days[i] = dy_i+i
       endfor
       days = days*24/24. 
       day_time = findgen(24)   
;############################################################################### 
    period =  N_ELEMENTS(D)/60
    time_title = ' Tiempo Universal [dias de '+old_month+'].'
    window_title = STRING(old_month, dy_f, yr_i, FORMAT='(A,X,I2,"," ,X, I4)')
    
    periodo = 'Periodo: '+string(period, FORMAT='(I2)')+' [h]'        
;###############################################################################               
    PLOT, f_k, pws_Ds, /XLOG, /YLOG, POSITION=[0.07,0.1,0.95,0.9],$
    BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA    

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.97   
   XYOUTS, X, y, window_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5               
;###############################################################################           
    i = WHERE(f_k GE 1e-4 AND f_k LE 0.5)    
    pws_tw = pws_Ds[i]
    
    ysup = MAX(pws_tw)
    yinf = MIN(pws_tw)
    f_min= 1e-4
    f_may= 0.5
  
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0)]
               
   ; periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0]
    
    PLOT, f_k, pws_Ds, /XLOG, /YLOG, XRANGE = [f_min, fn], POSITION=[0.07,0.1,0.45,0.9],$
    YRANGE=[yinf, ysup], BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA,$
    /NOERASE

    OPLOT, f_k, pws_Ds, COLOR=azul, THICK=5    
;###############################################################################    
        AXIS, XAXIS = 0, XRANGE=[f_min, fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frecuencia [Hz]',$
                         COLOR=negro, $
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[f_min, fn], $;.0/(!X.CRANGE), $
                         /XLOG,$
                       ;  XTICKS=6,$
                      ;   XMINOR=4,$
                        ; XTICKV=freqs,$                         
                        ; XTICKN=STRING(periods, FORMAT='(F4.1)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.2,$
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                         COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                         COLOR=negro, $
                         /ylog,$
                         ystyle=1, $
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5

   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.935, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.02, y, 'Componente espectral, componente D [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5     
;###############################################################################     
    pws_tw = pws_Zs[i]    
    ysup = MAX(pws_tw)
    yinf = MIN(pws_tw)
            
    PLOT, f_k, pws_Zs, /XLOG, /YLOG, XRANGE = [f_min, fn], POSITION=[0.55,0.1,0.95,0.9],$
    YRANGE=[yinf, ysup], BACKGROUND = blanco, COLOR=negro, $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA,$
    /NOERASE

    OPLOT, f_k, pws_Zs, COLOR=rojo, THICK=5    
    
        AXIS, XAXIS = 0, XRANGE=[f_min, fn], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = 'Frecuencia [Hz]',$
                         COLOR=negro, $
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[f_min, fn], $;.0/(!X.CRANGE), $
                         /XLOG,$
                       ;  XTICKS=6,$
                      ;   XMINOR=4,$
                        ; XTICKV=freqs,$                         
                        ; XTICKN=STRING(periods, FORMAT='(F4.1)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.2,$
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                         COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                         COLOR=negro, $
                         /ylog,$
                         ystyle=1, $
                         CHARSIZE = 1.0,$
                         CHARTHICK=1.5

   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.935, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.52, y, 'Componente espectral, componente Z [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5     
;###############################################################################
; saving png
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!    
    TVLCT, R_bak, G_bak, B_bak, /get  
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak2  
    path = '../rutidl/output/SDP/'
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'SDP_DZ'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'SDP_DZ'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'SDP_DZ'+Date+'.png'
                print, ''
        ENDIF
        RETURN   
end	
