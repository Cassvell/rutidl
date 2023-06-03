;
;Name:
;	get_data_date
;purpose:
;	this routine will call read a certain number of files containing DST 
;   magnetic index.
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   Getting the data magnetic, date and time within several files. 
;
;calling sequence:
;
;
;parameters:
;
;
;dependencies:
;
;
;input files
;
;
;output files:
;
function dst_data, initial

	On_error, 2
	compile_opt idl2, HIDDEN

	year = string(initial, format = '(I4)')
	;type_data = string(tp, format = '(A)')
		file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_D.dat'
		
        header = 25             ; Defining number of lines of the header 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		
	    IF opened_files NE N_ELEMENTS(file) THEN begin
	        file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_P.dat'
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN begin
    	        file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_Q.dat'
	            file = FILE_SEARCH(file_name, COUNT=opened_files)    	        
    	        IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  
    	    ENDIF    	    	    
	    ENDIF

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
        DOY : 0, Dst: 0}

		res_dat = REPLICATE(DataStruct, number_of_lines-header)	        
        
		READS, data[header:number_of_lines-1], res_dat, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,8X,I3,X,I6)'
		
		return, res_dat
end

function DH_teo, date

	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, format = '(I4, I02, I02)')
       ; sts  = string(stats, format = '(A5)')
		
		file_name = '../rutidl/dH_teo/'+'teo_'+date+'.dst.early'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
	   ; print, number_of_lines
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        DStruct = {hora : 0, D_stdesv : 0., D : 0., H_stdesv : 0., H : 0., $
        Z_stdesv : 0., Z : 0., N_stdesv : 0., N : 0., F_stdesv : 0., F : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(I2, F10, F8, F10, F10, F10, F10, F10, F10, F10, F10)'
		
		return, teo_mag		
end

function list_dst, res_dat, initial
    
    On_error, 2
	compile_opt idl2, HIDDEN

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;calling the get_data_date function
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	a = initial
	;b = tp
	
	dat = dst_data(a) 

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Defining variables to write output files indicating the events where Dst index
; got lower than -150 nT.
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	tmp = n_elements(dat.year)
	
	indexes = WHERE(dat.Dst LE -150, count)
	IF count LE 0 THEN MESSAGE, 'ERROR'
		
	dst0      = dat.Dst
    doy0     = dat.DOY
    year0    = dat.year
    month0   = dat.month
    day0     = dat.day
    hour0    = dat.hour 
    
    idx_dst  = dst0[indexes]
	doy     = doy0[indexes]
	year    = year0[indexes]
	month   = month0[indexes]
	day     = day0[indexes]
	hour    = hour0[indexes]
    
	idx_dst_tmp   = idx_dst*0
	doy_tmp      = doy*0
	year_tmp     = year*0
	month_tmp    = month*0
	day_tmp      = day*0
	hour_tmp     = hour*0   

    j=0l
    idx_dst_tmp[j] = idx_dst_tmp[0]
	year_tmp[j]   = year[0]
	month_tmp[j]  = month[0]
    day_tmp[j]    = day[0]
	hour_tmp[j]   = hour[0]
    doy_tmp[j]    = doy[0]   
    
    Date    = strmid(string(dat.year[0]), 8, 4)
    outfile = '../rutidl/output/'+'dst_'+Date[0]+'TGM.txt'   ; defining the output file
    ;names and path directories

    FOR i=1l, N_ELEMENTS(idx_dst)-1 DO BEGIN
	IF doy_tmp[j] EQ doy[i] THEN BEGIN
	        IF idx_dst[i] LT idx_dst_tmp[j] THEN BEGIN
        	        idx_dst_tmp[j] = idx_dst[i]
		            year_tmp[j]   = year[i]
	                month_tmp[j]  = month[i]
        	        day_tmp[j]    = day[i]
	                hour_tmp[j]   = hour[i]
	                doy_tmp[j]    = doy[i]
	        ENDIF
	ENDIF ELSE BEGIN
                j+=1
                
	        idx_dst_tmp[j] = idx_dst[i]
	        year_tmp[j]   = year[i]
	        month_tmp[j]  = month[i]
        	day_tmp[j]    = day[i]
	        hour_tmp[j]   = hour[i]
	        doy_tmp[j]    = doy[i]
	ENDELSE
    ENDFOR
     
    
    ;print, i, idx_mag[i]
    ;print, outfile
    ;print, tmp
    ;print, idx_mag[i], N_ELEMENTS(i)
   
 ;   OPENW, lun, Outfile, /GET_LUN
   
    print, '###################################################################'
    print, 'lista de eventos de tormenta geomagnética '
    print, '###################################################################'
    print, '                                                                    '
    print, 'Descripción: Identificación de la fecha y hora en formato '
    print, 'fecha yy/mm/dd hh, día del año de cuando el índice Dst descendió por'
    print, 'debajo de -150 nT, lo que es un indicativo de la ocurrencia de un '
    print, 'un evento de tormenta geomagnética intensa y de interés  '
    print, '                                                                   '
    print, format='("Fecha", 6X, "Hora", X, "DOY", 8X, "Indice Dst")'
    print, '------------------------------'
    ;PRINTF,lun, tmp[i], i, format = "(A11, 2X, I5)"
   
    for i=0, N_ELEMENTS(indexes)-1 do begin

           if doy_tmp[i] ne 0 then begin

            ;idx_kp[idx] = idx_kp[i]

                print, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_dst_tmp[i], $
                             FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 2X, I03, X, I)'  
               ; printf, lun, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_dst_tmp[i], $
                ;            FORMAT = '(I4, X, I02, X, I02, 2X, I02, 2X, I03, X, I4)'             
            endif 
   
        ;printf, lun, year[i], month[i], day[i], hour[i], doy[i], idx_kp[i], $
        ;FORMAT = '(I4, "/", I02, "/", I02, X, I02, 2X, I03, 4X, I)'       
    endfor
    
  ;  close,lun
   ; FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
    print, '                                                                    '
    print, '###################################################################'
    print, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    print, 'límites de ventana del tiempo en formato DOY entorno a cada evento'
     
end 

pro list, date_i, date_f

	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]	
;##############################################################################
; reading data files
;##############################################################################
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = strarr(file_number)
        string_date     = strarr(file_number)
       
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

        H    = FLTARR(file_number*24)
                      
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        ;print, tmp_year, tmp_month, tmp_day
                        dat = DH_teo([tmp_year, tmp_month, tmp_day])
                        
                        H[i*24:(i+1)*24-1] = dat.H[*]
                ENDIF ELSE BEGIN
                         H[i*24:(i+1)*24-1] = 999999.0
                ENDELSE                
        ENDFOR
	idx = WHERE(H LE -100, count)
	IF count LE 0 THEN MESSAGE, 'ERROR'

    time_w = timegen(n_elements(file_number), final=julday(mh_f, dy_f, yr_f, 23), $
                start=julday(mh_i, dy_i, yr_i, 0) , units='H')

    caldat, time_w, m, d, y, hr
;   print, m, d, format='(I02, X, I02)'

    y_idx    = y[idx]   
    m_idx    = m[idx]
    d_idx    = d[idx]
    H_idx    = H[idx]
    hr_idx   = hr[idx]
    
    y_idx_tmp    = y_idx*0
    m_idx_tmp    = m_idx*0
    d_idx_tmp    = d_idx*0
    H_idx_tmp    = H_idx*0
    hr_idx_tmp   = hr_idx*0
;print, H[idx]
    j=0l
    y_idx_tmp[j]    = y_idx[0]
    m_idx_tmp[j]    = m_idx[0]
    d_idx_tmp[j]    = d_idx[0]
    H_idx_tmp[j]    = H_idx[0]
    hr_idx_tmp[j]   = hr_idx[0]
    
    FOR i=1l, N_ELEMENTS(H_idx)-1 DO BEGIN
	IF d_idx_tmp[j] EQ d_idx[i] THEN BEGIN
	        IF H_idx[i] GT H_idx_tmp[j] THEN BEGIN
        	        H_idx_tmp[j]   = H_idx[i]
		            y_idx_tmp[j]   = y_idx[i]
	                m_idx_tmp[j]   = m_idx[i]
        	        d_idx_tmp[j]   = d_idx[i]
	                hr_idx_tmp[j]  = hr_idx[i]	               
	        ENDIF
	ENDIF ELSE BEGIN
                j+=1
                
        	    H_idx_tmp[j]   = H_idx[i]
		        y_idx_tmp[j]   = y_idx[i]
	            m_idx_tmp[j]   = m_idx[i]
        	    d_idx_tmp[j]   = d_idx[i]
	            hr_idx_tmp[j]  = hr_idx[i]	
	ENDELSE
    ENDFOR
    
    outfile='../rutidl/output/'+'DH_list_TGM.txt'
        OPENU, lun, outfile, /GET_LUN, /append
        
    print, '###################################################################'
    print, 'lista de eventos de tormenta geomagnética '
    print, '###################################################################'
    print, '                                                                    '
    print, 'Descripción: Identificación de la fecha y hora en formato '
    print, 'fecha yy/mm/dd hh, día del año de cuando el índice DH descendió por'
    print, 'debajo de -100 nT, significando un evento de tormenta geomagnética '
    print, 'intensa.'
    print, '                                                                    '    
    print, format='(2X, "Fecha", 4X, "Hora", 2X, "Indice DH")'
    print, '---------------------------------'
    print, '                                                                    '
         
    for i=0, N_ELEMENTS(idx)-1 do begin
           if d_idx_tmp[i] ne 0 then begin

                print, y_idx_tmp[i], m_idx_tmp[i], d_idx_tmp[i], hr_idx_tmp[i], $
                H_idx_tmp[i], $
                FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I04)' 
  
                printf, lun, y_idx_tmp[i], m_idx_tmp[i], d_idx_tmp[i], $
                hr_idx_tmp[i], H_idx_tmp[i], $ 
                FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I04)'                         
        endif        
    endfor
    
    close,lun
    FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
    print, '                                                                   '
    print, '###################################################################'
    print, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    print, 'límites de ventana del tiempo en formato DOY entorno a cada evento'
 
end
  
pro magdat, res_dat, initial, ini, fn

	On_error, 2
	compile_opt idl2, HIDDEN
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of Dst Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    yr  = initial
    ;dt  = tp  

    dat = dst_data(yr)
    t = n_elements(dat.year)
    ;print, t
    
    i_dst = dat.Dst
    ;print, t
    year = dat.year
    tiempo = TIMEGEN(t, START=julday(dat.month[0], dat.day[0],  $
                     dat.year[0], dat.hour[0]), UNITS='Hours')                                     
        
    ini_time  = JULDAY(1, ini, initial)
    fnl_time  = JULDAY(1, fn, initial)
    
    time_w  = tiempo[ini:fn]
    tw = n_elements(time_w)
    tot_days= findgen(tw*24)/24.0
    tot_days_2= (findgen(tw*24)/24.0)-6
                                             
    caldat, ini_time, ini_month, ini_day, ini_year
    caldat, fnl_time, fnl_month, fnl_day, fnl_year
    
    Date = string(year[0], ini_month, ini_day, FORMAT='(I4, "-", I02, "-", I02)')    
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of DH Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	yr_i	= ini_year
	mh_i	= ini_month
	dy_i 	= ini_day	

	yr_f	= fnl_year
	mh_f	= fnl_month
	dy_f 	= fnl_day
		
;##############################################################################
; reading data files
;##############################################################################
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = strarr(file_number)
        string_date     = strarr(file_number)
       
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

        H    = FLTARR(file_number*24)
        D    = FLTARR(file_number*24)
        Z    = FLTARR(file_number*24)
        F    = FLTARR(file_number*24)
        N    = FLTARR(file_number*24)
        
        H_STDESV    = FLTARR(file_number*24)
        D_STDESV    = FLTARR(file_number*24)
        Z_STDESV    = FLTARR(file_number*24) 
        F_STDESV    = FLTARR(file_number*24)
        N_STDESV    = FLTARR(file_number*24)
                       
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        ;print, tmp_year, tmp_month, tmp_day
                        dat = DH_teo([tmp_year, tmp_month, tmp_day])
                        
                        H[i*24:(i+1)*24-1] = dat.H[*]
                        D[i*24:(i+1)*24-1] = dat.D[*]
                        Z[i*24:(i+1)*24-1] = dat.Z[*]
                        F[i*24:(i+1)*24-1] = dat.F[*]
                        N[i*24:(i+1)*24-1] = dat.N[*]
                                                
                        H_STDESV[i*24:(i+1)*24-1] = dat.H_stdesv[*]
                        D_STDESV[i*24:(i+1)*24-1] = dat.D_stdesv[*]
                        Z_STDESV[i*24:(i+1)*24-1] = dat.Z_stdesv[*]  
                        F_STDESV[i*24:(i+1)*24-1] = dat.F_stdesv[*]
                        N_STDESV[i*24:(i+1)*24-1] = dat.N_stdesv[*]
                                                                                              
                ENDIF ELSE BEGIN
                         H[i*24:(i+1)*24-1] = 999999.0
                         D[i*24:(i+1)*24-1] = 999999.0
                         Z[i*24:(i+1)*24-1] = 999999.0
                         F[i*24:(i+1)*24-1] = 999999.0
                         N[i*24:(i+1)*24-1] = 999999.0

                         N_STDESV[i*24:(i+1)*24-1] = 999999.0                        
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                ENDELSE                
        ENDFOR
        
        for i=0, n_elements(H)-1 do begin
            ;idx = where(H eq 999999.0, inan)
            if H[i] eq 999999.0 then begin
                H[where(H[*] eq 999999.0)] = !Values.F_NAN          
            endif
        endfor
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of Dst Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        IF tw GT 7 THEN tmp_spam = 1.5
        IF tw GT 15 THEN tmp_spam = 2.
        
        Xsize=fix(800*tmp_spam)
        Ysize=800
        ;DEVICE, decompose=0
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]
             
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        amarillo  = 220
        verde     = 180
        negro     = 0
        azul      = 30
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    path = '../rutidl/output/globfig_to_reg/'

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    
    dst      = i_dst[(ini*24)-24:fn*24]
 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
     X_label   = STRARR(tw+1)+' '
    ; print, n_elements(x_label)
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        old_month = ini_month
       ; print, old_month
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(1, ini, initial)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR    

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;print, up, down
time_title = '2005/05/14 to 2005/05/18'
                
MAG_source = 'Source: International Service of Geomagnetic Indices'  
        
   ; plot, tot_days, dst, XTICKS=tw, xminor = 8, POSITION=[0.07,0.55,0.95,0.9],$
   ; XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=6,$
   ; title = time_title, ytitle = 'Indice DST [nT]',  XTICKNAME=REPLICATE(' ', tw+1), $
   ; XRANGE=[0, tw], YRANGE=[down,up], BACKGROUND = blanco, COLOR=negro;, /noerase             
    ;print, t
   ; oplot, tot_days, H, color=rojo
     
;     down  = min(dst)
;     up    = max(dst)    
   
    
    plot, dst, H, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = chr_size1, CHARTHICK=chr_thick1, POSITION=[0.20,0.20,0.80,0.80], $
     XSTYLE = 5, ySTYLE = 6, psym=2

     
        AXIS, XAXIS = 0, XRANGE=[-500,100], $
                         ;XTICKS=tw, $
                       ;  XMINOR=8, $
                         XTITLE = 'Dst [nT] ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                       ;  XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.5, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[-500,100], $
                     ;    XTICKS=tw, $
                      ;   XMINOR=8, $
                   ;      XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[-500,100], $
                         YTITLE = 'DH [nT]', $
                         COLOR=negro, $
                         CHARSIZE = 0.5;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[-500,100], $
                         YTITLE = 'DH [nT]', $
                       ;  YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         CHARSIZE = 0.5;, $
                        ; CHARTHICK=chr_thick1;, $   


    ;if tw gt 15 then begin
    ;    XYOUTS, 0.05, 0.170 , /NORMAL, $
     ;           '            Dst index,               DH index', COLOR=negro, $
     ;           CHARSIZE = chr_size1, $
     ;           CHARTHICK=chr_thick1     
     
  ;      POLYFILL, [0.07,0.10,0.10,0.07], [0.178,0.178,0.180,0.180], color = negro, /NORMAL
  ;      POLYFILL, [0.19,0.22,0.22,0.19], [0.178,0.178,0.180,0.180], color = rojo, /NORMAL
   ; endif else begin
    ;    XYOUTS, 0.03, 0.168 , /NORMAL, $
      ;          '            Dst index,         DH index', COLOR=negro, $
       ;         CHARSIZE = chr_size1, $
        ;        CHARTHICK=chr_thick1     
     
  ;      POLYFILL, [0.07,0.10,0.10,0.07], [0.178,0.178,0.180,0.180], color = negro, /NORMAL
   ;     POLYFILL, [0.19,0.22,0.22,0.19], [0.178,0.178,0.180,0.180], color = rojo, /NORMAL
  ;  endelse
   ; LOADCT, 0, /SILENT
    
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
                write_jpeg, path+'dst_DH_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'dst_DH_'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'dst_DH_'+Date
                print, ''
        ENDIF
        RETURN
end
    
;###############################################################################
;############################################################################### 
;###############################################################################
;###############################################################################
;Kp vs Km

function kp_data, in

	On_error, 2
	compile_opt idl2, HIDDEN

        header = 36             ; Defining number of lines of the header 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        year = string(in, format = '(I4)')
		file_name = '../rutidl/kp/Kp_'+ year+'-01-01_'+year+'-12-31_D.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)

	    IF opened_files NE N_ELEMENTS(file) THEN begin
	        file_name = '../rutidl/kp/Kp_'+ year+'-01-01_'+year+'-12-31_P.dat'
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN begin
    	        file_name = '../rutidl/kp/Kp_'+ year+'-01-01_'+year+'-12-31_Q.dat'
	            file = FILE_SEARCH(file_name, COUNT=opened_files)    	        
    	        IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  
    	    ENDIF    	    	    
	    ENDIF     
        
		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
                      DOY : 0, Kp: 0, Kp_str: '', Ap: 0}

		resulting_data0 = REPLICATE(DataStruct, number_of_lines-header)	
        ;print, number_of_lines-header-1, number_of_lines-header+1
        
        
		READS, data[header:number_of_lines-1], resulting_data0, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,8X,I3,X,I1,A1,X,I3)'
		
		indexes_07 = WHERE(resulting_data0[*].Kp_str EQ '-')
		indexes_03 = WHERE(resulting_data0[*].Kp_str EQ '+')
		indexes_00 = WHERE(resulting_data0[*].Kp_str EQ 'o')
		
		Kp_tmp = resulting_data0[*].Kp*10
		
		Kp_tmp[indexes_07] = Kp_tmp[indexes_07]-3
		Kp_tmp[indexes_03] = Kp_tmp[indexes_03]+3


                DataStruct2 = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
                      DOY : 0, Kp: 0, Ap: 0}

		resulting_data = REPLICATE(DataStruct2, number_of_lines-header)	

		resulting_data[*].year   = resulting_data0[*].year
		resulting_data[*].month  = resulting_data0[*].month
		resulting_data[*].day    = resulting_data0[*].day
		resulting_data[*].hour   = resulting_data0[*].hour
		resulting_data[*].minute = resulting_data0[*].minute
		resulting_data[*].DOY    = resulting_data0[*].DOY
		resulting_data[*].Kp     = Kp_tmp[*]
		resulting_data[*].Ap     = resulting_data0[*].AP				
		return, resulting_data
end   
    
function kmex, date
	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, format = '(I4, I02, I02)')
		
		name = 'teo_'+date+'.index.'
		
		file_name = '../rutidl/Kmex/'+name+'final'		
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
	    IF opened_files NE N_ELEMENTS(file) THEN begin
	        file_name = '../rutidl/Kmex/'+name+'early'
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  
	    
	    endif

		number_of_lines = FILE_LINES(file)
	   ; print, number_of_lines
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		IF err NE 0 THEN MESSAGE, 'Error opening '+file_name[0]
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-        
        idx_kmex      = {k_mex      : intarr(8), $
                         k_mex_sum  : 0, $
                         a_mex      : intarr(8), $
                         a_med      : 0}
        
        struct = {x : [0, 0, 0, 0, 0, 0, 0, 0], y : 0}
        
        tmp_var = replicate(struct, 2)

	;	idx_kmex = REPLICATE(DStruct, number_of_lines-header)	
  
		READS, data, tmp_var, FORMAT='(I3, I4, I4, I4, I4, I4, I4, I4, I4)'
		
		idx_kmex.k_mex[*]   = tmp_var[0].x
		idx_kmex.a_mex[*]   = tmp_var[1].x
        idx_kmex.k_mex_sum  = tmp_var[0].y
        idx_kmex.a_med      = tmp_var[1].y				
		return, idx_kmex	
end    

pro list_kp, resulting_data, in
    
    On_error, 2
	compile_opt idl2, HIDDEN	

	a = in
		
	dat = kp_data(a)    
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Defining variables to write output files indicating the events where Dst index
; got lower than -150 nT.
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	tmp = n_elements(dat.year)
	indexes = WHERE(dat.Kp GE 70, count)
	IF count LE 0 THEN MESSAGE, 'ERROR'
		
	kp0      = dat.Kp
    doy0     = dat.DOY
    year0    = dat.year
    month0   = dat.month
    day0     = dat.day
    hour0    = dat.hour 
    
    idx_kp  = kp0[indexes]
	doy     = doy0[indexes]
	year    = year0[indexes]
	month   = month0[indexes]
	day     = day0[indexes]
	hour    = hour0[indexes]

	idx_kp_tmp   = idx_kp*0
	doy_tmp      = doy*0
	year_tmp     = year*0
	month_tmp    = month*0
	day_tmp      = day*0
	hour_tmp     = hour*0

    Date    = strmid(string(dat.year[0]), 8, 4)
    outfile = '../rutidl/output/'+'kp_'+Date[0]+'TGM.txt'; defining the 
    ;output file names and path directories
    
    j=0l
    idx_kp_tmp[j] = idx_kp_tmp[0]
	year_tmp[j]   = year[0]
	month_tmp[j]  = month[0]
    day_tmp[j]    = day[0]
	hour_tmp[j]   = hour[0]
    doy_tmp[j]    = doy[0]  

    FOR i=1l, N_ELEMENTS(idx_kp)-1 DO BEGIN
	IF doy_tmp[j] EQ doy[i] THEN BEGIN
	        IF idx_kp[i] GT idx_kp_tmp[j] THEN BEGIN
        	        idx_kp_tmp[j] = idx_kp[i]
		            year_tmp[j]   = year[i]
	                month_tmp[j]  = month[i]
        	        day_tmp[j]    = day[i]
	                hour_tmp[j]   = hour[i]
	                doy_tmp[j]    = doy[i]
	        ENDIF
	ENDIF ELSE BEGIN
                j+=1
                
	        idx_kp_tmp[j] = idx_kp[i]
	        year_tmp[j]   = year[i]
	        month_tmp[j]  = month[i]
        	day_tmp[j]    = day[i]
	        hour_tmp[j]   = hour[i]
	        doy_tmp[j]    = doy[i]
	ENDELSE
    ENDFOR
   
    ;OPENW, lun, outfile, /GET_LUN

    ;PRINTF,lun, tmp[i], i, format = "(A50, 2X, I5)"
    ;idx   = where(idx_kp GT 6, count) 

    ;idx = findgen(n_elements(idx_kp))
 
    print, '###################################################################'
    print, 'lista de eventos de tormenta geomagnética '
    print, '###################################################################'
    print, '                                                                    '
    print, 'Descripción: Identificación de la fecha y hora en formato '
    print, 'fecha yy/mm/dd hh, día del año de cuando el índice Kp ascendió por'
    print, 'encima de 7, significando un evento de tormenta geomagnética intensa.'
    print, '                                                                    '
    
    print, format='(2X, "Fecha", 4X, "Hora", 3X, "DOY", 2X, "Indice Kp")'
    print, '---------------------------------'
       
    for i=0, N_ELEMENTS(indexes)-1 do begin
        ;idx   = where(idx_kp GT 6, count)
        
            if doy_tmp[i] ne 0 then begin

            ;idx_kp[idx] = idx_kp[i]

                print, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_kp_tmp[i], $
                             FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I03, 5X, I02)'  
                ;printf, lun, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_kp_tmp[i], $
                           ; FORMAT = '(I4, X, I02, X, I02, 2X, I02, 4X, I03, X, I02)'                      
        endif        
    endfor    
   ; close,lun
    ;FREE_LUN, lun
    print, '                                                                   '
    print, '###################################################################'
    print, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    print, 'límites de ventana del tiempo en formato DOY entorno a cada evento'

end     

pro list_kmex, date_i, date_f

	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]	
;##############################################################################
; reading data files
;##############################################################################
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = strarr(file_number)

        string_date     = strarr(file_number)
       
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.final'
		        
		        file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	            IF opened_files NE N_ELEMENTS(file) THEN begin
	                data_file_name[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.early'    
	            ENDIF		        
	        
        ENDFOR
	
        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

        k_mex    = INTARR(file_number*8)
        a_mex    = INTARR(file_number*8)

        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        ;print, tmp_year, tmp_month, tmp_day
                        dat = kmex([tmp_year, tmp_month, tmp_day])
                        
                        k_mex[i*8:(i+1)*8-1] = dat.k_mex[*]/10
                        a_mex[i*8:(i+1)*8-1] = dat.a_mex[*]
                ENDIF 
        ENDFOR
    
    k_mex[where(k_mex[*] eq 99)] = !Values.F_NAN     

	idx = WHERE(k_mex GE 7, count)
	IF count LE 0 THEN MESSAGE, 'ERROR'

    time_w = timegen(n_elements(file_number), final=julday(mh_f, dy_f, yr_f, 23), $
                start=julday(mh_i, dy_i, yr_i, 0) , units='H')

    caldat, time_w, m, d, y, hr

    y_idx        = y[idx]   
    m_idx        = m[idx]
    d_idx        = d[idx]
    k_mex_idx    = k_mex[idx]
    hr_idx       = hr[idx]
    
    y_idx_tmp        = y_idx*0
    m_idx_tmp        = m_idx*0
    d_idx_tmp        = d_idx*0
    k_mex_idx_tmp    = k_mex_idx*0
    hr_idx_tmp       = hr_idx*0
;print, H[idx]
    j=0l
    y_idx_tmp[j]        = y_idx[0]
    m_idx_tmp[j]        = m_idx[0]
    d_idx_tmp[j]        = d_idx[0]
    k_mex_idx_tmp[j]    = k_mex_idx[0]
    hr_idx_tmp[j]       = hr_idx[0]
    
    FOR i=1l, N_ELEMENTS(k_mex_idx)-1 DO BEGIN
	IF d_idx_tmp[j] EQ d_idx[i] THEN BEGIN
	        IF k_mex_idx[i] GT k_mex_idx_tmp[j] THEN BEGIN
        	        k_mex_idx_tmp[j]   = k_mex_idx[i]
		            y_idx_tmp[j]       = y_idx[i]
	                m_idx_tmp[j]       = m_idx[i]
        	        d_idx_tmp[j]       = d_idx[i]
	                hr_idx_tmp[j]      = hr_idx[i]	               
	        ENDIF
	ENDIF ELSE BEGIN
                j+=1
                
        	    k_mex_idx_tmp[j]   = k_mex_idx[i]
		        y_idx_tmp[j]       = y_idx[i]
	            m_idx_tmp[j]       = m_idx[i]
        	    d_idx_tmp[j]       = d_idx[i]
	            hr_idx_tmp[j]      = hr_idx[i]	
	ENDELSE
    ENDFOR
    
    ;outfile='../rutidl/output/'+'Kmex_list_TGM.txt'
     ;   OPENW, lun, outfile, /GET_LUN, /append
        
    print, '###################################################################'
    print, 'lista de eventos de tormenta geomagnética '
    print, '###################################################################'
    print, '                                                                    '
    print, 'Descripción: Identificación de la fecha y hora en formato '
    print, 'fecha yy/mm/dd hh, día del año de cuando el índice DH ascendió por'
    print, 'encima de 6, significando un evento de tormenta geomagnética '
    print, 'intensa.'
    print, '                                                                    '    
    print, format='(2X, "Fecha", 4X, "Hora", 2X, "Indice Kmex")'
    print, '---------------------------------'
    print, '                                                                    '
         
    for i=0, N_ELEMENTS(idx)-1 do begin
           if d_idx_tmp[i] ne 0 then begin

                print, y_idx_tmp[i], m_idx_tmp[i], d_idx_tmp[i], hr_idx_tmp[i], $
                k_mex_idx_tmp[i], $
                FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I03)' 
  
               ; printf, lun, y_idx_tmp[i], m_idx_tmp[i], d_idx_tmp[i], $
               ; hr_idx_tmp[i], k_mex_idx_tmp[i], $ 
               ; FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I03)'                         
        endif        
    endfor
    ;close,lun
    ;FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
   print, '                                                                    '
    print, '###################################################################'
    print, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    print, 'límites de ventana del tiempo en formato DOY entorno a cada evento' 
end

pro plot_k, resulting_data, in, ini, fn, PNG = png, JPEG = jpeg;, DIR = dir

	On_error, 2
	compile_opt idl2, HIDDEN	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Define a time window
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-    
    yr  = in
    
    dat = kp_data(yr)
    t = n_elements(dat.year)

    i_kp = dat.Kp
    
    DUM = LABEL_DATE( DATE_FORMAT= ['%N/%D'])    ; time laber format for the 
    ;Dst graphics
    
    year    = dat.year
    month   = dat.month
    day     = dat.day
    hour    = dat.hour
    doy     = dat.DOY
        
    ventana_t   = timegen(t, START=julday(1, doy[0], year[0], dat.hour[0]), units='H')
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    time_w  = ventana_t[ini:fn]
    tw = n_elements(time_w)
    tot_days= findgen(tw*8)/8.0
    
    t_ini = julday(1, ini, year[0])
    t_fnl = julday(1, fn,  year[0])
    
    caldat, t_ini, ini_month, ini_day, ini_year
    caldat, t_fnl, fnl_month, fnl_day, fnl_year

    
    Kp      = i_kp[(ini*8)-7:fn*8]
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;Datos de Kmex
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	yr_i	= ini_year
	mh_i	= ini_month
	dy_i 	= ini_day	

	yr_f	= fnl_year
	mh_f	= fnl_month
	dy_f 	= fnl_day

        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = strarr(file_number)

        string_date     = strarr(file_number)
       
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.final'
		        
		        file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	            IF opened_files NE N_ELEMENTS(file) THEN begin
	                data_file_name[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.early'    
	            ENDIF		        
	        
        ENDFOR
	
        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

        k_mex    = INTARR(file_number*8)
        a_mex    = INTARR(file_number*8)

        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        ;print, tmp_year, tmp_month, tmp_day
                        dat = kmex([tmp_year, tmp_month, tmp_day])
                        
                        k_mex[i*8:(i+1)*8-1] = dat.k_mex[*]/10
                        a_mex[i*8:(i+1)*8-1] = dat.a_mex[*]
                ENDIF 
        ENDFOR
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

    path = '../rutidl/output/globfig_to_reg/kp-km_v1/'
    date = string(year[0], ini_month, ini_day, FORMAT='(I4, "-", I02, "-", I02)')
    
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        IF tw GT 7 THEN tmp_spam = 1.5
        IF tw GT 15 THEN tmp_spam = 2.
        
        Xsize=fix(800*tmp_spam)
        Ysize=800
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
        gris      = 10
        morado    = 16
                        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-    
     X_label   = STRARR(tw+1)+' '
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        old_month = ini_month
        ;print, old_month
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(1, ini, in)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR        
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; PLOTTING
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;IF dt eq 'D' then plot_title = 'Definitive Planetary K index'
;if dt eq 'P' then plot_title = 'Provisional Planetary K index'
;if dt eq 'Q' then plot_title = 'Quick look Planetary K index'

plot_title = 'Planetary K index vs Regional Kmex index'

time_title = 'Time window begin: '+string(year[0], ini_month, ini_day, $
                FORMAT='(I4, "/", I02, "/", I02)')+' 00:00 UTC'
                
MAG_source = 'Source: International Service of Geomagnetic Indices'                
    
    plot, tot_days, Kp, psym=6, /NoDATA, MAX_VALUE=9., XTICKS=tw, xminor=8, $
                    TITLE = plot_title, XTITLE = 's', YTITLE = 's', $
                    BACKGROUND = blanco, COLOR=negro, YRANGE=[0,90], YTICKS=9, $
                    YMINOR=0, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                    POSITION=[0.07,0.56,0.95,0.91], XSTYLE = 5, ystyle = 5,$
                    XTICKNAME=REPLICATE(' ', tw+1), XRANGE=[0, tw]

        j = N_ELEMENTS(Kp)

        FOR i = 0, j-1 DO BEGIN
                IF Kp[i] LE 90 THEN BEGIN
                                        color = 0
                                        step  = (Kp[i] EQ 0) ? 0.1 : 0.
                                        CASE 1 OF
                                                Kp[i] EQ 40 : color = amarillo
                                                Kp[i] GE 40 : color = rojo
                                                ELSE       : color = verde
                                        ENDCASE
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], [0,0,Kp[i]$
                                        +step,Kp[i]+step], color=color
                                      ENDIF $
                                      ELSE BEGIN
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], $
                                        [0,0,9.,9.], color=morado, /LINE_FILL, $
                                        ORIENTATION=45., linestyle = 0
                                         
                                         
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], $
                                        [0,0,9.,9.],color=morado, /LINE_FILL, $
                                         ORIENTATION=-45., linestyle = 0
                                      ENDELSE
        ENDFOR
        
 FOR i = 0, tw-1 DO BEGIN
                OPLOT, [i,i], [0.,90.], linestyle=1, COLOR=negro
        ENDFOR

        FOR i=50, 80, 10 DO OPLOT, [0,tw], [i,i], linestyle=1, COLOR=negro

        AXIS, XAXIS = 0, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         XTICKFORMAT="(A1)",$
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         ;CHARSIZE = 0.7, $
                         ;CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04


        AXIS, YAXIS = 0, YRANGE=[0,90], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTITLE = 'Kp index', $
                         COLOR=negro, $
                         CHARSIZE = 0.7, $
                         CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[0,90], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTICKNAME=[' ', ' ', ' ', ' ', ' ', 'G1', 'G2', 'G3', 'G4', 'G5'], $
                         COLOR=negro, $
                         CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1;, $

       ; XYOUTS, 0.01, 0.015 , /NORMAL, $
       ;         LANCE_banner, COLOR=negro, $
       ;         CHARSIZE = chr_size1, $
       ;                  CHARTHICK=chr_thick1

       ; XYOUTS, 0.01, 0.070 , /NORMAL, $
        ;        MAG_source, COLOR=negro, $
         ;       CHARSIZE = chr_size1, $
          ;               CHARTHICK=chr_thick1

    plot, tot_days, k_mex, psym=6, /NoDATA, MAX_VALUE=9., XTICKS=tw, xminor=8, $
                    XTITLE = 's', YTITLE = 's', SUBTITLE = time_title, $
                    BACKGROUND = blanco, COLOR=negro, YRANGE=[0,9], YTICKS=9, $
                    YMINOR=0, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                    POSITION=[0.07,0.20,0.95,0.55], XSTYLE = 5, ystyle = 5,$
                    XTICKNAME=REPLICATE(' ', tw+1), XRANGE=[0, tw], /noerase


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
                                        space,0.+space]+tot_days[i], [0,0,k_mex[i]$
                                        +step,k_mex[i]+step], color=color
                                      ENDIF $
                                      ELSE BEGIN                                        
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], $
                                        [0,0,9.,9.], color=gris, /LINE_FILL, $
                                        ORIENTATION=45., linestyle = 0
                                         
                                         
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], $
                                        [0,0,9.,9.],color=gris, /LINE_FILL, $
                                         ORIENTATION=-45., linestyle = 0
                                      ENDELSE
        ENDFOR
 
 FOR i = 0, file_number-1 DO BEGIN
                OPLOT, [i,i], [0.,9.], linestyle=1, COLOR=negro
        ENDFOR

        FOR i=5, 8, 1 DO OPLOT, [0,tw], [i,i], linestyle=1, COLOR=negro

        AXIS, XAXIS = 0, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.7, $
                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[0,9], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTITLE = 'Kmex index', $
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


    if tw gt 15 then begin
    
        XYOUTS, 0.01, 0.095 , /NORMAL, $
                'Color Code:        quiet,            disturbed,           storm,                   data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
       
        POLYFILL, [0.07,0.10,0.10,0.07], [0.090,0.090,0.115,0.115], color = verde, /NORMAL
        POLYFILL, [0.15,0.18,0.18,0.15], [0.090,0.090,0.115,0.115], color = amarillo, /NORMAL
        POLYFILL, [0.25,0.28,0.28,0.25], [0.090,0.090,0.115,0.115], color = rojo, /NORMAL
        POLYFILL, [0.37,0.40,0.40,0.37], [0.090,0.090,0.115,0.115], color = gris, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        POLYFILL, [0.37,0.40,0.40,0.37], [0.090,0.090,0.115,0.115], color = gris, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0
    
    endif else begin
        XYOUTS, 0.01, 0.095 , /NORMAL, $
                'Color Code:       quiet,          disturbed,           storm,                 data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
      
        POLYFILL, [0.09,0.12,0.12,0.09], [0.090,0.090,0.115,0.115], color = verde, /NORMAL
        POLYFILL, [0.19,0.22,0.22,0.19], [0.090,0.090,0.115,0.115], color = amarillo, /NORMAL
        POLYFILL, [0.32,0.35,0.35,0.32], [0.090,0.090,0.115,0.115], color = rojo, /NORMAL
        POLYFILL, [0.47,0.50,0.50,0.47], [0.090,0.090,0.115,0.115], color = gris, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        POLYFILL, [0.47,0.50,0.50,0.47], [0.090,0.090,0.115,0.115], color = gris, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0
    endelse
    
        ;XYOUTS, 0.65, 0.015 , /NORMAL, $
        ;        UPDATE_banner, COLOR=negro, $
        ;        CHARSIZE = chr_size1, $
        ;                 CHARTHICK=chr_thick1

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
                write_jpeg, path+'kp_'+date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'kp_'+date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'kp_'+date
                print, ''
        ENDIF
        RETURN    
end




                      

