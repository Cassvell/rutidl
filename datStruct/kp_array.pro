;Name:
;	kp_array.pro
;purpose:
;	kp_data     : Create data structure from planetary geomagnetic kp data index files
;   kp_array    : Generate time series from the daily data structure
;
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data handling
;
;calling sequence:
;   .r kp_array
;   kp_array(idate, fdate, variable)
;parameters:
;   kp data files from ISGI format
;   idate/fdate: format[yyyy, mm, dd]
;   variable: 'kp' or 'Ap'
;
;dependencies:
;   Geomagnetic Service
;   Geophysics Institute
;   ISGI
;
;input files
;   Kp data files
;
;output files:
;   Kp or Ap time series
;
;version
; Dec, 2022

FUNCTION str_data, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
;reading data files
        date = STRING(year, month, day, FORMAT = '(I4,"-",I02,"-",I02)')
        path='/home/isaac/datos'
		file_name = path+'/kp/daily/kp_'+date+'.dat'
        header = 1             ; Defining number of lines of the header 
;###############################################################################
;reading data files
        file = FILE_SEARCH(file_name, COUNT=opened_files)

        IF opened_files NE N_ELEMENTS(file) THEN begin
                file_name = path+'/kp/daily/kp_'+date+'.txt'
        ENDIF

        number_of_lines = FILE_LINES(file)
        data = STRARR(number_of_lines)

        OPENR, lun, file, /GET_LUN, ERROR=err
        READF, lun, data, FORMAT = '(A)'
        CLOSE, lun
        FREE_LUN, lun
;###############################################################################
;extracting data and denfining an structure data
        if file_name eq path+'/kp/daily/kp_'+date+'.txt' then begin
        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, second : 0, doy : 0,$
                      Kp: 0, Kp_str: '', Ap: 0}

		r_kp = REPLICATE(DataStruct, number_of_lines-header)	                
		READS, data[header:number_of_lines-1], r_kp, FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,X,I02,I4,I02,A1,I04)'          
        endif
        if file_name eq path+'/kp/daily/kp_'+date+'.dat' then begin
                DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, second : 0,$
                      Kp: 0, Kp_str: '', Ap: 0}

		r_kp = REPLICATE(DataStruct, number_of_lines-header)	                
		READS, data[header:number_of_lines-1], r_kp, FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,X,I02,I2,A1,I04)'                
        endif
                		
		RETURN, r_kp
END   


FUNCTION kp_array, date_i, date_f, HELP=help
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2] 

;###############################################################################    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1  

; define DH variables
        data_path='/home/isaac/datos'
        data_file_name_kp  = strarr(file_number)                         
        string_date_2    = strarr(file_number)
             
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date_2[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')                
		        data_file_name_kp[i] = data_path+'/kp/daily/kp_'+string_date_2[i]+'.dat'
                        ;print, data_file_name_kp[i]
                file = FILE_SEARCH(data_file_name_kp[i], COUNT=opened_files)	            
	            IF opened_files NE N_ELEMENTS(file) THEN begin
	                data_file_name_kp[i] = data_path+'/kp/daily/kp_'+string_date_2[i]+'.txt'
	            ENDIF 	      	                            
        ENDFOR

        exist_data_file_kp   = FILE_TEST(data_file_name_kp)
        capable_to_plot_kp   = N_ELEMENTS(where(exist_data_file_kp EQ 1))

        IF capable_to_plot_kp NE N_ELEMENTS(data_file_name_kp) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.kp_index.',A,' impossible to plot all data.')"              
        ENDIF
;###############################################################################
;Kp Data                       
        kp    = FLTARR(file_number*8)
        ap    = FLTARR(file_number*8)
        str   = STRARR(file_number*8)                              
        FOR i = 0, N_ELEMENTS(exist_data_file_kp)-1 DO BEGIN
                IF exist_data_file_kp[i] EQ 1 THEN BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date_2[i] = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')                
                        dat = str_data([tmp_year, tmp_month, tmp_day])
                        kp[i*8:(i+1)*8-1]   = dat.Kp[*]                                               
                        ap[i*8:(i+1)*8-1]   = dat.Ap[*]
                        str[i*8:(i+1)*8-1]= dat.Kp_str[*]                                                                      
                ENDIF ELSE BEGIN
                         kp[i*8:(i+1)*8-1] = 999999.0
                         ap[i*8:(i+1)*8-1] = 999999.0                     
                ENDELSE                
        ENDFOR
        
		indexes_07 = WHERE(str EQ '-')
		indexes_03 = WHERE(str EQ '+')
		indexes_00 = WHERE(str EQ 'o')

    IF indexes_03[0] NE -1 AND indexes_07[0] NE -1 THEN BEGIN
        kp[indexes_07] = kp[indexes_07]-0.3
        kp[indexes_03] = kp[indexes_03]+0.3
    ENDIF                  

    var = {kp : kp, ap : ap}
    RETURN, var
END 
