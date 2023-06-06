;Name: H_array.pro
;	
;purpose:
;	Generate data structure from H component observation
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
;   .r teo
;   teo(date[yyyy,mm,dd])
;parameters:
;   Teoloyucan geomagnetic data file observations
;
;dependencies:
;   Instituto de geofísica, UNAM
;   Servicio Magnético
;
;input files
;   Teoloyucan data file
;
;output files:
;   Data Structure 
;VERSION
;   Dec, 2022
;
FUNCTION teo, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, FORMAT = '(I4, I02, I02)')
       ; sts  = string(stats, format = '(A5)')
		data_dir = '/home/isaac/MEGAsync/datos/'
		file_name = data_dir+'teoloyucan/hourly/'+'teo_'+date+'h23.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)

		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        DStruct = {H : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(F10)'		
		RETURN, teo_mag		
END

FUNCTION coe_min, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, FORMAT = '(I4, I02, I02)')
       ; sts  = string(stats, format = '(A5)')
		data_dir = '/home/isaac/MEGAsync/datos/'
		file_name = data_dir+'coeneo/min/'+'coe_'+date+'m23.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)

		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        DStruct = {H : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(F10)'		
		RETURN, coe_mag			
END

FUNCTION teo_min, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, FORMAT = '(I4, I02, I02)')
       ; sts  = string(stats, format = '(A5)')
		data_dir = '/home/isaac/MEGAsync/datos/'
		file_name = data_dir+'teoloyucan/min/'+'teo_'+date+'m23.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)

		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        DStruct = {H : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(F10)'		
		RETURN, teo_mag		
END

FUNCTION H_array, date_i, date_f, HELP=help
	On_error, 2
	COMPILE_OPT idl2, HIDDEN 

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]  

    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
;###############################################################################
; define H variables

        string_date        = STRARR(file_number)               
        data_file_name_h  = STRARR(file_number)                        
        
     data_path = '/home/isaac/MEGAsync/datos'                         
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')       
                data_file_name_h[i]  = data_path+'/teoloyucan/hourly/'+'teo_'+string_date[i]+'h23.dat'              
		        
		        file_h  = FILE_SEARCH(data_file_name_h[i], COUNT=opened_files)

	            IF opened_files NE N_ELEMENTS(file_h) THEN begin
	                data_file_name_h[i] = data_path+'/teoloyucan/hourly/'+'teo_'+string_date[i]+'h.dat'   
	            ENDIF	            	                            
        ENDFOR

        exist_data_file_h   = FILE_TEST(data_file_name_h)
        capable_to_plot_h   = N_ELEMENTS(WHERE(exist_data_file_h EQ 1))
      
        
        IF capable_to_plot_h NE N_ELEMENTS(data_file_name_h) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.H data.',A,' impossible to plot all data.')"              
        ENDIF

;###############################################################################
; Generate the time variables to plot H time series                     
        H    = FLTARR(file_number*24)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_h)-1 DO BEGIN
                IF exist_data_file_h[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_h = teo([tmp_year, tmp_month, tmp_day])
                        
                        H[i*24:(i+1)*24-1] = d_h.H[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        H[i*24:(i+1)*24-1] = 999999.0
                ENDELSE                
        ENDFOR
        RETURN, H
END


FUNCTION H_array_min, date_i, date_f, HELP=help
	On_error, 2
	COMPILE_OPT idl2, HIDDEN 

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]  

    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
;###############################################################################
; define H variables

        string_date        = STRARR(file_number)               
        data_file_name_h  = STRARR(file_number)                        
        
     data_path = '/home/isaac/MEGAsync/datos'                         
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')       
                data_file_name_h[i]  = data_path+'/teoloyucan/min/'+'teo_'+string_date[i]+'m23.dat'              
		        
		        file_h  = FILE_SEARCH(data_file_name_h[i], COUNT=opened_files)

	            IF opened_files NE N_ELEMENTS(file_h) THEN begin
	                data_file_name_h[i] = data_path+'/teoloyucan/min/'+'teo_'+string_date[i]+'m23.dat'   
	            ENDIF	            	                            
        ENDFOR

        exist_data_file_h   = FILE_TEST(data_file_name_h)
        capable_to_plot_h   = N_ELEMENTS(WHERE(exist_data_file_h EQ 1))
      
        
        IF capable_to_plot_h NE N_ELEMENTS(data_file_name_h) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.H data.',A,' impossible to plot all data.')"              
        ENDIF

;###############################################################################
; Generate the time variables to plot H time series                     
        H    = FLTARR(file_number*1440)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_h)-1 DO BEGIN
                IF exist_data_file_h[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_h = teo_min([tmp_year, tmp_month, tmp_day])
                        
                        H[i*1440:(i+1)*1440-1] = d_h.H[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        H[i*1440:(i+1)*1440-1] = 999999.0
                ENDELSE                
        ENDFOR
        RETURN, H
        
END        
