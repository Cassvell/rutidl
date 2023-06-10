FUNCTION coeH, date, station, idx
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        @set_up_commons
        set_up	
        
        date = string(year, month, day, FORMAT = '(I4, I02, I02)')		
        str_year = STRING(year, FORMAT = '(I4)')	
        station_code    = set_var.gms_code[idx]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu		

       ; sts  = string(stats, format = '(A5)')
        dir = set_var.Mega_dir+station
		data_dir = set_var.Mega_dir+station+'/min/'
		file_name = data_dir+station_code+'_'+date+'m.dat'
;		print, file_name
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

		coe_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], coe_mag, FORMAT='(F9)'	
	
		RETURN, coe_mag		
END



FUNCTION coe_array, date_i, date_f, station, idx 
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
        @set_up_commons
        set_up

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
	
	station_code    = set_var.gms_code[idx]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
;############################################################################### 

    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
;###############################################################################
; define H variables

        string_date        = STRARR(file_number)               
        data_file_name_h  = STRARR(file_number)                        
        
        dir = set_var.Mega_dir+station               
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')       
                data_file_name_h[i]  = dir+'/min/'+station_code+'_'+string_date[i]+'m.dat'              
		        
		      ;  file_h  = FILE_SEARCH(data_file_name_h[i], COUNT=opened_files)

	      ;      IF opened_files NE N_ELEMENTS(file_h) THEN begin
	       ;         data_file_name_h[i] = data_path+'/teoloyucan/min/'+'teo_'+string_date[i]+'m23.dat'   
	        ;    ENDIF	            	                            
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
                        d_h = coeH([tmp_year, tmp_month, tmp_day], station, idx)
                        
                        H[i*1440:(i+1)*1440-1] = d_h.H[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        H[i*1440:(i+1)*1440-1] = 99999.0
                ENDELSE                
        ENDFOR
        RETURN, H
        

	;RETURN, H
END



