;Name:
;	dh_array.pro
;purpose:
;	DH_teo: Create data structure from local geomagnetic data index files from Teo
;   dh_array: from data structure, generates time series of the set variables within the structure
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
;   .r dh_array
;   dh_array, [yyyy, mm, dd]
;parameters:
;
;
;dependencies:
;   Geomagnetic Service
;   Geophysics Institute
;   National Space Weather Laboratory
;
;input files
;   dH data files
;
;output files:
;   DH data structure
;
;version
; Dec, 2022

FUNCTION DH_data, date, idx
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
        @set_up_commons
        set_up	
;reading data files
        date = STRING(year, month, day, FORMAT = '(I4, I02, I02)')
        str_year = STRING(year, FORMAT = '(I4)')	
        station_code    = set_var.gms_code[FIX(idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
        
        dir = set_var.Mega_dir+'dH_'+STRLOWCASE(station_code)+'/'        

	IF station_code EQ 'teo' THEN BEGIN
		file_name = dir+STRLOWCASE(station_code)+'_'+date+'.dst.final'
	ENDIF ELSE BEGIN
	 	file_name = dir+STRLOWCASE(station_code)+'_'+date+'.delta_H.final'
	ENDELSE
	 		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
	    IF opened_files NE N_ELEMENTS(file) THEN BEGIN
			IF station_code EQ 'teo' THEN BEGIN
				file_name = dir+STRLOWCASE(station_code)+'_'+date+'.dst.early'
			ENDIF ELSE BEGIN
	 			file_name = dir+STRLOWCASE(station_code)+'_'+date+'.delta_H.early'
			ENDELSE	    	        
	        ;name = STRLOWCASE(station_code)+'_'+date+'.index'+status
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  	    
	    ENDIF
		print, file_name
		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;###############################################################################
;extracting data and denfining an structure data
		index_str      =  {d_h     : FLTARR(24), $
						d_h_sum  : 0.0, $
                        d_sigma      : FLTARR(24), $
						d_sigma2      : 0.0}
        
        struct = {x : FLTARR(24), y : 0}        
        tmp_var = REPLICATE(struct, 2)
		READS, data, tmp_var, FORMAT='(25(F9))'
		
		index_str.d_h[*]   = tmp_var[0].x
		index_str.d_sigma[*]   = tmp_var[1].x
        index_str.d_h_sum  = tmp_var[0].y
        index_str.d_sigma2      = tmp_var[1].y				
		RETURN, index_str		
END

FUNCTION dh_array, date_i, date_f, idx;, resolution 
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2] 

;###############################################################################
	@set_up_commons
	set_up
           
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1  
; define DH variables
	station_code    = set_var.gms_code[FIX(idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
    dir = set_var.Mega_dir+'dH_'+STRLOWCASE(station_code)+'/'      

        
        string_date        = STRARR(file_number)                
        data_file_name_dh  = STRARR(file_number) 
             
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')	   
                               
                ;data_file_name_dh[i] = data_path+'/dH_teo/teo_'+string_date[i]+'.dst.early'
                data_file_name_dh[i] = dir+STRLOWCASE(station_code)+'_'+string_date[i]+'.delta_H.early'
              ;  print, data_file_name_dh[i]
            ;    file_dh = FILE_SEARCH(data_file_name_dh[i], COUNT=opened_files)
		    ENDFOR    		        
	  ;           IF opened_files NE N_ELEMENTS(file_dh) THEN begin
	   ;             data_file_name_dh[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'    
	         ;   ENDIF


        exist_data_file_dh   = FILE_TEST(data_file_name_dh)
        capable_to_plot_dh   = N_ELEMENTS(WHERE(exist_data_file_dh EQ 1))
                
        IF capable_to_plot_dh NE N_ELEMENTS(data_file_name_dh) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.dh_index.',A,' impossible to plot all data.')"              
        ENDIF
;###############################################################################
; Generate the time variables to plot dH time series                                   
        dH    = FLTARR(file_number*24)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_dh)-1 DO BEGIN
                IF exist_data_file_dh[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_dh = DH_data([tmp_year, tmp_month, tmp_day], STRING(idx))
                        
                        dH[i*24:(i+1)*24-1] = d_dh.d_h[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        dH[i*24:(i+1)*24-1] = 999999.0
                ENDELSE                
        ENDFOR
    RETURN, dH

	dH = add_nan(dH, 999999.0, 'equal')
END
