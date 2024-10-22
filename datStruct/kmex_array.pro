;Name:
;	kmex_array.pro
;purpose:
;	kmex:   Create data structure from local geomagnetic data index files from observatory/magnetic
;           station
;   kmex_array: generate k or a local index from the data structure generated in kmex function
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
;   .r kmex_array
;   kmex_array(idate, fdate, variable)  
;
;parameters:
;   idate/fdate: format [yyyy,mm,dd]
;   variable    : vector array to be selected
;       options: 'k_mex' and 'a_mex'
;
;dependencies:
;   Geomagnetic Service
;   Geophysics Institute
;   National Space Weather Laboratory
;
;input files
;   Kmex and amex time series
;
;output files:
;   Kmex data structure
;
;version
; Dec, 2022


FUNCTION kmex, date, idx
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
    
    date = STRING(year, month, day, FORMAT = '(I4, I02, I02)')	
;###############################################################################
;###############################################################################
	@set_up_commons
	set_up

;set station data	
	station_code    = set_var.gms_code[FIX(idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
    dir = set_var.Mega_dir+'Kmex/'          
	name = STRLOWCASE(station_code)+'_'+date+'.k_index.final'
	file_name = dir+name
	
	IF station_code EQ 'teo' THEN BEGIN
		name = STRLOWCASE(station_code)+'_'+date+'.index.final'
		file_name = dir+name	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
	    IF opened_files NE N_ELEMENTS(file) THEN BEGIN
	        name = STRLOWCASE(station_code)+'_'+date+'.index.early'
			
			file_name = dir+name
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'  	    
	    ENDIF	
	ENDIF ELSE BEGIN
	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN BEGIN
			name = STRLOWCASE(station_code)+'_'+date+'.k_index.early'
				
			file_name = dir+name
			file = FILE_SEARCH(file_name, COUNT=opened_files) 
			IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'  	    
		ENDIF
	 ENDELSE   
;reading data files
		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		IF err NE 0 THEN MESSAGE, 'Error opening '+file_name[0]
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;###############################################################################
;extracting data and denfining an structure data
IF station_code EQ 'teo' THEN BEGIN
        result      = {k_mex      : intarr(8), $
                         k_mex_sum  : 0, $
                         a_mex      : intarr(8), $
                         a_med      : 0}
        
        struct = {x : [0, 0, 0, 0, 0, 0, 0, 0], y : 0}        
        tmp_var = REPLICATE(struct, 2)
		READS, data, tmp_var, FORMAT='(I3, I4, I4, I4, I4, I4, I4, I4, I4)'
		
		result.k_mex[*]   = tmp_var[0].x
		result.a_mex[*]   = tmp_var[1].x
        result.k_mex_sum  = tmp_var[0].y
        result.a_med      = tmp_var[1].y				
ENDIF  ELSE BEGIN    
        dat_str = { z : [0,0,0,0,0,0,0,0], y : 0}
        tmp_var = REPLICATE(dat_str,6)

        result = { $
                  K_mex        : INTARR(8), $
                  K_SUM        : 0, $
                  a_mex        : INTARR(8), $ 
                  A_median     : 0, $
                  K_mex_max    : INTARR(8), $
                  K_SUM_max    : 0, $
                  a_mex_max    : INTARR(8), $ 
                  A_median_max : 0, $
                  K_mex_min    : INTARR(8), $
                  K_SUM_min    : 0, $
                  a_mex_min    : INTARR(8), $ 
                  A_median_min : 0 $
                 }    
        
        READS, data, tmp_var, FORMAT='(I3,X,I3,X,I3,X,I3,X,I3,X,I3,X,I3,X,I3,X,I3)';'(8(I3,X), I3)'
                   
        result.K_mex[*] = tmp_var[0].z
        result.a_mex[*] = tmp_var[1].z
        result.K_SUM    = tmp_var[0].y
        result.A_median = tmp_var[1].y
        result.K_mex_max[*] = tmp_var[2].z
        result.a_mex_max[*] = tmp_var[3].z
        result.K_SUM_max    = tmp_var[2].y
        result.A_median_max = tmp_var[3].y
        result.K_mex_min[*] = tmp_var[4].z
        result.a_mex_min[*] = tmp_var[5].z
        result.K_SUM_min    = tmp_var[4].y
        result.A_median_min = tmp_var[5].y
	ENDELSE
	
RETURN, result
END   

FUNCTION kmex_array, date_i, date_f, variable, station, idx, HELP=help
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2] 
;##############################################################################
 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
	@set_up_commons
	set_up
	
    station         = set_var.gms[FIX(idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu	        
;###############################################################################    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1  

; define DH variables
        data_path='/home/isaac/MEGAsync/datos'
        
        string_date        = strarr(file_number)
               
        data_file_name_km  = strarr(file_number)
        data_file_name_kp  = strarr(file_number)  
                       
        string_date_2    = strarr(file_number)
             
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')	
                data_file_name_km[i] = data_path+'/Kmex/'+station_code+'_'+string_date[i]+'.index.final'
                		       
		        file = FILE_SEARCH(data_file_name_km[i], COUNT=opened_files)
	            IF opened_files NE N_ELEMENTS(file) THEN begin
	                data_file_name_km[i] = data_path+'/Kmex/'+station_code+'_'+string_date[i]+'.index.early'    
	            ENDIF 	
        	                            
        ENDFOR

        exist_data_file_km   = FILE_TEST(data_file_name_km)
        capable_to_plot_km   = N_ELEMENTS(where(exist_data_file_km EQ 1))

        IF capable_to_plot_km NE N_ELEMENTS(data_file_name_km) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.km_index.',A,' impossible to plot all data.')"              
        ENDIF
;###############################################################################
; Generate the time variables to plot kmex time series 
        k_mex    = fltarr(file_number*8)
        a_mex    = INTARR(file_number*8)
        FOR i = 0, N_ELEMENTS(exist_data_file_km)-1 DO BEGIN
                IF exist_data_file_km[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'                 
                        d_km = kmex([tmp_year, tmp_month, tmp_day], idx)
                        
                        k_mex[i*8:(i+1)*8-1] = d_km.k_mex[*]/10.
                        a_mex[i*8:(i+1)*8-1] = d_km.a_mex[*]
                ENDIF             
        ENDFOR
    
        var = {k : k_mex, a : a_mex}

    RETURN, var

END  
