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
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
	    IF opened_files NE N_ELEMENTS(file) THEN BEGIN
	        name = STRLOWCASE(station_code)+'_'+date+'.k_index.early'
			file_name = dir+name
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'  	    
	    ENDIF
	    
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
;        idx_kmex      = {k_mex      : intarr(8), $
 ;                        k_mex_sum  : 0, $
  ;                       a_mex      : intarr(8), $
  ;                       a_med      : 0}
        
;        struct = {x : [0, 0, 0, 0, 0, 0, 0, 0], y : 0}        
 ;       tmp_var = REPLICATE(struct, 2)
	;	READS, data, tmp_var, FORMAT='(I3, I4, I4, I4, I4, I4, I4, I4, I4)'
		
	;	idx_kmex.k_mex[*]   = tmp_var[0].x
	;	idx_kmex.a_mex[*]   = tmp_var[1].x
     ;   idx_kmex.k_mex_sum  = tmp_var[0].y
      ;  idx_kmex.a_med      = tmp_var[1].y				
      
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

RETURN, result
END   
