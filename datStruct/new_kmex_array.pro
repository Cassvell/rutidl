;Name:
;	new_kmex_data
;purpose:
;	Create data structure from local geomagnetic base line data files 
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
;   .r DH_teo
;   DH_teo, [yyyy, mm, dd]
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



FUNCTION new_kmex_data, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
;reading data files
        date = STRING(year, month, day, FORMAT = '(I4, I02, I02)')
		path='/home/isaac/geomstorm/datos'
		
		name = +date+'.dat'
		
		file_name = path+'/Kmex/new_kmex/'+name		
		file = FILE_SEARCH(file_name, COUNT=opened_files)	
		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		IF err NE 0 THEN MESSAGE, 'Error opening '+file_name[0]
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;###############################################################################
;extracting data and denfining an structure data
        idx_newkmex      = {n_kmex1      : intarr(8), $
                            n_kmex2      : intarr(8)}
        
        struct = {x : [0, 0, 0, 0, 0, 0, 0, 0]}
        
        tmp_var = REPLICATE(struct, 2)  
		READS, data, tmp_var, FORMAT='(I3, I3, I3, I3, I3, I3, I3, I3)'
	;	PRINT, data
		idx_newkmex.n_kmex1[*]   = tmp_var[0].x
		idx_newkmex.n_kmex2[*]   = tmp_var[1].x				
		RETURN, idx_newkmex	
END

FUNCTION new_kmex_array, date_i, date_f, variable, HELP=help
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
        data_path='/home/isaac/geomstorm/datos/Kmex/new_kmex/'
        data_file_name_kmn = STRARR(file_number)                                                             
        string_date        = STRARR(file_number)
              
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
		        data_file_name_kmn[i]= data_path+string_date[i]+'.dat'		
              ;  file_kmn = FILE_SEARCH(data_file_name_kmn[i], COUNT=opened_files)	
               ; PRINT,     data_file_name_kmn[i]                             
        ENDFOR

        exist_data_file_kmn   = FILE_TEST(data_file_name_kmn)
        capable_to_plot_kmn   = N_ELEMENTS(WHERE(exist_data_file_kmn EQ 1))
      ;  PRINT,     FILE_SEARCH(data_file_name_kmn)
        IF capable_to_plot_kmn NE N_ELEMENTS(data_file_name_kmn) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.kmn_index.',A,' impossible to plot all data.')"              
        ENDIF
;###############################################################################
; Generate the time variables to plot new kmex time series  
        new_kmex1    = FLTARR(file_number*8)
        new_kmex2    = FLTARR(file_number*8)
        FOR i = 0, N_ELEMENTS(exist_data_file_kmn)-1 DO BEGIN
                IF exist_data_file_kmn[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'                 
                        d_km = new_kmex_data([tmp_year, tmp_month, tmp_day])
                        
                        new_kmex1[i*8:(i+1)*8-1] = d_km.n_kmex1[*]/10.0
                        new_kmex2[i*8:(i+1)*8-1] = d_km.n_kmex2[*]/10.0
                        ;PRINT, new_kmex1[i*8:(i+1)*8-1]
                ENDIF             
        ENDFOR
   ; PRINT, new_kmex1
    CASE variable of
        'new_kmex1'    : variable = new_kmex1 
        'new_kmex2'    : variable = new_kmex2
        ELSE : PRINT, 'variable selected is not avaiable or valid'        
    ENDCASE
    
    RETURN, variable
END 
