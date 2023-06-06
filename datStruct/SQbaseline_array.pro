;Name:
;	SQbaseline_data
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

FUNCTION SQbaseline_data, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr	= date[0]
	mh	= date[1]
	dy  = date[2]

        date = string(yr, mh, dy, FORMAT = '(I4,I02,I02)')
        header=0

		file_name = '../rutidl/output/Bsq_baselines/'+'Bsq_'+date+'h23.dat'
	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
		
        DStruct = {Bsq : 0.}
		B_sq = REPLICATE(DStruct, number_of_lines-header)	
		READS, data[header:number_of_lines-1], B_sq, FORMAT='(F08.4)' 		
		RETURN, B_sq
END	

FUNCTION SQbaseline_data_min, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr	= date[0]
	mh	= date[1]
	dy  = date[2]

        date = string(yr, mh, dy, FORMAT = '(I4,I02,I02)')
        header=0

		file_name = '../rutidl/output/Bsq_baselines/'+'Bsq_'+date+'m23.dat'
	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
		
        DStruct = {Bsq : 0.}
		B_sq = REPLICATE(DStruct, number_of_lines-header)	
		READS, data[header:number_of_lines-1], B_sq, FORMAT='(F08.4)' 		
		RETURN, B_sq
END

FUNCTION SQbaseline_array, date_i, date_f, HELP=help
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
        data_path='/home/isaac/MEGAsync/datos'
        data_file_name_bsq = STRARR(file_number) 
        string_date        = STRARR(file_number) 
              
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')   
                data_file_name_bsq[i]= '../rutidl/output/Bsq_baselines/Bsq_'+string_date[i]+'h23.dat'
                file_sq = FILE_SEARCH(data_file_name_bsq[i], COUNT=opened_files)                              		                                    
        ENDFOR

        exist_data_file_bsq   = FILE_TEST(data_file_name_bsq)
        capable_to_plot_bsq   = N_ELEMENTS(WHERE(exist_data_file_bsq EQ 1))  

      IF capable_to_plot_bsq NE N_ELEMENTS(data_file_name_bsq) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.bsq_index.',A,' impossible to plot all data.')"              
        ENDIF
        
;############################################################################### 
; define diurnal baseline  
        Bsq    = FLTARR(file_number*24)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_bsq)-1 DO BEGIN
                IF exist_data_file_bsq[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        sqline = SQbaseline_data([tmp_year, tmp_month, tmp_day])
                        
                        Bsq[i*24:(i+1)*24-1] = sqline.Bsq[*]
                ENDIF                
        ENDFOR        
        
    RETURN, Bsq        
END


FUNCTION SQbaseline_array_min, date_i, date_f, HELP=help
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
        data_path='/home/isaac/MEGAsync/datos'
        data_file_name_bsq = STRARR(file_number) 
        string_date        = STRARR(file_number) 
              
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')   
                data_file_name_bsq[i]= '../rutidl/output/Bsq_baselines/Bsq_'+string_date[i]+'m23.dat'
                file_sq = FILE_SEARCH(data_file_name_bsq[i], COUNT=opened_files)                              		                                    
        ENDFOR

        exist_data_file_bsq   = FILE_TEST(data_file_name_bsq)
        capable_to_plot_bsq   = N_ELEMENTS(WHERE(exist_data_file_bsq EQ 1))  

      IF capable_to_plot_bsq NE N_ELEMENTS(data_file_name_bsq) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.bsq_index.',A,' impossible to plot all data.')"              
        ENDIF
        
;############################################################################### 
; define diurnal baseline  
        Bsq    = FLTARR(file_number*1440)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_bsq)-1 DO BEGIN
                IF exist_data_file_bsq[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        sqline = SQbaseline_data_min([tmp_year, tmp_month, tmp_day])
                        
                        Bsq[i*1440:(i+1)*1440-1] = sqline.Bsq[*]
                ENDIF                
        ENDFOR        
        
    RETURN, Bsq  
        
END        
