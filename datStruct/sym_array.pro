;
;Name:
;	sym_array.pro
;purpose:
;	sym_data:   Generate an structured arrar of data from planetary sym index database for day
;   sym_array:  from daily data structure, generate time series of the structure data base
;
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
;   .r sym_array
;   sym_array([yyyy,mm,dd])
;
;parameters:
;   Sym index data files
;
;dependencies:
;   ISGI
;
;input files
;   sym_yyyymmddh.dat
;
;output files:
;   sym time series for data analysis
;
;Version
; March, 2023



FUNCTION sym_data, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    
	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
;reading data files
    @set_up_commons
    set_up 
        path=set_var.Mega_dir
        date = STRING(year, month, day, format = '(I4,"-",I02,"-",I02)')
		file_name = path+'/sym/daily/sym_'+date+'m_D.dat'

        header = 0             ; Defining number of lines of the header 
;###############################################################################
;reading data files
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN BEGIN
		    file_name = path+'/sym/daily/sym_'+date+'m_P.dat'
		    file = FILE_SEARCH(file_name, COUNT=opened_files)
		    file = FILE_SEARCH(file_name, COUNT=opened_files)               		    
		ENDIF
		 
		file = FILE_SEARCH(file_name, COUNT=opened_files)		
        IF opened_files NE N_ELEMENTS(file) THEN BEGIN		    
		    file_name = path+'/sym/daily/sym_'+date+'m_Q.dat'
		    file = FILE_SEARCH(file_name, COUNT=opened_files)
		    file = FILE_SEARCH(file_name, COUNT=opened_files)
		    ENDIF	
		    	
		file = FILE_SEARCH(file_name, COUNT=opened_files)		
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'							

		number_of_lines = FILE_LINES(file)
		;
		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

        DataStruct = {ASY_D : 0, ASY_H : 0, SYM_D : 0, SYM_H : 0}
		r_sym = REPLICATE(DataStruct, number_of_lines-header)	        
       ; PRINT, data[header:number_of_lines-1]
		READS, data[header:number_of_lines-1], r_sym, FORMAT='(I4, X, I4, X, I5, X, I5)'
		RETURN, r_sym
END

FUNCTION sym_array, date_i, date_f, HELP=help
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
        data_path=set_var.Mega_dir

        string_date_2      = STRARR(file_number)
        data_file_name_sym = STRARR(file_number)                 
             
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date_2[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')

                data_file_name_sym[i]= data_path+'/sym/daily/sym_'+string_date_2[i]+'m_D.dat'	
                	        	                                        
             	file_sym = FILE_SEARCH(data_file_name_sym[i], COUNT=opened_files)                
                IF opened_files NE N_ELEMENTS(file_sym) THEN BEGIN
                    data_file_name_sym[i] = data_path+'/sym/daily/sym_'+string_date_2[i]+'m_P.dat'
                ENDIF
                
                file_sym = FILE_SEARCH(data_file_name_sym[i], COUNT=opened_files)
                IF opened_files NE N_ELEMENTS(file_sym) THEN BEGIN
                    data_file_name_sym[i] = data_path+'/sym/daily/sym_'+string_date_2[i]+'m_Q.dat'
                ENDIF                    
        	   ; PRINT, data_file_name_sym                        
        ENDFOR
        exist_data_file_sym   = FILE_TEST(data_file_name_sym)
        capable_to_plot_sym   = N_ELEMENTS(where(exist_data_file_sym EQ 1))

        IF capable_to_plot_sym NE N_ELEMENTS(data_file_name_sym) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.sym_index.',A,' impossible to plot all data.')"              
        ENDIF           
;###############################################################################
;sym Data                       
        tmp_symH    = FLTARR(file_number*1440)
        tmp_symD    = FLTARR(file_number*1440)
        tmp_AsyH    = FLTARR(file_number*1440)
        tmp_AsyD    = FLTARR(file_number*1440) 
     ;   asym   = FLTARR(file_number*1440)                                      
        FOR i = 0, N_ELEMENTS(exist_data_file_sym)-1 DO BEGIN
                IF exist_data_file_sym[i] EQ 1 THEN BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date_2[i] = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')                
                        dat = sym_data([tmp_year, tmp_month, tmp_day])
                        
                        tmp_symH[i*1440:(i+1)*1440-1] = dat.SYM_H[*]                                                
                        tmp_AsyH[i*1440:(i+1)*1440-1] = dat.ASY_H[*]     
                        tmp_symD[i*1440:(i+1)*1440-1] = dat.SYM_D[*]                                                
                        tmp_AsyD[i*1440:(i+1)*1440-1] = dat.ASY_D[*]                                                                                         
                ENDIF ELSE BEGIN
                        tmp_symH[i*1440:(i+1)*1440-1] = 9999
                        tmp_AsyH[i*1440:(i+1)*1440-1] = 9999         
                        tmp_symD[i*1440:(i+1)*1440-1] = 9999                                            
                        tmp_AsyD[i*1440:(i+1)*1440-1] = 9999                        
                ENDELSE                
        ENDFOR
        
        variable = {symH : tmp_symH, symD : tmp_AsyH, asyH : tmp_AsyH, asyD : tmp_AsyD}

    RETURN, variable        
    
END      
