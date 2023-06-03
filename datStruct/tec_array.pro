;Name:
;	tec_data.pro
;purpose:
;	Create data structure from local Total Electron Content data index files 
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
;   .r tec_data
;   tec_data([yyyy, mm, dd])
;parameters:
;   TEC data of 2H frequency
;
;dependencies:
;   Geomagnetic Service
;   Geophysics Institute
;   National Space Weather Laboratory
;
;input files
;   TEC data files
;
;output files:
;   TEC data structure
;
;version
; Dec, 2022



FUNCTION tec_data, idate
	On_error, 2
	compile_opt idl2, HIDDEN

	iyear	= idate[0]
	imonth	= idate[1]
	iday 	= idate[2]		

        header = 1      ; Defining number of lines of the header 
        path='/home/isaac/geomstorm/datos'
        idate = string(iyear, imonth, iday, format = '(I4, "-", I02, "-", I02)')
		file_name = path+'/tec/'+'tec_'+idate+'.txt'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

        DStruct = {doy : 0, tec : 0., med : 0.}                                   
		r_tec = REPLICATE(DStruct, number_of_lines-header)	      
		READS, data[header:number_of_lines-1], r_tec, $
	format='(F3, X, F5, X, F5)'		
		RETURN, r_tec
END


FUNCTION tec_array , date_i, date_f, variable, HELP=help
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
; define TEC variable daily name
        string_date        = STRARR(file_number)              
        data_file_name_tec  = STRARR(file_number)        
        string_date_2        = STRARR(file_number)
        
     data_path = '/home/isaac/geomstorm/datos'                         
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date_2[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4, "-", I02, "-", I02)')

                data_file_name_tec[i] = data_path+'/tec/'+'tec_'+string_date_2[i]+'.txt'
		        file_tec = FILE_SEARCH(data_file_name_tec[i], COUNT=opened_files)            	                            
        ENDFOR

        exist_data_file_tec   = FILE_TEST(data_file_name_tec)
        capable_to_plot_tec   = N_ELEMENTS(WHERE(exist_data_file_tec EQ 1))

        IF capable_to_plot_tec NE N_ELEMENTS(data_file_name_tec) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.tec_index.',A,' impossible to plot all data.')"              
        ENDIF
;###############################################################################
; Generate the time variables to plot TEC time series         
        tec  = fltarr(file_number*12)
        med  = fltarr(file_number*12)
        FOR i = 0, N_ELEMENTS(exist_data_file_tec)-1 DO BEGIN
                IF exist_data_file_tec[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date_2[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,X,I02,X,I02)'                 
                        d_tec = tec_data([tmp_year, tmp_month, tmp_day])
                        
                        tec[i*12:(i+1)*12-1] = d_tec.tec[*]
                        med[i*12:(i+1)*12-1] = d_tec.med[*] 
                ENDIF ELSE BEGIN
                        tec[i*12:(i+1)*12-1] = 999.0
                        med[i*12:(i+1)*12-1] = 999.0
                ENDELSE                
        ENDFOR

    CASE variable of 
        'tec' : variable = tec
        'med' : variable = med
        ELSE : PRINT, 'variable set is not avaiable or valid. Please, type help for variable references'
    ENDCASE
    
    RETURN, variable
END
