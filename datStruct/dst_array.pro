;
;Name:
;	dst_array.pro
;purpose:
;	dst_data:   Generate an structured arrar of data from planetary Dst index database for day
;   dst_array:  from daily data structure, generate time series of the structure data base
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
;   .r dst_array
;   dst_array([yyyy,mm,dd])
;
;parameters:
;   Dst index data files
;
;dependencies:
;   ISGI
;
;input files
;   ip_yyyymmddh.dat
;
;output files:
;   dst time series for data analysis
;
;Version
; Dec, 2022



FUNCTION dst_data, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    
	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
;reading data files
        path='/home/isaac/datos'
        date = STRING(year, month, day, format = '(I4,"-",I02,"-",I02)')
		file_name = path+'/dst/daily/dst_'+date+'.txt'

        header = 1             ; Defining number of lines of the header 
;###############################################################################
;reading data files
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'		

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, second : 0, Dst: 0}
		r_dst = REPLICATE(DataStruct, number_of_lines-header)	        
        
		READS, data[header:number_of_lines-1], r_dst, FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,X,I2,I05)'
		RETURN, r_dst
END

FUNCTION dst_array, date_i, date_f, HELP=help
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

        string_date_2      = STRARR(file_number)
        data_file_name_dst = STRARR(file_number)                 
             
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date_2[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')

                data_file_name_dst[i]= data_path+'/dst/daily/dst_'+string_date_2[i]+'.txt'
                	                                        
             	file_dst = FILE_SEARCH(data_file_name_dst[i], COUNT=opened_files)

        	                            
        ENDFOR
        exist_data_file_dst   = FILE_TEST(data_file_name_dst)
        capable_to_plot_dst   = N_ELEMENTS(where(exist_data_file_dst EQ 1))

        IF capable_to_plot_dst NE N_ELEMENTS(data_file_name_dst) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.dst_index.',A,' impossible to plot all data.')"              
        ENDIF           
;###############################################################################
;Dst Data                       
        dst    = FLTARR(file_number*24)
        hour   = FLTARR(file_number*24)                             
        FOR i = 0, N_ELEMENTS(exist_data_file_dst)-1 DO BEGIN
                IF exist_data_file_dst[i] EQ 1 THEN BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date_2[i] = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')                
                        dat = dst_data([tmp_year, tmp_month, tmp_day])
                        
                        dst[i*24:(i+1)*24-1] = dat.Dst[*]                                                
                        hour[i*24:(i+1)*24-1] = dat.hour[*]                                                                      
                ENDIF ELSE BEGIN
                         dst[i*24:(i+1)*24-1] = 999999.0                      
                ENDELSE                
        ENDFOR
     
    var = {dst : dst, hour : hour}

    RETURN, var   
END                       
