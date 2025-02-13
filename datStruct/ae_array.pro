FUNCTION ae_struct, date
        On_error, 2
        COMPILE_OPT idl2, HIDDEN

        year	= date[0]
        month	= date[1]
        day 	= date[2]	
        ;###############################################################################
        ;reading data files
        @set_up_commons
        set_up 

        path = set_var.Mega_dir+'ae/daily/'
        date = STRING(year, month, day, format = '(I4,I02,I02)')
                file_name = path+'ae_'+date+'.dat'

        header = 0             ; Defining number of lines of the header 
        ;###############################################################################
        ;reading data files
                        
        file = FILE_SEARCH(file_name, COUNT=opened_files)		
        IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'							

        number_of_lines = FILE_LINES(file)
        ;
        data = STRARR(number_of_lines)

        OPENR, lun, file, /GET_LUN, ERROR=err
        READF, lun, data, FORMAT = '(A)'
        CLOSE, lun
        FREE_LUN, lun

        DataStruct = {AE : 0, AU : 0, AL : 0, AO : 0}
        r_ip = REPLICATE(DataStruct, number_of_lines-header)	        
        ; PRINT, data[header:number_of_lines-1]
        READS, data[header:number_of_lines-1], r_ip, FORMAT='(I6, I7, I7, I7)'
        RETURN, r_ip 
END

FUNCTION ae_array, date_i, date_f
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

    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    ; define DH variables

    data_path= set_var.Mega_dir+'ae/daily/'
    string_date      = STRARR(file_number)
    fname= STRARR(file_number)  

    FOR i=0ll, file_number-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')

        fname[i]= data_path+'ae_'+string_date[i]+'.dat'	  
                                                                   
       ; PRINT, data_file_name_sym                        
    ENDFOR
    exist_data_file   = FILE_TEST(fname)
    capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

    IF capable_to_plot NE N_ELEMENTS(fname) THEN BEGIN 
            PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
            PRINT, FORMAT="('                missing GMS_YYYYMMDD.IP electric field data .',A,' impossible to plot all data.')"              
    ENDIF           
    ;###############################################################################
    ;IP Data                      
    tmp_AE    = FLTARR(file_number*1440) 
    tmp_AU    = FLTARR(file_number*1440) 
    tmp_AL    = FLTARR(file_number*1440) 
    tmp_AO    = FLTARR(file_number*1440) 

    FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
        IF exist_data_file[i] EQ 1 THEN BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i] = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')                
                dat = ae_struct([tmp_year, tmp_month, tmp_day])
                tmp_AE[i*1440:((i+1)*1440)-1] = dat.AE[*]    
                tmp_AU[i*1440:((i+1)*1440)-1] = dat.AU[*]
                tmp_AL[i*1440:((i+1)*1440)-1] = dat.AL[*]
                tmp_AO[i*1440:((i+1)*1440)-1] = dat.AO[*]                                
        ;   asym[i*1440:(i+1)*1440-1] = dat.ASY_H[*]                                                                      
        ENDIF ELSE BEGIN
                tmp_AE[i*1440:((i+1)*1440)-1] = 999.99
                tmp_AU[i*1440:((i+1)*1440)-1] = 9999.99
                tmp_AL[i*1440:((i+1)*1440)-1] = 9999.99
                tmp_AO[i*1440:((i+1)*1440)-1] = 99999.9
             
                ; asym[i*1440:(i+1)*1440-1] =999999.0                      
        ENDELSE                
    ENDFOR
    tmp_AE = add_nan(tmp_AE, 999.99, 'equal')
    tmp_AU = add_nan(tmp_AU, 9999.99, 'equal')	
    tmp_AL = add_nan(tmp_AL, 9999.99, 'equal')	
    tmp_AO = add_nan(tmp_AO, 99999.9, 'equal')	 
    
    ae_struct = {AE : tmp_AE, AU : tmp_AU, AL : tmp_AL, AO : tmp_AO}

    RETURN, ae_struct
    

END