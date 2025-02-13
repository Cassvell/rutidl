FUNCTION ip_structm, date
        On_error, 2
        COMPILE_OPT idl2, HIDDEN

        year	= date[0]
        month	= date[1]
        day 	= date[2]	
        ;###############################################################################
        ;reading data files
        @set_up_commons
        set_up 

        path = set_var.Mega_dir+'ip/Ey/daily/'
        date = STRING(year, month, day, format = '(I4,I02,I02)')
                file_name = path+'ip_'+date+'1min.dat'

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

        DataStruct = {year : 0, doy : 0, hour : 0, minute : 0, Bt : 0.0, Bz : 0.0, Vx : 0.0, n_P : 0.0, T_P : 0.0, Pdyn : 0.0, Ey : 0.0}
        r_ip = REPLICATE(DataStruct, number_of_lines-header)	        
        ; PRINT, data[header:number_of_lines-1]
        READS, data[header:number_of_lines-1], r_ip, FORMAT='(I4, I5, I4, I4, F8, F10, F9, F8, F10, F7, F8)'
        RETURN, r_ip 
END

FUNCTION ip_arraym, date_i, date_f
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

    data_path= set_var.Mega_dir+'ip/Ey/daily/'
    string_date      = STRARR(file_number)
    fname= STRARR(file_number)  

    FOR i=0ll, file_number-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')

        fname[i]= data_path+'ip_'+string_date[i]+'1min.dat'	  
                                                                   
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
    tmp_Ey    = FLTARR(file_number*1440) 
    tmp_Bt    = FLTARR(file_number*1440) 
    tmp_Bz    = FLTARR(file_number*1440) 
    tmp_Vx    = FLTARR(file_number*1440) 
    tmp_np    = FLTARR(file_number*1440) 
    tmp_tp    = FLTARR(file_number*1440) 
    tmp_pdyn    = FLTARR(file_number*1440) 

    FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
        IF exist_data_file[i] EQ 1 THEN BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i] = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')                
                dat = ip_structm([tmp_year, tmp_month, tmp_day])
                tmp_Ey[i*1440:((i+1)*1440)-1] = dat.Ey[*]    
                tmp_Bz[i*1440:((i+1)*1440)-1] = dat.Bz[*]
                tmp_Bt[i*1440:((i+1)*1440)-1] = dat.Bt[*]
                tmp_Vx[i*1440:((i+1)*1440)-1] = dat.Vx[*]     
                tmp_np[i*1440:((i+1)*1440)-1] = dat.n_P[*]    
                tmp_tp[i*1440:((i+1)*1440)-1] = dat.T_P[*]
                tmp_pdyn[i*1440:((i+1)*1440)-1] = dat.Pdyn[*]                                 
        ;   asym[i*1440:(i+1)*1440-1] = dat.ASY_H[*]                                                                      
        ENDIF ELSE BEGIN
                tmp_Ey[i*1440:((i+1)*1440)-1] = 999.99
                tmp_Bz[i*1440:((i+1)*1440)-1] = 9999.99
                tmp_Bt[i*1440:((i+1)*1440)-1] = 9999.99
                tmp_Vx[i*1440:((i+1)*1440)-1] = 99999.9
                tmp_np[i*1440:((i+1)*1440)-1] = 999.99
                tmp_tp[i*1440:((i+1)*1440)-1] = 9999999
                tmp_pdyn[i*1440:((i+1)*1440)-1] = 99.99                
                ; asym[i*1440:(i+1)*1440-1] =999999.0                      
        ENDELSE                
    ENDFOR
    tmp_Ey = add_nan(tmp_Ey, 999.99, 'equal')
    tmp_Bt = add_nan(tmp_Bt, 9999.99, 'equal')	
    tmp_Bz = add_nan(tmp_Bz, 9999.99, 'equal')	
    tmp_Vx = add_nan(tmp_Vx, 99999.9, 'equal')	
    tmp_np = add_nan(tmp_np, 999.99, 'equal')
    tmp_tp = add_nan(tmp_tp, 9999999, 'equal')	
    tmp_pdyn = add_nan(tmp_pdyn, 99.99, 'equal')    
    
    ip_struct = {Ey : tmp_Ey, Bt : tmp_Bt, Bz : tmp_Bz, Vx : tmp_Vx, n_P : tmp_np, T_P : tmp_tp, Pdyn : tmp_pdyn}

    RETURN, ip_struct
    

END