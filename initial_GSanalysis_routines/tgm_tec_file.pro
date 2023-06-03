FUNCTION dst_data, date

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    
	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
;reading data files
        path='/home/c-isaac/Escritorio/proyecto/master_thesis/datos'
        date = string(year, month, day, format = '(I4,"-",I02,"-",I02)')
		file_name = path+'/dst/daily/dst_'+date+'.txt'
        header = 1             ; Defining number of lines of the header 
;###############################################################################
;reading data files
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'		

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
        second : 0, DOY : 0, Dst: 0}

		r_dst = REPLICATE(DataStruct, number_of_lines-header)	        
        
		READS, data[header:number_of_lines-1], r_dst, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,X,I2,I04,I5)'
		RETURN, r_dst
END


FUNCTION DH_teo, date
	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;###############################################################################
;reading data files
        date = string(year, month, day, format = '(I4, I02, I02)')

        path='/home/c-isaac/Escritorio/proyecto/master_thesis/datos'		
		file_name = path+'/dH_teo/'+'teo_'+date+'.dst.early'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;###############################################################################
;extracting data and denfining an structure data
        DStruct = {hora : 0, D_stdesv : 0., D : 0., H_stdesv : 0., H : 0., $
        Z_stdesv : 0., Z : 0., N_stdesv : 0., N : 0., F_stdesv : 0., F : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(I2, F10, F8, F10, F10, F10, F10, F10, F10, F10, F10)'		
		RETURN, teo_mag		
END

FUNCTION tec_data, idate
	On_error, 2
	compile_opt idl2, HIDDEN

	iyear	= idate[0]
	imonth	= idate[1]
	iday 	= idate[2]		

        header = 1      ; Defining number of lines of the header 
        path='/home/c-isaac/Escritorio/proyecto/master_thesis/datos'
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

PRO tgm_tec_file, date_i, date_f

	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]      
	
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 

    idate    = string(yr_i, mh_i, dy_i, FORMAT='(I4, I02,I02)')
    fdate    = string(yr_f, mh_f, dy_f, FORMAT='(I4, I02,I02)')
;###############################################################################
; define DH variables
        data_path='/home/c-isaac/Escritorio/proyecto/master_thesis/datos'
        
        string_date        = strarr(file_number)
        data_file_name_tec  = strarr(file_number)         
        data_file_name_dh  = strarr(file_number)                 
        data_file_name_dst = strarr(file_number)                        
        string_date_2    = strarr(file_number)
             
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                string_date_2[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,"-",I02,"-",I02)')
	
                data_file_name_dst[i]= data_path+'/dst/daily/dst_'+string_date_2[i]+'.txt'		        	        
                data_file_name_dh[i] = data_path+'/dH_teo/'+'teo_'+string_date[i]+'.dst.early'                   		       
                data_file_name_tec[i]= data_path+'/tec/tec_'+string_date_2[i]+'.txt'
                	            
		        file_dh = FILE_SEARCH(data_file_name_dh[i], COUNT=opened_files)
	            IF opened_files NE N_ELEMENTS(file_dh) THEN begin
	                data_file_name_dh[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'    
	            ENDIF

		        file_tec = FILE_SEARCH(data_file_name_tec[i], COUNT=opened_files)		        
        	                            
        ENDFOR


        exist_data_file_dh   = FILE_TEST(data_file_name_dh)
        capable_to_plot_dh   = N_ELEMENTS(where(exist_data_file_dh EQ 1))

        exist_data_file_dst   = FILE_TEST(data_file_name_dst)
        capable_to_plot_dst   = N_ELEMENTS(where(exist_data_file_dst EQ 1))

        IF capable_to_plot_dst NE N_ELEMENTS(data_file_name_dst) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.dst_index.',A,' impossible to plot all data.')"              
        ENDIF
                
        IF capable_to_plot_dh NE N_ELEMENTS(data_file_name_dh) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.dh_index.',A,' impossible to plot all data.')"              
        ENDIF   
        
        exist_data_file_tec   = FILE_TEST(data_file_name_tec)
        capable_to_plot_tec   = N_ELEMENTS(where(exist_data_file_tec EQ 1))
        IF capable_to_plot_tec NE N_ELEMENTS(data_file_name_tec) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.tec_index.',A,' impossible to plot all data.')"              
        ENDIF
            
;###############################################################################
; Generate the time variables to plot dH time series                                   
        H    = FLTARR(file_number*24)                       
        FOR i = 0, N_ELEMENTS(exist_data_file_dh)-1 DO BEGIN
                IF exist_data_file_dh[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_dh = DH_teo([tmp_year, tmp_month, tmp_day])
                        
                        H[i*24:(i+1)*24-1]   = d_dh.H[*]                                                                                             
                ENDIF ELSE BEGIN
                        H[i*24:(i+1)*24-1] = 999999.0
                ENDELSE                
        ENDFOR
;###############################################################################
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
;###############################################################################
;Dst Data                       
        dst    = FLTARR(file_number*24)     
        doy    = FLTARR(file_number*24) 
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
                      doy[i*24:(i+1)*24-1] = dat.DOY[*]    
                      hour[i*24:(i+1)*24-1] =dat.hour[*]                                                                                                                    
                ENDIF ELSE BEGIN
                         dst[i*24:(i+1)*24-1] = 999999.0                      
                ENDELSE                
        ENDFOR  
;############################################################################################################        
        i_nan1 = where(H eq 999999.0, ncount)
        i_nan2 = where(H gt 100.0, n2count)
        
        prcent_nan = FLOAT(ncount+n2count)*100.0
        print,'porcentaje de valores NaN:', prcent_nan/n_elements(H),'%'
        
        for i=0, n_elements(H)-1 do begin
            if H[i] eq 999999.0 then begin
                H[where(H[*] eq 999999.0)] = !Values.F_NAN          
            endif
        endfor
        
        for i=0, n_elements(H)-1 do begin
            if H[i] ge 100.0 then begin
                H[where(H[*] ge 100.0)] = !Values.F_NAN          
            endif
        endfor        
;############################################################################################################
    mlat         = 28.06*!pi
    ld           = cos(mlat/180)    
    dst_l    = dst*ld
    mag_diff = ABS(H-dst_l)
 
    mdif_pc  = mag_diff/ABS(dst_l)
;print, mdif_pc
    tec_diff = ABS(tec-med)
    tc_dif   = tec_diff
    tcdif_pc = tec_diff/med        
;############################################################################################################
    ; se genera un mag_diff con resolución bi-horaria
print, '#####################################################################3'    
    mag_diff_2H = FINDGEN(N_ELEMENTS(mag_diff)/6)
    FOR i=0, N_ELEMENTS(mag_diff_2H)-1 DO BEGIN
        ;print, mag_diff[(i+1)*2-1]
        mag_diff_2H[i] = MEAN(mag_diff[i*2:(i+1)*2-1], /NAN)    
    ENDFOR
   mg_dif   = mag_diff_2H
   
   
rank_mg = MEDIAN(mag_diff_2H) + STDDEV( mag_diff_2H, /NAN)
rank_tc = MEDIAN(tec_diff) + STDDEV( tec_diff, /NAN) 

    mg_out = WHERE(mag_diff_2H GE rank_mg)
    mg_in  = WHERE(mag_diff_2H LT rank_mg)
    
    tc_out = WHERE(tec_diff GE rank_tc)
    tc_in  = WHERE(tec_diff LT rank_tc)
    
    
    IF N_ELEMENTS(mg_out) AND N_ELEMENTS(tc_out) GE 1 THEN BEGIN
    ;mgdif = mag_diff_2H[mg_out]
    mag_diff_2H[mg_in] = !Values.F_NAN
    
    ;tcdif = tec_diff[tc_out]
    tec_diff[tc_in] = !Values.F_NAN
    ENDIF
    
;print, mag_diff_2H 
       
print, '#####################################################################3'    
;print, tec_diff 
  ;  tec_diff_6H = FINDGEN(N_ELEMENTS(tec_diff)/3)
  ;  FOR i=0, N_ELEMENTS(tec_diff_6H)-1 DO BEGIN
        ;print, mag_diff[(i+1)*2-1]
  ;      tec_diff_6H[i] = MEAN(tec_diff[i*3:(i+1)*3-1], /NAN)    
  ;  ENDFOR   
    ; print, tec_diff_4H   
;############################################################################################################
    outfile='../master_thesis/datos/tgm/article_events/tgm_tecdata'+idate+'_'+fdate+'.dat'
    OPENW, lun, outfile, /GET_LUN

    printf, lun, FORMAT = '("DOY",2X,"Hora",X,"mdif",4X,"tcdif")'
    FOR i=0, N_ELEMENTS(mag_diff_2H)-1 DO BEGIN

     ;  print, doy[i], hour[i], mag_diff_4H[i], tec_diff_4H[i], FORMAT = '(I03, F5.1, F7.2, F7.2)' 
     ;   print, doy[i], hour[i], mag_diff[i], ABS(dst_l[i]), FORMAT = '(I03, F5.1, F10.2, F8.1)'           
        printf, lun, doy[i], hour[i], mag_diff_2H[i], tec_diff[i], FORMAT = '(I03, F5.1, F7.2, F7.2)'              
    ENDFOR
    
    close,lun
    FREE_LUN, lun

END

