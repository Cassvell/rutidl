


function Date2DOY, idate
;	------------------------------------------------------------
;+							18-Sep-91
;	NAME: 
;		Date2DOY
;	PURPOSE:
;		Convert yymmdd into DOY (day of the year).  Input can
;		be either string or integer.  The year is an Optional 
;		return parameter.
;	CALLING SEQUENCE:
;		Date2DOY, idate, DOY [, yr]
;	INPUT:
;		idate	input format for the date: yymmdd.
;			Data-type can be either string or integer.
;	OUTPUT:
;		DOY	integer with the day of the year.
;	Output/Optional:
;		yr	year of the returned DOY.
;	Note:	If input data-type is string the returned values are
;		string-type and if input-type is longword the returned
;		parameters (DOY and yr) are integers.
;	HISTORY:
;		written by GAL 18-Sep-91
;-
;	-----------------------------------------------------------------
;	ON_ERROR, 2	;force a return to caller on error

;	Check data type of input set ascII flag and convert to yy,mm,dd:
	info = SIZE(idate)
	IF (info(0) eq 0) THEN BEGIN
	  scalar = 1				;scalar flag set
	ENDIF ELSE BEGIN
	  scalar = 0				;vector input
	ENDELSE

	IF (info(info(0) + 1) eq 7) THEN BEGIN
	  ascII = 1				;ascII input flag set
	  yy = FIX(STRMID(idate,0,2))		;extract year
	  mm = FIX(STRMID(idate,2,2))		;extract month
	  dd = FIX(STRMID(idate,4,2))		;extract day
	ENDIF ELSE BEGIN			;should be a longWord
	  ascII = 0				;non-ascII input
	  sdate = STRTRIM(STRING(idate),2)	;convert to string 
	  yy = FIX(STRMID(sdate,0,2))		;extract year
	  mm = FIX(STRMID(sdate,2,2))		;extract month
	  dd = FIX(STRMID(sdate,4,2))		;extract day
	ENDELSE

;	Check for leap year and compute DOY:
;       	      J   F   M   A   M   J   J   A   S   O   N   D
	imonth = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	IF (scalar) THEN BEGIN			;scalar input
	  IF ((yy MOD 4) eq 0) THEN BEGIN	;leap year
	    imonth(2) = 29			;set feb
	  ENDIF
	  DOY = FIX( TOTAL(imonth(0:mm-1)) ) + dd
	ENDIF ELSE BEGIN
	  DOY = dd				;set correct len on vector
	  leapYrs = WHERE( (yy MOD 4) eq 0)	;index of leap years
	  nonLeap = WHERE( (yy MOD 4) ne 0)	;index of non-leap years
	  IF (nonLeap(0) ne -1) THEN BEGIN
	    FOR i=0, N_elements(nonLeap)-1 DO BEGIN
	      DOY(nonLeap(i)) = FIX( TOTAL(imonth(0:mm(nonLeap(i))-1)) ) + $
				dd(nonLeap(i))
	    ENDFOR
	  ENDIF
	  IF (leapYrs(0) ne -1) THEN BEGIN
	    imonth(2) = 29			;set feb
	    FOR i =0, N_elements(leapYrs)-1 DO BEGIN
	      DOY(leapYrs(i)) = FIX( TOTAL(imonth(0:mm(leapYrs(i))-1)) ) + $
				dd(leapYrs(i))
	    ENDFOR
	  ENDIF
	ENDELSE

	IF (N_PARAMS() EQ 3) THEN BEGIN         ;pass year back to caller
          IF (ascII) THEN BEGIN
	    DOY = STRTRIM( STRING(DOY), 2)	;convert to string	    
	    yr = STRTRIM( STRING(yy), 2)	;convert to string	  
	  ENDIF ELSE BEGIN
	    yr = yy	
	  ENDELSE			
	ENDIF ELSE BEGIN			;pass DOY only
	  IF (ascII) THEN BEGIN
	    DOY = STRTRIM( STRING(DOY), 2)	;convert to string
	  ENDIF
	ENDELSE
	;print, uint(DOY)  
	return, DOY

	END

function kp_data, in

	On_error, 2
	compile_opt idl2, HIDDEN

        header = 36             ; Defining number of lines of the header 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        year = string(in, format = '(I4)')
		file_name = '../rutidl/kp/Kp_'+ year+'-01-01_'+year+'-12-31_D.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)

	    IF opened_files NE N_ELEMENTS(file) THEN begin
	        file_name = '../rutidl/kp/Kp_'+ year+'-01-01_'+year+'-12-31_P.dat'
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN begin
    	        file_name = '../rutidl/kp/Kp_'+ year+'-01-01_'+year+'-12-31_Q.dat'
	            file = FILE_SEARCH(file_name, COUNT=opened_files)    	        
    	        IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  
    	    ENDIF    	    	    
	    ENDIF     
        
		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
                      DOY : 0, Kp: 0, Kp_str: '', Ap: 0}

		resulting_data0 = REPLICATE(DataStruct, number_of_lines-header)	
        ;print, number_of_lines-header-1, number_of_lines-header+1
        
        
		READS, data[header:number_of_lines-1], resulting_data0, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,8X,I3,X,I1,A1,X,I3)'
		
		indexes_07 = WHERE(resulting_data0[*].Kp_str EQ '-')
		indexes_03 = WHERE(resulting_data0[*].Kp_str EQ '+')
		indexes_00 = WHERE(resulting_data0[*].Kp_str EQ 'o')
		
		Kp_tmp = resulting_data0[*].Kp*10
		
		Kp_tmp[indexes_07] = Kp_tmp[indexes_07]-3
		Kp_tmp[indexes_03] = Kp_tmp[indexes_03]+3


                DataStruct2 = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
                      DOY : 0, Kp: 0, Ap: 0}

		kp_data = REPLICATE(DataStruct2, number_of_lines-header)	

		kp_data[*].year   = resulting_data0[*].year
		kp_data[*].month  = resulting_data0[*].month
		kp_data[*].day    = resulting_data0[*].day
		kp_data[*].hour   = resulting_data0[*].hour
		kp_data[*].minute = resulting_data0[*].minute
		kp_data[*].DOY    = resulting_data0[*].DOY
		kp_data[*].Kp     = Kp_tmp[*]
		kp_data[*].Ap     = resulting_data0[*].AP				
		return, kp_data
end   


function kmex, date
	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, format = '(I4, I02, I02)')
		
		name = 'teo_'+date+'.index.'
		
		file_name = '../rutidl/Kmex/'+name+'final'		
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
	    IF opened_files NE N_ELEMENTS(file) THEN begin
	        file_name = '../rutidl/Kmex/'+name+'early'
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  
	    
	    endif

		number_of_lines = FILE_LINES(file)
	   ; print, number_of_lines
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		IF err NE 0 THEN MESSAGE, 'Error opening '+file_name[0]
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-        
        idx_kmex      = {k_mex      : intarr(8), $
                         k_mex_sum  : 0, $
                         a_mex      : intarr(8), $
                         a_med      : 0}
        
        struct = {x : [0, 0, 0, 0, 0, 0, 0, 0], y : 0}
        
        tmp_var = replicate(struct, 2)

	;	idx_kmex = REPLICATE(DStruct, number_of_lines-header)	
  
		READS, data, tmp_var, FORMAT='(I3, I4, I4, I4, I4, I4, I4, I4, I4)'
		
		idx_kmex.k_mex[*]   = tmp_var[0].x
		idx_kmex.a_mex[*]   = tmp_var[1].x
        idx_kmex.k_mex_sum  = tmp_var[0].y
        idx_kmex.a_med      = tmp_var[1].y				
		return, idx_kmex	
end   


;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################

    
pro magdat2_corrV2, kp_data, DOY, PNG = png, JPEG = jpeg

	    On_error, 2
	    compile_opt idl2, HIDDEN
	    
	    yr_i1	= 2003
	    mh_i1	= 11
	    dy_i1 	= 19	

	    yr_f1	= 2003
	    mh_f1	= 11
	    dy_f1 	= 25	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat1 = kp_data(yr_i1)
        t1 = n_elements(dat1.year)
        ;print, t
        
        i_ap1 = dat1.ap
        i_kp1  = dat1.Kp
        ;print, t
        year1 = dat1.year
        tiempo1 = TIMEGEN(t1, START=julday(dat1.month[0], dat1.day[0],  $
                         dat1.year[0], dat1.hour[0]), UNITS='Hours')                                     
        
        iyear1 = strmid(string(yr_i1, format='(I4)'),2,2)
        fyear1 = strmid(string(yr_f1, format='(I4)'),2,2)
                
        idoy1      = Date2DOY(string(iyear1, mh_i1, dy_i1,format = '(I02,I02,I02)'))
        fdoy1      = Date2DOY(string(fyear1, mh_f1, dy_f1,format = '(I02,I02,I02)'))   
        
        time_w1  = tiempo1[idoy1:fdoy1]
        tw1 = n_elements(time_w1)
        tot_days1= findgen(tw1*8)/8.0
        ap1      = i_ap1[(idoy1*8)-8:fdoy1*8]  
        kp1      = i_kp1[(idoy1*8)-8:fdoy1*8]
        kp1      = float(kp1)/10                                                                   		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number1    = (JULDAY(mh_f1, dy_f1, yr_f1) - JULDAY(mh_i1, dy_i1, yr_i1))+1
            data_file_name1 = strarr(file_number1)
            string_date1     = strarr(file_number1)
           
            FOR i=0ll, file_number1-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i1, dy_i1, yr_i1)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date1[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name1[i] = '../rutidl/Kmex/'+'teo_'+string_date1[i]+'index.final'
                                        
                    file1 = FILE_SEARCH(data_file_name1[i], COUNT=opened_files)
	                IF opened_files NE N_ELEMENTS(file1) THEN begin
	                    data_file_name1[i] = '../rutidl/Kmex/'+'teo_'+string_date1[i]+'.index.early'    
	                ENDIF	
            ENDFOR

            exist_data_file1   = FILE_TEST(data_file_name1)
            capable_to_plot1   = N_ELEMENTS(where(exist_data_file1 EQ 1))

            IF capable_to_plot1 NE N_ELEMENTS(data_file_name1) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            k_mex1    = INTARR(file_number1*8)
            a_mex1    = INTARR(file_number1*8)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file1)-1 DO BEGIN
                    IF exist_data_file1[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date1[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat1 = kmex([tmp_year, tmp_month, tmp_day])
                            
                            k_mex1[i*8:(i+1)*8-1] = dat1.k_mex[*]/10
                            a_mex1[i*8:(i+1)*8-1] = dat1.a_mex[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             k_mex1[i*8:(i+1)*8-1] = 999999.0

                             a_mex1[i*8:(i+1)*8-1] = 999999.0                        
                    ENDELSE                
            ENDFOR                 
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
	    yr_i2	= 2004
	    mh_i2	= 11
	    dy_i2 	= 6	

	    yr_f2	= 2004
	    mh_f2	= 11
	    dy_f2 	= 12	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat2 = kp_data(yr_i2)
        t2 = n_elements(dat2.year)
        ;print, t
        
        i_ap2 = dat2.ap
        i_kp2  = dat2.Kp
        ;print, t
        year2 = dat2.year
        tiempo2 = TIMEGEN(t2, START=julday(dat2.month[0], dat2.day[0],  $
                         dat2.year[0], dat2.hour[0]), UNITS='Hours')                                     
        
        iyear2 = strmid(string(yr_i2, format='(I4)'),2,2)
        fyear2 = strmid(string(yr_f2, format='(I4)'),2,2)
                
        idoy2      = Date2DOY(string(iyear2, mh_i2, dy_i2,format = '(I02,I02,I02)'))
        fdoy2      = Date2DOY(string(fyear2, mh_f2, dy_f2,format = '(I02,I02,I02)'))   
        
        time_w2  = tiempo2[idoy2:fdoy2]
        tw2 = n_elements(time_w2)
        tot_days2= findgen(tw2*8)/8.0
        ap2      = i_ap2[(idoy2*8)-8:fdoy2*8]  
        kp2      = i_kp2[(idoy2*8)-8:fdoy2*8]
        kp2      = float(kp2)/10                                                                           		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number2    = (JULDAY(mh_f2, dy_f2, yr_f2) - JULDAY(mh_i2, dy_i2, yr_i2))+1
            data_file_name2 = strarr(file_number2)
            string_date2     = strarr(file_number2)
           
            FOR i=0ll, file_number2-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i2, dy_i2, yr_i2)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date2[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name2[i] = '../rutidl/Kmex/'+'teo_'+string_date2[i]+'index.final'
                    
                    file2 = FILE_SEARCH(data_file_name2[i], COUNT=opened_files)
	                IF opened_files NE N_ELEMENTS(file2) THEN begin
	                    data_file_name2[i] = '../rutidl/Kmex/'+'teo_'+string_date2[i]+'.index.early'    
	                ENDIF                    
            ENDFOR

            exist_data_file2   = FILE_TEST(data_file_name2)
            capable_to_plot2   = N_ELEMENTS(where(exist_data_file2 EQ 1))

            IF capable_to_plot2 NE N_ELEMENTS(data_file_name2) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            k_mex2    = INTARR(file_number2*8)
            a_mex2    = INTARR(file_number2*8)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file2)-1 DO BEGIN
                    IF exist_data_file2[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date2[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat2 = kmex([tmp_year, tmp_month, tmp_day])
                            
                            k_mex2[i*8:(i+1)*8-1] = dat2.k_mex[*]/10
                            a_mex2[i*8:(i+1)*8-1] = dat2.a_mex[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             k_mex2[i*8:(i+1)*8-1] = 999999.0

                             a_mex2[i*8:(i+1)*8-1] = 999999.0                        
                    ENDELSE                
            ENDFOR                 
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;############################################################################### 
	    yr_i3	= 2005
	    mh_i3	= 5
	    dy_i3 	= 14	

	    yr_f3	= 2005
	    mh_f3	= 5
	    dy_f3 	= 20	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat3 = kp_data(yr_i3)
        t3 = n_elements(dat3.year)
        ;print, t
        
        i_ap3 = dat3.ap
        i_kp3  = dat3.Kp
        ;print, t
        year3 = dat3.year
        tiempo3 = TIMEGEN(t3, START=julday(dat3.month[0], dat3.day[0],  $
                         dat3.year[0], dat3.hour[0]), UNITS='Hours')                                     
        
        iyear3 = strmid(string(yr_i3, format='(I4)'),2,2)
        fyear3 = strmid(string(yr_f3, format='(I4)'),2,2)
                
        idoy3      = Date2DOY(string(iyear3, mh_i3, dy_i3,format = '(I02,I02,I02)'))
        fdoy3      = Date2DOY(string(fyear3, mh_f3, dy_f3,format = '(I02,I02,I02)'))   
        
        time_w3  = tiempo3[idoy3:fdoy3]
        tw3 = n_elements(time_w3)
        tot_days3= findgen(tw3*8)/8.0
        ap3      = i_ap3[(idoy3*8)-8:fdoy3*8]  
        kp3      = i_kp3[(idoy3*8)-8:fdoy3*8]
        kp3      = float(kp3)/10                                                                           		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number3    = (JULDAY(mh_f3, dy_f3, yr_f3) - JULDAY(mh_i3, dy_i3, yr_i3))+1
            data_file_name3 = strarr(file_number3)
            string_date3     = strarr(file_number3)
           
            FOR i=0ll, file_number3-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i3, dy_i3, yr_i3)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date3[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name3[i] = '../rutidl/Kmex/'+'teo_'+string_date3[i]+'index.final'
                    
                    file3 = FILE_SEARCH(data_file_name3[i], COUNT=opened_files)
	                IF opened_files NE N_ELEMENTS(file3) THEN begin
	                    data_file_name3[i] = '../rutidl/Kmex/'+'teo_'+string_date3[i]+'.index.early'    
	                ENDIF                    
            ENDFOR

            exist_data_file3   = FILE_TEST(data_file_name3)
            capable_to_plot3   = N_ELEMENTS(where(exist_data_file3 EQ 1))

            IF capable_to_plot3 NE N_ELEMENTS(data_file_name3) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            k_mex3    = INTARR(file_number3*8)
            a_mex3    = INTARR(file_number3*8)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file3)-1 DO BEGIN
                    IF exist_data_file3[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date3[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat3 = kmex([tmp_year, tmp_month, tmp_day])
                            
                            k_mex3[i*8:(i+1)*8-1] = dat3.k_mex[*]/10
                            a_mex3[i*8:(i+1)*8-1] = dat3.a_mex[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             k_mex3[i*8:(i+1)*8-1] = 999999.0

                             a_mex3[i*8:(i+1)*8-1] = 999999.0                        
                    ENDELSE
             ENDFOR           
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
	    yr_i4	= 2015
	    mh_i4	= 3
	    dy_i4 	= 15	

	    yr_f4	= 2015
	    mh_f4	= 3
	    dy_f4 	= 21	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat4 = kp_data(yr_i4)
        t4 = n_elements(dat4.year)
        ;print, t
        
        i_ap4 = dat4.ap
        i_kp4  = dat4.Kp
        ;print, t
        year4 = dat4.year
        tiempo4 = TIMEGEN(t4, START=julday(dat4.month[0], dat4.day[0],  $
                         dat4.year[0], dat4.hour[0]), UNITS='Hours')                                     
        
        iyear4 = strmid(string(yr_i4, format='(I4)'),2,2)
        fyear4 = strmid(string(yr_f4, format='(I4)'),2,2)
                
        idoy4      = Date2DOY(string(iyear4, mh_i4, dy_i4,format = '(I02,I02,I02)'))
        fdoy4      = Date2DOY(string(fyear4, mh_f4, dy_f4,format = '(I02,I02,I02)'))   
        
        time_w4  = tiempo4[idoy4:fdoy4]
        tw4 = n_elements(time_w4)
        tot_days4= findgen(tw4*8)/8.0
        ap4      = i_ap4[(idoy4*8)-8:fdoy4*8]  
        kp4      = i_kp4[(idoy4*8)-8:fdoy4*8]
        kp4      = float(kp4)/10                                                                           		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number4    = (JULDAY(mh_f4, dy_f4, yr_f4) - JULDAY(mh_i4, dy_i4, yr_i4))+1
            data_file_name4 = strarr(file_number4)
            string_date4     = strarr(file_number4)
           
            FOR i=0ll, file_number4-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i4, dy_i4, yr_i4)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date4[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name4[i] = '../rutidl/Kmex/'+'teo_'+string_date4[i]+'index.final'
                    
                    file4 = FILE_SEARCH(data_file_name4[i], COUNT=opened_files)
	                IF opened_files NE N_ELEMENTS(file4) THEN begin
	                    data_file_name4[i] = '../rutidl/Kmex/'+'teo_'+string_date4[i]+'.index.early'    
	                ENDIF                    
            ENDFOR

            exist_data_file4   = FILE_TEST(data_file_name4)
            capable_to_plot4   = N_ELEMENTS(where(exist_data_file4 EQ 1))

            IF capable_to_plot4 NE N_ELEMENTS(data_file_name4) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            k_mex4    = INTARR(file_number4*8)
            a_mex4    = INTARR(file_number4*8)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file4)-1 DO BEGIN
                    IF exist_data_file4[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date4[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat4 = kmex([tmp_year, tmp_month, tmp_day])
                            
                            k_mex4[i*8:(i+1)*8-1] = dat4.k_mex[*]/10
                            a_mex4[i*8:(i+1)*8-1] = dat4.a_mex[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             k_mex4[i*8:(i+1)*8-1] = 999999.0

                             a_mex4[i*8:(i+1)*8-1] = 999999.0                        
                    ENDELSE
            ENDFOR                
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
	    yr_i5	= 2017
	    mh_i5	= 5
	    dy_i5 	= 26	

	    yr_f5	= 2017
	    mh_f5	= 6
	    dy_f5 	= 1	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat5 = kp_data(yr_i5)
        t5 = n_elements(dat5.year)
        ;print, t
        
        i_ap5 = dat5.ap
        i_kp5  = dat5.Kp
        ;print, t
        year5 = dat5.year
        tiempo5 = TIMEGEN(t5, START=julday(dat5.month[0], dat5.day[0],  $
                         dat5.year[0], dat5.hour[0]), UNITS='Hours')                                     
        
        iyear5 = strmid(string(yr_i5, format='(I4)'),2,2)
        fyear5 = strmid(string(yr_f5, format='(I4)'),2,2)
                
        idoy5      = Date2DOY(string(iyear5, mh_i5, dy_i5,format = '(I02,I02,I02)'))
        fdoy5      = Date2DOY(string(fyear5, mh_f5, dy_f5,format = '(I02,I02,I02)'))   
        
        time_w5  = tiempo5[idoy5:fdoy5]
        tw5 = n_elements(time_w5)
        tot_days5= findgen(tw5*8)/8.0
        ap5      = i_ap5[(idoy5*8)-8:fdoy5*8]  
        kp5      = i_kp5[(idoy5*8)-8:fdoy5*8]  
        kp5      = float(kp5)/10                                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number5    = (JULDAY(mh_f5, dy_f5, yr_f5) - JULDAY(mh_i5, dy_i5, yr_i5))+1
            data_file_name5 = strarr(file_number5)
            string_date5     = strarr(file_number5)
           
            FOR i=0ll, file_number5-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i5, dy_i5, yr_i5)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date5[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name5[i] = '../rutidl/Kmex/'+'teo_'+string_date5[i]+'index.final'
                    
                    file5 = FILE_SEARCH(data_file_name5[i], COUNT=opened_files)
	                IF opened_files NE N_ELEMENTS(file5) THEN begin
	                    data_file_name5[i] = '../rutidl/Kmex/'+'teo_'+string_date5[i]+'.index.early'    
	                ENDIF                    
            ENDFOR

            exist_data_file5   = FILE_TEST(data_file_name5)
            capable_to_plot5   = N_ELEMENTS(where(exist_data_file5 EQ 1))

            IF capable_to_plot5 NE N_ELEMENTS(data_file_name5) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            k_mex5    = INTARR(file_number5*8)
            a_mex5    = INTARR(file_number5*8)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file5)-1 DO BEGIN
                    IF exist_data_file5[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date5[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat5 = kmex([tmp_year, tmp_month, tmp_day])
                            
                            k_mex5[i*8:(i+1)*8-1] = dat5.k_mex[*]/10
                            a_mex5[i*8:(i+1)*8-1] = dat5.a_mex[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             k_mex5[i*8:(i+1)*8-1] = 999999.0

                             a_mex5[i*8:(i+1)*8-1] = 999999.0                        
                    ENDELSE 
            ENDFOR                        
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;############################################################################### 
	    yr_i6	= 2017
	    mh_i6	= 9
	    dy_i6 	= 6	

	    yr_f6	= 2017
	    mh_f6	= 9
	    dy_f6 	= 11	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat6 = kp_data(yr_i6)
        t6 = n_elements(dat6.year)
        ;print, t
        
        i_ap6 = dat6.ap
        i_kp6  = dat6.Kp
        ;print, t
        year6 = dat6.year
        tiempo6 = TIMEGEN(t6, START=julday(dat6.month[0], dat6.day[0],  $
                         dat6.year[0], dat6.hour[0]), UNITS='Hours')                                     
        
        iyear6 = strmid(string(yr_i6, format='(I4)'),2,2)
        fyear6 = strmid(string(yr_f6, format='(I4)'),2,2)
                
        idoy6      = Date2DOY(string(iyear6, mh_i6, dy_i6,format = '(I02,I02,I02)'))
        fdoy6      = Date2DOY(string(fyear6, mh_f6, dy_f6,format = '(I02,I02,I02)'))   
        
        time_w6  = tiempo6[idoy6:fdoy6]
        tw6 = n_elements(time_w6)
        tot_days6= findgen(tw6*8)/8.0
        ap6      = i_ap6[(idoy6*8)-8:fdoy6*8]  
        kp6      = i_kp6[(idoy6*8)-8:fdoy6*8]  
        kp6      = float(kp6)/10                                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number6    = (JULDAY(mh_f6, dy_f6, yr_f6) - JULDAY(mh_i6, dy_i6, yr_i6))+1
            data_file_name6 = strarr(file_number6)
            string_date6     = strarr(file_number6)
           
            FOR i=0ll, file_number6-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i6, dy_i6, yr_i6)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date6[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name6[i] = '../rutidl/Kmex/'+'teo_'+string_date6[i]+'index.final'
                    
                    file6 = FILE_SEARCH(data_file_name6[i], COUNT=opened_files)
	                IF opened_files NE N_ELEMENTS(file6) THEN begin
	                    data_file_name6[i] = '../rutidl/Kmex/'+'teo_'+string_date6[i]+'.index.early'    
	                ENDIF                    
            ENDFOR

            exist_data_file6   = FILE_TEST(data_file_name6)
            capable_to_plot6   = N_ELEMENTS(where(exist_data_file6 EQ 1))

            IF capable_to_plot6 NE N_ELEMENTS(data_file_name6) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            k_mex6    = INTARR(file_number6*8)
            a_mex6    = INTARR(file_number6*8)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file6)-1 DO BEGIN
                    IF exist_data_file6[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date6[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat6 = kmex([tmp_year, tmp_month, tmp_day])
                            
                            k_mex6[i*8:(i+1)*8-1] = dat6.k_mex[*]/10
                            a_mex6[i*8:(i+1)*8-1] = dat6.a_mex[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             k_mex6[i*8:(i+1)*8-1] = 999999.0

                             a_mex6[i*8:(i+1)*8-1] = 999999.0                        
                    ENDELSE  
              ENDFOR                           
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################                               
            Device_bak = !D.Name 
            SET_PLOT, 'Z'
            
            Xsize=800
            Ysize=800
            ;DEVICE, decompose=0
            DEVICE, SET_RESOLUTION = [Xsize,Ysize]
            DEVICE, z_buffering=O
            DEVICE, set_character_size = [10, 12]
                 
            chr_size1 = 1.2
            chr_thick1= 1.5
            space     = 0.015
            rojo      = 248
            naranja  = 220
            verde     = 150
            negro     = 0
            azul      = 60
            blanco    = 255
            gris      = 130
            morado    = 30
            amarillo  = 200
            
        TVLCT, R_bak, G_bak, B_bak, /GET
            
        LOADCT, 39, /SILENT
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Write a post Script
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    path = '../rutidl/output/eventos_tgm/'
       str = TeXtoIDL('Amex')
    window_title = 'Correlacion de Ap y '+str+' bajo condiciones de tormenta'    
                         
    up0  =  450
    down0=  -50
    plot, ap1, a_mex1, POSITION=[0.1,0.05,0.95,0.95],$
    xstyle = 5, ystyle=5, YRANGE=[down0,up0], psym=1,$
    BACKGROUND = blanco, COLOR=negro, XRANGE=[down0,up0], CHARTHICK=1.2 
;###############################################################################
X = [-1, 0, 1, 0, -1]
Y = [0, 1, 0, -1, 0]
USERSYM, X, Y, /fill

    oplot, ap2, a_mex2, psym=8, color=rojo
    
A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL
    oplot, ap3, a_mex3, psym=8, color=amarillo   
     
X = [-1,0,1,-1]
Y = [-1,1,-1,-1]
USERSYM, X, Y, /fill    
    oplot, ap4, a_mex4, psym=8, color=morado
    
X = [-1,-1,1, 1,-1]
Y = [-1, 1,1,-1,-1]
USERSYM, X, Y, /fill        
    oplot, ap5, a_mex5, psym=8, color=verde
    oplot, ap6, a_mex6, psym=2, color=azul
    oplot, ap1, a_mex1, psym=1, color=negro
;###############################################################################
;###############################################################################
;###############################################################################    
    oplot, [20,20], [408, 408], color=negro, linestyle=0, psym=1
X = [-1, 0, 1, 0, -1]
Y = [0, 1, 0, -1, 0]
USERSYM, X, Y, /fill    
    oplot, [20,20], [391, 391], color=rojo, linestyle=0, psym=8
    
A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL    
    oplot, [20,20], [376, 376], color=amarillo, linestyle=0, psym=8
    
X = [-1,0,1,-1]
Y = [-1,1,-1,-1]
USERSYM, X, Y, /fill      
    oplot, [20,20], [358, 358], color=morado, linestyle=0, psym=8
    
X = [-1,-1,1, 1,-1]
Y = [-1, 1,1,-1,-1]
USERSYM, X, Y, /fill     
    oplot, [20,20], [342, 342], color=verde, linestyle=0, psym=8
    oplot, [20,20], [325, 325], color=azul, linestyle=0, psym=2                

    ;print, n_elements(H1), n_elements(dst1)

;###############################################################################
;###############################################################################
;###############################################################################     
   slope = findgen(601)-500
   x1= slope
   y1=slope
   plot, x1,y1, xstyle=5, ystyle=5, color=negro, background=blanco, $
   position=[0.1,0.08,0.95,0.95], title = window_title, /noerase, CHARSIZE = 1.2,$
   CHARTHICK=1.2
        AXIS, XAXIS = 0, XRANGE=[down0,up0], $
                         COLOR=negro, $
                         xstyle=1, $
                         CHARTHICK=1.2,$
                         CHARSIZE = 1.2, $
                         XTITLE = '', $
;                         CHARTHICK=chr_thick1, $
                         XTICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[down0,up0], $
                         COLOR=negro, $
                         xstyle=1, $
                         XTICKFORMAT='(A1)',$
                         XTICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down0,up0], $
                         YTITLE = '', $                          
                         COLOR=negro, $
                         ystyle=1, $
                         CHARSIZE = 1.2, $
                         CHARTHICK=1.2
                        
        AXIS, YAXIS = 1, YRANGE=[down0,up0], $
                         ystyle=1, $
                         COLOR=negro, $
                         yTICKFORMAT='(A1)',$
                         CHARSIZE = 1.2;, $
                        ; CHARTHICK=chr_thick1;, $   
    
        XYOUTS, 0.192, 0.89 , /NORMAL, $
                'Eventos:', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1        
    
        XYOUTS, 0.24, 0.869 , /NORMAL, $
                'TGM 1', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1

        XYOUTS, 0.24, 0.838 , /NORMAL, $
                'TGM 2', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
                
        XYOUTS, 0.24, 0.809 , /NORMAL, $
                'TGM 3', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1                

        XYOUTS, 0.24, 0.779 , /NORMAL, $
                'TGM 4', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1 

        XYOUTS, 0.24, 0.749 , /NORMAL, $
                'TGM 5', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1 

        XYOUTS, 0.24, 0.719 , /NORMAL, $
                'TGM 6', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1 

        POLYFILL, [0.185,0.32,0.32,0.185], [0.713,0.713,0.714,0.714], color = negro, /NORMAL
        POLYFILL, [0.185,0.32,0.32,0.185], [0.91,0.91,0.911,0.911], color = negro, /NORMAL
        
        POLYFILL, [0.185,0.18505,0.18505,0.185], [0.713,0.713,0.911,0.911], color = negro, /NORMAL
        POLYFILL, [0.32,0.3205,0.3205,0.32], [0.713,0.713,0.911,0.911], color = negro, /NORMAL                                
;###############################################################################    
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.03, y, str+'  [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.2

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.02, 'Ap [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.2               

    ;print, n_elements(H1), n_elements(dst1)

;###############################################################################
;###############################################################################
;###############################################################################     

 
                                                
         Image=TVRD() 
        TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
        
        TVLCT, R_bak, G_bak, B_bak
            
        ;DEVICE, /CLOSE
        SET_PLOT, Device_bak   
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; open the post stript device
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
            IF keyword_set(jpeg) THEN BEGIN
                    info = size(Image)
                    nx = info[1]
                    ny = info[2]
                    true_image = bytarr(3,nx,ny)
                    true_image[0,*,*] = reds[image]
                    true_image[1,*,*] = greens[image]
                    true_image[2,*,*] = blues[image]
                    write_jpeg, path+'ap_amex_corr.jpg', True_Image, true=1
            ENDIF ELSE BEGIN
                    IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                    WRITE_PNG, path+'ap_amex_corr.png', Image, reds,greens,blues
            ENDELSE

            IF NOT keyword_set(quiet) THEN BEGIN
                    print, '        Saving: '+path+'ap_amex_corr'
                    print, ''
            ENDIF
            RETURN
end
;############################################################################################################   
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################

                                                      
