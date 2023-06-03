


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

function dst_data, initial

	On_error, 2
	compile_opt idl2, HIDDEN

	year = string(initial, format = '(I4)')
	;type_data = string(tp, format = '(A)')
		file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_D.dat'
		
        header = 25             ; Defining number of lines of the header 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		
	    IF opened_files NE N_ELEMENTS(file) THEN begin
	        file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_P.dat'
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN begin
    	        file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_Q.dat'
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
        DOY : 0, Dst: 0}

		r_dst = REPLICATE(DataStruct, number_of_lines-header)	        
        
		READS, data[header:number_of_lines-1], r_dst, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,8X,I3,X,I6)'
		
		return, r_dst
end


function DH_teo, date

	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, format = '(I4, I02, I02)')
       ; sts  = string(stats, format = '(A5)')
		
		file_name = '../rutidl/dH_teo/'+'teo_'+date+'.dst.early'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
	   ; print, number_of_lines
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        DStruct = {hora : 0, D_stdesv : 0., D : 0., H_stdesv : 0., H : 0., $
        Z_stdesv : 0., Z : 0., N_stdesv : 0., N : 0., F_stdesv : 0., F : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(I2, F10, F8, F10, F10, F10, F10, F10, F10, F10, F10)'
		
		return, teo_mag		
end


;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################

    
pro magdat_corrV2, res_dat, DOY, PNG = png, JPEG = jpeg

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
        dat1 = dst_data(yr_i1)
        t1 = n_elements(dat1.year)
        ;print, t
        
        i_dst1 = dat1.Dst
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
        tot_days1= findgen(tw1*24)/24.0
                                                         
        Date1 = string(yr_i1, mh_i1, dy_i1, FORMAT='(I4, "-", I02, "-", I02)')    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of DH Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-		    
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
                    data_file_name1[i] = '../rutidl/dH_teo/'+'teo_'+string_date1[i]+'.dst.early'
            ENDFOR

            exist_data_file1   = FILE_TEST(data_file_name1)
            capable_to_plot1   = N_ELEMENTS(where(exist_data_file1 EQ 1))

            IF capable_to_plot1 NE N_ELEMENTS(data_file_name1) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H1    = FLTARR(file_number1*24)
            
            H_STDESV1    = FLTARR(file_number1*24)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file1)-1 DO BEGIN
                    IF exist_data_file1[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date1[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat1 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H1[i*24:(i+1)*24-1] = dat1.H[*]

                            H_STDESV1[i*24:(i+1)*24-1] = dat1.H_stdesv[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             H1[i*24:(i+1)*24-1] = 999999.0

                             H_STDESV1[i*24:(i+1)*24-1] = 999999.0                        
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H1)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H1[i] eq 999999.0 then begin
                    H1[where(H1[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst1      = i_dst1[(idoy1*24)-24:fdoy1*24-1]
    H_tmp   = H1
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H1 = H_tmp        
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
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
        dat2 = dst_data(yr_i2)
        t2 = n_elements(dat2.year)
        ;print, t        
        i_dst2 = dat2.Dst
        ;print, t
        year2 = dat2.year
        tiempo2 = TIMEGEN(t2, START=julday(dat2.month[0], dat2.day[0],  $
                         dat2.year[0], dat2.hour[0]), UNITS='Hours')                                     
        
        iyear2 = strmid(string(yr_i2, format='(I4)'),2,2)
        fyear2 = strmid(string(yr_f2, format='(I4)'),2,2)
                
        idoy2      = Date2DOY(string(iyear2, mh_i2, dy_i2,format = '(I02,I02,I02)'))
        fdoy2      = Date2DOY(string(fyear2, mh_f2, dy_f2,format = '(I02,I02,I02)'))   
        
        time_w2  = tiempo2[idoy2:fdoy2]
        tw2      = n_elements(time_w2)
        tot_days2= findgen(tw2*24)/24.0                                                         		    
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
                    data_file_name2[i] = '../rutidl/dH_teo/'+'teo_'+string_date2[i]+'.dst.early'
            ENDFOR

            exist_data_file2   = FILE_TEST(data_file_name2)
            capable_to_plot2   = N_ELEMENTS(where(exist_data_file2 EQ 1))

            IF capable_to_plot2 NE N_ELEMENTS(data_file_name2) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H2    = FLTARR(file_number2*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file2)-1 DO BEGIN
                    IF exist_data_file2[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date2[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat2 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H2[i*24:(i+1)*24-1] = dat2.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                            H2[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H2)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H2[i] eq 999999.0 then begin
                    H2[where(H2[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst2      = i_dst2[(idoy2*24)-24:fdoy2*24-1]            
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
        dat3 = dst_data(yr_i3)
        t3 = n_elements(dat3.year)
        ;print, t        
        i_dst3 = dat3.Dst
        ;print, t
        year3 = dat3.year
        tiempo3 = TIMEGEN(t3, START=julday(dat3.month[0], dat3.day[0],  $
                         dat3.year[0], dat3.hour[0]), UNITS='Hours')                                     
        
        iyear3 = strmid(string(yr_i3, format='(I4)'),2,2)
        fyear3 = strmid(string(yr_f3, format='(I4)'),2,2)
                
        idoy3      = Date2DOY(string(iyear3, mh_i3, dy_i3,format = '(I02,I02,I02)'))
        fdoy3      = Date2DOY(string(fyear3, mh_f3, dy_f3,format = '(I02,I02,I02)'))   
        
        time_w3  = tiempo3[idoy3:fdoy3]
        tw3      = n_elements(time_w3)
        tot_days3= findgen(tw3*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number3    = (JULDAY(mh_f3, dy_f3, yr_f3) - JULDAY(mh_i3, dy_i3, yr_i3))+1
            data_file_name3 = strarr(file_number3)
            string_date3    = strarr(file_number3)
           
            FOR i=0ll, file_number2-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i3, dy_i3, yr_i3)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date3[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name3[i] = '../rutidl/dH_teo/'+'teo_'+string_date3[i]+'.dst.early'
            ENDFOR

            exist_data_file3   = FILE_TEST(data_file_name3)
            capable_to_plot3   = N_ELEMENTS(where(exist_data_file3 EQ 1))

            IF capable_to_plot3 NE N_ELEMENTS(data_file_name3) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H3    = FLTARR(file_number3*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file3)-1 DO BEGIN
                    IF exist_data_file3[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date3[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat3 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H3[i*24:(i+1)*24-1] = dat3.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H3[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H3)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H3[i] eq 999999.0 then begin
                    H3[where(H3[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst3      = i_dst3[(idoy3*24)-24:fdoy3*24]
    H_tmp   = H3
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H3 = H_tmp                    
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
        dat4    = dst_data(yr_i4)
        t4      = n_elements(dat4.year)      
        i_dst4  = dat4.Dst
        year4   = dat4.year
        
        tiempo4 = TIMEGEN(t4, START=julday(dat4.month[0], dat4.day[0],  $
                         dat4.year[0], dat4.hour[0]), UNITS='Hours')                                     
        
        iyear4 = strmid(string(yr_i4, format='(I4)'),2,2)
        fyear4 = strmid(string(yr_f4, format='(I4)'),2,2)
                
        idoy4  = Date2DOY(string(iyear4, mh_i4, dy_i4,format = '(I02,I02,I02)'))
        fdoy4  = Date2DOY(string(fyear4, mh_f4, dy_f4,format = '(I02,I02,I02)'))   
        
        time_w4  = tiempo4[idoy4:fdoy4]
        tw4      = n_elements(time_w4)
        tot_days4= findgen(tw4*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number4    = (JULDAY(mh_f4, dy_f4, yr_f4) - JULDAY(mh_i4, dy_i4, yr_i4))+1
            data_file_name4 = strarr(file_number4)
            string_date4    = strarr(file_number4)
           
            FOR i=0ll, file_number2-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i4, dy_i4, yr_i4)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date4[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name4[i] = '../rutidl/dH_teo/'+'teo_'+string_date4[i]+'.dst.early'
            ENDFOR

            exist_data_file4   = FILE_TEST(data_file_name4)
            capable_to_plot4   = N_ELEMENTS(where(exist_data_file4 EQ 1))

            IF capable_to_plot4 NE N_ELEMENTS(data_file_name4) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H4    = FLTARR(file_number4*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file4)-1 DO BEGIN
                    IF exist_data_file4[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date4[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat4 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H4[i*24:(i+1)*24-1] = dat4.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H4[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H4)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H4[i] eq 999999.0 then begin
                    H4[where(H4[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst4      = i_dst4[(idoy4*24)-24:fdoy4*24]
    H_tmp   = H4
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H4 = H_tmp                    
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
        dat5    = dst_data(yr_i5)
        t5      = n_elements(dat5.year)      
        i_dst5  = dat5.Dst
        year5   = dat5.year
        
        tiempo5 = TIMEGEN(t5, START=julday(dat5.month[0], dat5.day[0],  $
                         dat5.year[0], dat5.hour[0]), UNITS='Hours')                                     
        
        iyear5 = strmid(string(yr_i5, format='(I4)'),2,2)
        fyear5 = strmid(string(yr_f5, format='(I4)'),2,2)
                
        idoy5  = Date2DOY(string(iyear5, mh_i5, dy_i5,format = '(I02,I02,I02)'))
        fdoy5  = Date2DOY(string(fyear5, mh_f5, dy_f5,format = '(I02,I02,I02)'))   
        
        time_w5  = tiempo5[idoy5:fdoy5]
        tw5      = n_elements(time_w5)
        tot_days5= findgen(tw5*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number5    = (JULDAY(mh_f5, dy_f5, yr_f5) - JULDAY(mh_i5, dy_i5, yr_i5))+1
            data_file_name5 = strarr(file_number5)
            string_date5    = strarr(file_number5)
           
            FOR i=0ll, file_number5-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i5, dy_i5, yr_i5)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date5[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name5[i] = '../rutidl/dH_teo/'+'teo_'+string_date5[i]+'.dst.early'
            ENDFOR

            exist_data_file5   = FILE_TEST(data_file_name5)
            capable_to_plot5   = N_ELEMENTS(where(exist_data_file5 EQ 1))

            IF capable_to_plot5 NE N_ELEMENTS(data_file_name5) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H5    = FLTARR(file_number5*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file5)-1 DO BEGIN
                    IF exist_data_file3[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date5[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat5 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H5[i*24:(i+1)*24-1] = dat5.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H5[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H5)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H5[i] eq 999999.0 then begin
                    H5[where(H5[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst5      = i_dst5[(idoy5*24)-24:fdoy5*24] 
    H_tmp   = H5
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H5 = H_tmp                   
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
        dat6    = dst_data(yr_i6)
        t6      = n_elements(dat6.year)      
        i_dst6  = dat6.Dst
        year6   = dat6.year
        
        tiempo6 = TIMEGEN(t6, START=julday(dat6.month[0], dat6.day[0],  $
                         dat6.year[0], dat6.hour[0]), UNITS='Hours')                                     
        
        iyear6 = strmid(string(yr_i6, format='(I4)'),2,2)
        fyear6 = strmid(string(yr_f6, format='(I4)'),2,2)
                
        idoy6  = Date2DOY(string(iyear6, mh_i6, dy_i6,format = '(I02,I02,I02)'))
        fdoy6  = Date2DOY(string(fyear6, mh_f6, dy_f6,format = '(I02,I02,I02)'))   
        
        time_w6  = tiempo6[idoy6:fdoy6]
        tw6      = n_elements(time_w6)
        tot_days6= findgen(tw6*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number6    = (JULDAY(mh_f6, dy_f6, yr_f6) - JULDAY(mh_i6, dy_i6, yr_i6))+1
            data_file_name6 = strarr(file_number6)
            string_date6    = strarr(file_number6)
           
            FOR i=0ll, file_number6-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i6, dy_i6, yr_i6)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date6[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name6[i] = '../rutidl/dH_teo/'+'teo_'+string_date6[i]+'.dst.early'
            ENDFOR

            exist_data_file6   = FILE_TEST(data_file_name6)
            capable_to_plot6   = N_ELEMENTS(where(exist_data_file6 EQ 1))

            IF capable_to_plot6 NE N_ELEMENTS(data_file_name6) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H6    = FLTARR(file_number6*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file6)-1 DO BEGIN
                    IF exist_data_file6[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date6[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat6 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H6[i*24:(i+1)*24-1] = dat6.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H6[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H6)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H6[i] eq 999999.0 then begin
                    H6[where(H6[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst6      = i_dst6[(idoy6*24)-24:fdoy6*24]   
    H_tmp   = H6
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H6 = H_tmp                 
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
       dh = TeXtoIDL('\DeltaH')
    window_title = 'Correlacion de Dst y '+dh+' bajo condiciones de tormenta'    
                         
    up0  =  100
    down0=-500
    plot, dst1, H1, POSITION=[0.1,0.08,0.95,0.95],$
    xstyle = 5, ystyle=5, YRANGE=[down0,up0], psym=1,$
    BACKGROUND = blanco, COLOR=negro, XRANGE=[down0,up0]
;###############################################################################
X = [-1, 0, 1, 0, -1]
Y = [0, 1, 0, -1, 0]
USERSYM, X, Y, /fill

    oplot, dst2, H2, psym=8, color=rojo
    
A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL
    oplot, dst3, H3, psym=8, color=amarillo   
     
X = [-1,0,1,-1]
Y = [-1,1,-1,-1]
USERSYM, X, Y, /fill    
    oplot, dst4, H4, psym=8, color=morado
    
X = [-1,-1,1, 1,-1]
Y = [-1, 1,1,-1,-1]
USERSYM, X, Y, /fill        
    oplot, dst5, H5, psym=8, color=verde
    oplot, dst6, H6, psym=2, color=azul
;###############################################################################
;###############################################################################
;###############################################################################    
    oplot, [-420,-420], [50, 50], color=negro, linestyle=0, psym=1
X = [-1, 0, 1, 0, -1]
Y = [0, 1, 0, -1, 0]
USERSYM, X, Y, /fill    
    oplot, [-420,-420], [30, 30], color=rojo, linestyle=0, psym=8
    
A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL    
    oplot, [-420,-420], [10, 10], color=amarillo, linestyle=0, psym=8
    
X = [-1,0,1,-1]
Y = [-1,1,-1,-1]
USERSYM, X, Y, /fill      
    oplot, [-420,-420], [-10, -10], color=morado, linestyle=0, psym=8
    
X = [-1,-1,1, 1,-1]
Y = [-1, 1,1,-1,-1]
USERSYM, X, Y, /fill     
    oplot, [-420,-420], [-30, -30], color=verde, linestyle=0, psym=8
    oplot, [-420,-420], [-50, -50], color=azul, linestyle=0, psym=2                

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
                         CHARSIZE = 1.2, $
                      ;   XTITLE = 'Dst  [nT]', $
                         CHARTHICK=chr_thick1, $
                         XTICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[down0,up0], $
                         COLOR=negro, $
                         xstyle=1, $
                         XTICKFORMAT='(A1)',$
                         XTICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down0,up0], $
                       ;  YTITLE = , $                          
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
   XYOUTS, 0.03, y, dh+'  [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.2

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.02, 'Dst [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.2
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
                    write_jpeg, path+'dst_DH_corr.jpg', True_Image, true=1
            ENDIF ELSE BEGIN
                    IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                    WRITE_PNG, path+'dst_DH_corr.png', Image, reds,greens,blues
            ENDELSE

            IF NOT keyword_set(quiet) THEN BEGIN
                    print, '        Saving: '+path+'dst_DH_corr'
                    print, ''
            ENDIF
            RETURN
    end
;############################################################################################################   
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################

pro corr, res_dat, DOY, PNG = png, JPEG = jpeg

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
        dat1 = dst_data(yr_i1)
        t1 = n_elements(dat1.year)
        ;print, t
        
        i_dst1 = dat1.Dst
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
        tot_days1= findgen(tw1*24)/24.0
                                                         
        Date1 = string(yr_i1, mh_i1, dy_i1, FORMAT='(I4, "-", I02, "-", I02)')    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of DH Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-		    
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
                    data_file_name1[i] = '../rutidl/dH_teo/'+'teo_'+string_date1[i]+'.dst.early'
            ENDFOR

            exist_data_file1   = FILE_TEST(data_file_name1)
            capable_to_plot1   = N_ELEMENTS(where(exist_data_file1 EQ 1))

            IF capable_to_plot1 NE N_ELEMENTS(data_file_name1) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H1    = FLTARR(file_number1*24)
            
            H_STDESV1    = FLTARR(file_number1*24)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file1)-1 DO BEGIN
                    IF exist_data_file1[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date1[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat1 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H1[i*24:(i+1)*24-1] = dat1.H[*]

                            H_STDESV1[i*24:(i+1)*24-1] = dat1.H_stdesv[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             H1[i*24:(i+1)*24-1] = 999999.0

                             H_STDESV1[i*24:(i+1)*24-1] = 999999.0                        
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H1)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H1[i] eq 999999.0 then begin
                    H1[where(H1[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst1      = i_dst1[(idoy1*24)-24:fdoy1*24-1]
    H_tmp   = H1
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H1 = H_tmp        
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
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
        dat2 = dst_data(yr_i2)
        t2 = n_elements(dat2.year)
        ;print, t        
        i_dst2 = dat2.Dst
        ;print, t
        year2 = dat2.year
        tiempo2 = TIMEGEN(t2, START=julday(dat2.month[0], dat2.day[0],  $
                         dat2.year[0], dat2.hour[0]), UNITS='Hours')                                     
        
        iyear2 = strmid(string(yr_i2, format='(I4)'),2,2)
        fyear2 = strmid(string(yr_f2, format='(I4)'),2,2)
                
        idoy2      = Date2DOY(string(iyear2, mh_i2, dy_i2,format = '(I02,I02,I02)'))
        fdoy2      = Date2DOY(string(fyear2, mh_f2, dy_f2,format = '(I02,I02,I02)'))   
        
        time_w2  = tiempo2[idoy2:fdoy2]
        tw2      = n_elements(time_w2)
        tot_days2= findgen(tw2*24)/24.0                                                         		    
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
                    data_file_name2[i] = '../rutidl/dH_teo/'+'teo_'+string_date2[i]+'.dst.early'
            ENDFOR

            exist_data_file2   = FILE_TEST(data_file_name2)
            capable_to_plot2   = N_ELEMENTS(where(exist_data_file2 EQ 1))

            IF capable_to_plot2 NE N_ELEMENTS(data_file_name2) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H2    = FLTARR(file_number2*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file2)-1 DO BEGIN
                    IF exist_data_file2[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date2[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat2 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H2[i*24:(i+1)*24-1] = dat2.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                            H2[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H2)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H2[i] eq 999999.0 then begin
                    H2[where(H2[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst2      = i_dst2[(idoy2*24)-24:fdoy2*24-1]            
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
        dat3 = dst_data(yr_i3)
        t3 = n_elements(dat3.year)
        ;print, t        
        i_dst3 = dat3.Dst
        ;print, t
        year3 = dat3.year
        tiempo3 = TIMEGEN(t3, START=julday(dat3.month[0], dat3.day[0],  $
                         dat3.year[0], dat3.hour[0]), UNITS='Hours')                                     
        
        iyear3 = strmid(string(yr_i3, format='(I4)'),2,2)
        fyear3 = strmid(string(yr_f3, format='(I4)'),2,2)
                
        idoy3      = Date2DOY(string(iyear3, mh_i3, dy_i3,format = '(I02,I02,I02)'))
        fdoy3      = Date2DOY(string(fyear3, mh_f3, dy_f3,format = '(I02,I02,I02)'))   
        
        time_w3  = tiempo3[idoy3:fdoy3]
        tw3      = n_elements(time_w3)
        tot_days3= findgen(tw3*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number3    = (JULDAY(mh_f3, dy_f3, yr_f3) - JULDAY(mh_i3, dy_i3, yr_i3))+1
            data_file_name3 = strarr(file_number3)
            string_date3    = strarr(file_number3)
           
            FOR i=0ll, file_number2-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i3, dy_i3, yr_i3)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date3[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name3[i] = '../rutidl/dH_teo/'+'teo_'+string_date3[i]+'.dst.early'
            ENDFOR

            exist_data_file3   = FILE_TEST(data_file_name3)
            capable_to_plot3   = N_ELEMENTS(where(exist_data_file3 EQ 1))

            IF capable_to_plot3 NE N_ELEMENTS(data_file_name3) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H3    = FLTARR(file_number3*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file3)-1 DO BEGIN
                    IF exist_data_file3[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date3[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat3 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H3[i*24:(i+1)*24-1] = dat3.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H3[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H3)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H3[i] eq 999999.0 then begin
                    H3[where(H3[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst3      = i_dst3[(idoy3*24)-24:fdoy3*24]
    H_tmp   = H3
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H3 = H_tmp                    
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
        dat4    = dst_data(yr_i4)
        t4      = n_elements(dat4.year)      
        i_dst4  = dat4.Dst
        year4   = dat4.year
        
        tiempo4 = TIMEGEN(t4, START=julday(dat4.month[0], dat4.day[0],  $
                         dat4.year[0], dat4.hour[0]), UNITS='Hours')                                     
        
        iyear4 = strmid(string(yr_i4, format='(I4)'),2,2)
        fyear4 = strmid(string(yr_f4, format='(I4)'),2,2)
                
        idoy4  = Date2DOY(string(iyear4, mh_i4, dy_i4,format = '(I02,I02,I02)'))
        fdoy4  = Date2DOY(string(fyear4, mh_f4, dy_f4,format = '(I02,I02,I02)'))   
        
        time_w4  = tiempo4[idoy4:fdoy4]
        tw4      = n_elements(time_w4)
        tot_days4= findgen(tw4*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number4    = (JULDAY(mh_f4, dy_f4, yr_f4) - JULDAY(mh_i4, dy_i4, yr_i4))+1
            data_file_name4 = strarr(file_number4)
            string_date4    = strarr(file_number4)
           
            FOR i=0ll, file_number2-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i4, dy_i4, yr_i4)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date4[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name4[i] = '../rutidl/dH_teo/'+'teo_'+string_date4[i]+'.dst.early'
            ENDFOR

            exist_data_file4   = FILE_TEST(data_file_name4)
            capable_to_plot4   = N_ELEMENTS(where(exist_data_file4 EQ 1))

            IF capable_to_plot4 NE N_ELEMENTS(data_file_name4) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H4    = FLTARR(file_number4*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file4)-1 DO BEGIN
                    IF exist_data_file4[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date4[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat4 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H4[i*24:(i+1)*24-1] = dat4.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H4[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H4)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H4[i] eq 999999.0 then begin
                    H4[where(H4[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst4      = i_dst4[(idoy4*24)-24:fdoy4*24]
    H_tmp   = H4
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H4 = H_tmp                    
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
        dat5    = dst_data(yr_i5)
        t5      = n_elements(dat5.year)      
        i_dst5  = dat5.Dst
        year5   = dat5.year
        
        tiempo5 = TIMEGEN(t5, START=julday(dat5.month[0], dat5.day[0],  $
                         dat5.year[0], dat5.hour[0]), UNITS='Hours')                                     
        
        iyear5 = strmid(string(yr_i5, format='(I4)'),2,2)
        fyear5 = strmid(string(yr_f5, format='(I4)'),2,2)
                
        idoy5  = Date2DOY(string(iyear5, mh_i5, dy_i5,format = '(I02,I02,I02)'))
        fdoy5  = Date2DOY(string(fyear5, mh_f5, dy_f5,format = '(I02,I02,I02)'))   
        
        time_w5  = tiempo5[idoy5:fdoy5]
        tw5      = n_elements(time_w5)
        tot_days5= findgen(tw5*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number5    = (JULDAY(mh_f5, dy_f5, yr_f5) - JULDAY(mh_i5, dy_i5, yr_i5))+1
            data_file_name5 = strarr(file_number5)
            string_date5    = strarr(file_number5)
           
            FOR i=0ll, file_number5-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i5, dy_i5, yr_i5)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date5[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name5[i] = '../rutidl/dH_teo/'+'teo_'+string_date5[i]+'.dst.early'
            ENDFOR

            exist_data_file5   = FILE_TEST(data_file_name5)
            capable_to_plot5   = N_ELEMENTS(where(exist_data_file5 EQ 1))

            IF capable_to_plot5 NE N_ELEMENTS(data_file_name5) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H5    = FLTARR(file_number5*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file5)-1 DO BEGIN
                    IF exist_data_file3[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date5[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat5 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H5[i*24:(i+1)*24-1] = dat5.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H5[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H5)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H5[i] eq 999999.0 then begin
                    H5[where(H5[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst5      = i_dst5[(idoy5*24)-24:fdoy5*24] 
    H_tmp   = H5
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H5 = H_tmp                   
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
        dat6    = dst_data(yr_i6)
        t6      = n_elements(dat6.year)      
        i_dst6  = dat6.Dst
        year6   = dat6.year
        
        tiempo6 = TIMEGEN(t6, START=julday(dat6.month[0], dat6.day[0],  $
                         dat6.year[0], dat6.hour[0]), UNITS='Hours')                                     
        
        iyear6 = strmid(string(yr_i6, format='(I4)'),2,2)
        fyear6 = strmid(string(yr_f6, format='(I4)'),2,2)
                
        idoy6  = Date2DOY(string(iyear6, mh_i6, dy_i6,format = '(I02,I02,I02)'))
        fdoy6  = Date2DOY(string(fyear6, mh_f6, dy_f6,format = '(I02,I02,I02)'))   
        
        time_w6  = tiempo6[idoy6:fdoy6]
        tw6      = n_elements(time_w6)
        tot_days6= findgen(tw6*24)/24.0                                                         		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number6    = (JULDAY(mh_f6, dy_f6, yr_f6) - JULDAY(mh_i6, dy_i6, yr_i6))+1
            data_file_name6 = strarr(file_number6)
            string_date6    = strarr(file_number6)
           
            FOR i=0ll, file_number6-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i6, dy_i6, yr_i6)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date6[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name6[i] = '../rutidl/dH_teo/'+'teo_'+string_date6[i]+'.dst.early'
            ENDFOR

            exist_data_file6   = FILE_TEST(data_file_name6)
            capable_to_plot6   = N_ELEMENTS(where(exist_data_file6 EQ 1))

            IF capable_to_plot6 NE N_ELEMENTS(data_file_name6) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H6    = FLTARR(file_number6*24)            
                           
            FOR i = 0, N_ELEMENTS(exist_data_file6)-1 DO BEGIN
                    IF exist_data_file6[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date6[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat6 = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H6[i*24:(i+1)*24-1] = dat6.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H6[i*24:(i+1)*24-1] = 999999.0                     
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H6)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H6[i] eq 999999.0 then begin
                    H6[where(H6[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
        dst6      = i_dst6[(idoy6*24)-24:fdoy6*24]   
    H_tmp   = H6
    H_exist = where(finite(H_tmp), ngooddata, complement=baddata, ncomplement=nbaddata)
    ; interpolate at the locations of the bad data using the good data
    
    if nbaddata gt 0 then H_tmp[baddata] = interpol(H_tmp[H_exist], H_exist, baddata)
    H6 = H_tmp
    
;############################################################################### 
;###############################################################################

    
;###############################################################################
;###############################################################################        
    H = [H1, H2, H3, H4, H5, H6]
    dst=[dst1,dst2,dst3,dst4,dst5,dst6]
    
    corr  = CORRELATE(dst, H, /double)    
    corr1 = CORRELATE(dst1, H1, /double)    
    corr2 = CORRELATE(dst2, H2, /double)
    corr3 = CORRELATE(dst3, H3, /double)
    corr4 = CORRELATE(dst4, H4, /double)
    corr5 = CORRELATE(dst5, H5, /double)
    corr6 = CORRELATE(dst6, H6, /double)
    print, 'correlación de los valores de DST vs DH'
    print, 'TGM1, TGM2, TGM3, TGM4, TGM5, TGM6, TGM(general)'                
    print, corr1, corr2, corr3, corr4, corr5, corr6, corr
    print, '####################################################################'                    

    idst = where(dst lt -50, complement=jdst1)
    iH   = where(H le -50, complement=jH1)
    idst1 = where(dst1 lt -50, complement=jdst1)
    iH1   = where(H1 le -50, complement=jH1)    
    idst2 = where(dst2 lt -50, complement=jdst2)
    iH2   = where(H2 le -50, complement=jH2)
    idst3 = where(dst3 lt -50, complement=jdst3)
    iH3   = where(H3 le -50, complement=jH3)   
    idst4 = where(dst4 lt -50, complement=jdst4)
    iH4   = where(H4 le -50, complement=jH4)    
    idst5 = where(dst5 lt -50, complement=jdst5)
    iH5   = where(H5 le -50, complement=jH5)    
    idst6 = where(dst6 lt -50, complement=jdst6)
    iH6   = where(H6 le -50, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -50 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr
    print, '####################################################################' 
  
  
    idst = where(dst lt -60, complement=jdst1)
    iH   = where(H le -60, complement=jH1)      
    idst1 = where(dst1 lt -60, complement=jdst1)
    iH1   = where(H1 le -60, complement=jH1)    
    idst2 = where(dst2 lt -60, complement=jdst2)
    iH2   = where(H2 le -60, complement=jH2)
    idst3 = where(dst3 lt -60, complement=jdst3)
    iH3   = where(H3 le -60, complement=jH3)   
    idst4 = where(dst4 lt -60, complement=jdst4)
    iH4   = where(H4 le -60, complement=jH4)    
    idst5 = where(dst5 lt -60, complement=jdst5)
    iH5   = where(H5 le -60, complement=jH5)    
    idst6 = where(dst6 lt -60, complement=jdst6)
    iH6   = where(H6 le -60, complement=jH6)


    corr = CORRELATE(dst[idst], H[iH], /double)     
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -60 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr
    print, '####################################################################'    


    idst = where(dst lt -70, complement=jdst1)
    iH   = where(H le -70, complement=jH1)     
    idst1 = where(dst1 lt -70, complement=jdst1)
    iH1   = where(H1 le -70, complement=jH1)    
    idst2 = where(dst2 lt -70, complement=jdst2)
    iH2   = where(H2 le -70, complement=jH2)
    idst3 = where(dst3 lt -70, complement=jdst3)
    iH3   = where(H3 le -70, complement=jH3)   
    idst4 = where(dst4 lt -70, complement=jdst4)
    iH4   = where(H4 le -70, complement=jH4)    
    idst5 = where(dst5 lt -70, complement=jdst5)
    iH5   = where(H5 le -70, complement=jH5)    
    idst6 = where(dst6 lt -70, complement=jdst6)
    iH6   = where(H6 le -70, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -70 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr
    print, '####################################################################'
    


    idst = where(dst lt -80, complement=jdst1)
    iH   = where(H le -80, complement=jH1)         
    idst1 = where(dst1 lt -80, complement=jdst1)
    iH1   = where(H1 le -80, complement=jH1)    
    idst2 = where(dst2 lt -80, complement=jdst2)
    iH2   = where(H2 le -80, complement=jH2)
    idst3 = where(dst3 lt -80, complement=jdst3)
    iH3   = where(H3 le -80, complement=jH3)   
    idst4 = where(dst4 lt -80, complement=jdst4)
    iH4   = where(H4 le -80, complement=jH4)    
    idst5 = where(dst5 lt -80, complement=jdst5)
    iH5   = where(H5 le -80, complement=jH5)    
    idst6 = where(dst6 lt -80, complement=jdst6)
    iH6   = where(H6 le -80, complement=jH6)


    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -80 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr
    print, '####################################################################'
    

    idst = where(dst lt -90, complement=jdst1)
    iH   = where(H le -90, complement=jH1)        
    idst1 = where(dst1 lt -90, complement=jdst1)
    iH1   = where(H1 le -90, complement=jH1)    
    idst2 = where(dst2 lt -90, complement=jdst2)
    iH2   = where(H2 le -90, complement=jH2)
    idst3 = where(dst3 lt -90, complement=jdst3)
    iH3   = where(H3 le -90, complement=jH3)   
    idst4 = where(dst4 lt -90, complement=jdst4)
    iH4   = where(H4 le -90, complement=jH4)    
    idst5 = where(dst5 lt -90, complement=jdst5)
    iH5   = where(H5 le -90, complement=jH5)    
    idst6 = where(dst6 lt -90, complement=jdst6)
    iH6   = where(H6 le -90, complement=jH6)


    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -90 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr
    print, '####################################################################'
        

    idst = where(dst lt -100, complement=jdst1)
    iH   = where(H le -100, complement=jH1)    
    idst1 = where(dst1 lt -100, complement=jdst1)
    iH1   = where(H1 le -100, complement=jH1)    
    idst2 = where(dst2 lt -100, complement=jdst2)
    iH2   = where(H2 le -100, complement=jH2)
    idst3 = where(dst3 lt -100, complement=jdst3)
    iH3   = where(H3 le -100, complement=jH3)   
    idst4 = where(dst4 lt -100, complement=jdst4)
    iH4   = where(H4 le -100, complement=jH4)    
    idst5 = where(dst5 lt -100, complement=jdst5)
    iH5   = where(H5 le -100, complement=jH5)    
    idst6 = where(dst6 lt -100, complement=jdst6)
    iH6   = where(H6 le -100, complement=jH6)


    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -100 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr
    print, '####################################################################'
        
 
    idst = where(dst lt -110, complement=jdst1)
    iH   = where(H le -110, complement=jH1)    
    idst1 = where(dst1 lt -110, complement=jdst1)
    iH1   = where(H1 le -110, complement=jH1)    
    idst2 = where(dst2 lt -110, complement=jdst2)
    iH2   = where(H2 le -110, complement=jH2)
    idst3 = where(dst3 lt -110, complement=jdst3)
    iH3   = where(H3 le -110, complement=jH3)   
    idst4 = where(dst4 lt -110, complement=jdst4)
    iH4   = where(H4 le -110, complement=jH4)    
    idst5 = where(dst5 lt -110, complement=jdst5)
    iH5   = where(H5 le -110, complement=jH5)    
    idst6 = where(dst6 lt -110, complement=jdst6)
    iH6   = where(H6 le -110, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -110 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr  
    print, '####################################################################'    


    idst = where(dst lt -120, complement=jdst1)
    iH   = where(H le -120, complement=jH1)     
    idst1 = where(dst1 lt -120, complement=jdst1)
    iH1   = where(H1 le -120, complement=jH1)    
    idst2 = where(dst2 lt -120, complement=jdst2)
    iH2   = where(H2 le -120, complement=jH2)
    idst3 = where(dst3 lt -120, complement=jdst3)
    iH3   = where(H3 le -120, complement=jH3)   
    idst4 = where(dst4 lt -120, complement=jdst4)
    iH4   = where(H4 le -120, complement=jH4)    
    idst5 = where(dst5 lt -120, complement=jdst5)
    iH5   = where(H5 le -120, complement=jH5)    
    idst6 = where(dst6 lt -120, complement=jdst6)
    iH6   = where(H6 le -120, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)     
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -120 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr   
    print, '####################################################################'  
    

    idst = where(dst lt -130, complement=jdst1)
    iH   = where(H le -130, complement=jH1)    
    idst1 = where(dst1 lt -130, complement=jdst1)
    iH1   = where(H1 le -130, complement=jH1)    
    idst2 = where(dst2 lt -130, complement=jdst2)
    iH2   = where(H2 le -130, complement=jH2)
    idst3 = where(dst3 lt -130, complement=jdst3)
    iH3   = where(H3 le -130, complement=jH3)   
    idst4 = where(dst4 lt -130, complement=jdst4)
    iH4   = where(H4 le -130, complement=jH4)    
    idst5 = where(dst5 lt -130, complement=jdst5)
    iH5   = where(H5 le -130, complement=jH5)    
    idst6 = where(dst6 lt -130, complement=jdst6)
    iH6   = where(H6 le -130, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    ;corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    ;corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -130 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr  
    print, '####################################################################'    


    idst = where(dst lt -140, complement=jdst1)
    iH   = where(H le -140, complement=jH1)     
    idst1 = where(dst1 lt -140, complement=jdst1)
    iH1   = where(H1 le -140, complement=jH1)    
    idst2 = where(dst2 lt -140, complement=jdst2)
    iH2   = where(H2 le -140, complement=jH2)
    idst3 = where(dst3 lt -140, complement=jdst3)
    iH3   = where(H3 le -140, complement=jH3)   
    idst4 = where(dst4 lt -140, complement=jdst4)
    iH4   = where(H4 le -140, complement=jH4)    
    idst5 = where(dst5 lt -140, complement=jdst5)
    iH5   = where(H5 le -140, complement=jH5)    
    idst6 = where(dst6 lt -140, complement=jdst6)
    iH6   = where(H6 le -140, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    ;corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    ;corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -140 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr  
    print, '####################################################################'     
    

    idst = where(dst lt -150, complement=jdst1)
    iH   = where(H le -150, complement=jH1)    
    idst1 = where(dst1 lt -150, complement=jdst1)
    iH1   = where(H1 le -150, complement=jH1)    
    idst2 = where(dst2 lt -150, complement=jdst2)
    iH2   = where(H2 le -150, complement=jH2)
    idst3 = where(dst3 lt -150, complement=jdst3)
    iH3   = where(H3 le -150, complement=jH3)   
    idst4 = where(dst4 lt -150, complement=jdst4)
    iH4   = where(H4 le -150, complement=jH4)    
    idst5 = where(dst5 lt -150, complement=jdst5)
    iH5   = where(H5 le -150, complement=jH5)    
    idst6 = where(dst6 lt -150, complement=jdst6)
    iH6   = where(H6 le -150, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    ;corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    ;corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -150 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr
    print, '####################################################################'       
    
    
    idst = where(dst lt -160, complement=jdst1)
    iH   = where(H le -160, complement=jH1)    
    idst1 = where(dst1 lt -160, complement=jdst1)
    iH1   = where(H1 le -160, complement=jH1)    
    idst2 = where(dst2 lt -160, complement=jdst2)
    iH2   = where(H2 le -160, complement=jH2)
    idst3 = where(dst3 lt -160, complement=jdst3)
    iH3   = where(H3 le -160, complement=jH3)   
    idst4 = where(dst4 lt -160, complement=jdst4)
    iH4   = where(H4 le -160, complement=jH4)    
    idst5 = where(dst5 lt -160, complement=jdst5)
    iH5   = where(H5 le -160, complement=jH5)    
    idst6 = where(dst6 lt -160, complement=jdst6)
    iH6   = where(H6 le -160, complement=jH6)

    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
    ;corr55 = CORRELATE(dst5[idst5], H5[iH5], /double)
    ;corr66 = CORRELATE(dst6[idst6], H6[iH6], /double)     
    print, 'correlación de los valores menores a -160 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr   
    print, '####################################################################'     
       
    
    idst = where(dst lt -170, complement=jdst1)
    iH   = where(H le -170, complement=jH1)      
    idst1 = where(dst1 lt -170, complement=jdst1)
    iH1   = where(H1 le -170, complement=jH1)    
    idst2 = where(dst2 lt -170, complement=jdst2)
    iH2   = where(H2 le -170, complement=jH2)
    idst3 = where(dst3 lt -170, complement=jdst3)
    iH3   = where(H3 le -170, complement=jH3)   
    idst4 = where(dst4 lt -170, complement=jdst4)
    iH4   = where(H4 le -170, complement=jH4)    


    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)
     
    print, 'correlación de los valores menores a -170 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr  
    print, '####################################################################'     
    

    idst = where(dst lt -180, complement=jdst1)
    iH   = where(H le -180, complement=jH1)
    idst1 = where(dst1 lt -180, complement=jdst1)
    iH1   = where(H1 le -180, complement=jH1)    
    idst2 = where(dst2 lt -180, complement=jdst2)
    iH2   = where(H2 le -180, complement=jH2)
    idst3 = where(dst3 lt -180, complement=jdst3)
    iH3   = where(H3 le -180, complement=jH3)   
    idst4 = where(dst4 lt -180, complement=jdst4)
    iH4   = where(H4 le -180, complement=jH4)    


    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)     
    print, 'correlación de los valores menores a -180 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr  
    print, '####################################################################'
    

    idst = where(dst lt -190, complement=jdst1)
    iH   = where(H le -190, complement=jH1)    
    idst1 = where(dst1 lt -190, complement=jdst1)
    iH1   = where(H1 le -190, complement=jH1)    
    idst2 = where(dst2 lt -190, complement=jdst2)
    iH2   = where(H2 le -190, complement=jH2)
    idst3 = where(dst3 lt -190, complement=jdst3)
    iH3   = where(H3 le -190, complement=jH3)   
    idst4 = where(dst4 lt -190, complement=jdst4)
    iH4   = where(H4 le -190, complement=jH4)    


    corr = CORRELATE(dst[idst], H[iH], /double)        
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)    
    print, 'correlación de los valores menores a -190 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr   
    print, '####################################################################'
    

    idst = where(dst lt -200, complement=jdst1)
    iH   = where(H le -200, complement=jH1)     
    idst1 = where(dst1 lt -200, complement=jdst1)
    iH1   = where(H1 le -200, complement=jH1)    
    idst2 = where(dst2 lt -200, complement=jdst2)
    iH2   = where(H2 le -200, complement=jH2)
    idst3 = where(dst3 lt -200, complement=jdst3)
    iH3   = where(H3 le -200, complement=jH3)   
    idst4 = where(dst4 lt -200, complement=jdst4)
    iH4   = where(H4 le -200, complement=jH4)    


    corr = CORRELATE(dst[idst], H[iH], /double)    
    corr11 = CORRELATE(dst1[idst1], H1[iH1], /double)    
    corr22 = CORRELATE(dst2[idst2], H2[iH2], /double)
    corr33 = CORRELATE(dst3[idst3], H3[iH3], /double)
    corr44 = CORRELATE(dst4[idst4], H4[iH4], /double)   
    print, 'correlación de los valores menores a -200 nT de DST vs DH'             
    print, corr11, corr22, corr33, corr44, corr55, corr66, corr   
    print, '####################################################################'                      
end                                                        
