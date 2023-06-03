


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

    
pro magdat_corrV3, res_dat, DOY, PNG = png, JPEG = jpeg

	    On_error, 2
	    compile_opt idl2, HIDDEN
	    yr_i = [2003, 2004, 2005, 2015, 2017, 2017]
	    mh_i = [11, 11, 5, 3, 5, 9]
	    dy_i = [19, 6, 14, 15, 26, 6]
	    
	    yr_f = [2003, 2004, 2005, 2015, 2017, 2017]
	    mh_f = [11, 11, 5, 3, 6, 9]
	    dy_f = [25, 12, 20, 21, 1, 11]
	   	    
;hacer estructura con yr_i, yr_f, mh_i, mh_f, dy_i, dy_f


for i=1,6 do begin	    	    	    	  	    		    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat = dst_data(yr_i[i])
        t = n_elements(dat.year)
        ;print, t
        
        i_dst = dat.Dst
        ;print, t
        year = dat.year
        tiempo = TIMEGEN(t, START=julday(dat.month[0], dat.day[0],  $
                         dat.year[0], dat.hour[0]), UNITS='Hours')                                     
        
        iyear = strmid(string(yr_i[i], format='(I4)'),2,2)
        fyear = strmid(string(yr_i[i], format='(I4)'),2,2)
                
        idoy      = Date2DOY(string(iyear, mh_i[i], dy_i[i],format = '(I02,I02,I02)'))
        fdoy      = Date2DOY(string(fyear, mh_f[i], dy_f[i],format = '(I02,I02,I02)'))   
        
        time_w  = tiempo[idoy:fdoy]
        tw = n_elements(time_w)
        tot_days= findgen(tw*24)/24.0
        
        dst      = i_dst[(idoy*24)-24:fdoy*24]                                                          
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of DH Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number    = (JULDAY(mh_f[i], dy_f[i], yr_f[i]) - JULDAY(mh_i[i], dy_i[i], yr_i[i]))+1
            data_file_name = strarr(file_number)
            string_date     = strarr(file_number)
           
            FOR j=0ll, file_number-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i[i], dy_i[i], yr_i[i])

                    CALDAT, tmp_julday+j, tmp_month, tmp_day, tmp_year
                    string_date[j]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name[j] = '../rutidl/dH_teo/'+'teo_'+string_date+'.dst.early'
            ENDFOR

            exist_data_file   = FILE_TEST(data_file_name)
            capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

            IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            H    = FLTARR(file_number*24)
                                       
            FOR j = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                    IF exist_data_file[j] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date[j], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H[j*24:(j+1)*24-1] = dat.H[*]                                                                                                  
                    ENDIF ELSE BEGIN
                             H[j*24:(j+1)*24-1] = 999999.0
                    ENDELSE                
            ENDFOR            
            for j=0, n_elements(H[i])-1 do begin
                if H[j] eq 999999.0 then begin
                    H[where(H[*] eq 999999.0)] = !Values.F_NAN          
                endif
            endfor
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
       
            Device_bak = !D.Name 
            SET_PLOT, 'Z'
            
            Xsize=800
            Ysize=800
            ;DEVICE, decompose=0
            DEVICE, SET_RESOLUTION = [Xsize,Ysize]
            DEVICE, z_buffering=O
            DEVICE, set_character_size = [10, 12]
                 
            chr_size1 = 0.9
            chr_thick1= 1.0
            space     = 0.015
            rojo      = 248
            amarillo  = 220
            verde     = 180
            negro     = 0
            azul      = 60
            blanco    = 255
            gris      = 130
            morado    = 16
            
        TVLCT, R_bak, G_bak, B_bak, /GET
            
        LOADCT, 39, /SILENT
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Write a post Script
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        path = '../rutidl/output/eventos_tgm/'
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-        
window_title = 'Six TGM events '
                    
    up0  =  100
    down0=-500     
    
   slope = findgen(601)-500
   x1= slope
   y1=slope
   plot, x1,y1, xstyle=5, ystyle=5, YRANGE=[down0,up0], psym=1,$
    title = window_title, BACKGROUND = blanco, COLOR=negro, XRANGE=[down0,up0]

    oplot, dst, H, psym=[i+1], color=azul
    
   
        AXIS, XAXIS = 0, XRANGE=[down0,up0], $
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
                         XTITLE = 'Dst  [nT]', $
;                         CHARTHICK=chr_thick1, $
                         XTICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[down0,up0], $
                         COLOR=negro, $
                         XTICKFORMAT='(A1)',$
                         XTICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down0,up0], $
                         YTITLE = 'DH [nT]', $                          
                         COLOR=negro, $
                         ystyle=2, $
                         CHARSIZE = 0.6;, $

                        
        AXIS, YAXIS = 1, YRANGE=[down0,up0], $
                         ystyle=2, $
                         COLOR=negro, $
                         yTICKFORMAT='(A1)',$
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $   
endfor        
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
