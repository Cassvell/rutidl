;
;Name:
;	magdat_v2
;purpose:
;	this routine will call read a certain number of files containing magnetic 
;   data index.
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   Getting the data magnetic, date and time within several files. 
;
;calling sequence:
;
;
;parameters:
;
;
;dependencies:
;
;
;input files
;
;
;output files:
;

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


pro magdat_corr, r_dst, idate, fdate
	On_error, 2
	compile_opt idl2, HIDDEN
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of Dst Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	    yr_i	= idate[0]
	    mh_i	= idate[1]
	    dy_i 	= idate[2]	

	    yr_f	= fdate[0]
	    mh_f	= fdate[1]
	    dy_f 	= fdate[2]	
    ;dt  = tp  

    d_dst = dst_data(yr_i)
    t = n_elements(d_dst.year)
    ;print, t
    
    i_dst = d_dst.Dst
   
    year = d_dst.year
    tiempo = TIMEGEN(t, START=julday(d_dst.month[0], d_dst.day[0],  $
                     d_dst.year[0], d_dst.hour[0]), UNITS='Hours')

        iyear = strmid(string(yr_i, format='(I4)'),2,2)
        fyear = strmid(string(yr_f, format='(I4)'),2,2)
                
    idoy      = Date2DOY(string(iyear, mh_i, dy_i,format = '(I02,I02,I02)'))
    fdoy      = Date2DOY(string(fyear, mh_f, dy_f,format = '(I02,I02,I02)')) 
               
    time_w  = tiempo[idoy:fdoy]
    tw = n_elements(time_w)
    days_dst= findgen(tw*24)/24.0
    
    dst = i_dst[(idoy*24)-24:fdoy*24-1]
                                       
    Date = string(year[0], mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
    
;##############################################################################
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of DH Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        
        data_file_name_dh  = strarr(file_number)
        data_file_name_km  = strarr(file_number)
        
        string_date        = strarr(file_number)
        
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                
                data_file_name_dh[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'
                data_file_name_km[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.final'
		        
		        file = FILE_SEARCH(data_file_name_km[i], COUNT=opened_files)
	            IF opened_files NE N_ELEMENTS(file) THEN begin
	                data_file_name_km[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.early'    
	            ENDIF                
        ENDFOR

        exist_data_file_dh   = FILE_TEST(data_file_name_dh)
        capable_to_plot_dh   = N_ELEMENTS(where(exist_data_file_dh EQ 1))

        IF capable_to_plot_dh NE N_ELEMENTS(data_file_name_dh) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

        H    = FLTARR(file_number*24)
        
        H_STDESV    = FLTARR(file_number*24)
                       
        FOR i = 0, N_ELEMENTS(exist_data_file_dh)-1 DO BEGIN
                IF exist_data_file_dh[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_dh = DH_teo([tmp_year, tmp_month, tmp_day])
                        
                        H[i*24:(i+1)*24-1] = d_dh.H[*]
                                                
                        H_STDESV[i*24:(i+1)*24-1] = d_dh.H_stdesv[*]
                                                                                              
                ENDIF ELSE BEGIN
                         H[i*24:(i+1)*24-1] = 999999.0

                         H_STDESV[i*24:(i+1)*24-1] = 999999.0                        
                ENDELSE                
        ENDFOR
        
        for i=0, n_elements(H)-1 do begin
            if H[i] eq 999999.0 then begin
                H[where(H[*] eq 999999.0)] = !Values.F_NAN          
            endif
        endfor    
        
        for i=0, n_elements(H)-1 do begin
            ;idx = where(H eq 999999.0, inan)
            if H[i] ge 100.0 then begin
                H[where(H[*] ge 100.0)] = !Values.F_NAN          
            endif
        endfor  
;##############################################################################
; defining Kmex and time data variables
;############################################################################## 
	
        exist_data_file_km   = FILE_TEST(data_file_name_km)
        capable_to_plot_km   = N_ELEMENTS(where(exist_data_file_km EQ 1))

        IF capable_to_plot_km NE N_ELEMENTS(data_file_name_km) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

        k_mex    = fltarr(file_number*8)
        a_mex    = INTARR(file_number*8)

        FOR i = 0, N_ELEMENTS(exist_data_file_km)-1 DO BEGIN
                IF exist_data_file_km[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        ;print, tmp_year, tmp_month, tmp_day
                        d_km = kmex([tmp_year, tmp_month, tmp_day])
                        
                        k_mex[i*8:(i+1)*8-1] = d_km.k_mex[*]
                        a_mex[i*8:(i+1)*8-1] = d_km.a_mex[*]
                ENDIF 
        ENDFOR     
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; initiate the figure Device, defining colors and figure dim
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-        
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
        amarillo  = 198
        verde     = 160
        negro     = 0
        azul      = 80
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT

    path = '../rutidl/output/eventos_tgm/'     
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;file_name = ''
;IF file_name eq '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_D.dat' then plot_title = 'Definitive Dst index'
 
;if file_name eq '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_P.dat' then plot_title = 'Provisional Dst index'

;if file_name eq '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_Q.dat' then plot_title = 'Quick look Dst index'

window_title = 'from '+string(yr_i, mh_i, dy_i, $
                FORMAT='(I4, "/", I02, "/", I02)')+' to '$
                +string(yr_f, mh_f, dy_f, $
                FORMAT='(I4, "/", I02, "/", I02)')

time_title = 'Time  [UT]' 
                
MAG_source = 'Source: International Service of Geomagnetic Indices'  

    if max(H) gt max(dst) then up0 = max(H) else up0 = max(dst)
    if min(H) lt min(dst) then down0 = min(H) else down0 = min(dst)
            
    plot, dst, H, POSITION=[0.1,0.1,0.9,0.9],$
    xstyle = 5, ystyle=6, YRANGE=[down0,up0], psym=1,$
    title = window_title, BACKGROUND = blanco, COLOR=negro, XRANGE=[down0,up0]

   slope = findgen(601)-500
   x1= slope
   y1=slope
   plot, x1,y1, xstyle=5, ystyle=5, color=negro, background=blanco, $
   position= [0.1,0.1,0.9,0.9], /noerase 
   
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
                write_jpeg, path+'corr_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'corr_'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'corr_'+Date+'.png'
                print, ''
        ENDIF
        RETURN
end
    
;###############################################################################
;############################################################################### 
;###############################################################################
;###############################################################################


