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

    pro magdat2_V3, kp_data, doy, idate, fdate, PNG = png, JPEG = jpeg;, DIR = dir

	    On_error, 2
	    compile_opt idl2, HIDDEN
	    
	    yr_i	= idate[0]
	    mh_i	= idate[1]
	    dy_i 	= idate[2]	

	    yr_f	= fdate[0]
	    mh_f	= fdate[1]
	    dy_f 	= fdate[2]	    	
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Define a time window
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-            
        dat = kp_data(yr_i)
        t = n_elements(dat.year)

        i_ap = dat.ap
        
        year    = dat.year
        month   = dat.month
        day     = dat.day
        hour    = dat.hour
        doy     = dat.DOY
            
        ventana_t   = timegen(t, START=julday(1, doy[0], year[0], dat.hour[0]), units='H')
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; plot the Dst time series for each event
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        iyear = strmid(string(yr_i, format='(I4)'),2,2)
        fyear = strmid(string(yr_f, format='(I4)'),2,2)
                
        idoy      = Date2DOY(string(iyear, mh_i, dy_i,format = '(I02,I02,I02)'))
        fdoy      = Date2DOY(string(fyear, mh_f, dy_f,format = '(I02,I02,I02)')) 
                
        time_w  = ventana_t[idoy:fdoy]
        tw      = n_elements(time_w)
        tot_days= findgen(tw*8)/8.0
                
       Ap      = i_ap[(idoy*8)-8:fdoy*8]
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ;Datos de Kmex
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

            file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
            data_file_name = strarr(file_number)

            string_date     = strarr(file_number)
           
            FOR i=0ll, file_number-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.final'
		            
		            file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	                IF opened_files NE N_ELEMENTS(file) THEN begin
	                    data_file_name[i] = '../rutidl/Kmex/'+'teo_'+string_date[i]+'.index.early'    
	                ENDIF		        
	            
            ENDFOR
	    
            exist_data_file   = FILE_TEST(data_file_name)
            capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

            IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

            k_mex    = INTARR(file_number*8)
            a_mex    = INTARR(file_number*8)

            FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                    IF exist_data_file[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat = kmex([tmp_year, tmp_month, tmp_day])
                            
                            k_mex[i*8:(i+1)*8-1] = dat.k_mex[*]/10
                            a_mex[i*8:(i+1)*8-1] = dat.a_mex[*]
                    ENDIF 
            ENDFOR
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Write a post Script
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        path = '../rutidl/output/eventos_tgm/'
        date = string(year[0], mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
        
            Device_bak = !D.Name 
            SET_PLOT, 'Z'
            
            tmp_spam = 1
            IF tw GT 7 THEN tmp_spam = 1.5
            IF tw GT 15 THEN tmp_spam = 2.
            
            Xsize=fix(800*tmp_spam)
            Ysize=200
            DEVICE, SET_RESOLUTION = [Xsize,Ysize]
            DEVICE, z_buffering=O
            DEVICE, set_character_size = [10, 12]
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ;definición de color
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-        
            chr_size1 = 0.9
            chr_thick1= 1.0
            space     = 0.015
            rojo      = 248
            naranja   = 220
            amarillo  = 198
            verde     = 150
            negro     = 0
            gris_o    = 100
            blanco    = 255
            gris      = 10
            morado    = 16
                            
        TVLCT, R_bak, G_bak, B_bak, /GET
            
        LOADCT, 39, /SILENT
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Write a post Script
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-    
         X_label   = STRARR(tw+1)+' '
            months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
            old_month = mh_i
            ;print, old_month
            FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    
                    IF i LT N_ELEMENTS(X_label)-1 THEN $
                            X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                    old_month = tmp_month
            ENDFOR        
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; PLOTTING
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    plot_title = 'Nov 2004'

 ;   time_title = 'Time window begin: '+string(year[0], ini_month, ini_day, $
 ;                   FORMAT='(I4, "/", I02, "/", I02)')+' 00:00 UTC'
                    
    MAG_source = 'Source: International Service of Geomagnetic Indices'                        

    if max(Ap) gt max(a_mex) then up = max(Ap) else up = max(a_mex)
    if min(Ap) gt min(a_mex) then down=min(Ap) else down=min(a_mex)    

        plot, tot_days, a_mex, XTICKS=tw, xminor=8, title = plot_title,$
                        XTITLE = 's', YTITLE = 's',  $
                        BACKGROUND = blanco, COLOR=negro, YRANGE=[down,up], $
                        CHARSIZE = 1, CHARTHICK=chr_thick1, $
                        POSITION=[0.09,0.2,0.95,0.88], XSTYLE = 5, ystyle = 6,$
                        XTICKNAME=REPLICATE(' ', tw+1), XRANGE=[0, tw]
        
        oplot, tot_days, Ap, color=verde, thick=2
                     
    if up-down gt 300 then begin
        for i = -600., 1000., 100. do oplot, [0,tw], [i,i], linestyle=1, COLOR=negro
    endif else begin
        for i = -600., 1000., 50. do oplot, [0,tw], [i,i], linestyle=1, COLOR=negro
    endelse     

;###############################################################################
    ;Delimiter lines for TGM1
    ;oplot, [1.5,1.5], [down-100,up+100], linestyle=2, COLOR=negro ;12:00 hr del nov 20
    ;oplot, [2.4,2.4], [down-100,up+100], linestyle=2, COLOR=negro ;09:36 hr del nov 21
;###############################################################################
    ;Delimiter lines for TGM2
    oplot, [1.75,1.75], [down-100,up+100], linestyle=2, COLOR=negro ;18:00 hr del nov 07
    oplot, [2.3,2.3], [down-100,up+100], linestyle=2, COLOR=negro ;  07:12 hr del nov 08

    oplot, [3.7,3.7], [down-100,up+100], linestyle=2, COLOR=negro  ;  16:48 hr del nov 09       
    oplot, [4.9,4.9], [down-100,up+100], linestyle=2, COLOR=negro ;   21:36 hr del nov 10
;############################################################################### 
    ;Delimiter lines for TGM3
   ; oplot, [0.9,0.9], [down-100,up+100], linestyle=2, COLOR=negro ;21:36 hr de may 14
    ;oplot, [1.5,1.5], [down-100,up+100], linestyle=2, COLOR=negro ;12:00 hr del may 15

;    oplot, [2,2], [down-100,up+100], linestyle=2, COLOR=negro  ;  00:01 hr del may 16       
 ;   oplot, [2.4,2.4], [down-100,up+100], linestyle=2, COLOR=negro ;09:36 hr de may 16
;############################################################################### 
    ;Delimiter lines for TGM4
   ; oplot, [1,1], [down-100,up+100], linestyle=2, COLOR=negro ;21:36 hr de may 14    
   ; oplot, [3.5,3.5], [down-100,up+100], linestyle=2, COLOR=negro ;09:36 hr de may 16
;###############################################################################   
    ;Delimiter lines for TGM5
    ;oplot, [0.75,0.75], [down-100,up+100], linestyle=2, COLOR=negro ;18:00 hr de may 27    
    ;oplot, [2,2], [down-100,up+100], linestyle=2, COLOR=negro ;     00:01 hr de may 29
;###############################################################################    
    ;Delimiter lines for TGM6
   ; oplot, [1.85,1.85], [down-100,up+100], linestyle=2, COLOR=negro ;20:24 hr de sept 07    
   ; oplot, [3,3], [down-100,up+100], linestyle=2, COLOR=negro ;     00:01 hr de sept 09
;###############################################################################     
            AXIS, XAXIS = 0, XRANGE=[0,tw], $
                             XTICKS=tw, $
                             XMINOR=8, $
                             XTITLE = 'Time  [UT] ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                             XTICKNAME=X_label, $
                             COLOR=negro, $
                             CHARSIZE = 0.9, $
    ;                         CHARTHICK=chr_thick1, $
                             TICKLEN=0.04
                             
            AXIS, XAXIS = 1, XRANGE=[0,tw], $
                             XTICKS=tw, $
                             XMINOR=8, $
                            ; xtitle = 'November 2003', $
                             XTICKNAME=REPLICATE(' ', tw+1), $
                             COLOR=negro, $
                             TICKLEN=0.04

            AXIS, YAXIS = 0, YRANGE=[down,up], $
                             ystyle=2, $
                             YTITLE = 'Amex and Ap [nT]', $
                             COLOR=negro, $
                             CHARSIZE = 0.9;, $
                             ;TICKLEN=0.00

            AXIS, YAXIS = 1, YRANGE=[down,up], $
                             ;YTITLE = 'DH [nT]', $
                             ystyle=2, $
                             COLOR=negro, $
                             CHARSIZE = 0.9;, $
                            ; CHARTHICK=chr_thick1;, $  

        
            ;XYOUTS, 0.65, 0.015 , /NORMAL, $
            ;        UPDATE_banner, COLOR=negro, $
            ;        CHARSIZE = chr_size1, $
            ;                 CHARTHICK=chr_thick1

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
                    write_jpeg, path+'kp_'+date+'.jpg', True_Image, true=1
            ENDIF ELSE BEGIN
                    IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                    WRITE_PNG, path+'kp_'+date+'.png', Image, reds,greens,blues
            ENDELSE

            IF NOT keyword_set(quiet) THEN BEGIN
                    print, '        Saving: '+path+'kp_'+date
                    print, ''
            ENDIF
            RETURN    
    end
 
