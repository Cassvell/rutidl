


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

    
    pro magdat_V3, res_dat, DOY, idate, fdate, PNG = png, JPEG = jpeg

	    On_error, 2
	    compile_opt idl2, HIDDEN
	    
	    yr_i1	= idate[0]
	    mh_i1	= idate[1]
	    dy_i1 	= idate[2]	

	    yr_f1	= fdate[0]
	    mh_f1	= fdate[1]
	    dy_f1 	= fdate[2]
	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        dat = dst_data(yr_i1)
        t = n_elements(dat.year)
        ;print, t
        
        i_dst = dat.Dst
        ;print, t
        year = dat.year
        tiempo = TIMEGEN(t, START=julday(dat.month[0], dat.day[0],  $
                         dat.year[0], dat.hour[0]), UNITS='Hours')                                     
        
        iyear = strmid(string(yr_i1, format='(I4)'),2,2)
        fyear = strmid(string(yr_f1, format='(I4)'),2,2)
                
        idoy      = Date2DOY(string(iyear, mh_i1, dy_i1,format = '(I02,I02,I02)'))
        fdoy      = Date2DOY(string(fyear, mh_f1, dy_f1,format = '(I02,I02,I02)'))   
        
        time_w  = tiempo[idoy:fdoy]
        tw = n_elements(time_w)
        tot_days= findgen(tw*24)/24.0
        tot_days_2= (findgen(tw*24)/24.0)-6
                                                         
        Date = string(yr_i1, mh_i1, dy_i1, FORMAT='(I4, "-", I02, "-", I02)')    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of DH Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-		    
    ;##############################################################################
    ; reading data files
    ;##############################################################################
            file_number    = (JULDAY(mh_f1, dy_f1, yr_f1) - JULDAY(mh_i1, dy_i1, yr_i1))+1
            data_file_name = strarr(file_number)
            string_date     = strarr(file_number)
           
            FOR i=0ll, file_number-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i1, dy_i1, yr_i1)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                    data_file_name[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'
            ENDFOR

            exist_data_file   = FILE_TEST(data_file_name)
            capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

            IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                    PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                    PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
            ENDIF

            fecha = string(yr_i1, mh_i1, dy_i1, yr_f1, mh_f1, dy_f1, format = '(I4,I02,I02,"_",I4,I02,I02)')

            H    = FLTARR(file_number*24)
            
            H_STDESV    = FLTARR(file_number*24)
                           
            FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                    IF exist_data_file[i] EQ 1 THEN BEGIN
                            tmp_year    = 0
                            tmp_month   = 0
                            tmp_day     = 0
                            READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                            ;print, tmp_year, tmp_month, tmp_day
                            dat = DH_teo([tmp_year, tmp_month, tmp_day])
                            
                            H[i*24:(i+1)*24-1] = dat.H[*]

                            H_STDESV[i*24:(i+1)*24-1] = dat.H_stdesv[*]
                                                                                                  
                    ENDIF ELSE BEGIN
                             H[i*24:(i+1)*24-1] = 999999.0

                             H_STDESV[i*24:(i+1)*24-1] = 999999.0                        
                    ENDELSE                
            ENDFOR
            
            for i=0, n_elements(H)-1 do begin
                ;idx = where(H eq 999999.0, inan)
                if H[i] eq 999999.0 then begin
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
            
            tmp_spam = 1
            IF tw GT 7 THEN tmp_spam = 1.5
            IF tw GT 15 THEN tmp_spam = 2.
            
            Xsize=fix(800*tmp_spam)
            Ysize=200
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
        dst      = i_dst[(idoy*24)-24:fdoy*24]     
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; plot the Dst time series for each event

         X_label   = STRARR(tw+1)+' '
        ; print, n_elements(x_label)
            months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
            old_month = mh_i1
           ; print, old_month
            FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                    tmp_year    = 0
                    tmp_month   = 0
                    tmp_day     = 0
                    tmp_julday  = JULDAY(mh_i1, dy_i1, yr_i1)

                    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                    
                    IF i LT N_ELEMENTS(X_label)-1 THEN $
                            X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                    old_month = tmp_month
            ENDFOR    

    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; plot the Dst time series for each event
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ;print, up, down
    time_title = 'September 2017'
                     
    dH = TeXtoIDL('\DeltaH')
    
    if max(dst) gt max(H) then up = max(dst) else up = max(H)
    if min(dst) gt min(H) then down=min(dst) else down=min(H)
                      
        plot, tot_days, H, XTICKS=tw, xminor = 12, POSITION=[0.09,0.2,0.94,0.88],$
        XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=6, CHARSIZE = 1.0,$
        title = time_title, ytitle = 'Indice DST [nT]',  XTICKNAME=REPLICATE(' ', tw+1), $
        XRANGE=[0, tw], YRANGE=[down-20,up], BACKGROUND = blanco, COLOR=negro ;, /noerase             
        ;print, t
        oplot, tot_days, dst, color=azul
    
    if up-down gt 300 then begin
        for i = -600., 100., 100. do oplot, [0,tw], [i,i], linestyle=1, COLOR=negro
    endif else begin
        for i = -600., 100., 50. do oplot, [0,tw], [i,i], linestyle=1, COLOR=negro
    endelse
         
    ;FOR i = 0, file_number-1 DO BEGIN
            ;        OPLOT, [i,i], [down-100,up+100], linestyle=1, COLOR=negro
            ;ENDFOR
    
;###############################################################################
    ;Delimiter lines for TGM1
    ;oplot, [1.65,1.65], [down-100,up+100], linestyle=2, COLOR=negro ;15:36 hr del nov 20
    ;oplot, [2,2], [down-100,up+100], linestyle=2, COLOR=negro ;00:01 hr del nov 21
;###############################################################################
    ;Delimiter lines for TGM2
   ; oplot, [0.88,0.88], [down-100,up+100], linestyle=2, COLOR=negro ;21:07 hr del nov 07
    ;oplot, [1.55,1.55], [down-100,up+100], linestyle=2, COLOR=negro ;  13:12 hr del nov 08

    ;oplot, [2.7,2.7], [down-100,up+100], linestyle=2, COLOR=negro  ;  16:48 hr del nov 09       
    ;oplot, [3.6,3.6], [down-100,up+100], linestyle=2, COLOR=negro ;   14:24 hr del nov 10
;############################################################################### 
    ;Delimiter lines for TGM3
   ; oplot, [1.3,1.3], [down-100,up+100], linestyle=2, COLOR=negro ;21:36 hr de may 14
;    oplot, [1.7,1.7], [down-100,up+100], linestyle=2, COLOR=negro ;12:00 hr del may 15

 ;   oplot, [2.2,2.2], [down-100,up+100], linestyle=2, COLOR=negro  ;  00:01 hr del may 16       
  ;  oplot, [3.7,3.7], [down-100,up+100], linestyle=2, COLOR=negro ;09:36 hr de may 16
;############################################################################### 
    ;Delimiter lines for TGM4
   ; oplot, [1.5,1.5], [down-100,up+100], linestyle=2, COLOR=negro ;07:12 hr de march 17
    ;oplot, [2.1,2.1], [down-100,up+100], linestyle=2, COLOR=negro
;###############################################################################   
    ;Delimiter lines for TGM5
   ; oplot, [0.7,0.7], [down-100,up+100], linestyle=2, COLOR=negro ;18:00 hr de may 27    
   ; oplot, [1.7,1.7], [down-100,up+100], linestyle=2, COLOR=negro ;     00:01 hr de may 29
;###############################################################################    
    ;Delimiter lines for TGM6
    oplot, [1,1], [down-100,up+100], linestyle=2, COLOR=negro ;20:24 hr de sept 07    
    oplot, [3,3], [down-100,up+100], linestyle=2, COLOR=negro ;     00:01 hr de sept 09
;############################################################################### 

         
            AXIS, XAXIS = 0, XRANGE=[0,tw], $
                             XTICKS=tw, $
                             XMINOR=12, $
                             XTITLE = 'Time [UT] ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                             XTICKNAME=X_label, $
                             COLOR=negro, $
                             CHARSIZE = 0.9, $
    ;                         CHARTHICK=chr_thick1, $
                             TICKLEN=0.04
                             
            AXIS, XAXIS = 1, XRANGE=[0,tw], $
                             XTICKS=tw, $
                             XMINOR=12, $
                            ; xtitle = 'November 2003', $
                             XTICKNAME=REPLICATE(' ', tw+1), $
                             COLOR=negro, $
                             TICKLEN=0.04

            AXIS, YAXIS = 0, YRANGE=[down-20,up], $
                             ystyle=2, $
                             YTITLE = dH+' and Dst [nT]', $
                             COLOR=negro, $
                             CHARSIZE = 0.9;, $
                             ;TICKLEN=0.00

            AXIS, YAXIS = 1, YRANGE=[down-20,up], $
                             ;YTITLE = 'DH [nT]', $
                             ystyle=2, $
                             COLOR=negro, $
                             CHARSIZE = 0.9;, $
                            ; CHARTHICK=chr_thick1;, $   


        ;if tw gt 15 then begin
        ;    XYOUTS, 0.05, 0.170 , /NORMAL, $
         ;           '            Dst index,               DH index', COLOR=negro, $
         ;           CHARSIZE = chr_size1, $
         ;           CHARTHICK=chr_thick1     
         
      ;      POLYFILL, [0.07,0.10,0.10,0.07], [0.178,0.178,0.180,0.180], color = negro, /NORMAL
      ;      POLYFILL, [0.19,0.22,0.22,0.19], [0.178,0.178,0.180,0.180], color = rojo, /NORMAL
       ; endif else begin
         ;   XYOUTS, 0.43, 0.168 , /NORMAL, $
        ;            '            Dst index,         DH index', COLOR=negro, $
        ;            CHARSIZE = chr_size1, $
        ;            CHARTHICK=chr_thick1     
         
       ;     POLYFILL, [0.77,0.80,0.80,0.77], [0.278,0.278,0.280,0.280], color = azul, /NORMAL
       ;     POLYFILL, [0.79,0.82,0.82,0.79], [0.278,0.278,0.280,0.280], color = negro, /NORMAL
      ;  endelse
       ; LOADCT, 0, /SILENT
        
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
                    write_jpeg, path+'dst_DH_'+Date+'.jpg', True_Image, true=1
            ENDIF ELSE BEGIN
                    IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                    WRITE_PNG, path+'dst_DH_'+Date+'.png', Image, reds,greens,blues
            ENDELSE

            IF NOT keyword_set(quiet) THEN BEGIN
                    print, '        Saving: '+path+'dst_DH_'+Date
                    print, ''
            ENDIF
            RETURN
    end
;############################################################################################################   
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################
