;
;Name:
;	tgm_file.pro
;purpose:
;	generate a txt file with Dst and dH data in order to correlate their data
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data writting
;
;calling sequence:
;   .r tgm_file
;   tgm_file, date_i, date_f
;parameters:
;   date(_i,_f): format = [yyyy,mm,dd]
;
;dependencies:
;
;
;input files
;   dH and Dst data files
;
;output files:
;   txt files containing dH and Dst data with a certain format set to generate a global events
;   correlation and scatter analysis
;   imported to /master_thesis/datos/tgm/dir1/ssubdir/tgmdatayyyymmdd_yyyymmdd.txt
;
;   dir and subdir refer to directories which depend on the study and classification of event
;
;version
;   Dec, 2022
;

function cmreplicate, array, dims

  if n_params() EQ 0 then begin
      message, 'RARRAY = CMREPLICATE(ARRAY, DIMS)', /info
      return, 0L
  endif
      
  if n_elements(dims) EQ 0 then return, array
  if n_elements(array) EQ 0 then $
    message, 'ERROR: ARRAY must have at least one element'

  ;; Construct new dimensions, being careful about scalars
  sz = size(array)
  type = sz(sz(0)+1)
  if sz(0) EQ 0 then return, make_array(value=array, dimension=dims)
  onedims = [sz(1:sz(0)), dims*0+1] ;; For REFORM, to extend # of dims.
  newdims = [sz(1:sz(0)), dims    ] ;; For REBIN, to enlarge # of dims.
  nnewdims = n_elements(newdims)
  
  if nnewdims GT 8 then $
    message, 'ERROR: resulting array would have too many dimensions.'

  if type NE 7 AND type NE 8 AND type NE 10 AND type NE 11 then begin
      ;; Handle numeric types

      ;; Argghh!  Why doesn't REBIN take an *array* of dimensions!
      ;; *Instead* we need to run EXECUTE(), with a string like this:
      ;; rebin(array1, newdims(0), newdims(1), ...)
      ;; That's what the following format string does.
      fmt = '('+strtrim(nnewdims,2)+'("newdims(",I0,")",:,","))'
      arglist = string(lindgen(nnewdims), format=fmt)
      cmd = 'return, rebin(reform([array], onedims),'+arglist+')'
      dummy = execute(cmd)
      
      ;; If execution reaches here then an error occurred.
      message, 'ERROR: array could not be resized'
      return, 0L

  endif else begin
      ;; Handle strings, structures, pointers, and objects separately
      
      ;; Handle structures, which are never scalars
      if type EQ 8 AND sz(0) EQ 1 AND n_elements(array) EQ 1 then $
        return, make_array(value=array, dimension=dims)

      nold = n_elements(array)
      nadd = 1L
      for i = 0L, n_elements(dims)-1 do nadd = nadd * long(dims(i))
      array1 = make_array(type=sz(sz(0)+1), dimension=[nold,nadd])
      array2 = reform([array], n_elements(array))
      
      ;; Efficient copying, done by powers of two
      array1(0,0) = array2
      stride = 1L   ;; stride increase by a factor of two in each iteration
      i = 1L & nleft = nadd - 1
      while nleft GT stride do begin
          array1(0,i) = array1(*,0:stride-1)  ;; Note sneaky IDL optimization
          i = i + stride & nleft = nleft - stride
          stride = stride * 2
      endwhile
      if nleft GT 0 then array1(0,i) = array1(*,0:nleft-1)

      return, reform(array1, newdims, /overwrite)
  endelse

end

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


PRO tgm_file, date_i, date_f

	On_error, 2
	compile_opt idl2, HIDDEN
	@set_up_commons
	set_up
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]      
	
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 

    idate    = string(yr_i, mh_i, dy_i, FORMAT='(I4, I02,I02)')
    fdate    = string(yr_f, mh_f, dy_f, FORMAT='(I4, I02,I02)')
    iyear = strmid(string(yr_i, format='(I4)'),2,2)
    idoy      = Date2DOY(string(iyear, mh_i, dy_i,format = '(I02,I02,I02)'))
    doy = INTARR(file_number)
;###############################################################################  
    H  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'teo', '1')
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    time = TIMEGEN(N_ELEMENTS(dst), START=julday(mh_i, dy_i, dy_i, 0), UNITS='Hours')       
    CALDAT, time, M, D, Y, hour

string_date        = STRARR(file_number)
	       FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, iyear)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')	   
                               
                ;data_file_name_dh[i] = data_path+'/dH_teo/teo_'+string_date[i]+'.dst.early'
                ;data_file_name_dh[i] = dir+STRLOWCASE(station_code)+'_'+string_date[i]+'.dst.early'
                tmp_doy = Date2DOY(string(tmp_year, tmp_month, tmp_day,format = '(I02,I02,I02)'))
              ;  print, tmp_doy
        	    doy[i] =    tmp_doy[*]                      
        ENDFOR        
;###############################################################################
; Initialize an empty output array
n = 24
output_array = FLTARR(N_ELEMENTS(doy) * n)

; Loop through each element of the input array
for i = 0, N_ELEMENTS(doy) - 1 do begin
    ; Compute the starting index for the current repetition
    start_index = i * n

    ; Repeat the current element n times in the output array
    output_array[start_index:start_index + n - 1] = REPLICATE(doy[i], n)
endfor

; Print the output array

;	
doy = FIX(output_array)

;###############################################################################


;############################################################################### 
; Generate the time series variables 
; define H variables                  

; Generate the time variables to plot TEC time series         
;    tec  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'tec')
;    med  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'med')
    
;###############################################################################
;Identifying the NAN values         
    H = add_nan(H, 999999.0, 'equal')
    H = add_nan(H, 99999.0, 'equal')    
;############################################################################################################
    outfile= set_var.Mega_dir+'/article_events/tgmdata'+idate+'_'+fdate+'.dat'
    OPENW, lun, outfile, /GET_LUN

   ; PRINTF, lun, 'DOY', 'hora', 'Dst', 'DH', FORMAT = '(3A,6A,6A,6A)'
    FOR i=0, N_ELEMENTS(dst)-1 DO BEGIN

        print, doy[i], hour[i], dst[i], H[i], FORMAT = '(I03, F5.1, I5, F6.1)' 
        PRINTF, lun, doy[i], hour[i], dst[i], H[i], FORMAT = '(I03, F5.1, I5, F6.1)'           
    ;	PRINTF, lun, doy[i], FORMAT = '(I03)'
    ;	PRINTF, lun, hour[i] , FORMAT = '(F5.1)'
    ;	PRINTF, lun, dst[i] , FORMAT = '(I5)'
    ;	PRINTF, lun, H[i],  FORMAT = '(F6.1)'
          
    ENDFOR
    
    close,lun
    FREE_LUN, lun

END
