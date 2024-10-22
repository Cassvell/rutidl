FUNCTION date2doy, idate
On_error, 2
COMPILE_OPT idl2, HIDDEN

@set_up_commons
set_up 
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
	IF (info[0] eq 0) THEN BEGIN
	  scalar = 1				;scalar flag set
	ENDIF ELSE BEGIN
	  scalar = 0				;vector input
	ENDELSE

	IF (info[info[0] + 1] eq 7) THEN BEGIN
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

	IF (scalar) THEN BEGIN          ; scalar input
		IF ((yy MOD 4) EQ 0) THEN BEGIN   ; leap year
		  imonth[2] = 29                 ; set feb to 29 days
		ENDIF
		IF (mm GT 1) THEN BEGIN          ; If month > January
		  DOY = FIX(TOTAL(imonth[0:mm-1])) + dd
		ENDIF ELSE BEGIN                 ; For January, DOY is just the day of the month
		  DOY = dd
		ENDELSE
	  ENDIF ELSE BEGIN
		DOY = dd                          ; Set correct length on vector
		leapYrs = WHERE((yy MOD 4) EQ 0)  ; Index of leap years
		nonLeap = WHERE((yy MOD 4) NE 0)  ; Index of non-leap years
	  
		; Process non-leap years
		IF (nonLeap[0] NE -1) THEN BEGIN
		  FOR i = 0, N_ELEMENTS(nonLeap) - 1 DO BEGIN
			IF (mm[nonLeap[i]] GT 1) THEN BEGIN
			  DOY[nonLeap[i]] = FIX(TOTAL(imonth[0:mm[nonLeap[i]]-1])) + dd[nonLeap[i]]
			ENDIF ELSE BEGIN
			  DOY[nonLeap[i]] = dd[nonLeap[i]]  ; For January, set DOY to day of the month
			ENDELSE
		  ENDFOR
		ENDIF
	  
		; Process leap years
		IF (leapYrs[0] NE -1) THEN BEGIN
		  imonth[2] = 29                   ; set feb to 29 days
		  FOR i = 0, N_ELEMENTS(leapYrs) - 1 DO BEGIN
			IF (mm[leapYrs[i]] GT 1) THEN BEGIN
			  DOY[leapYrs[i]] = FIX(TOTAL(imonth[0:mm[leapYrs[i]]-1])) + dd[leapYrs[i]]
			ENDIF ELSE BEGIN
			  DOY[leapYrs[i]] = dd[leapYrs[i]]  ; For January, set DOY to day of the month
			ENDELSE
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