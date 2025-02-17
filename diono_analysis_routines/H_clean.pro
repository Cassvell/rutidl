;
;Name:
;	H_clean.pro
;purpose:
;	derivación de las líneas base de dia a dia a partir de archivos raw
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data generator
;
;calling sequence:
;   .r H_filmaker
;   H_filmaker, idate[yyyy,mm,dd], fdate[yyyy,mm,dd], MAKE_FILE=make_file
;parameters:
;   KEYWORD = make_file. Set this keyword to generate H data files in minute and hourly resolution
;
;dependencies:
;
;
;input files
;   geomagnetic field raw measurements from a certain observatory or geomagnetic station.
;
;output files:
;   .dat file of H data with no day to day variation
;
;   imported to: 
;version
;   Dec, 2022
;   Feb, 2023
;   Jun, 2023
;
;note
;   For following analysis, this routine has to be run to create clean H obs data
;

FUNCTION rawH, date, station, idx

	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]
        @set_up_commons
        set_up		
;###############################################################################
;reading data files
;###############################################################################
        date = STRING(year, month, day, FORMAT = '(I4, I02, I02)')		
        str_year = STRING(year, FORMAT = '(I4)')	
        station_code    = set_var.gms_code[idx]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu


    CASE station_code of
        'coe'   : gms_net = 'regmex'
        'teo'   : gms_net = 'regmex'
        'tuc'   : gms_net = 'intermagnet'
        'bsl'   : gms_net = 'intermagnet'
        ELSE : PRINT, 'non avaiable gms data'
    ENDCASE

		
    IF gms_net EQ 'regmex' THEN BEGIN 		
		data_dir = set_var.Mega_dir+station+'/'+station+'/'
		file_name = data_dir+station_code+'_'+date+'.clean.dat'
       ; print, file_name
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, LUN, file, /GET_LUN, ERROR=err
		READF, LUN, data, FORMAT = '(A)'
		CLOSE, LUN
		FREE_LUN, LUN
;###############################################################################
;extracting data and denfining an structure data
;###############################################################################
        DStruct = {year:0, month:0, day:0, hour:0, minutes:0, DOY:0, $ 
                   D:0., H:0., Z:0., F:0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
        header = 0             ; Defining number of lines of the header 

		READS, data[header:number_of_lines-1], teo_mag, $
		FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,8X,I03,F12,F10,F10,F10)'
    ENDIF
    
    IF gms_net EQ 'intermagnet' THEN BEGIN

		data_dir = set_var.gic_dir+str_year+'/'+STRUPCASE(station_code)+'/daily/'
		file_name = data_dir+station_code+date+'qmin.min.out'

		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN BEGIN
		    file_name = data_dir+station_code+date+'pmin.min.out'
		    file = FILE_SEARCH(file_name, COUNT=opened_files)
		ENDIF 
		
		
		IF opened_files NE N_ELEMENTS(file) THEN BEGIN
		    file_name = data_dir+station_code+date+'dmin.min.out'
		    file = FILE_SEARCH(file_name, COUNT=opened_files)		
        ENDIF

		
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		OPENR, LUN, file, /GET_LUN, ERROR=err
		READF, LUN, data, FORMAT = '(A)'
		CLOSE, LUN
		FREE_LUN, LUN
;###############################################################################
;extracting data and denfining an structure data
;###############################################################################
        DStruct = {year:0, month:0, day:0, hour:0, minuntes:0, DOY:0, $ 
                   X:0., Y:0., Z:0., F:0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
        header = 0             ; Defining number of lines of the header 

		READS, data[header:number_of_lines-1], teo_mag, $
		FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,8X,I03,F13,F10,F10,F10)'
    
    ENDIF
		RETURN, teo_mag		
END


FUNCTION rawH_array, date_i, date_f, station, idx
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]

        @set_up_commons
        set_up
        
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = STRARR(file_number)
        string_date     = STRARR(file_number)
        str_year = STRING(yr_i, FORMAT = '(I4)')
        station_code    = set_var.gms_code[idx]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu

    CASE station_code of
        'coe'   : gms_net = 'regmex'
        'teo'   : gms_net = 'regmex'
        'tuc'   : gms_net = 'intermagnet'
        'bsl'   : gms_net = 'intermagnet'
        ELSE : PRINT, 'non avaiable gms data'
    ENDCASE
    	
    IF gms_net EQ 'regmex' THEN BEGIN 
		        
       dir = set_var.Mega_dir+station+'/'+station+'/'
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = dir+station_code+'_'+string_date[i]+'.clean.dat'
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(WHERE(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.H data.',A,' impossible to plot all data.')"              
        ENDIF

        H    = FLTARR(file_number*1440)                               
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        dat = rawH([tmp_year, tmp_month, tmp_day], station, idx)
                        
                        H[i*1440:(i+1)*1440-1] = dat.H[*]                                                
                ENDIF ELSE BEGIN
                         H[i*1440:(i+1)*1440-1] = 999999.0                       
                ENDELSE                
        ENDFOR
    ENDIF
    
    IF gms_net EQ 'intermagnet' THEN BEGIN 
       dir = set_var.gic_dir+str_year+'/'+STRUPCASE(station_code)+'/daily/'
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = dir+station_code+string_date[i]+'qmin.min.out'
                ;print, data_file_name[i]
                file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	            IF opened_files NE N_ELEMENTS(file) THEN BEGIN
	                data_file_name[i] = dir+station_code+string_date[i]+'pmin.min.out'
	                file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)  
	            ENDIF; 
	             IF opened_files NE N_ELEMENTS(file) THEN BEGIN
	                data_file_name[i] = dir+station_code+string_date[i]+'dmin.min.out'
	                file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	            ENDIF
	            ;PRINT, data_file_name[i]           
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(WHERE(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.H data.',A,' impossible to plot all data.')"              
        ENDIF

        H    = FLTARR(file_number*1440)                               
        X    = FLTARR(file_number*1440) 
        Y    = FLTARR(file_number*1440)                          
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        dat = rawH([tmp_year, tmp_month, tmp_day], station, idx)
                        
                        X[i*1440:(i+1)*1440-1] = dat.X[*]  
                        Y[i*1440:(i+1)*1440-1] = dat.Y[*]                                              
                ENDIF ELSE BEGIN
                        X[i*1440:(i+1)*1440-1] = 99999.0
                        Y[i*1440:(i+1)*1440-1] = 99999.0                     
                ENDELSE                
        ENDFOR
        X = add_nan(X, 99999.0, 'equal')        
        Y = add_nan(Y, 99999.0, 'equal')
        H = SQRT((X)^2 + (Y)^2)
       ; PRINT, X
    ENDIF
    
    
    RETURN, H
END


FUNCTION H_clean, date_i, date_f, station_idx, QUIETD=QUIETD
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
        @set_up_commons
        set_up

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]	
;###############################################################################    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    tot_days= FINDGEN(file_number*24)/24.0    
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')

    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
    
; Generate the time series variables 
; define H variables                  
    H  = rawH_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, FIX(station_idx))

    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 99999.0, 'equal')           
  ;  PRINT, H
;Applying a detrend function
    H_24h=FINDGEN(N_ELEMENTS(H)/1440)   
    FOR i=0, N_ELEMENTS(H_24h)-1 DO BEGIN
        H_24h[i] = MEDIAN(H[i*1440:(i+1)*1440-1])    
    ENDFOR
    
    n = 1.5
    
        IF KEYWORD_SET(QUIETD) THEN BEGIN
    	n = 2
    ENDIF
    
    IQR = PERCENTILES(H_24h, CONFLIMIT=0.5) 
    IQR_n = (IQR[1]-IQR[0])*n
 
    index_out = WHERE(H_24h GE MEDIAN(H_24h)+IQR_n OR H_24h LE MEDIAN(H_24h)-IQR_n[0])
    ;index_in  = WHERE(H_24h LE QR3 AND H_24h GE QR1)          
  ;  PRINT, index_out 
    time = FINDGEN(N_ELEMENTS(H))/1440.0
   
    IF (index_out[0]) NE -1 THEN BEGIN
        H_24h[index_out] = !Values.F_NAN
        H_24h = fillnan(H_24h)
    ENDIF    
    PRINT, '#######################################################################'     
  
    ;PRINT, H_24h   
    PRINT, '#######################################################################'     
 
        
;plotting to check data    
   ; H_trend = INTERPOL(H_24h, N_ELEMENTS(time), /QUADRATIC)

    H_trend = INTERPOLATE(H_24h, time, CUBIC=-0.5,  /GRID, /MISSING)
    ;print, H_trend
    QR1 = cgPercentiles(H_trend, Percentiles=[0.25])  
    QR3 = cgPercentiles(H_trend, Percentiles=[0.75])

    IQR_n = (QR3-QR1)*n
    ;PRINT, QR1, QR3
    sup = MEDIAN(H_trend)+IQR_n
    inf = MEDIAN(H_trend)-IQR_n
    l_sup = FLTARR(N_ELEMENTS(H_trend))
    l_sup[*] = sup

    l_inf = FLTARR(N_ELEMENTS(H_trend))
    l_inf[*] = inf             
    H_det = H - H_trend
;###############################################################################    
    dif_Hdet = TS_DIFF(H_det,1)
	spikes = whitaker_hayer(dif_Hdet, 20)    
	IF spikes[0] NE -1 THEN BEGIN
		print, 'spikes num: ', N_ELEMENTS(spikes)
	 ; dif_Hdet[spikes] = !Values.F_NAN
		H_det[spikes] = !Values.F_NAN
	;dif_Hdet = fillnan(dif_Hdet)	
		;H_det = fillnan(H_det)		
	ENDIF ELSE BEGIN
		;MESSAGE, 'no spikes detected'	
	ENDELSE 															 
    
   ; PRINT, MIN(dif_Hdet, /NAN)
	PRINT, '#################################################################################'	
	PRINT, 'AVR H0: ', MEAN(H_det, /NAN)
	PRINT, 'MED H det: ', MEDIAN(H_det)	
;###############################################################################

;###############################################################################
;###############################################################################

;###############################################################################	
;###############################################################################
;###############################################################################
	
;###############################################################################
;###############################################################################
;Bsq baseline
	H1 = H_det[0:1439]
	
	H2 = H_det[(file_number-1)*1440:N_ELEMENTS(H_det)-1]

	ndays   = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
	bsq = bsq(H1, H2, ndays)

	H_clean = H_det - bsq.m
	;H_clean = H_det[(day1_diff-1)*1440:((file_number)-day2_diff)*1440] - bsq.m
	;print, H_clean
;###############################################################################
;###############################################################################   
;    DEVICE, true=24, retain=2, decomposed=0
;    TVLCT, R_bak, G_bak, B_bak, /GET        
;    LOADCT, 39, /SILENT    
        
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')
    new_month = month_name(mh_f, 'english')
    IF mh_i NE mh_f THEN BEGIN
    	time_name = 'days of '+old_month+' and '+ new_month
    ENDIF ELSE BEGIN 
    	time_name = 'days of '+old_month
    ENDELSE
 ;  WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE='H raw data'
  ;  PLOT, time,H, YRANGE=[MIN(H, /NAN),MAX(H, /NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0, $
   ; CHARTHICK=2.0, YTITLE = 'BH [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
    
;    OPLOT, time, H_trend, LINESTYLE=2, THICK=2, color=254
 ;   PLOTS, FINDGEN(file_number), H_24h, PSYM=4, symsize=2, THICK=4, color=0 
  ;  OPLOT, time, l_sup, LINESTYLE=1, THICK=2, color=0
   ; OPLOT, time, l_inf, LINESTYLE=1, THICK=2, color=0    
    
;    WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE='H cleaned'
 ;   PLOT, time, H_det, XSTYLE=1, XRANGE=[0, file_number], $
  ;  YRANGE=[MIN(H_det, /NAN),MAX(H_det,/NAN)], CHARSIZE = 1.8, background=255, color=0, $
   ; CHARTHICK=2.0, YTITLE = 'H [nT]',XTITLE = time_name, /NODATA, XTICKS=file_number, $
   ; XTICKNAME=X_label    
	
	;OPLOT, time, H_clean, COLOR=0, THICK=2
	
	;OPLOT, time, H_det, COLOR=254
 ;   WHILE (!MOUSE.button NE 4) DO BEGIN  ; repeat printing H trend value until right mouse button is pressed
    ;  CURSOR, x, y, /DOWN, /DATA
    ;  PRINT, y    
    ;ENDWHILE 
RETURN, H_clean

END
