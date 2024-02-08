;
;Name:
;	H_filemaker.pro
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

FUNCTION rawmag, date, station_code

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
        
           ;regmex		  0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
        							; intermagnet bmt:0, bou:1, brd:2, bsl:3, cki:4, cyg:5, gui:6, hbk:7, ipm:8, kak:9, $
    											 ;kdu:10, kmh:11, pil:12, sjg:13, tam:14, tdc:15, tuc:16
		gms_class = gms_class(station_code)
		
    IF gms_class EQ 'regmex' THEN BEGIN 		
		data_dir = set_var.Mega_dir+'regmex/'+station_code+'/'+station_code+'_raw/'
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

		mag_data = REPLICATE(DStruct, number_of_lines)	
        header = 0             ; Defining number of lines of the header 

		READS, data[header:number_of_lines-1], mag_data, $
		FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,8X,I03,F12,F10,F10,F10)'
    ENDIF
    
    IF gms_class EQ 'intermagnet' THEN BEGIN

		data_dir = set_var.google_dir+'intermagnet/'+str_year+'/'+STRUPCASE(station_code)+'/'
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

		IF opened_files NE N_ELEMENTS(file) THEN BEGIN
		    file_name = data_dir+station_code+date+'vmin.min.out'
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

		mag_data = REPLICATE(DStruct, number_of_lines)	
        header = 0             ; Defining number of lines of the header 

		READS, data[header:number_of_lines-1], mag_data, $
		FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,8X,I03,F13,F10,F10,F10)'
    
    ENDIF
		RETURN, mag_data		
END

FUNCTION rawmag_midday, date, station_code

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
		class = gms_class(station_code)  ; set if the gms is from REGMEX (local) or INTERMAGNET
		;according to this, there are some differences in the pre-processing
    
    
    IF class EQ 'intermagnet' THEN BEGIN

		data_dir = set_var.google_dir+'intermagnet/'+str_year+'/'+STRUPCASE(station_code)+'/'
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

		IF opened_files NE N_ELEMENTS(file) THEN BEGIN
		    file_name = data_dir+station_code+date+'vmin.min.out'
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

		mag_data = REPLICATE(DStruct, number_of_lines)	
        header = 0             ; Defining number of lines of the header 

		READS, data[header:number_of_lines-1], mag_data, $
		FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,8X,I03,F13,F10,F10,F10)'
		mag_data = mag_data[0:59]
		zeroes1 = FLTARR(1380)
		;zeroes2 = FLTARR(660)
		X = [mag_data.X, zeroes1]
		Y = [mag_data.Y, zeroes1]
        
        mag_data2 = {X:FLTARR(N_ELEMENTS(X)), Y:FLTARR(N_ELEMENTS(Y))}
        mag_data2.X = X[*]
        mag_data2.Y = Y[*] 
    ENDIF
    
    IF class EQ 'regmex' THEN BEGIN 		
		data_dir = set_var.Mega_dir+'regmex/'+station_code+'/'+station_code+'_raw/'
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

		mag_data = REPLICATE(DStruct, number_of_lines)	
        header = 0             ; Defining number of lines of the header 

		READS, data[header:number_of_lines-1], mag_data, $
		FORMAT='(I4,X,I02,X,I02,X,I02,X,I02,8X,I03,F12,F10,F10,F10)'
		mag_data = mag_data[0:59]
		zeroes1 = FLTARR(1380)
		;zeroes2 = FLTARR(660)
		H = [mag_data.H, zeroes1]
		D = [mag_data.D, zeroes1]
        
        mag_data2 = {D:FLTARR(N_ELEMENTS(D)), H:FLTARR(N_ELEMENTS(H))}
        mag_data2.H = H[*]
        mag_data2.D = D[*]        
    ENDIF
    
		RETURN, mag_data2
END

FUNCTION rawmag_array, date_i, date_f, station_code
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
        
		gms_net = gms_class(station_code)
    	
    IF gms_net EQ 'regmex' THEN BEGIN 
		        
       dir = set_var.Mega_dir+'regmex/'+station_code+'/'+station_code+'_raw/'
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
        D    = FLTARR(file_number*1440)
        X    = FLTARR(file_number*1440) 
        Y    = FLTARR(file_number*1440)
        Z    = FLTARR(file_number*1440)
        F    = FLTARR(file_number*1440)
        I    = FLTARR(file_number*1440)
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        dat = rawmag([tmp_year, tmp_month, tmp_day], station_code)
                        
                        H[i*1440:(i+1)*1440-1] = dat.H[*]
                        D[i*1440:(i+1)*1440-1] = dat.D[*]
                        Z[i*1440:(i+1)*1440-1] = dat.Z[*]
                        F[i*1440:(i+1)*1440-1] = dat.F[*]
                ENDIF ELSE BEGIN
                        H[i*1440:(i+1)*1440-1] = 999999.0 
                        D[i*1440:(i+1)*1440-1] = 999999.0 
                        Z[i*1440:(i+1)*1440-1] = 999999.0 
                        F[i*1440:(i+1)*1440-1] = 999999.0                        
                ENDELSE                
        ENDFOR
        D = add_nan(D, 999999.0 , 'equal')
        D = add_nan(D, 9999.0 , 'equal') 
        H = add_nan(H, 999999.0 , 'equal') 
        Z = add_nan(Z, 999999.0 , 'equal')        
        F = add_nan(F, 999999.0 , 'equal') 

		Ddeg = D/60
		deg2rad = (!PI/180)
		D = deg2rad*Ddeg
		
		X = H*COS(D)
		Y = H*SIN(D)

        X = H*COS(D)
        Y = H*SIN(D)
        I = ATAN(Z/H)
        mag_data = {H : FLTARR(N_ELEMENTS(H)), D : FLTARR(N_ELEMENTS(D)), Z : FLTARR(N_ELEMENTS(Z)),$
        			F : FLTARR(N_ELEMENTS(F)), X : FLTARR(N_ELEMENTS(X)), Y : FLTARR(N_ELEMENTS(Y)),$
        			I : FLTARR(N_ELEMENTS(I))}
        
        mag_data.H = H[*]
        mag_data.D = D[*]
        mag_data.I = I[*]
        mag_data.X = X[*]
        mag_data.Y = Y[*]
        mag_data.Z = Z[*]
        mag_data.F = F[*]        
    ENDIF
    
    IF gms_net EQ 'intermagnet' THEN BEGIN 
       dir =  set_var.google_dir+'intermagnet/'+str_year+'/'+STRUPCASE(station_code)+'/'
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = dir+station_code+string_date[i]+'qmin.min.out'
                file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	            IF opened_files NE N_ELEMENTS(file) THEN BEGIN
	                data_file_name[i] = dir+station_code+string_date[i]+'pmin.min.out'
	                file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)  
	            ENDIF; 
	             IF opened_files NE N_ELEMENTS(file) THEN BEGIN
	                data_file_name[i] = dir+station_code+string_date[i]+'dmin.min.out'
	                file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	            ENDIF
	            
	             IF opened_files NE N_ELEMENTS(file) THEN BEGIN
	                data_file_name[i] = dir+station_code+string_date[i]+'vmin.min.out'
	                file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
	            ENDIF	            
	           ; PRINT, data_file_name[i]           
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
        Z    = FLTARR(file_number*1440)
        F    = FLTARR(file_number*1440)
        D    = FLTARR(file_number*1440)
        I    = FLTARR(file_number*1440)
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        dat = rawmag([tmp_year, tmp_month, tmp_day], station_code)
                        
                        X[i*1440:(i+1)*1440-1] = dat.X[*]  
                        Y[i*1440:(i+1)*1440-1] = dat.Y[*]        
                        Z[i*1440:(i+1)*1440-1] = dat.Z[*]  
                        F[i*1440:(i+1)*1440-1] = dat.F[*] 
                     
                ENDIF ELSE BEGIN
                        X[i*1440:(i+1)*1440-1] = 99999.0
                        Y[i*1440:(i+1)*1440-1] = 99999.0         
                        Z[i*1440:(i+1)*1440-1] = 99999.0
                        F[i*1440:(i+1)*1440-1] = 99999.0                          
                ENDELSE                
        ENDFOR
        X = add_nan(X, 99999.0, 'equal')        
        Y = add_nan(Y, 99999.0, 'equal')
        Z = add_nan(Z, 99999.0, 'equal')        
        F = add_nan(F, 99999.0, 'equal')        
        H = SQRT((X)^2 + (Y)^2)                
        D = ATAN(Y/X)
        I = ATAN(Z/H)
        
        mag_data = {H : FLTARR(N_ELEMENTS(H)), X : FLTARR(N_ELEMENTS(X)), Y : FLTARR(N_ELEMENTS(Y)), $
        			Z : FLTARR(N_ELEMENTS(Z)), F : FLTARR(N_ELEMENTS(F)), D : FLTARR(N_ELEMENTS(D)), $
        			I : FLTARR(N_ELEMENTS(I))}
        			
        mag_data.H = H[*]
        mag_data.D = D[*]
        mag_data.I = I[*]
        mag_data.X = X[*]
        mag_data.Y = Y[*]
        mag_data.Z = Z[*]
        mag_data.F = F[*]          			
    ENDIF
    
    
    RETURN, mag_data
END




PRO H_filemaker, date_i, date_f, MAKE_FILE=make_file
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
	
	station_class = ''
	PRINT, 'Enter GMS class code: 		0:regmex or 1:intermagnet'
	READ, station_class, PROMPT = '> '
	
	CASE station_class of
		'0' : station_class = 'regmex'
		'1' : station_class = 'intermagnet'
		 ELSE : PRINT, 'non avaiable gms class'
	END
	PRINT, 'Enter GMS idx: If do not know the GMS idx, please run PRO gms code table'
	READ, station_idx, PROMPT = '> '
;###############################################################################
;###############################################################################    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    tot_days= FINDGEN(file_number*24)/24.0    
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')


    IF station_class EQ 'regmex' THEN  BEGIN    
    station = set_var.gms[FIX(station_idx)]
    station_code = set_var.gms_code[FIX(station_idx)]  
    ENDIF 
    
    IF station_class EQ 'intermagnet' THEN  BEGIN 
    station = set_var.gmsi[FIX(station_idx)] 
    station_code = set_var.gmsi_code[FIX(station_idx)] 
    ENDIF
	PRINT,  'GMS selected: '+station+' IAGA code: '+station_code   
;###############################################################################	
;###############################################################################     
; Generate the time series variables 
; define Geomagnetic data component variables                  
    mag_array  = rawmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code)
	X = mag_array.X
	Y = mag_array.Y
	Z = mag_array.Z
	
    X = add_nan(X, 999999.0, 'equal')        
    X = add_nan(X, 99999.0, 'equal') 
	Y = add_nan(Y, 99999.0, 'equal')
	Y = add_nan(Y, 9999.0, 'equal')      
    Z = add_nan(X, 999999.0, 'equal') 	
	Z = add_nan(Y, 99999.0, 'equal')
	Z = add_nan(Y, 9999.0, 'equal')	
	
	X_det = day2day(X, 'X')
	Y_det = day2day(Y, 'Y')
	Z_det = day2day(Z, 'Z')
	;print, X_det
   ; day2day_struct = day2day([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code)
  	
  	;X_trend = day2day_struct.X_trend
 	;X_24h = day2day_struct.X_24h

  	;Y_trend = day2day_struct.Y_trend
 	;Y_24h = day2day_struct.Y_24h
;print,     X_24h
	print, Z_det
    STOP, "detención momentanea de prueba" 	
;###############################################################################
;###############################################################################     
;###############################################################################    
;###############################################################################    
    dif_Xdet = TS_DIFF(X_det,1)
	Xspikes = whitaker_hayer(dif_Xdet, 20)    
	IF Xspikes[0] NE -1 THEN BEGIN
		print, 'spikes num: ', N_ELEMENTS(spikes)
	 ; dif_Hdet[spikes] = !Values.F_NAN
		X_det[Xspikes] = !Values.F_NAN
	;dif_Hdet = fillnan(dif_Hdet)	
		;H_det = fillnan(H_det)		
	ENDIF ELSE BEGIN
		MESSAGE, 'no spikes detected in X'	
	ENDELSE 															 
;############################################################################### 
     dif_Ydet = TS_DIFF(Y_det,1)
	Yspikes = whitaker_hayer(dif_Ydet, 20)    
	IF Yspikes[0] NE -1 THEN BEGIN
		print, 'spikes num: ', N_ELEMENTS(Yspikes)
	 ; dif_Hdet[spikes] = !Values.F_NAN
		Y_det[Yspikes] = !Values.F_NAN
	;dif_Hdet = fillnan(dif_Hdet)	
		;H_det = fillnan(H_det)		
	ENDIF ELSE BEGIN
		MESSAGE, 'no spikes detected in Y'	
	ENDELSE 
 ;###############################################################################   
 ;###############################################################################   

   ; PRINT, MIN(dif_Hdet, /NAN)
	PRINT, '#################################################################################'	
	PRINT, 'AVR X0: ', MEAN(X_det, /NAN)
	PRINT, 'MED X det: ', MEDIAN(X_det)	
	PRINT, 'AVR Y0: ', MEAN(Y_det, /NAN)
	PRINT, 'MED Y det: ', MEDIAN(Y_det)
	PRINT, '#################################################################################'	
	;PRINT, H_det

;###############################################################################
;###############################################################################
;###############################################################################   
    DEVICE, true=24, retain=2, decomposed=0
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT    
        
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')
    new_month = month_name(mh_f, 'english')
    IF mh_i NE mh_f THEN BEGIN
    	time_name = 'days of '+old_month+' and '+ new_month
    ENDIF ELSE BEGIN 
    	time_name = 'days of '+old_month
    ENDELSE
   ; set_plot, 'x'   
       time = FINDGEN(N_ELEMENTS(H))/1440.0   
;###############################################################################
   WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE='X raw data'
    PLOT, time,X, YRANGE=[MIN(X, /NAN),MAX(X, /NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'BX [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
    
    OPLOT, time, X_trend, LINESTYLE=2, THICK=2, color=254
    PLOTS, FINDGEN(file_number), X_24h, PSYM=4, symsize=2, THICK=4, color=0 
    OPLOT, time, Xl_sup, LINESTYLE=1, THICK=2, color=0
    OPLOT, time, Xl_inf, LINESTYLE=1, THICK=2, color=0    
   ; PRINT, H_t
    
    WINDOW, 0, XSIZE=1000, YSIZE=400, TITLE='X detrended'
    PLOT, time, X_det, YRANGE=[MIN(X_det, /NAN),MAX(X_det,/NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'X [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label	
;###############################################################################
   WINDOW, 1, XSIZE=1000, YSIZE=400, TITLE='Y raw data'
    PLOT, time,Y, YRANGE=[MIN(Y, /NAN),MAX(Y, /NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'BY [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
    
    OPLOT, time, Y_trend, LINESTYLE=2, THICK=2, color=254
    PLOTS, FINDGEN(file_number), Y_24h, PSYM=4, symsize=2, THICK=4, color=0 
    OPLOT, time, Yl_sup, LINESTYLE=1, THICK=2, color=0
    OPLOT, time, Yl_inf, LINESTYLE=1, THICK=2, color=0    
    
    WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE='Y detrended'
    PLOT, time, Y_det, YRANGE=[MIN(X_det, /NAN),MAX(X_det,/NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'Y [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label	
;###############################################################################
   WINDOW, 4, XSIZE=1000, YSIZE=400, TITLE='H raw data'
    PLOT, time,H, YRANGE=[MIN(H, /NAN),MAX(H, /NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'BH [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label
    
   ; OPLOT, time, H_trend, LINESTYLE=2, THICK=2, color=254
   ; PLOTS, FINDGEN(file_number), Y_24h, PSYM=4, symsize=2, THICK=4, color=0 
   ; OPLOT, time, Yl_sup, LINESTYLE=1, THICK=2, color=0
   ; OPLOT, time, Yl_inf, LINESTYLE=1, THICK=2, color=0    
    
    WINDOW, 5, XSIZE=1000, YSIZE=400, TITLE='H detrended'
    PLOT, time, H_det, YRANGE=[MIN(X_det, /NAN),MAX(X_det,/NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'H [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label	


;Selection of Qdays
	QD = ['QD1', 'QD2', 'QD3', 'QD4', 'QD5']     
;###############################################################################
;Selection of Q days Based on Kmex criteria
	;fiveQD_1 = getting_local_qdays([yr_i,mh_i,dy_i], station_idx)

   ;PRINT, '                                                                    '
;PRINT, "Five Qdays according to K criteria"

 ;   FOR i=0, 4 DO BEGIN
 ;      PRINT, fiveQD_1.year[i], fiveQD_1.month[i], $
 ;      fiveQD_1.day[i], QD[i], FORMAT = '(I4, "-", I02, "-", I02, 4X, A)'                                 
 ;   ENDFOR
    
 ;   IF mh_i NE mh_f THEN BEGIN
 ;   	fiveQD_2 = getting_local_qdays([yr_f,mh_f,dy_f], station_idx)
 ;   	PRINT, ''
 ;   	PRINT, ''    
;		FOR i=0, 4 DO BEGIN
;		   PRINT, fiveQD_2.year[i], fiveQD_2.month[i], $
;		   fiveQD_2.day[i], QD[i], FORMAT = '(I4, "-", I02, "-", I02, 4X,  A)'                                 
;		ENDFOR 
;	ENDIF	

;###############################################################################
;Selection of Q days Based on IQR criteria    
	fiveQD2_1 = sel_qdayV2([yr_i,mh_i,dy_i], station_idx)  
	PRINT, "Five Qdays according to IQR criteria"	
    FOR i=0, 4 DO BEGIN
       PRINT, STRING(fiveQD2_1.year[i], fiveQD2_1.month[i], fiveQD2_1.day[i], QD[i], fiveQD2_1.iqr[i],$
      ;  PRINT, y[i], m[i], d[i], IQR_hr[i], $
        FORMAT = '(I4, "-", I02, "-", I02,  4X,A, F6.2)')                                 
    ENDFOR
    
    IF 	mh_i NE mh_f THEN BEGIN
    	fiveQD2_2 = sel_qdayV2([yr_f,mh_f,dy_f], station_idx)

		FOR i=0, 4 DO BEGIN
		   PRINT, STRING(fiveQD2_2.year[i], fiveQD2_2.month[i], fiveQD2_2.day[i], QD[i], fiveQD2_2.iqr[i],$
		  ;  PRINT, y[i], m[i], d[i], IQR_hr[i], $
		    FORMAT = '(I4, "-", I02, "-", I02,  4X,A, F6.2)')                                 
		ENDFOR
        	
    ENDIF	
;###############################################################################
;###############################################################################
	qday1 = ''
	PRINT, 'select the first QD: [dd]'
	READ, qday1, PROMPT = '> '

	qday2 = ''
	PRINT, 'select the second QD: [dd]'
	READ, qday2, PROMPT = '> '
	
	dy_1 	= FIX(qday1)	
	dy_2 	= FIX(qday2)	
;###############################################################################
;###############################################################################
;Bsq baseline
	day1_diff = (JULDAY(mh_i,dy_1,yr_i)-JULDAY(mh_i,dy_i,yr_i))
	day2_diff = (JULDAY(mh_f,dy_f,yr_f)-JULDAY(mh_f,dy_2,yr_f))
	
	H1 = H_det[day1_diff*1440:((day1_diff+1)*1440)-1]
	H2 = H_det[(file_number-(day2_diff+1))*1440:((file_number-day2_diff)*1440)-1]
	print, N_ELEMENTS(H2)
	
	
	ndays = (JULDAY(mh_f,dy_2,yr_f)-JULDAY(mh_i,dy_1,yr_i))+1
	
	bsq = bsq_V2(H1, H2, [yr_i, mh_i, dy_1], [yr_f, mh_f, dy_2], ndays, station_idx, $
	MAKE_FILE="make_file")
	;print, N_ELEMENTS(H_det[10:7])
	;print, N_ELEMENTS(bsq.m), (file_number-day2_diff)*1440
	H_clean = H_det[day1_diff*1440:(file_number-day2_diff)*1440] - bsq.m

    X_label2 = xlabel([yr_i, mh_i, dy_1], ndays)
    WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE='H cleaned'
    PLOT, time, H_clean, XSTYLE=1, XRANGE=[day1_diff,(file_number-day2_diff)], $
    YRANGE=[MIN(H_clean, /NAN),MAX(H_clean,/NAN)], CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'H [nT]',XTITLE = time_name, /NODATA, XTICKS=ndays, $
    XTICKNAME=X_label2    
	
	OPLOT, time[day1_diff*1440:(file_number-day2_diff)*1440], H_clean, COLOR=0, THICK=2
	
	OPLOT, time[day1_diff*1440:(file_number-day2_diff)*1440], $
	H_det[day1_diff*1440:(file_number-day2_diff)*1440], COLOR=254
 ;   WHILE (!MOUSE.button NE 4) DO BEGIN  ; repeat printing H trend value until right mouse button is pressed
    ;  CURSOR, x, y, /DOWN, /DATA
    ;  PRINT, y    
    ;ENDWHILE 
    


IF KEYWORD_SET(make_file) THEN BEGIN    
    outfile = STRARR(ndays)    
;Generación de archivo en muestreo de horas 

;###############################################################################    
    H_hr = FINDGEN(N_ELEMENTS(H_clean)/60)
    FOR i=0, N_ELEMENTS(H_hr)-1 DO BEGIN
        H_hr[i] = MEDIAN(H_clean[i*60:(i+1)*60-1])
          
    ENDFOR  

    string_date     = STRARR(ndays)  
    ;PRINT, ndays, N_ELEMENTS(H_clean)/1440
    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_1, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        

        outfile[i] = set_var.Mega_dir+station+'/min/'+station_code+'_'+string_date[i]+'m.dat'    
        OPENW, LUN, outfile[i], /GET_LUN        
        PRINTF, LUN, H_clean[i*1440:(i+1)*1440-1], format='(F9.4)'
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR      

    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        string_year = 0
        tmp_julday  = JULDAY(mh_i, dy_1, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        
        str_year = STRMID(STRING(tmp_year, format='(I4)'),2,2)
        
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')                
        outfile[i] = set_var.Mega_dir+station+'/hourly/'+station_code+'_'+string_date[i]+'h'+'.dat'
        ;print, outfile[i]
        OPENW, LUN, outfile[i], /GET_LUN        
        PRINTF, LUN, H_hr[i*24:(i+1)*24-1], format='(F9.4)'
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR   

ENDIF
RETURN

END
