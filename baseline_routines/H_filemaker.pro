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

		data_dir = set_var.Mega_dir+'intermagnet/'+str_year+'/'+STRUPCASE(station_code)+'/'
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
    print, station_code
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
       dir =  set_var.Mega_dir+'intermagnet/'+str_year+'/'+STRUPCASE(station_code)+'/'
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




PRO H_filemaker, date_i, date_f;, MAKE_FILE=make_file
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

    print, 'number of days in time window: ', file_number
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
	H = mag_array.H
	
    X = add_nan(X, 999999.0, 'equal')        
    X = add_nan(X, 99999.0, 'equal')
    ;X = add_nan(X, 2.76e4, 'greater')
	Y = add_nan(Y, 99999.0, 'equal')
	Y = add_nan(Y, 9999.0, 'equal')
    ;Y = add_nan(Y, 2800, 'greater')	
    Z = add_nan(Z, 999999.0, 'equal') 	
	Z = add_nan(Z, 99999.0, 'equal')
	Z = add_nan(Z, 9999.0, 'equal')	
	;Z = add_nan(Z, 3.04e4, 'greater')	
  ;  H = add_nan(H, 2.76e4, 'greater')


	X_det = day2day(X, 'X',[yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
	Y_det = day2day(Y, 'Y',[yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
	Z_det = day2day(Z, 'Z',[yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
	H_det = day2day(H, 'H',[yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
;STOP, 'end of the test'
;###############################################################################
;###############################################################################    
	time = FINDGEN(N_ELEMENTS(X_det))/1440.0  
;###############################################################################    
;###############################################################################    
	PRINT, "Press value for Z thershold (50 by default)"
	Z_Xthreshold = '100'
	;READ, Z_Xthreshold, PROMPT = '> '	
	Xspikes = whitaker_hayer(X_det, FIX(Z_Xthreshold), 'X')
;	X_det = fillnan(X_det)
;APPLY MOVING Average

;    x_diff = TS_DIFF(X_det,1)
;	xd_med = MEDIAN(x_diff)
;	MAD    = MEDIAN(ABS(x_diff-xd_med))
	
;	nfact  = 0.6745
;	z  	   = nfact*(x_diff-xd_med)/MAD
	
;	spikes = WHERE(abs(z) GT 50)
;	WINDOW, 4, XSIZE=1000, YSIZE=400, TITLE='X changing point1'
;	plot, time[5702:5730],  TS_DIFF(X_det[5702:5730], 1)
;	for i =0, N_ELEMENTS(spikes)-1 do begin
	
;		print, i, spikes[i],  X_det[spikes[i]]
;	endfor

;	X_det[5717:5725] = fix_offset(X_det[5717:5725])
 
;	WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE='X changing point'
;	plot, time[5702:5730],  X_det[5702:5730]
	
;	print, X_det[5702:5729]	
	;X_det = fillnan(X_det)
;###############################################################################
;###############################################################################     	
;############################################################################### 	
	;Z_Ythreshold = ''
	PRINT, "Press value for Z thershold (50 by default)"
	;Z_Ythreshold = '100'
	;READ, Z_Ythreshold, PROMPT = '> '
	;Yspikes = whitaker_hayer(Y_det, FIX(Z_Ythreshold), 'Y')    	
	;Y_det = fillnan(Y_det)
 ;###############################################################################   
 ;###############################################################################
	;Z_Zthreshold = ''
	;PRINT, "Press value for Z thershold (50 by default)"
	;Z_Zthreshold = '100'
	;READ, Z_Ythreshold, PROMPT = '> '
	;Zspikes = whitaker_hayer(Z_det, FIX(Z_Ythreshold), 'Y')    	
	;Z_det = fillnan(Z_det)
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
;###############################################################################    
	PRINT, "Press value for Z thershold (100 by default)"
	Z_Hthreshold = '100'
	;READ, Z_Xthreshold, PROMPT = '> '	
	Hspikes = whitaker_hayer(H_det, FIX(Z_Hthreshold), 'H')
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
       time = FINDGEN(N_ELEMENTS(X_det))/1440.0   
;###############################################################################   
    WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE='X component'
    PLOT, time, X_det, YRANGE=[MIN(X_det, /NAN),MAX(X_det,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
    background=255, color=0, CHARTHICK=2.0, YTITLE = 'X [nT]', XTITLE = time_name, $
    XTICKS=file_number, XTICKNAME=X_label, YSTYLE=1	
;###############################################################################    
    WINDOW, 1, XSIZE=1000, YSIZE=400, TITLE='Y component'
    PLOT, time, Y_det, YRANGE=[MIN(Y_det, /NAN),MAX(Y_det,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
    background=255, color=0, CHARTHICK=2.0, YTITLE = 'Y [nT]', XTITLE = time_name, $
    XTICKS=file_number, XTICKNAME=X_label, YSTYLE=1	    
;###############################################################################    
    WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE='H component'
    PLOT, time, H_det, YRANGE=[MIN(H_det, /NAN),MAX(H_det,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
    background=255, color=0, CHARTHICK=2.0, YTITLE = 'H [nT]', XTITLE = time_name, $
    XTICKS=file_number, XTICKNAME=X_label, YSTYLE=1
;###############################################################################    
    WINDOW, 1, XSIZE=1000, YSIZE=400, TITLE='Z component'
    PLOT, time, Z_det, YRANGE=[MIN(Z_det, /NAN),MAX(Z_det,/NAN)], XSTYLE=1, CHARSIZE = 1.8, $
    background=255, color=0, CHARTHICK=2.0, YTITLE = 'Z [nT]', XTITLE = time_name, $
    XTICKS=file_number, XTICKNAME=X_label, YSTYLE=1	    
;###############################################################################  

;###############################################################################
	QD = ['QLD1', 'QLD2', 'QLD3', 'QLD4', 'QLD5', 'QLD6', 'QLD7', 'QLD8', 'QLD9', 'QLD10']     
;###############################################################################
;Selection of Q days Based on IQR criteria    
	tenQD2_1 = sel_qdayv2([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f], station_code)  
	PRINT, "Ten local Qdays according to IQR criteria for "	
    FOR i=0, 9 DO BEGIN
       PRINT, STRING(tenQD2_1.year[i], tenQD2_1.month[i], tenQD2_1.day[i], QD[i], tenQD2_1.iqr[i],$
        ;PRINT, y[i], m[i], d[i], IQR_hr[i], $
        FORMAT = '(I4, "-", I02, "-", I02,  4X,A, F6.2)')                                 
    ENDFOR
;###############################################################################
;###############################################################################
	qday1 = ''
	mh_1  = ''
	PRINT, 'select the first QD: [dd, \n mm]'
	READ, qday1, mh_1, PROMPT = '> '
 
	qday2 = ''
	mh_2  = ''
	PRINT, 'select the second QD: [dd,\n mm]'
	READ, qday2, mh_2,PROMPT = '> '

	dy_1 	= FIX(qday1)	
	dy_2 	= FIX(qday2)
	mh_1    = FIX(mh_1)
	mh_2    = FIX(mh_2)
;###############################################################################
		day1_diff = (JULDAY(mh_1,dy_1,yr_i)-JULDAY(mh_i,dy_i,yr_i))
;###############################################################################
;###############################################################################
;###############################################################################
;testing Jesper daily baseline
	idx_ini = day1_diff*1440

;###############################################################################
;###############################################################################
;###############################################################################
;Bsq baseline

		X1 = X_det[day1_diff*1440:((day1_diff+1)*1440)-1]
		Y1 = Y_det[day1_diff*1440:((day1_diff+1)*1440)-1]
		Z1 = Z_det[day1_diff*1440:((day1_diff+1)*1440)-1]		
		H1 = H_det[day1_diff*1440:((day1_diff+1)*1440)-1]
	
	
	IF dy_f EQ dy_2 THEN BEGIN
		day2_diff = 0
	ENDIF ELSE BEGIN
		day2_diff = (JULDAY(mh_f,dy_f,yr_f)-JULDAY(mh_2,dy_2,yr_f))
	ENDELSE
;	PRINT, 
		ndays = (JULDAY(mh_2,dy_2,yr_f)-JULDAY(mh_1,dy_1,yr_i))+1
		
		X2 = X_det[(file_number-(day2_diff+1))*1440:((file_number-(day2_diff))*1440)-1]
		Y2 = Y_det[(file_number-(day2_diff+1))*1440:((file_number-(day2_diff))*1440)-1]
		H2 = H_det[(file_number-(day2_diff+1))*1440:((file_number-(day2_diff))*1440)-1]
		Z2 = Z_det[(file_number-(day2_diff+1))*1440:((file_number-(day2_diff))*1440)-1]
		
	;	bsq_X = bsq(X1, X2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
	;	bsq_Y = bsq(Y1, Y2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
	;	bsq_H = bsq(H1, H2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
	;	bsq_Z = bsq(Z1, Z2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
		
		bsq_X2 = bsq_v2(X1, X2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
		bsq_Y2 = bsq_v2(Y1, Y2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
		bsq_H2 = bsq_v2(H1, H2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
		bsq_Z2 = bsq_v2(Z1, Z2, [yr_i, mh_1, dy_1], [yr_f, mh_2, dy_2], ndays, station_code)
		
		PRINT, "QDL 1: "+ STRING(yr_i, mh_1, dy_1, FORMAT='(I4, "-", I02, "-", I02)') 
		PRINT, "QDL 2: "+ STRING(yr_i, mh_2, dy_2, FORMAT='(I4, "-", I02, "-", I02)') 

	
;	STOP,"detención momentanea"
	;PRINT, X_det[((file_number-day2_diff)*1440)-1], X_det[N_ELEMENTS(X_det)-1]
;	X_clean = X_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_X.m
;	Y_clean = Y_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_Y.m
;	H_clean = H_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_H.m
;	Z_clean = Z_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_Z.m
	
	X_clean2 = X_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_X2.m
	Y_clean2 = Y_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_Y2.m
	H_clean2 = H_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_H2.m
	Z_clean2 = Z_det[day1_diff*1440:((file_number-day2_diff)*1440)-1] - bsq_Z2.m

   ; H_det   = (SQRT((X_det)^2 + (Y_det)^2))*(-1) 
   ; print, H_det[day1_diff*1440:(file_number-day2_diff)*1440] - bsq_H.m
    X_label2 = xlabel([yr_i, mh_1, dy_1], ndays)
 
 	time2 = FINDGEN(ndays*1440.0)/1440.
 
   	WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE='H cleaned'
    PLOT, time2, H_clean2, XSTYLE=1,YSTYLE=2, XRANGE=[0,ndays], $
    YRANGE=[MIN(H_clean2, /NAN),MAX(H_clean2,/NAN)], CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'X [nT]',XTITLE = time_name, /NODATA, XTICKS=ndays, $
    XTICKNAME=X_label2    
	
	OPLOT, time2, H_clean2, COLOR=0, THICK=2
	;OPLOT, time2, H_clean2, COLOR=100, THICK=1
	;print, H_clean
	OPLOT, time2, $
	H_det[day1_diff*1440:((file_number-day2_diff)*1440)-1], COLOR=254
 ;	OPLOT, time2, bsq_H.m, COLOR=100, THICK=1
 ;PRINT, N_ELEMENTS(H_clean), N_ELEMENTS(H_det[day1_diff*1440:(file_number-day2_diff)*1440])
 ;   WHILE (!MOUSE.button NE 4) DO BEGIN  ; repeat printing H trend value until right mouse button is pressed
    ;  CURSOR, x, y, /DOWN, /DATA
    ;  PRINT, y    
    ;ENDWHILE 

  	WINDOW, 6, XSIZE=1000, YSIZE=400, TITLE='H component'
    PLOT, time2, H_clean2, XSTYLE=1,YSTYLE=2, XRANGE=[0,ndays], $
    YRANGE=[MIN(H_clean2, /NAN),MAX(H_clean2,/NAN)], CHARSIZE = 1.8, background=255, color=0, $
    CHARTHICK=2.0, YTITLE = 'X [nT]',XTITLE = time_name, /NODATA, XTICKS=ndays, $
    XTICKNAME=X_label2    
	

	OPLOT, time2, H_clean2, COLOR=0, THICK=2
	;OPLOT, time[day1_diff*1440:(file_number-day2_diff)*1440], H_clean2, COLOR=100, THICK=1
	;print, H_clean
	
;STOP, 'detención momentanea'

    PRINT, "Press y if wanna create magnetic data daily file from time window. Else, press any other key"
    decision = ''
    READ, decision, PROMPT = '> '

IF decision EQ 'y' THEN BEGIN    
;Generación de archivo en muestreo de horas 

;###############################################################################    
    H_hr = FINDGEN(N_ELEMENTS(H_clean2)/60)
    FOR i=0, N_ELEMENTS(H_hr)-1 DO BEGIN
        H_hr[i] = MEDIAN(H_clean2[i*60:(i+1)*60-1])          
    ENDFOR
    
    X_hr = FINDGEN(N_ELEMENTS(X_clean2)/60)
    FOR i=0, N_ELEMENTS(X_hr)-1 DO BEGIN
        X_hr[i] = MEDIAN(X_clean2[i*60:(i+1)*60-1])          
    ENDFOR  
    
    Y_hr = FINDGEN(N_ELEMENTS(Y_clean2)/60)
    FOR i=0, N_ELEMENTS(Y_hr)-1 DO BEGIN
        Y_hr[i] = MEDIAN(Y_clean2[i*60:(i+1)*60-1])          
    ENDFOR     
    
    Z_hr = FINDGEN(N_ELEMENTS(Z_clean2)/60)
    FOR i=0, N_ELEMENTS(Z_hr)-1 DO BEGIN
        Z_hr[i] = MEDIAN(Z_clean2[i*60:(i+1)*60-1])          
    ENDFOR   
    
	gms_group = gms_class(station_code)
	
	dir = set_var.Mega_dir+gms_group+'/'+station_code+'/min/'
	test = FILE_TEST(dir, /DIRECTORY) 
	IF test EQ 0 THEN BEGIN
		FILE_MKDIR, dir
		PRINT, 'PATH directory '+dir
		PRINT, 'created'
	ENDIF ELSE BEGIN
		PRINT, ''
	ENDELSE
	outfile = STRARR(ndays) 
	string_date = STRARR(ndays)             
    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_1, dy_1, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        

        outfile[i] = dir+station_code+'_'+string_date[i]+'m.dat'    
        OPENW, LUN, outfile[i], /GET_LUN        
        PRINTF, LUN, X_clean2[i*1440:(i+1)*1440-1], FORMAT='(1440(F10.4))'
        PRINTF, LUN, Y_clean2[i*1440:(i+1)*1440-1], FORMAT='(1440(F10.4))'
        PRINTF, LUN, H_clean2[i*1440:(i+1)*1440-1], FORMAT='(1440(F10.4))'
        PRINTF, LUN, Z_clean2[i*1440:(i+1)*1440-1], FORMAT='(1440(F10.4))'
        			 
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR      


    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        string_year = 0
        tmp_julday  = JULDAY(mh_1, dy_1, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        
        str_year = STRMID(STRING(tmp_year, format='(I4)'),2,2)
        
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')                
        outfile[i] = dir+station_code+'_'+string_date[i]+'h'+'.dat'
       ; print, outfile[i]
        OPENW, LUN, outfile[i], /GET_LUN        
        PRINTF, LUN, X_hr[i*24:(i+1)*24-1], FORMAT='(1440(F10.4))'
		PRINTF, LUN, Y_hr[i*24:(i+1)*24-1], FORMAT='(1440(F10.4))' 
        PRINTF, LUN, H_hr[i*24:(i+1)*24-1], FORMAT='(1440(F10.4))'
        PRINTF, LUN, Z_hr[i*24:(i+1)*24-1], FORMAT='(1440(F10.4))'
        			 
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR   

ENDIF ELSE BEGIN 
	PRINT, "no file created"
ENDELSE	

    PRINT, "Press y if wanna create magnetic data daily file from time window. Else, press any other key"
    decision2 = ''
    READ, decision2, PROMPT = '> '

	IF decision2 EQ 'y' THEN BEGIN

		dyi = ''
		mhi  = ''
		PRINT, 'select the first QD: [dd, \n mm]'
		READ, dyi, mhi, PROMPT = '> '
	 
		dyf = ''
		mhf  = ''
		PRINT, 'select the second QD: [dd,\n mm]'
		READ, dyf, mhf,PROMPT = '> '

		dy_1 	= FIX(qday1)	
		dy_2 	= FIX(qday2)
		mh_1    = FIX(mh_1)
		mh_2    = FIX(mh_2)
		
		FFT_output, [yr_i, mhi, dyi], [yr_f, mhf, dyf], station_code, ps='ps'
	ENDIF	
RETURN

END
