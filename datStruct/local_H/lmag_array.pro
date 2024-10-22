;
;Name:
;	H_array.pro
;purpose:
;	generación de vector H sin variación día a día
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
;   .r H_array
;   H = H_array(idate[yyyy,mm,dd], fdate[yyyy,mm,dd], 'station', n, resolution)
;
;parameters:
;   'station': set by set_var.gms[FIX(station_idx)],  where ;0:coeneo, 1:teoloyuca, 2:tucson, 
;                                                           3:bsl, 4:iturbide
;
;   'n': set by set_var.gms_code[FIX(station_idx)], where ;0:coeneo, 1:teoloyuca, 2:tucson, 
;                                                           3:bsl, 4:iturbide
;
;   resolution : 'H' for hourly data, 'min' for min resolution data 
;
;dependencies:
;   INTERMAGNET
;   REGMEX
;
;input files
;   geomagnetic field measurements from a certain observatory or geomagnetic station.
;
;output files:
;   H vector from n obs/station
;   
;    
;version
;   Dec, 2022
;   Feb, 2023
;   Jun, 2023
;	feb, 2024
;note
;   For following analysis, this routine has to be run to create clean H obs data
;


FUNCTION struct, date, station_code, resolution
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        @set_up_commons
        set_up	
        
        date = string(year, month, day, FORMAT = '(I4, I02, I02)')		
        str_year = STRING(year, FORMAT = '(I4)')	

           ;regmex		  0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
           ;intermagnet bmt:0, bou:1, brd:2, bsl:3, cki:4, cyg:5, gui:6, hbk:7, ipm:8, kak:9, $
    					;kdu:10, kmh:11, pil:12, sjg:13, tam:14, tdc:15, tuc:16
		gms_class = gms_class(station_code)
	
	extension = ''
	IF resolution EQ 'min' THEN extension = 'm.dat' ELSE extension = 'h.dat'
		   
       ; sts  = string(stats, format = '(A5)')
        dir = set_var.Mega_dir+'/'+gms_class+'/'+station_code+'/min/'
		file_name = dir+station_code+'_'+date+extension
;		print, file_name
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)

		data = STRARR(number_of_lines)

		OPENR, lun, file, /GET_LUN, ERROR=err
		READF, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	IF resolution EQ 'min' THEN BEGIN
        DStruct = {X : FLTARR(1440), Y : FLTARR(1440), H : FLTARR(1440), Z : FLTARR(1440)}
        struct = {comp : FLTARR(1440)}
        H_mag = REPLICATE(struct, 4) 

;		 = REPLICATE(DStruct, number_of_lines)	  
		READS, data, H_mag, FORMAT='(1440(F10))'	
	ENDIF ELSE BEGIN
        DStruct = {X : FLTARR(24), Y : FLTARR(24), H : FLTARR(24), Z : FLTARR(24)}
        struct = {comp : FLTARR(24)}
        H_mag = REPLICATE(struct, 4) 

;		 = REPLICATE(DStruct, number_of_lines)	  
		READS, data, H_mag, FORMAT='(24(F10))'	
	ENDELSE
	
		DStruct.X[*]    = H_mag[0].comp 
		DStruct.Y[*]    = H_mag[1].comp  
		DStruct.H[*]    = H_mag[2].comp
		DStruct.Z[*]	= H_mag[3].comp

		RETURN, DStruct		
END



FUNCTION lmag_array, date_i, date_f, station_code, resolution 
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
        @set_up_commons
        set_up

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
	
	;station_code    = set_var.gms_code[FIX(idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
;############################################################################### 
	gms_class = gms_class(station_code)
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
;###############################################################################
; define H variables

        string_date        = STRARR(file_number)               
        data_file_name  = STRARR(file_number)                                
        dir = set_var.Mega_dir+'/'+gms_class+'/'+station_code+'/min/'              
        
    IF resolution EQ 'min' THEN BEGIN        
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')       
                data_file_name[i]  = dir+station_code+'_'+string_date[i]+'m.dat'                          	                            
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(WHERE(exist_data_file EQ 1))
      
        
        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.magnetic XYZH ',A,' impossible to plot all data.')"              
        ENDIF

;###############################################################################
; Generate the time variables to plot H time series                     
        H    = FLTARR(file_number*1440)                               
        X    = FLTARR(file_number*1440) 
        Y    = FLTARR(file_number*1440)
        Z    = FLTARR(file_number*1440)                    
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_h = struct([tmp_year, tmp_month, tmp_day], station_code, resolution)
                        
                        H[i*1440:(i+1)*1440-1] = d_h.H[*]
                        X[i*1440:(i+1)*1440-1] = d_h.X[*]  
                        Y[i*1440:(i+1)*1440-1] = d_h.Y[*]        
                        Z[i*1440:(i+1)*1440-1] = d_h.Z[*]
                ENDIF ELSE BEGIN
                        H[i*1440:(i+1)*1440-1] = 99999.0
                        X[i*1440:(i+1)*1440-1] = 99999.0
                        Y[i*1440:(i+1)*1440-1] = 99999.0         
                        Z[i*1440:(i+1)*1440-1] = 99999.0                        
                ENDELSE                
        ENDFOR

    ENDIF


        
    IF resolution EQ 'H' THEN BEGIN        
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')       
                data_file_name[i]  = dir+station_code+'_'+string_date[i]+'h.dat'                          	                            
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(WHERE(exist_data_file EQ 1))      
        
        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.magnetic XYZH data.',A,' impossible to plot all data.')"              
        ENDIF

;###############################################################################
; Generate the time variables to plot H time series                     
        H    = FLTARR(file_number*24)                               
        X    = FLTARR(file_number*24) 
        Y    = FLTARR(file_number*24)
        Z    = FLTARR(file_number*24)                          
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_h = struct([tmp_year, tmp_month, tmp_day], station_code, resolution)
                        
                        H[i*24:(i+1)*24-1] = d_h.H[*]
                    	X[i*24:(i+1)*24-1] = d_h.X[*]
                        Y[i*24:(i+1)*24-1] = d_h.Y[*]
                        Z[i*24:(i+1)*24-1] = d_h.Z[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        H[i*24:(i+1)*24-1] = 99999.0
                        X[i*24:(i+1)*24-1] = 99999.0
                        Y[i*24:(i+1)*24-1] = 99999.0
                        Z[i*24:(i+1)*24-1] = 99999.0
                ENDELSE                
        ENDFOR       
    ENDIF
	
        mag_data = {H : FLTARR(N_ELEMENTS(H)), X : FLTARR(N_ELEMENTS(X)), Y : FLTARR(N_ELEMENTS(Y)), $
        			Z : FLTARR(N_ELEMENTS(Z))}
        			
        mag_data.H = H[*]
        mag_data.X = X[*]
        mag_data.Y = Y[*]
        mag_data.Z = Z[*]
        
	RETURN, mag_data
END
