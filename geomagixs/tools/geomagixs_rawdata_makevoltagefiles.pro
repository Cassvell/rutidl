;+
; NAME:
;       geomagixs_rawdata_makevoltagefiles.pro
;
;
; PURPOSE:
;
;       makes out voltage, magnetic and temperature data files 1-second resolution from raw data.
;
; AUTHOR:
;
;       Pedro Corona Romero
;       Insituto de Geofisica, Unidad Michoacan
;       UNAM, Campus Morelia
;       Antigua Carretera a Patzcuaro, Morelia, Michoacan
;       piter.cr@gmail.com
;       Mexico, 26.X.mmxxii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;
;       geomagixs_rawdata_makedatafiles, initial_date, final_date [, STATION=GMS_name, OFFSET=offset /QUIET, /FORCE_ALL]
;
;       Description:
;       update magnetic data
;
;
; PARAMETERS:
;       initial_date                                   : [YYYY, MM, DD] , initial date and time at which the data is read from, array of integers
;       final_date                                     : [YYYY, MM, DD] , final date and time at which the data is read from, array of integers
;
; KEYWORD PARAMETERS:
;
;       STATION                                        : a string with the geomagnetic station (GMS) name where the data is taken from
;       OFFSET                                         : an 3-float array with offsets for D, H, and Z
;       QUIET                                          : sets messages from the program off
;       FORCE:ALL                                      : force to generate the *.dat files despite there are not the original data-files.
;                                                        for the case of abset data, the resulting *.dat will be filled with data-gaps.
;
; DEPENDENCIES:
;       GEOMAGIXS system                               : software and data structure
;
; INPUT FILES:
;       ./data_source/raw_data/DDmmm/GMSDOYHH.YYs  [ray data file]
;
; OUTPUT FILES:
;       GMSDDMMM.YY[v/m].hres                      [REGMEX data files format in high temporal resolution]
;
; HISTORIA:
;               26/10/2022      Start development from geomagixs_magneticdata_prepare v1.0
;
;-

;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; AUXILIAR FUNCTION
FUNCTION getting_month_name, initial

        On_error, 2
        COMPILE_OPT idl2, hidden

        year   = initial[0]
        month  = initial[1]
        day    = initial[2]

        CASE 1 OF
                month EQ 1  : tmp_string = 'jan'
                month EQ 2  : tmp_string = 'feb'
                month EQ 3  : tmp_string = 'mar'
                month EQ 4  : tmp_string = 'apr'
                month EQ 5  : tmp_string = 'may'
                month EQ 6  : tmp_string = 'jun'
                month EQ 7  : tmp_string = 'jul'
                month EQ 8  : tmp_string = 'aug'
                month EQ 9  : tmp_string = 'sep'
                month EQ 10 : tmp_string = 'oct'
                month EQ 11 : tmp_string = 'nov'
                month EQ 12 : tmp_string = 'dec'
                ELSE: MESSAGE, 'Critial error'
        ENDCASE


        RETURN, tmp_string
END




;##############################################################################
;##############################################################################
;##############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; AUXILIAR FUNCTION
FUNCTION getting_rawdata_day, initial, STATION=station, $
                                        QUIET=quiet
        On_error, 2
        COMPILE_OPT idl2, hidden
;##############################################################################
; initialize directories
;##############################################################################
        @geomagixs_commons
        

        ;geomagixs_check_gms, STATION=station, QUIET=quiet

        IF gms[system.gms].name EQ 'planetary' OR gms[system.gms].name EQ 'teoloyucan' THEN RETURN, 0
        
;##############################################################################
; initializing dates and hours
;##############################################################################
        year   = initial[0]
        month  = initial[1]
        day    = initial[2]
        doy    = JULDAY(month,day,year)-JULDAY(1,0,year)
        hours_number = 24
        minutes_per_hour   = 60
        seconds_per_minute = 60
        seconds_per_day    = seconds_per_minute*minutes_per_hour*hours_number
        ;file_name    = STRARR(hours_number)
        hours_of_day       = INDGEN(hours_number)
        ;secons_in_a_day    = LINDGEN(seconds_per_day)

        tmp_string = getting_month_name(initial)

;        CASE 1 OF
;                month EQ 1  : tmp_string = 'jan'
;                month EQ 2  : tmp_string = 'feb'
;                month EQ 3  : tmp_string = 'mar'
;                month EQ 4  : tmp_string = 'apr'
;                month EQ 5  : tmp_string = 'may'
;                month EQ 6  : tmp_string = 'jun'
;                month EQ 7  : tmp_string = 'jul'
;                month EQ 8  : tmp_string = 'aug'
;                month EQ 9  : tmp_string = 'sep'
;                month EQ 10 : tmp_string = 'oct'
;                month EQ 11 : tmp_string = 'nov'
;                month EQ 12 : tmp_string = 'dec'
;                ELSE: MESSAGE, 'Critial error'
;        ENDCASE

        
        
        file_name = gms[system.gms].code+string(doy, FORMAT='(I03)')+string(hours_of_day[*], FORMAT='(I02)')+'.'+ string(year MOD 1000, FORMAT='(I02)') +'s'
        dir_name  = string(day,FORMAT='(I02)')+tmp_string


;##############################################################################
; reading data files
;##############################################################################
        exist_files = FILE_TEST( system.datasource_dir+'/raw_data/'+gms[system.gms].name+'/'+dir_name+'/'+file_name, /READ)
        verified_files = WHERE (exist_files EQ 1, located_files)
        
        IF located_files NE N_ELEMENTS(exist_files) THEN $
                IF not keyword_set(quiet) THEN BEGIN
                                PRINT, N_ELEMENTS(exist_files)-located_files, FORMAT='("        WARNING: A total of ",I0," hours of rawdata are missing.")'
                                PRINT, '        Missing data are going to be filled with data gaps.'
                                print, ''
                ENDIF
        
        files = FILE_SEARCH(system.datasource_dir+'/raw_data/'+gms[system.gms].name+'/'+dir_name+'/'+file_name[verified_files], COUNT=located_files)

        header_lines    = 5
;print, FILE_LINES(files[*])
;print, TOTAL(FILE_LINES(files[*])), 24*FILE_LINES(files[0])
        number_of_lines = LONG(TOTAL(FILE_LINES(files[*]))-located_files*header_lines)
        raw_data        = STRARR(number_of_lines)
;print,  located_files,number_of_lines, number_of_lines/located_files, seconds_per_day, FORMAT='("found files: ",I0,  " - total of lines: ", I0, " - lines per file: ", I0, " / ", I0)'
        ;number_of_lines
        last_line = 0

        IF not keyword_set(quiet) THEN  print, '        Reading raw data from '+STRUPCASE(gms[system.gms].code)+' for the date '+ string(year, month, day, FORMAT='(I4,"/",I02,"/",I02)') +'.'

        ;temporal_data   = STRARR(total(number_of_lines))
;print, number_of_lines

tot_tmp = 0
        FOR i = 0, N_ELEMENTS(files)-1 DO BEGIN
                lines_number = FILE_LINES(files[i])
                tot_tmp += lines_number
                ;print, lines_number, tot_tmp, number_of_lines, located_files*header_lines
                tmp_data     = STRARR(lines_number)
                
                IF lines_number GT 5 THEN BEGIN
                        OPENR, lun, files[i], /GET_LUN, ERROR=err
                                READF, lun, tmp_data, FORMAT='(A)'
                        CLOSE, lun
                        FREE_LUN, lun
                        
;print, i, last_line, last_line+(lines_number-5), number_of_lines
                        raw_data [last_line:last_line+(lines_number-5)-1] = tmp_data[5:lines_number-1]
;print, last_line, tmp_data[5], ' *** ', last_line+(lines_number-5)-1, tmp_data[lines_number-1], lines_number-5
                ENDIF
                last_line += (lines_number-5)
        ENDFOR


;##############################################################################
; extracting data
;##############################################################################
        DataStruct  =  { HH : 0L, MM : 0L, SS : 0L, $
                         CH_H : 0L, CH_D : 0L, CH_Z : 0L, CH_Tc : 0L, CH_Ts : 0L }
                         

        line_lenght = STRLEN(raw_data[*])
        bad_lines   = WHERE( line_lenght NE 53, bad_lines_number )
        ;print, N_ELEMENTS(raw_data), bad_lines_number
        IF bad_lines_number GT 0 THEN BEGIN
                good_lines   = WHERE( line_lenght EQ 53, number_of_lines )
                tmp_data     = raw_data[good_lines]
                raw_data     = tmp_data
        ENDIF

        resulting_data = REPLICATE(DataStruct, number_of_lines)

;print, N_ELEMENTS(raw_data), N_ELEMENTS(resulting_data[*].HH)

        READS, raw_data[*], resulting_data, $
               FORMAT='(I2,X,I2,X,I2,X,I8,X,I8,X,I8,X,I8,X,I8)'
        tempvar = SIZE(TEMPORARY(raw_data)) ; liberar memoria de la info no usada
;I2XI2XI2XI8XI8XI8XI8XI8
;00 00 00 -5318374 -1481143 +2023158 +0900269 +0916536

        ;print, secons_in_a_day/(seconds_per_minute*minutes_per_hour)
        ;print, secons_in_a_day/(seconds_per_minute) MOD 60
;        print, secons_in_a_day MOD 60

;print, TOTAL(FILE_LINES(files[*]))
;help, resulting_data[*].HH
;help, secons_in_a_day[*]
        ;resulting_data[*].HH = secons_in_a_day[*]/(seconds_per_minute*minutes_per_hour)
        ;resulting_data[*].MM = secons_in_a_day[*]/(seconds_per_minute) MOD 60
        ;resulting_data[*].SS = secons_in_a_day[*] MOD 60


        IF not keyword_set(quiet) THEN BEGIN
                print, '        - Raw data succesfuly extracted.'
                print, ''
        ENDIF

        ;tempvar = SIZE(TEMPORARY(resulting_data)) ; liberar memoria de la info no usada

        RETURN, resulting_data


END




;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
; AUXILIAR FUNCTION
PRO making_voltage_file, file_date, STATION=station, $
                                    QUIET=quiet

;+
; NAME:
;       making_voltage_file
;
;
; PURPOSE:
;
;       to make high resolution voltage files for a given day from raw data.
;
; AUTHOR:
;
;       Pedro Corona Romero
;       Insituto de Geofisica, Unidad Michoacan
;       UNAM, Campus Morelia
;       Antigua Carretera a Patzcuaro, Morelia, Michoacan
;       piter.cr@gmail.com
;       Mexico, 8.xii.mmxxii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;
;       making_voltage_file, file_date, [, STATION=station, /QUIET])
;
;       Description:
;       retrives high resolution row data, from a given GMS, process and compute it
;       to produce the corresponding high-resolution voltage file, which is located
;       at the GMS data input directory.
;
;
; PARAMETERS:
;       file_date                                      : [YYYY, MM, DD] , given day for which the voltage file is going to be produced.
;
; KEYWORD PARAMETERS:
;
;       STATION                                        : named variable with the name of the geomagnetic station at which raw data is taken from
;       /QUIET                                         : sets messages off
;
; DEPENDENCIES:
;       GEOMAGIXS system
;
; ANALYZED FILES:
;       gmsDOYhh.YYs                                   [gms - GeoMagnetic Station; DOY - day of the year; hh - hour of the day; YY last two digits of the year]
;
; OUTPUR FILES:
;       gmsddMMM.YYv.hres                              [gms - GeoMagnetic Station; dd - day of the month; MMM - current month; YY last two digits of the year]
;
; HISTORIA:
;               08/12/2022      first working version v1.0
;                
;-



        On_error, 2
        COMPILE_OPT idl2
;##############################################################################
; initialize directories
;##############################################################################
        @geomagixs_commons
        

;##############################################################################
; depuring inputs
;##############################################################################
        ;IF (resolution EQ 1 AND kmex_1min_file_number LT 1) THEN MESSAGE, 'No data files available!'


;##############################################################################
; initializing dates and hours
;##############################################################################
        initial_year   = file_date[0]
        initial_month  = file_date[1]
        initial_day    = file_date[2]


;##############################################################################
; reading data files
;##############################################################################
        file_name = ''
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        ;hours_of_day = INDGEN(hours_number)

        doy          = JULDAY(initial_month,initial_day, initial_year)-JULDAY(1,0,initial_year)


        raw_data           = getting_rawdata_day(file_date, STATION=station, QUIET=quiet)
        number_of_raw_data = N_ELEMENTS(raw_data[*].HH)

        file_header  = STRARR(4)

        file_header[0] = STRUPCASE(gms[system.gms].name)+' <'+STRING(doy, FORMAT='(I03)')+'> 1 Second. Reported data'
        file_header[1] = ''
        file_header[2] = 'DD MM YYYY HH MM SS   H(mv)     D(mv)     Z(mv)     Tc(mv)    Ts(mv)'
        file_header[3] = ''
        number_of_header   = N_ELEMENTS(file_header)

        tmp_string   = getting_month_name(file_date)
        ;file_name   = gms[system.gms].code+string(doy, FORMAT='(I03)')+string(hours_of_day[*], FORMAT='(I02)')+'.'+ string(tmp_year MOD 1000, FORMAT='(I02)') +'s'
        file_name    = gms[system.gms].code+string(initial_day, FORMAT='(I02)')+tmp_string+'.'+ string(initial_year MOD 1000, FORMAT='(I02)') +'v.hres'
        file_txt     = STRARR(number_of_raw_data+number_of_header)

        file_txt[0:number_of_header-1] = file_header[*]


        ;file_exists       = FILE_TEST( system.datasource_dir+'/raw_data/'+gms[system.gms].name+'/'+dir_name+'/'+file_name, /READ)
        ;index_file_exists = WHERE( file_exists EQ 1, number_of_existing_files)
        
        voltage_H          = FLTARR(number_of_raw_data)
        voltage_D          = FLTARR(number_of_raw_data)
        voltage_Z          = FLTARR(number_of_raw_data)
        voltage_Tc         = FLTARR(number_of_raw_data)
        voltage_Ts         = FLTARR(number_of_raw_data)
        
        voltage_Tc[*] = raw_data[*].CH_Tc*gms[system.gms].highresolution_constants[0]+gms[system.gms].highresolution_constants[1]+gms[system.gms].temperature_offset[0]

        voltage_Ts[*] = raw_data[*].CH_Ts*gms[system.gms].highresolution_constants[0]+gms[system.gms].highresolution_constants[1]+gms[system.gms].temperature_offset[1]
        
        voltage_H[*]  = raw_data[*].CH_H*gms[system.gms].highresolution_constants[0]+gms[system.gms].highresolution_constants[1] + $
                        (gms[system.gms].temperature_constants[0] - voltage_Tc[*])*gms[system.gms].temperature_constants[1]+ $
                        gms[system.gms].scale_offset[0]

        voltage_D[*]  = raw_data[*].CH_D*gms[system.gms].highresolution_constants[0]+gms[system.gms].highresolution_constants[1] + $
                        gms[system.gms].scale_offset[1]
        
        
        voltage_Z[*]  = raw_data[*].CH_Z*gms[system.gms].highresolution_constants[0]+gms[system.gms].highresolution_constants[1] + $
                        (gms[system.gms].temperature_constants[0] - voltage_Tc[*])*gms[system.gms].temperature_constants[2]+ $
                        gms[system.gms].scale_offset[2]

        FOR i = 0, number_of_raw_data-1 DO $
                file_txt[number_of_header+i] = STRING(initial_day, initial_month, initial_year, FORMAT='(I02,X,I02,X,I4)') + $
                                               STRING(raw_data[i].HH, raw_data[i].MM, raw_data[i].SS, FORMAT='(X,I02,X,I02,X,I02)') + $
                                               STRING(voltage_H[i], voltage_D[i], voltage_Z[i], voltage_Tc[i], voltage_Ts[i], FORMAT='(5(X,F+09.3))')
        

        ;voltagefile_txt[number_of_header:number_of_raw_data+number_of_header-1]    = $
        ;        STRING(initial_day, initial_month, initial_year, raw_data[*].HH, raw_data[*].MM, raw_data[*].SS, FORMAT='(I02,X,I02,X,I04,X,I02,X,I02,X,I02, $)') + $
        ;        STRING(voltage_H[*],voltage_D[*],voltage_Z[*],voltage_Tc[*],voltage_Ts[*], FORMAT='(5(X,F+9.3))')


        ;good_rawdata = WHERE( voltage_H[*] GT 0, number_of_good_rawdata)
        ;bad_rawdata  = WHERE( voltage_H[*] LE 0, number_of_bad_rawdata)
        
        ;print, number_of_good_rawdata, 60*60*24, 100.*(number_of_good_rawdata)/(60*60*24)
        ;print, number_of_bad_rawdata, 60*60*24, 100.*(number_of_bad_rawdata)/(60*60*24)
        ;print, number_of_bad_rawdata+number_of_good_rawdata, 60*60*24, 100.*((number_of_good_rawdata+number_of_bad_rawdata))/(60*60*24)
        
        ;IF number_of_good_rawdata LT 60*60*24 AND NOT keyword_set(quiet) THEN BEGIN
        ;        IF NOT keyword_set(quiet) THEN BEGIN
        ;                PRINT, 100.*((number_of_good_rawdata+number_of_bad_rawdata))/(60*60*24), FORMAT='("Input Warning: The available daily Raw-Data is ",F5.1,"%.")'
        ;                PRINT, 100.*(number_of_good_rawdata)/(60*60*24), FORMAT='("              ", F5.1, "% of raw data is capable to be processed")'
        ;                PRINT, 100.*(number_of_bad_rawdata)/(60*60*24), FORMAT='("               and the remaining ", F5.1, "% will be taken as data gaps.")'
        ;        ENDIF
        ;        error.value[3] += 1
        ;        error.log      += 'Raw-data is incomplete for '+string(initial_year, initial_month, initial_day, FORMAT='(I4,"/", I02, "/",I02)')+' date. '
        ;ENDIF 
        
        ;print, voltage_H[bad_rawdata]
        
        ;IF number_of_good_rawdata GE 1 THEN $
        ;        file_txt[number_of_header+good_rawdata] = STRING(initial_day, initial_month, initial_year, FORMAT='(I02,X,I02,X,I4,X)') + $
        ;                                                  STRING(resulting_data[good_rawdata].HH, resulting_data[good_rawdata].MM, resulting_data[good_rawdata].SS, FORMAT='(I02,X,I02,X,I02,X)') + $
        ;                                                  STRING(voltage_H[*], voltage_D[*], voltage_Z[*], voltage_Tc[*], voltage_Ts[*], FORMAT='(5(X,F+9.3))')

        ;IF number_of_bad_rawdata GE 1 THEN $
        ;        file_txt[number_of_header+good_rawdata] = STRING(initial_day, initial_month, initial_year, FORMAT='(I02,X,I02,X,I4,X)') + $
        ;                                                  STRING(resulting_data[good_rawdata].HH, resulting_data[good_rawdata].MM, resulting_data[good_rawdata].SS, FORMAT='(I02,X,I02,X,I02,X)') + $
        ;                                                  STRING(9999.999, 9999.999, 9999.999, 9999.999, 9999.999, FORMAT='(5(X,F+9.3))')


        
        ;FOR i=0ll, number_of_raw_data-1 DO BEGIN
        ;        ;print, i mod 60, i/60
        ;        IF good_rawdata[i] GT 0 THEN BEGIN
        ;                file_txt[i+number_of_header] = initial_day, initial_month, initial_year, $
        ;                                               resulting_data[i].HH, resulting_data[i].MM, resulting_data[i].SS, $
        ;                                               
        ;                                               99.9999, 99999.9, 99999.9, 99.9999, 99999.9, $
        ;                                               FORMAT='(I02,X,I02,X,I4,X,I02,X,I02,X,I02, F7.4, 2(X, F7.1), F7.4, F7.1)')
        ;                                ;05.2820 27300.2 29562.8 47.2785 40240.0
        ;                                ;99.9999 99999.9 99999.9 99.9999 99999.9
        ;        ENDIF ELSE BEGIN
        ;        
        ;        ENDELSE
        ;        IF file_exists THEN $
        ;                FOR j=j_inicio, number_of_lines[0]-1 DO BEGIN
        ;                        IF STRMID(tmp_data[j], 11,5) EQ STRMID(final_file[i], 11,5) THEN BEGIN
        ;                               string_tmp=tmp_data[j]
        ;                                final_file[i]=string_tmp
        ;                                j_inicio=j+1
        ;                                ;print, i, j_inicio, (j_inicio-i), ' -- ', final_file[i]
        ;                                break
        ;                        ENDIF
        ;                ENDFOR
        ;ENDFOR








        ;IF number_of_existing_files GE 1 THEN BEGIN
        ;        IF not keyword_set(quiet) THEN  print, '        Extracting data from: '+gms[system.gms].code+string(doy, FORMAT='(I03)')+'??.'+ string(tmp_year MOD 1000, FORMAT='(I02)') +'s files'
        ;        file = FILE_SEARCH(system.datasource_dir+gms[system.gms].name+'/'+file_name, COUNT=opened_files)
        ;                IF opened_files EQ 0 AND not keyword_set(quiet) THEN MESSAGE, 'Error finding data files or directories!'

        ;temporal_data   = STRARR(total(number_of_lines))
;print, number_of_lines
        ;        raw_data = getting_rawdata( [tmp_year, tmp_month, tmp_day], STATION=station, QUIET=quiet )
        ;ENDIF ELSE BEGIN
        ;        IF not keyword_set(quiet) THEN PRINT, '        No RAW-data file found!'
        ;ENDELSE



;##############################################################################
; creating data file
;##############################################################################
        ;output_datafile = station_code+'_'+string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')+'.dat'
        ;output_datafile = gms[system.gms].code+'_'+string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')+'.dat'
        output_path     = system.datasource_dir+gms[system.gms].name+'/'
        
        exist_dir = FILE_TEST(output_path, /DIRECTORY)
        
        IF not(exist_dir) THEN BEGIN
                IF NOT keyword_set(quiet) THEN BEGIN
                        PRINT, FORMAT="('Critical Error: Missing system directory ', A,'. ')", output_path
                        PRINT, FORMAT="('                Check out the directory tree.')"
                ENDIF
                error.value[0] += 1
                error.log      += 'System directory '+output_path+' not found. Impossible to continue. '
                RETURN
        ENDIF 
        ;print, output_path+output_datafile
        OPENW, lun, output_path+file_name, /GET_LUN, ERROR=err
                IF err EQ 0 THEN FOR i=0ll, number_of_raw_data+number_of_header-1 DO PRINTF, lun, file_txt[i] $
                ELSE BEGIN
                        IF NOT keyword_set(quiet) OR keyword_set(force_all) THEN BEGIN
                                PRINT, FORMAT="('Critical Error: Missing directories or permissions conflict.')"
                                PRINT, FORMAT="('                Impossible to save output file: ',A,'.')", file_name
                                PRINT, ''
                        ENDIF
                        error.value[1] += 1
                        error.log      += 'Impossible to save output file '+file_name+'. '
                ENDELSE
        CLOSE, lun
        FREE_LUN, lun

        IF not keyword_set(quiet) THEN BEGIN
                print, '        - Saving: '+file_name
                print, ''
        ENDIF


return
END






















;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
; MAIN SEQUENCE


PRO geomagixs_rawdata_makevoltagefiles, initial, final, STATION=station, $
                                              QUIET=quiet, $
                                              FORCE_ALL=force_all

        On_error, 2
        COMPILE_OPT idl2, HIDDEN
;##############################################################################
; initialize directories
;##############################################################################
        @geomagixs_commons
        geomagixs_setup_commons, QUIET=quiet
        geomagixs_check_system, QUIET=quiet

;##############################################################################
; depuring inputs
;##############################################################################
        geomagixs_check_gms, STATION=station, QUIET=quiet

        IF gms[system.gms].name EQ 'planetary' OR gms[system.gms].name EQ 'teoloyucan' THEN BEGIN
                IF NOT keyword_set(quiet) THEN BEGIN
                        PRINT, FORMAT="('CRITICAL ERROR: error in selection of GMS.')"
                        PRINT, FORMAT="('                RAW-data files are not available for ', A, '.')", STRUPCASE(gms[system.gms].code)
                ENDIF
                error.value[2] += 1
                error.log      += 'Raw-data files are not available for '+STRUPCASE(gms[system.gms].code)+'. Impossible to proceed. '
                RETURN
        ENDIF



        CASE 1 OF
                N_PARAMS() EQ 0 : BEGIN
                                        initial = system.today_date
                                        final   = initial
                                  END
                N_PARAMS() EQ 1 : final = initial
                N_PARAMS() EQ 2 : geomagixs_check_dates, initial, final, STATION=station, QUIET=quiet
                ELSE            : BEGIN
                                        IF NOT keyword_set(quiet) THEN BEGIN
                                                PRINT, FORMAT="('CRITICAL ERROR: conflict with input date(s), data inconsistent or invalid.')"
                                                PRINT, FORMAT="('                impossible to proceed with an ambiguoss range of dates.')"
                                        ENDIF
                                        error.value[2] += 1
                                        error.log      += 'Ambiguous range of dates, it is required only two input dates to define a time period. '
                                        RETURN
                                  END
        ENDCASE

;print, offset

;print, offset
;##############################################################################
; initializing dates and hours
;##############################################################################
        initial_year   = initial[0]
        initial_month  = initial[1]
        initial_day    = initial[2]

        final_year     = final[0]
        final_month    = final[1]
        final_day      = final[2]




;##############################################################################
; reading data files
;##############################################################################
        number_of_days          = ((JULDAY(final_month, final_day, final_year) - JULDAY(initial_month, initial_day, initial_year))+1)
        magnetic_data_file_name = strarr(number_of_days)
        voltage_data_file_name  = strarr(number_of_days)
        string_date             = strarr(number_of_days)
        ;dir_name                = strarr(number_of_days)
        
        number_of_rawdata_files = 24*number_of_days
        raw_file_name           = strarr(number_of_rawdata_files)
        hours_of_the_day        = indgen(24)
        exist_rawdata_file      = INTARR(24*number_of_days)
        ;exist_file     = intarr(number_of_days)
        
        FOR i=0ll, number_of_days-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(initial_month, initial_day, initial_year)
                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                doy    = JULDAY(tmp_month,tmp_day,tmp_year)-JULDAY(1,0,tmp_year)

                ;CASE 1 OF
                ;        tmp_month EQ 1  : tmp_string = 'jan'
                ;        tmp_month EQ 2  : tmp_string = 'feb'
                ;        tmp_month EQ 3  : tmp_string = 'mar'
                ;        tmp_month EQ 4  : tmp_string = 'apr'
                ;        tmp_month EQ 5  : tmp_string = 'may'
                ;        tmp_month EQ 6  : tmp_string = 'jun'
                ;        tmp_month EQ 7  : tmp_string = 'jul'
                ;        tmp_month EQ 8  : tmp_string = 'aug'
                ;        tmp_month EQ 9  : tmp_string = 'sep'
                ;        tmp_month EQ 10 : tmp_string = 'oct'
                ;        tmp_month EQ 11 : tmp_string = 'nov'
                ;        tmp_month EQ 12 : tmp_string = 'dec'
                ;        ELSE: MESSAGE, 'Critial error'
                ;ENDCASE

                tmp_string = getting_month_name(initial)


                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                ;dir_name[i]       = string(tmp_day, FORMAT='(I02)')+tmp_string
                
                        ;data_file_name[i] = station_code+string_date[i]+'rmin.min'
                        ;processed_file_name[i] = station_code+'_'+string_date[i]+'.dat'
                ;magnetic_data_file_name[i] = gms[system.gms].code+string(tmp_day, FORMAT='(I02)')+tmp_string+string(tmp_year MOD 1000, FORMAT="('.',I02,'m.hres')")
                voltage_data_file_name[i]  = gms[system.gms].code+string(tmp_day, FORMAT='(I02)')+tmp_string+string(tmp_year MOD 1000, FORMAT="('.',I02,'v.hres')")
                raw_file_name[i:i+23]      = gms[system.gms].code+string(doy, FORMAT='(I03)')+string(hours_of_the_day[*], FORMAT='(I02)')+'.'+ string(tmp_year MOD 1000, FORMAT='(I02)') +'s'
                ;print, magnetic_data_file_name[i],' ',  voltage_data_file_name[i]
                ;print, i+1, '  ', raw_file_name[i:i+23];, '  ', processed_file_name[i]
        ENDFOR

        ;file_name = gms[system.gms].code+string(doy, FORMAT='(I03)')+string(hours_of_day[*], FORMAT='(I02)')+'.'+ string(year MOD 1000, FORMAT='(I02)') +'s'
        ;dir_name  = string(day,FORMAT='(I02)')+tmp_string

        exist_voltagedata_file = FILE_TEST(system.datasource_dir+gms[system.gms].name+'/'+voltage_data_file_name, /WRITE) AND not(keyword_set(force_all))
        ;exist_rawdata_file      = FILE_TEST( system.datasource_dir+'/raw_data/'+gms[system.gms].name+'/'+dir_name+'/'+file_name, /READ)
        notexist_voltagedata_file_index = WHERE(exist_voltagedata_file EQ 0, updating_files)
        ;exist_rawdata_file_index         = WHERE(exist_rawdata_file EQ 1, number_of_existing_rawfiles)
        
        ;updating_files = number_of_notexisting_datafiles
;print, 
        ;IF N_ELEMENTS(where(exist_processed_file EQ 0)) GT 1 THEN updating_files = N_ELEMENTS(where(exist_processed_file EQ 0)) $
        ;        ELSE IF where(exist_processed_file EQ 0) GE 0 THEN updating_files = N_ELEMENTS(where(exist_processed_file EQ 0)) $
        ;                ELSE updating_files = 0
;print, data_file_name[where(exist_data_file EQ 0)]

        IF not keyword_set(quiet) THEN BEGIN
                IF updating_files GT 0 THEN BEGIN
                        PRINT, ''
                        PRINT, updating_files, FORMAT='("        A total of ",I," file(s) need to be updated.")' 
                        PRINT, ''
                ENDIF ELSE BEGIN
                        PRINT, "        No file requires to be updated."
                        RETURN
                ENDELSE
        
                IF updating_files NE N_ELEMENTS(exist_voltagedata_file) THEN BEGIN
                        PRINT, N_ELEMENTS(exist_voltagedata_file)-updating_files, FORMAT='("        There are still ",I," file(s) that can be updated.")'
                        PRINT, '        Use the /FORCE_ALL keyword to force the updating of all files.'
                        PRINT, ''
                ENDIF
        ENDIF

        
        proceed = 'Y'
        REPEAT BEGIN
                IF not (keyword_set(quiet) OR keyword_set(force_all)) THEN READ, proceed, PROMPT='        Continue (Y/N)?: '
                proceed=STRUPCASE(proceed)
                IF proceed EQ 'N' OR proceed EQ 'NO' THEN RETURN
        ENDREP UNTIL proceed EQ 'Y' OR proceed EQ 'YES'


        FOR i = 0ll, N_ELEMENTS(exist_voltagedata_file)-1 DO BEGIN
                IF exist_voltagedata_file[i] EQ 0 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        ;print, tmp_year, tmp_month, tmp_day
                        ;getting_rawdata (initial, STATION=station, QUIET=quiet
                        making_voltage_file, [tmp_year, tmp_month, tmp_day], QUIET=quiet, STATION=station
                ENDIF
        ENDFOR

        IF not keyword_set(quiet) THEN BEGIN
                PRINT, ''
                PRINT, '        Data-file(s) ready for computing!'
        ENDIF

RETURN


END




