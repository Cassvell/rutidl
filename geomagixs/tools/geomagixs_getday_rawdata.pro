;+
; NAME:
;       geomagixs_getday_rawdata.pro
;
;
; PURPOSE:
;
;       function that retrives raw 1-second data for a given date
;
; AUTHOR:
;
;       Pedro Corona Romero
;       Insituto de Geofisica, Unidad Michoacan
;       UNAM, Campus Morelia
;       Antigua Carretera a Patzcuaro, Morelia, Michoacan
;       piter.cr@gmail.com
;       Mexico, 25.x.mmxxii
;
; CATEGORY:
;
;       Numerical Data Analize and Management
;
; CALLING SEQUENCE:
;
;       raw_data = geomagixs_getday_rawdata (initial_date [, STATION=GMS_name, /QUIET, /FORCE_ALL])
;
;       Description:
;       retrive the corresponding raw data for the given date
;
;
; PARAMETERS:
;       initial_date                                   : [YYYY, MM, DD] , date for which the data is read
;
; KEYWORD PARAMETERS:
;
;       STATION                                        : a string with the geomagnetic station (GMS) name where the data is taken from
;       QUIET                                          : sets messages from the program off
;       FORCE_ALL                                      : force to generate the *.dat files despite there are not the original data-files.
;                                                        for the case of abset data, the resulting *.dat will be filled with data-gaps.
;
; DEPENDENCIES:
;       @geomagixs_commons
;       geomagixs_setup_commons
;       geomagixs_check_system
;       geomagixs_setup_dates
;       geomagixs_check_dates
;
;
; INPUT:
;       ./data_source/raw_data/DDmmm/GMSDOYHH.YYs  [ray data file]
;
; OUTPUT:
;       data structure with the magnetic data from the GMSYYYYMMDDrK.sec file.
;
; HISTORIA:
;               25/10/2022      Version 1.0 starts development from -geomagixs_magneticdata_getday v2.0-
;
;-

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
; FUNCION AUXILIAR
FUNCTION getting_rawdata, initial, STATION=station, $
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
        hours_of_day = INDGEN(hours_number)

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

        
        
        file_name = gms[system.gms].code+string(doy, FORMAT='(I03)')+string(hours_of_day[*], FORMAT='(I02)')+'.'+ string(year MOD 1000, FORMAT='(I02)') +'s'
        dir_name  = string(day,FORMAT='(I02)')+tmp_string


;##############################################################################
; reading data files
;##############################################################################
        exist_files = FILE_TEST( system.datasource_dir+'/raw_data/'+gms[system.gms].name+'/'+dir_name+'/'+file_name, /READ)
        verified_files = WHERE (exist_files EQ 1, located_files)
        
        IF located_files NE N_ELEMENTS(exist_files) THEN $
                IF not keyword_set(quiet) THEN BEGIN
                                print, ''
                                PRINT, N_ELEMENTS(exist_files)-located_files, FORMAT='("        WARNING: A total of ",I0," hours of rawdata are missing.")'
                                PRINT, '        Missing data are going to be filled with ZEROs.'
                ENDIF
        
        files = FILE_SEARCH(system.datasource_dir+'/raw_data/'+gms[system.gms].name+'/'+dir_name+'/'+file_name[verified_files], COUNT=located_files)

        header_lines    = 5
;print, FILE_LINES(files[*])
;print, TOTAL(FILE_LINES(files[*])), 24*FILE_LINES(files[0])
        number_of_lines = TOTAL(FILE_LINES(files[*]))-located_files*header_lines
        raw_data        = STRARR(number_of_lines)
;print,  located_files,number_of_lines, number_of_lines/located_files, seconds_per_day, FORMAT='("found files: ",I0,  " - total of lines: ", I0, " - lines per file: ", I0, " / ", I0)'
        ;number_of_lines
        last_line = 0
        
        FOR i = 0, N_ELEMENTS(files)-1 DO BEGIN
                lines_number = FILE_LINES(files[i])
                tmp_data     = STRARR(lines_number)
                
                IF lines_number GT 5 THEN BEGIN
                        OPENR, lun, files[i], /GET_LUN, ERROR=err
                                READF, lun, tmp_data, FORMAT='(A)'
                        CLOSE, lun
                        FREE_LUN, lun
                        
;print, i, last_line, last_line+(lines_number-5)
                        raw_data [last_line:last_line+(lines_number-5)-1] = tmp_data[5:lines_number-1]
                ENDIF
                last_line += (lines_number-5)
        ENDFOR


;##############################################################################
; extracting data
;##############################################################################
        DataStruct  =  { HH : 0, MM : 0, SS : 0, $
                         CH_H : 0L, CH_D : 0L, CH_Z : 0L, CH_Tc : 0L, CH_Ts : 0L }
                         

        resulting_data = REPLICATE(DataStruct, number_of_lines)

        READS, raw_data[0:number_of_lines-1], resulting_data, $
               FORMAT='(I2,X,I2,X,I2,X,I8,X,I8,X,I8,X,I8,X,I8)'
        tempvar = SIZE(TEMPORARY(raw_data)) ; liberar memoria de la info no usada
;I2XI2XI2XI8XI8XI8XI8XI8
;00 00 00 -5318374 -1481143 +2023158 +0900269 +0916536



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
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;SECUENCIA PRINCIPAL


FUNCTION geomagixs_getday_rawdata, initial, STATION=station, $
                                        QUIET=quiet

        On_error, 2
        COMPILE_OPT idl2, HIDDEN
        
;##############################################################################
; initialize directories
;##############################################################################
        @geomagixs_commons
        geomagixs_setup_commons, /QUIET
        geomagixs_check_system, /QUIET
        geomagixs_setup_dates, STATION=station, /QUIET
        geomagixs_check_dates, initial, /ONE_DATE, STATION=station, /QUIET
        geomagixs_check_gms, STATION=station, /QUIET
        
;##############################################################################
; depuring inputs
;##############################################################################

        ;update_flag = 0
        ;IF keyword_set(force_all) BEGIN
        CASE 1 OF
                N_PARAMS() EQ 0 : BEGIN
                                        initial = system.today_date
                                  END
                N_PARAMS() EQ 1 : BREAK
                ELSE            : BEGIN
                                        IF NOT keyword_set(quiet) THEN BEGIN
                                                PRINT, FORMAT="('CRITICAL ERROR: conflict with input date(s), data inconsistent or invalid.')"
                                                PRINT, FORMAT="('                impossible to proceed with an ambiguoss range of dates.')"
                                        ENDIF
                                        error.value[2] += 1
                                        error.log      += 'Ambiguous range of dates, it is required only one single input date to define a day. '
                                        RETURN, 0
                                  END
        ENDCASE

                
        ;ENDIF
        

;##############################################################################
; initializing dates and hours
;##############################################################################
        initial_date = initial


;##############################################################################
; reading data files
;##############################################################################
        tmp_year  = initial_date[0]
        tmp_month = initial_date[1]
        tmp_day   = initial_date[2]

        ;IF tmp_julian GE tmp_julian_1 AND tmp_julian LE tmp_julian_2 THEN $
        ;        file_name  = 'qd'+string(tmp_year, tmp_decade/10, FORMAT='(I4,I01)')+'x.txt' $
        ;ELSE file_name = 'qd'+string(tmp_year, tmp_decade+9, FORMAT='(I4,I02)')+'.txt'
        tmp        = getting_rawdata([tmp_year,tmp_month, tmp_day], STATION=station, QUIET=quiet)

;plot, tmp.H[*], MAX_VALUE=999990., YRANGE=[27250.,27380.], XRANGE=[900,1000]
;print, Min(tmp.H[*]), MAX(tmp.H[*])

        ;plot, SMOOTH(qday[*].H,60) , MAX_VALUE=999999., YRANGE=[27300.,27400.], Ystyle=1, XRANGE=[900.,1100.]
        ;for i=900, 1000 DO print, i, qday[i].H

RETURN, tmp

END




