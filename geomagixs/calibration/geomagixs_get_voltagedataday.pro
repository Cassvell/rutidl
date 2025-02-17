;+
; NAME:
;       kmex_update_datafile.pro
;
;
; PURPOSE:
;
;       update the files of magnetic_data directory with the data from Mexican
;       Geomagnetic Service
;
; AUTHOR:
;
;       Pedro Corona Romero
;       Insituto de Geofisica, Unidad Michoacan
;       UNAM, Campus Morelia
;       Antigua Carretera a Patzcuaro, Morelia, Michoacan
;       piter.cr@gmail.com
;       Mexico, 22.iii.mmxvii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;
;       kmex_update_datafile, initial_date, final_date [, STATION=GMS_name, /QUIET, /FORCE_ALL]
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
;       QUIET                                          : sets messages from the program off
;       FORCE:ALL                                      : force to generate the *.dat files despite there are not the original data-files.
;                                                        for the case of abset data, the resulting *.dat will be filled with data-gaps.
;
; DEPENDENCIES:
;       omniweb_setup                                  : initilizes the directory tree
;
; INPUT FILES:
;       GMSYYYYMMDDrmin.min     [original geomagnetic service data files]
;
; OUTPUT FILES:
;       GMS_YYYYMMDD.dat        [mangetic service data to use in SCIESMEX analysis]
;
; HISTORIA:
;               22/03/2017      First succesfully running code
;               27/04/2017      Version 1.0 ready
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

;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
FUNCTION getting_voltagedata, initial, STATION=station, $
                                        QUIET=quiet
        On_error, 2
        COMPILE_OPT idl2, hidden
;##############################################################################
; initialize directories
;##############################################################################
        @geomagixs_commons
        

;##############################################################################
; initializing dates and hours
;##############################################################################
        initial_year   = initial[0]
        initial_month  = initial[1]
        initial_day    = initial[2]


;##############################################################################
; reading data files
;##############################################################################

        CASE 1 OF
                initial_month EQ 1  : tmp_string = 'jan'
                initial_month EQ 2  : tmp_string = 'feb'
                initial_month EQ 3  : tmp_string = 'mar'
                initial_month EQ 4  : tmp_string = 'apr'
                initial_month EQ 5  : tmp_string = 'may'
                initial_month EQ 6  : tmp_string = 'jun'
                initial_month EQ 7  : tmp_string = 'jul'
                initial_month EQ 8  : tmp_string = 'aug'
                initial_month EQ 9  : tmp_string = 'sep'
                initial_month EQ 10 : tmp_string = 'oct'
                initial_month EQ 11 : tmp_string = 'nov'
                initial_month EQ 12 : tmp_string = 'dec'
                ELSE: MESSAGE, 'Critial error'
        ENDCASE

        calibration_dir = system.datasource_dir+gms[system.gms].name+'/';'~/DATA/calibration'


                file_name = gms[system.gms].code+string(initial_day, tmp_string, initial_year MOD 1000, FORMAT='(I02,A3,".",I02)')+'v'

                file = FILE_SEARCH(calibration_dir+file_name, COUNT=opened_files)
                IF opened_files NE N_ELEMENTS(file_name) THEN MESSAGE, file_name+' not found.'
        
                number_of_lines = FILE_LINES(file)
                ;total_of_data   = total(number_of_lines)
                ;temporal_data   = STRARR(total(number_of_lines))
                magnetic_data   = STRARR(number_of_lines)

;print, number_of_lines

                ;tmp_data = STRARR(number_of_lines)

                OPENR, lun, file, /GET_LUN, ERROR=err
                        READF, lun, magnetic_data, FORMAT='(A)'
                CLOSE, lun
                FREE_LUN, lun


;##############################################################################
; extracting data
;##############################################################################
        DataStruct  =  { day : 0, month : 0, year : 0, $
                         hour : 0, minute : 0, $
                         H : 0., D : 0., Z : 0., $
                         Tc : 0., Ts : 0. }
                         
        IF number_of_lines-5 GE 1 THEN BEGIN
                resulting_data = REPLICATE(DataStruct, number_of_lines-4)

                ;READS, magnetic_data[4:number_of_lines-1], resulting_data, $
                ;       FORMAT='(I2,X,I2,X,I4,X,I2,X,I2,X,F9,X,F9,X,F9,X,F8,X,F8)'
                ;help, number_of_lines
                FOR i=4, number_of_lines[0]-1 DO BEGIN
                        tmp_date = [0,0,0,0,0]
                        READS,magnetic_data[i], tmp_date, FORMAT = '(I2,X,I2,X,I4,X,I2,X,I2,:)'
                        resulting_data[i-4].year   = tmp_date[2]
                        resulting_data[i-4].month  = tmp_date[1]
                        resulting_data[i-4].day    = tmp_date[0]
                        resulting_data[i-4].hour   = tmp_date[3]
                        resulting_data[i-4].minute = tmp_date[4]

                        str_pos = 18
                        str_tmp = STRMID(magnetic_data[i], str_pos-1)
 
                        blank_pos = STRPOS(str_tmp, ' ')
                        resulting_data[i-4].H = FLOAT(STRMID(str_tmp, 0, blank_pos))
                        
                        str_tmp = STRMID(str_tmp, blank_pos+1)
                        blank_pos = STRPOS(str_tmp, ' ')
                        resulting_data[i-4].D = FLOAT(STRMID(str_tmp, 0, blank_pos))
                       
                        str_tmp = STRMID(str_tmp, blank_pos+1)
                        blank_pos = STRPOS(str_tmp, ' ')
                        resulting_data[i-4].Z = FLOAT(STRMID(str_tmp, 0, blank_pos))
                                
                        str_tmp = STRMID(str_tmp, blank_pos+1)
                        blank_pos = STRPOS(str_tmp, ' ')
                        resulting_data[i-4].Tc = FLOAT(STRMID(str_tmp, 0, blank_pos))

                        str_tmp = STRMID(str_tmp, blank_pos+1)
                        resulting_data[i-4].Ts = FLOAT(str_tmp)
                        
                ENDFOR
        ENDIF ELSE BEGIN
                resulting_data = REPLICATE(DataStruct, 1)
                
                resulting_data[0].day    = initial_day
                resulting_data[0].month  = initial_month
                resulting_data[0].year   = initial_year
                
                resulting_data[0].hour   = 0
                resulting_data[0].minute = 0
                
                resulting_data[0].H      = 9999.999
                resulting_data[0].D      = 9999.999
                resulting_data[0].Z      = 9999.999

                resulting_data[0].Tc     = 9999.999
                resulting_data[0].Ts     = 9999.999
        ENDELSE



        ;tempvar = SIZE(TEMPORARY(resulting_data)) ; liberar memoria de la info no usada
        return, resulting_data


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


FUNCTION geomagixs_get_voltagedataday, initial, STATION=station, $
                                    QUIET=quiet, $
                                    REAL_TIME=real_time, $
                                    FORCE_ALL=force_all

        On_error, 2
        COMPILE_OPT idl2, HIDDEN
        
;##############################################################################
; initialize directories
;##############################################################################
        @geomagixs_commons
        geomagixs_setup_commons, /QUIET
        geomagixs_check_system, /QUIET
        ;geomagixs_setup_dates, STATION=station, /QUIET
        ;geomagixs_check_dates, initial, /ONE_DATE, STATION=station, /QUIET
        
;##############################################################################
; depuring inputs
;##############################################################################

        update_flag = 0
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
                                        error.log      += 'Ambiguous range of dates, it is required only one input date to define a day. '
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
        tmp        = getting_voltagedata([tmp_year,tmp_month, tmp_day], STATION=station, QUIET=quiet)

;plot, tmp.H[*], MAX_VALUE=999990., YRANGE=[27250.,27380.], XRANGE=[900,1000]
;print, Min(tmp.H[*]), MAX(tmp.H[*])

        ;plot, SMOOTH(qday[*].H,60) , MAX_VALUE=999999., YRANGE=[27300.,27400.], Ystyle=1, XRANGE=[900.,1100.]
        ;for i=900, 1000 DO print, i, qday[i].H

RETURN, tmp

END




