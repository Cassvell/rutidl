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
; CATEGORY:https://www.l3harrisgeospatial.com/docs/r_correlate.html
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





FUNCTION detrending, y, yd, DETRENDING=dentrending

;+
;NAME:
;       detrend -> detrending
;PURPOSE:
;	Removes linear or higher order polynomial trends from 1-D data
;	vectors.
;
;SAMPLE CALLING SEQUENCE:
;	coeff = DETREND(trended_data, detrended_data): remove linear trend.
;	coeff = DETREND(trended_data, detrended_data, ORDER=2):remove parabolic trend.
;
;INPUT:
;	Y = 1-D data vector, any type; length = NPTS.
;
;OPTIONAL KEYWORD INPUT:
;	ORDER = the order of the fit to the data to be removed.
;
;RETURNS:
;	COEFF = 1-D array of coefficients to the trend fit.
;
;OUTPUT OUTPUT:
;	YD = 1-D float array of detrended data; length = NPTS.
;
;HISTORY:
;       12-Dec-96: T. Berger.
;       13-Feb-23: P. Corona-Romero
;-


ON_ERROR,2

        sz       = SIZE(y)
        n_points = sz[1]
;np1 = npts-1
        yd       = FLTARR(n_points)

        if not KEYWORD_SET(dentrending) then BEGIN
                yd = y
                RETURN, [0.]
        ENDIF; ELSE BEGIN
        ;        sm_y = SMOOTH( y, avrg )
        X    = FINDGEN(n_points)
        ;END


        CASE dentrending of

        ;Result = LINFIT( X, Y [, CHISQR=variable] [, COVAR=variable] [, /DOUBLE] [, MEASURE_ERRORS=vector] [, PROB=variable] [, SIGMA=variable] [, YFIT=variable] )
        ;Result = SMOOTH( Array, Width [, /EDGE_MIRROR] [, /EDGE_TRUNCATE] [, /EDGE_WRAP] [, /EDGE_ZERO] [, MISSING=value] [, /NAN] )

                0: begin
                        ;avrg     = 24*60
                        m_y = SMOOTH( y, 72*60, /EDGE_TRUNCATE )
                        yd    = y - sm_y
                        coeff = [0.]
                end

                1: begin
                        y_fitted = FLTARR(n_points)
                        coeff    = LINFIT( X, y, YFIT=y_fitted )
                        yd       = y - y_fitted
                end

                else: begin
                        y_fitted = FLTARR(n_points)
                        coeff    = POLY_FIT(X,y,dentrending,YFIT=y_fitted)
                        yd       = y - y_fitted
                end

        endcase

        RETURN,coeff
END













;##############################################################################
;##############################################################################
;##############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; FUNCION AUXILIAR
FUNCTION getting_verification_data, file_name

; PURPOSE:
;
;       complete gaps in data files from Mexican Magnetic Service
;       and removes the their headers
;
; AUTHOR:
;
;       Pedro Corona Romero
;       Insituto de Geofisica, Unidad Michoacan
;       UNAM, Campus Morelia
;       Antigua Carretera a Patzcuaro, Morelia, Michoacan
;       piter.cr@gmail.com
;       Mexico, 15.iii.mmxvii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;
;       DATA = kmex_getdata(initial_date, final_date [, RESOLUTION=resolution])
;
;       Description:
;       retrives magnetic data as measured by magnetic observatories from data files.
;
;
; PARAMETERS:
;       initial_date                                   : [YYYY, MM, DD, HH, mm] , initial date and time at which the data is read from, array of integers
;       final_date                                     : [YYYY, MM, DD, HH, mm] , final date and time at which the data is read from, array of integers
;
; KEYWORD PARAMETERS:
;
;       ESTACION                                       : magnetic station where the data is taken from
;       QUIET                                          : sets messages off
;
; DEPENDENCIAS:
;       omniweb_setup                                  : initilizes the directory tree
;
; ARCHIVOS ANALIZADOS:
;       teoYYYYMMDDrmin.min
;
; ARCHIVOS DE SALIDA:
;       YYYYMMDD_min.magnetic
;
; HISTORIA:
;               0.1     independent function
;               1.0     added as an auxiliary procedure in kmex_update_datafile 23/mar/2017
                



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

;##############################################################################
; reading data files
;##############################################################################

        file = FILE_SEARCH(file_name, COUNT=opened_files)        
                IF opened_files LT 1 THEN MESSAGE, file_name+' not found.'

        number_of_lines = FILE_LINES(file)
        file_data   = STRARR(number_of_lines)
        
        OPENR, lun, file, /GET_LUN, ERROR=err
                READF, lun, file_data, FORMAT='(A)'
        CLOSE, lun
        FREE_LUN, lun
        
        print, file_name
;print, STRLEN(file_data[0]),number_of_lines
;2023 01 13 00:12    -0.30    -13.10      5.70     -7.30
;  I4XI2XI2XI2XI2 2X    F7X
                DataStruct  =  { year : 0, month : 0, day : 0, $
                                 hour : 0, minute : 0, $
                                 D : 0., H : 0., Z : 0., F : 0. }
                
                
                format_for_reading = '(I4,X,I2,X,I2,X,I2,X,I2,2X,F7,X,F9,X,F9,X,F9)'
                
                PRINT, 'reading magnetic data from: '+file_name
        data = REPLICATE(DataStruct, number_of_lines)


        READS, file_data, data, FORMAT=format_for_reading

        dummy = SIZE(TEMPORARY(file_data))
                         

        Return, data
END


;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
;##############################################################################
; FUNCION AUXILIAR




;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################

















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


PRO geomagixs_make_processverification, initial, final, STATION=station, $
                                              QUIET=quiet, $
                                              FORCE_ALL=force_all, $
                                              H=h, D=d, Z=z, T=t, $
                                              DETRENDING=dentrending

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

        ;print, offset_tc
;##############################################################################
; initializing dates and hours
;##############################################################################
        initial_year   = initial[0]
        initial_month  = initial[1]
        initial_day    = initial[2]

        final_year     = final[0]
        final_month    = final[1]
        final_day      = final[2]

        number_of_days = JULDAY(final_month,final_day,final_year)-JULDAY(initial_month,initial_day-1,initial_year)
        
        time_array     = findgen(number_of_days*24*60)/(number_of_days*24*60-1)*(number_of_days*24*60)


;##############################################################################
; reading data files
;##############################################################################
        calibration_dir = '~/DATA/calibration'
        PRINT, 'WARNING:'
        PRINT, '        This program compares the processed values of AUXILIAR gms with those'
        PRINT, '        processed values from the selected station ['+STRUPCASE(gms[system.gms].name)+'].'
        PRINT, ''
        PRINT, '        The purpose of this comparisson is to verify the previous inter-callibration'
        PRINT, '        process during the given period between the selected GMS.'
        PRINT, ''
        PRINT, '   Serching for the files:'
        PRINT, '      '+'aux_'+STRING(initial_year, initial_month, initial_day, final_year, final_month, final_day, $
                                      FORMAT='(I4,I02,I02,"-",I4,I02,I02)')+'.verification.m'
        PRINT, '      '+gms[system.gms].code+'_'+STRING(initial_year, initial_month, initial_day, final_year, final_month, final_day, $
                                      FORMAT='(I4,I02,I02,"-",I4,I02,I02)')+'.verification.m'
        PRINT, '   at the directory: '+calibration_dir

        file_name = gms[system.gms].code+'_'+STRING(initial_year, initial_month, initial_day, final_year, final_month, final_day, $
                                      FORMAT='(I4,I02,I02,"-",I4,I02,I02)')+'.verification.m'
        files_station = FILE_SEARCH(calibration_dir+'/'+file_name, COUNT=opened_files)
        IF opened_files NE 1 THEN MESSAGE, '* FATAL ERROR: Missing calibration files: '+file_name
        file_name = 'aux_'+STRING(initial_year, initial_month, initial_day, final_year, final_month, final_day, $
                                      FORMAT='(I4,I02,I02,"-",I4,I02,I02)')+'.verification.m'
        files_auxiliar = FILE_SEARCH(calibration_dir+'/'+file_name, COUNT=opened_files)
        IF opened_files NE 1 THEN MESSAGE, '* FATAL ERROR: Missing calibration files: '+file_name

        PRINT, ''
        PRINT, '   Reading Data files.'


        station_magnetic_data  = getting_verification_data(files_station[0])
        ;station_voltage_data  = getting_data(files_station[1])
        auxiliar_magnetic_data = getting_verification_data(files_auxiliar[0])
        ;auxiliar_voltage_data  = getting_data(files_auxiliar[1])

        

        ;print, magnetic_indexes

        PRINT, ''
        PRINT, ''
        ;PRINT, '   Base-line reference values: <H> [nT]    <D> [°]    <Z> [nT]'
        ;H_base = 27393.3
        ;D_base = 5.434
        ;Z_base = 29444.6
        ;PRINT, H_base, D_base, Z_base, FORMAT='(32X,F7.1,6X,F5.3,5X,F7.1)'
        
        ;Tz_factor = 0.15
        ;Th_factor = 0.0


        magnetic_indexes       = WHERE(station_magnetic_data[*].H LT 999990. AND auxiliar_magnetic_data[*].H LT 999990. AND $
                                       station_magnetic_data[*].D LT 9990. AND auxiliar_magnetic_data[*].D LT 9990. AND $
                                       station_magnetic_data[*].Z LT 999990. AND auxiliar_magnetic_data[*].Z LT 999990. AND $
                                       station_magnetic_data[*].F LT 999990. AND auxiliar_magnetic_data[*].F LT 999990.);AND $ ;)
                                       ;ABS(station_magnetic_data[*].H-auxiliar_magnetic_data[*].H) LT 10.) ; esto es solo para las calibraciones finales

DEVICE, DECOMPOSED = 0
        WINDOW, 2, XSIZE=1200, YSIZE=800, TITLE='Processed Data '+string(initial[0],initial[1],initial[2],$
                                                                       final[0],final[1],final[2], FORMAT='("[",I4,I02,I02,"-",I4,I02,I02,"]")')
        !P.MULTI = [0, 4, 2]
LOADCT, /SILENT, 39
        avrg_period = 60.*72.
        
        X_max  = MAX(time_array[magnetic_indexes]/(24*60))
        X_min  = MIN(time_array[magnetic_indexes]/(24*60))

        Y_max = MAX([auxiliar_magnetic_data[magnetic_indexes].D,station_magnetic_data[magnetic_indexes].D])
        Y_min = MIN([auxiliar_magnetic_data[magnetic_indexes].D,station_magnetic_data[magnetic_indexes].D])
        
        PLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].D, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='D [Minute Deg] - Auxiliar(red) & Station(blue)', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].D, color=254
        OPLOT, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].D, color=50
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(auxiliar_magnetic_data[magnetic_indexes].D, avrg_period, /EDGE_TRUNCATE), color=254,thick=2
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(station_magnetic_data[magnetic_indexes].D, avrg_period, /EDGE_TRUNCATE), color=50,thick=2

        Y_max = MAX([auxiliar_magnetic_data[magnetic_indexes].H,station_magnetic_data[magnetic_indexes].H])
        Y_min = MIN([auxiliar_magnetic_data[magnetic_indexes].H,station_magnetic_data[magnetic_indexes].H])
        
        PLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].H, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='H [nT] - Auxiliar(red) & Station(blue)', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].H, color=254
        ;OPLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].H-H_base, color=0, NSUM=60.*24.,thick=1; SMOOTH( y, 24*60 )
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(auxiliar_magnetic_data[magnetic_indexes].H, avrg_period, /EDGE_TRUNCATE), color=254,thick=2; SMOOTH( y, 24*60 )
        OPLOT, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].H, color=50
        ;OPLOT, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].H-H_base, color=0, NSUM=60.*24.,thick=1
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(station_magnetic_data[magnetic_indexes].H, avrg_period, /EDGE_TRUNCATE), color=50,thick=2

        Y_max = MAX([auxiliar_magnetic_data[magnetic_indexes].Z,station_magnetic_data[magnetic_indexes].Z])
        Y_min = MIN([auxiliar_magnetic_data[magnetic_indexes].Z,station_magnetic_data[magnetic_indexes].Z])
        
        PLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].Z, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='Z [nT] - Auxiliar(red) & Station(blue)', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].Z, color=254
        OPLOT, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].Z, color=50
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(auxiliar_magnetic_data[magnetic_indexes].Z, avrg_period, /EDGE_TRUNCATE), color=254,thick=2
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(station_magnetic_data[magnetic_indexes].Z, avrg_period, /EDGE_TRUNCATE), color=50,thick=2

        Y_max = MAX([auxiliar_magnetic_data[magnetic_indexes].F,station_magnetic_data[magnetic_indexes].F])
        Y_min = MIN([auxiliar_magnetic_data[magnetic_indexes].F,station_magnetic_data[magnetic_indexes].F])
        
        PLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].F, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='F [nT] - Auxiliar(red) & Station(blue)', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), auxiliar_magnetic_data[magnetic_indexes].F, color=254
        OPLOT, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].F, color=50
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(auxiliar_magnetic_data[magnetic_indexes].F, avrg_period, /EDGE_TRUNCATE), color=254,thick=2
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(station_magnetic_data[magnetic_indexes].F, avrg_period, /EDGE_TRUNCATE), color=50,thick=2



        D_difference = auxiliar_magnetic_data[magnetic_indexes].D-station_magnetic_data[magnetic_indexes].D
        Y_max = MAX([D_difference])
        Y_min = MIN([D_difference])
        
        PLOT, time_array[magnetic_indexes]/(24*60), D_difference, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='D diff. [Minute Deg]', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), D_difference, color=254
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(D_difference, avrg_period, /EDGE_TRUNCATE), color=50,thick=2

        H_difference = auxiliar_magnetic_data[magnetic_indexes].H-station_magnetic_data[magnetic_indexes].H
        Y_max = MAX([H_difference])
        Y_min = MIN([H_difference])
        
        PLOT, time_array[magnetic_indexes]/(24*60), H_difference, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='H diff. [nT]', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), H_difference, color=254
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(H_difference, avrg_period, /EDGE_TRUNCATE), color=50,thick=2

        Z_difference = auxiliar_magnetic_data[magnetic_indexes].Z-station_magnetic_data[magnetic_indexes].Z
        Y_max = MAX([Z_difference])
        Y_min = MIN([Z_difference])
        
        PLOT, time_array[magnetic_indexes]/(24*60), Z_difference, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='Z diff. [nT]', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), Z_difference, color=254
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(Z_difference, avrg_period, /EDGE_TRUNCATE), color=50,thick=2

        F_difference = auxiliar_magnetic_data[magnetic_indexes].F-station_magnetic_data[magnetic_indexes].F
        Y_max = MAX([F_difference])
        Y_min = MIN([F_difference])
        
        PLOT, time_array[magnetic_indexes]/(24*60), F_difference, Xstyle=1, Ystyle=1, $
              color=0, background=255 , Title='F diff. [nT]', /NODATA, CHARSIZE=2., YRANGE=[Y_min, Y_max], XRANGE=[X_min, X_max]
        OPLOT, time_array[magnetic_indexes]/(24*60), F_difference, color=254
        OPLOT, time_array[magnetic_indexes]/(24*60), smooth(F_difference, avrg_period, /EDGE_TRUNCATE), color=50,thick=2



LOADCT, /SILENT, 0

IF keyword_set(T) THEN BEGIN

PRINT, '========================================================================'
PRINT, ' Temperature - analysis'
PRINT, '------------------------------------------------------------------------'
PRINT, ''

        ;magnetic_indexes    = (station_magnetic_data[*].H LE 99999.0) AND (auxiliar_magnetic_data[*].H LE 99999.0) AND $
        ;                      (station_voltage_data[*].Ts GE 180.0) AND (auxiliar_voltage_data[*].Ts GE 180.0)

        avrg_period = 60.*24.

        PRINT, '** SENSOR *********** Medians +/- STDDEV'
        PRINT, '                [C Deg]                [mV]'
        PRINT, 'STATION: ', MEDIAN(0.15*(station_voltage_data[magnetic_indexes].Ts-250.)+25.), $
                           STDDEV(0.15*(station_voltage_data[magnetic_indexes].Ts-250.)+25.), $
                           MEDIAN(station_voltage_data[magnetic_indexes].Ts), $
                           STDDEV(station_voltage_data[magnetic_indexes].Ts), $
                           FORMAT='(A, 5X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
        print, 'Auxiliar:', MEDIAN(0.15*(auxiliar_voltage_data[magnetic_indexes].Ts-250.)+25.), $
                            STDDEV(0.15*(auxiliar_voltage_data[magnetic_indexes].Ts-250.)+25.), $
                           MEDIAN(auxiliar_voltage_data[magnetic_indexes].Ts), $
                           STDDEV(auxiliar_voltage_data[magnetic_indexes].Ts), $
                           FORMAT='(A, 5X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
PRINT, '------------------------------------------------------------------------'
        

        print, 'Differences:', MEDIAN(0.15*(smooth(station_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE))), $
                               STDDEV(0.15*(smooth(station_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE))), $
                               MEDIAN(smooth(station_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)), $
                               STDDEV(smooth(station_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)), $
                               FORMAT='(A, 2X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
        PRINT, ''
PRINT, '------------------------------------------------------------------------'
        print, ''

        ;c_magnetic = MEDIAN(station_magnetic_data[magnetic_indexes].H-auxiliar_magnetic_data[magnetic_indexes].H)
        ;c_voltage  = MEDIAN(station_voltage_data[magnetic_indexes].H-auxiliar_voltage_data[magnetic_indexes].H)

        ;magneticfit = LINFIT(station_voltage_data[magnetic_indexes].Ts, 0.15*(station_voltage_data[magnetic_indexes].Ts-250.)+25.)
        ;print, 'Station fit b and m [nT/mV]:', magneticfit

;PRINT, '------------------------------------------------------------------------'

        ;auxiliarfit = LINFIT(auxiliar_voltage_data[magnetic_indexes].Ts, 0.15*(auxiliar_voltage_data[magnetic_indexes].Ts-250.)+25.)
        ;print, 'Auxiliar fit b and m [nT/mV]:', auxiliarfit

;PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'

        ;magnetic_indexes       = WHERE(station_magnetic_data[*].H LT 99999.0 AND auxiliar_magnetic_data[*].H LT 99999.0); AND $ ;)

        result_sensor = [MEDIAN(smooth(station_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Ts, avrg_period, /EDGE_TRUNCATE)), $
                  STDDEV(station_voltage_data[magnetic_indexes].Ts)/ STDDEV(auxiliar_voltage_data[magnetic_indexes].Ts)]
        
        ;print, result_sensor[0], result_sensor[1]
        
        result_contol = [MEDIAN(smooth(station_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)), $
                  STDDEV(station_voltage_data[magnetic_indexes].Tc)/ STDDEV(auxiliar_voltage_data[magnetic_indexes].Tc)]
        ;auxiliar_Ts_result = (auxiliar_voltage_data[magnetic_indexes].Ts-MEDIAN(auxiliar_voltage_data[magnetic_indexes].Ts))*result[1]+ MEDIAN(auxiliar_voltage_data[magnetic_indexes].Ts)+result[0]
        ;print, N_ELEMENTS((auxiliar_voltage_data[magnetic_indexes].Ts)), N_ELEMENTS(SMOOTH(auxiliar_voltage_data[magnetic_indexes].Ts, 60*24))
        ;print, '1st line fit:     ', result
        ;print, '    uncertainties:', uncertainties
        ;print, '      correlation:', correlate(auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic,station_magnetic_data[magnetic_indexes].H-H_base)
        ;print, '      correlation:', correlate(station_voltage_data[magnetic_indexes].Ts, $
        ;                                      (auxiliar_voltage_data[magnetic_indexes].Ts-MEDIAN(auxiliar_voltage_data[magnetic_indexes].Ts))*result[1]+ MEDIAN(auxiliar_voltage_data[magnetic_indexes].Ts)+result[0]))

        PRINT, '** CONTROL UNIT ***** Medians +/- STDDEV'
        PRINT, '                [C Deg]                [mV]'
        PRINT, 'STATION: ', MEDIAN(smooth((0.15*(station_voltage_data[magnetic_indexes].Tc-250.)+25.), avrg_period, /EDGE_TRUNCATE)), $
                           STDDEV(smooth((0.15*(station_voltage_data[magnetic_indexes].Tc-250.)+25.), avrg_period, /EDGE_TRUNCATE)), $
                           MEDIAN(smooth((station_voltage_data[magnetic_indexes].Tc), avrg_period, /EDGE_TRUNCATE)), $
                           STDDEV(smooth((station_voltage_data[magnetic_indexes].Tc), avrg_period, /EDGE_TRUNCATE)), $
                           FORMAT='(A, 5X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
        print, 'Auxiliar:', MEDIAN(smooth((0.15*(auxiliar_voltage_data[magnetic_indexes].Tc-250.)+25.), avrg_period, /EDGE_TRUNCATE)), $
                            STDDEV(smooth((0.15*(auxiliar_voltage_data[magnetic_indexes].Tc-250.)+25.), avrg_period, /EDGE_TRUNCATE)), $
                           MEDIAN(smooth((auxiliar_voltage_data[magnetic_indexes].Tc), avrg_period, /EDGE_TRUNCATE)), $
                           STDDEV(smooth((auxiliar_voltage_data[magnetic_indexes].Tc), avrg_period, /EDGE_TRUNCATE)), $
                           FORMAT='(A, 5X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
PRINT, '------------------------------------------------------------------------'
        print, 'Differences:', MEDIAN(0.15*(smooth(station_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE))), $
                               STDDEV(0.15*(smooth(station_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE))), $
                               MEDIAN(smooth(station_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)), $
                               STDDEV(smooth(station_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)-smooth(auxiliar_voltage_data[magnetic_indexes].Tc, avrg_period, /EDGE_TRUNCATE)), $
                               FORMAT='(A, 2X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
        PRINT, ''

PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'
        PRINT, ''
        PRINT, ''


DEVICE, DECOMPOSED = 0
        WINDOW, 0, XSIZE=600, YSIZE=600, TITLE='T - Callibration window'
        !P.MULTI = [0, 2, 2]
        
        ;tmp_arr = station_voltage_data[magnetic_indexes].Ts-auxiliar_voltage_data[magnetic_indexes].Ts

;plot, (tmp_arr[*]), linestyle=0, NSUM=50*24;, YRange=[-600,100.]

;plot, station_voltage_data[magnetic_indexes].Ts, linestyle=1
;oplot, auxiliar_voltage_data[magnetic_indexes].Ts, linestyle=0
LOADCT, /SILENT, 39

        ;Y_max = MAX([station_voltage_data[magnetic_indexes].Ts,(auxiliar_voltage_data[magnetic_indexes].Ts)])
        ;Y_min = MIN([station_voltage_data[magnetic_indexes].Ts,(auxiliar_voltage_data[magnetic_indexes].Ts)])
        ;plot, time_array[magnetic_indexes]/(24*60),station_voltage_data[magnetic_indexes].Ts, linestyle=0, title='mV', Xstyle=1, $
        ;      YRange=[Y_min,Y_max], /NoData, Color=0, background=255, CHARSIZE=1.0
        ;oplot, time_array[magnetic_indexes]/(24*60), station_voltage_data[magnetic_indexes].Ts, color = 50
        ;oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_voltage_data[magnetic_indexes].Ts), color = 254;, title='Station mV'
        ;;oplot, time_array[magnetic_indexes]/(24*60), station_voltage_data[magnetic_indexes].Ts, color = 50, NSUM=60.*24.,thick=2
        ;oplot, time_array[magnetic_indexes]/(24*60), SMOOTH(station_voltage_data[magnetic_indexes].Ts, 60.*24.,/EDGE_TRUNCATE), color = 50,thick=2
        ;oplot, time_array[magnetic_indexes]/(24*60), SMOOTH(auxiliar_voltage_data[magnetic_indexes].Ts, 60.*24.,/EDGE_TRUNCATE), color = 254,thick=2;, title='Station mV'


        ;Y_max = MAX([station_voltage_data[magnetic_indexes].Ts,(auxiliar_voltage_data[magnetic_indexes].Ts)+result_sensor[0]])
        ;Y_min = MIN([station_voltage_data[magnetic_indexes].Ts,(auxiliar_voltage_data[magnetic_indexes].Ts)+result_sensor[0]])
        ;plot, time_array[magnetic_indexes]/(24*60),station_voltage_data[magnetic_indexes].Ts, linestyle=0, title='mV', Xstyle=1, $
        ;      YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0
        ;oplot, time_array[magnetic_indexes]/(24*60),station_voltage_data[magnetic_indexes].Ts, color = 254

        ;oplot, time_array[magnetic_indexes]/(24*60),auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0], color = 50;, title='Station mV'


        Y_max = 0.15*(MAX([station_voltage_data[magnetic_indexes].Ts,(auxiliar_voltage_data[magnetic_indexes].Ts)+result_sensor[0]])-250)+25
        Y_min = 0.15*(MIN([station_voltage_data[magnetic_indexes].Ts,(auxiliar_voltage_data[magnetic_indexes].Ts)+result_sensor[0]])-250)+25
        plot, time_array[magnetic_indexes]/(24*60),0.15*(station_voltage_data[magnetic_indexes].Ts-250)+25, linestyle=0, title='Sensor [C Deg]', Xstyle=1, $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0
        oplot, time_array[magnetic_indexes]/(24*60),0.15*(station_voltage_data[magnetic_indexes].Ts-250)+25, color = 254
        oplot, time_array[magnetic_indexes]/(24*60),smooth(0.15*(station_voltage_data[magnetic_indexes].Ts-250)+25, avrg_period, /EDGE_TRUNCATE), color = 254,thick=2

        oplot, time_array[magnetic_indexes]/(24*60),0.15*((auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])-250)+25, color = 50;, title='Station mV'
        oplot, time_array[magnetic_indexes]/(24*60),smooth(0.15*((auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])-250)+25, avrg_period, /EDGE_TRUNCATE), color = 50,thick=2;, title='Station mV'


        Y_max = 0.15*(MAX(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])))
        Y_min = 0.15*(MIN(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])))
        plot, time_array[magnetic_indexes]/(24*60),station_voltage_data[magnetic_indexes].Ts, linestyle=0, title='Sensor Differences [C Deg]', Xstyle=1, $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0
        oplot, time_array[magnetic_indexes]/(24*60),0.15*(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])), color = 254
        oplot, time_array[magnetic_indexes]/(24*60),smooth(0.15*(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])), avrg_period, /EDGE_TRUNCATE), color = 50,thick=2


        Y_max = 0.15*(MAX([station_voltage_data[magnetic_indexes].Tc,(auxiliar_voltage_data[magnetic_indexes].Tc)+result_contol[0]])-250)+25
        Y_min = 0.15*(MIN([station_voltage_data[magnetic_indexes].Tc,(auxiliar_voltage_data[magnetic_indexes].Tc)+result_contol[0]])-250)+25
        plot, time_array[magnetic_indexes]/(24*60),0.15*(station_voltage_data[magnetic_indexes].Tc-250)+25, linestyle=0, title='Control U. [C Deg]', Xstyle=1, $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0
        oplot, time_array[magnetic_indexes]/(24*60),0.15*(station_voltage_data[magnetic_indexes].Tc-250)+25, color = 254
        oplot, time_array[magnetic_indexes]/(24*60),smooth(0.15*(station_voltage_data[magnetic_indexes].Tc-250)+25, avrg_period, /EDGE_TRUNCATE), color = 254,thick=2

        oplot, time_array[magnetic_indexes]/(24*60),0.15*((auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])-250)+25, color = 50;, title='Station mV'
        oplot, time_array[magnetic_indexes]/(24*60),smooth(0.15*((auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])-250)+25, avrg_period, /EDGE_TRUNCATE), color = 50,thick=2;, title='Station mV'


        Y_max = 0.15*(MAX(station_voltage_data[magnetic_indexes].Tc-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])))
        Y_min = 0.15*(MIN(station_voltage_data[magnetic_indexes].Tc-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])))
        plot, time_array[magnetic_indexes]/(24*60),0.15*(station_voltage_data[magnetic_indexes].Tc-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])), linestyle=0, title='Control U. Diff. [C Deg]', Xstyle=1, $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0
        oplot, time_array[magnetic_indexes]/(24*60),0.15*(station_voltage_data[magnetic_indexes].Tc-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])), color = 254
        oplot, time_array[magnetic_indexes]/(24*60),smooth(0.15*(station_voltage_data[magnetic_indexes].Tc-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])), avrg_period, /EDGE_TRUNCATE), color = 50,thick=2



        ;Y_max = MAX((station_voltage_data[magnetic_indexes].Ts)-(result_sensor[1]*(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0]/result_sensor[1])))
        ;Y_min = MIN((station_voltage_data[magnetic_indexes].Ts)-(result_sensor[1]*(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0]/result_sensor[1])))
        ;plot, time_array[magnetic_indexes]/(24*60), (station_voltage_data[magnetic_indexes].Ts)-result_sensor[1]*(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0]/result_sensor[1]), linestyle=0, title='Station-Auxiliar Difference [nT]', Xstyle=1, $
        ;      YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0
        ;oplot, time_array[magnetic_indexes]/(24*60), (station_voltage_data[magnetic_indexes].Ts)-result_sensor[1]*(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0]/result_sensor[1]), color = 50, NSUM=60.*24.,thick=2
        ;oplot, time_array[magnetic_indexes]/(24*60), (station_voltage_data[magnetic_indexes].Ts)-result_sensor[1]*(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0]/result_sensor[1]), color = 254
        
        ;Y_max = MAX((station_voltage_data[magnetic_indexes].Tc)-((auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])))
        ;Y_min = MIN((station_voltage_data[magnetic_indexes].Tc)-((auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])))
        ;plot, time_array[magnetic_indexes]/(24*60), (station_voltage_data[magnetic_indexes].Tc)-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0]), $
        ;      linestyle=0, title='Station-Auxiliar Difference [nT]', Xstyle=1, $
        ;      YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0
        ;oplot, time_array[magnetic_indexes]/(24*60), (station_voltage_data[magnetic_indexes].Tc)-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0]), color = 50, NSUM=60.*24.,thick=2
        ;oplot, time_array[magnetic_indexes]/(24*60), (station_voltage_data[magnetic_indexes].Tc)-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0]), color = 254

LOADCT, /SILENT, 0



PRINT, '------------------------------------------------------------------------'
        PRINT, '** ANALYSIS RESULTS * Medians +/- STDDEV'
        PRINT, '                  [C Deg]                  [mV]'
        print, 'Ts Difference:', MEDIAN(0.15*(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0]))), $
                               STDDEV(0.15*(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0]))), $
                               MEDIAN(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])), $
                               STDDEV(station_voltage_data[magnetic_indexes].Ts-(auxiliar_voltage_data[magnetic_indexes].Ts+result_sensor[0])), $
                               FORMAT='(A, X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
        print, 'Tc Difference:', MEDIAN(0.15*((station_voltage_data[magnetic_indexes].Tc)-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0]))), $
                               STDDEV(0.15*((station_voltage_data[magnetic_indexes].Tc)-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0]))), $
                               MEDIAN((station_voltage_data[magnetic_indexes].Tc)-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])), $
                               STDDEV((station_voltage_data[magnetic_indexes].Tc)-(auxiliar_voltage_data[magnetic_indexes].Tc+result_contol[0])), $
                               FORMAT='(A, X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
PRINT, '------------------------------------------------------------------------'
PRINT, '========================================================================'


ENDIF









; ##############################################################################
; ##############################################################################
; ##############################################################################
; ##############################################################################
; ##############################################################################
; ##############################################################################

IF keyword_set(H) THEN BEGIN
PRINT, '========================================================================'
PRINT, ' H - component analysis'
PRINT, '------------------------------------------------------------------------'

        valid_indexes       = (station_magnetic_data[*].H LE 99999.0) AND (auxiliar_magnetic_data[*].H LE 99999.0) AND $
                              (station_voltage_data[*].Ts GE 180.0) AND (auxiliar_voltage_data[*].Ts GE 180.0 ); AND $ ;)
                                       ;ABS(station_magnetic_data[*].H-auxiliar_magnetic_data[*].H) LT 10.) ; esto es solo para las calibraciones finales


        
        magnetic_indexes       = WHERE(station_magnetic_data[*].H LT 99999.0 AND auxiliar_magnetic_data[*].H LT 99999.0); AND $ ;)
                                       ;ABS(station_magnetic_data[*].H-auxiliar_magnetic_data[*].H) LT 10.) ; esto es solo para las calibraciones finales

        magnetic_indexes0      = WHERE(valid_indexes EQ 1)
        PRINT, ''
        PRINT, '   Correlations Station vs Auxiliar:'
        PRINT, '                  H [nT]       H [mV]'

        corr1 = CORRELATE(station_magnetic_data[magnetic_indexes].H-H_base, auxiliar_magnetic_data[magnetic_indexes].H-H_base)
        corr2 = CORRELATE(station_voltage_data[magnetic_indexes].H, auxiliar_voltage_data[magnetic_indexes].H)
        PRINT, corr1, corr2, FORMAT='(18X,F6.4,7X,F6.4, "  *** non-gap data ")'
        ;print, magnetic_indexes
        corr1 = CORRELATE(station_magnetic_data[magnetic_indexes0].H-H_base, auxiliar_magnetic_data[magnetic_indexes0].H-H_base)
        corr2 = CORRELATE(station_voltage_data[magnetic_indexes0].H, auxiliar_voltage_data[magnetic_indexes0].H)
        PRINT, corr1, corr2, FORMAT='(18X,F6.4,7X,F6.4, "  *** non-gap & valid-temp data ")'
        ;print, TOTAL(ABS(station_magnetic_data[magnetic_indexes].H-auxiliar_magnetic_data[magnetic_indexes].H))/TOTAL(station_magnetic_data[magnetic_indexes].H)
        ;print, CORRELATE(station_magnetic_data[magnetic_indexes].H-H_base, (auxiliar_magnetic_data[magnetic_indexes].H-H_base)/corr1), $
        ;       CORRELATE((station_magnetic_data[*].H-H_base)*valid_indexes, (auxiliar_magnetic_data[*].H-H_base)*valid_indexes/corr2)
        PRINT, ''
        PRINT, ''
        
        
        ;station_magnetic_median  = MEDIAN(station_magnetic_data[magnetic_indexes].H-H_base)
        ;station_magnetic_dev     = STDDEV(station_magnetic_data[magnetic_indexes].H-H_base)
        ;station_voltage_median   = MEDIAN(station_voltage_data[magnetic_indexes].H)
        ;station_voltage_dev      = STDDEV(station_voltage_data[magnetic_indexes].H)
        ;auxiliar_magnetic_median = MEDIAN(auxiliar_magnetic_data[magnetic_indexes].H-H_base)
        ;auxiliar_magnetic_dev    = STDDEV(auxiliar_magnetic_data[magnetic_indexes].H-H_base)
        ;auxiliar_voltage_median  = MEDIAN(auxiliar_voltage_data[magnetic_indexes].H)
        ;auxiliar_voltage_dev     = STDDEV(auxiliar_voltage_data[magnetic_indexes].H)

        coeff_magnetic_station  = detrending(station_magnetic_data[magnetic_indexes].H-H_base, station_magnetic_data_H, DETRENDING=dentrending)
        coeff_magnetic_auxiliar = detrending(auxiliar_magnetic_data[magnetic_indexes].H-H_base, auxiliar_magnetic_data_H, DETRENDING=dentrending)
        coeff_voltage_station   = detrending(station_voltage_data[magnetic_indexes].H, station_voltage_data_H, DETRENDING=dentrending)
        coeff_voltage_auxiliar  = detrending(auxiliar_voltage_data[magnetic_indexes].H, auxiliar_voltage_data_H, DETRENDING=dentrending)

        station_magnetic_median  = MEDIAN(station_magnetic_data_H)
        station_magnetic_dev     = STDDEV(station_magnetic_data_H)
        station_voltage_median   = MEDIAN(station_voltage_data_H)
        station_voltage_dev      = STDDEV(station_voltage_data_H)
        auxiliar_magnetic_median = MEDIAN(auxiliar_magnetic_data_H)
        auxiliar_magnetic_dev    = STDDEV(auxiliar_magnetic_data_H)
        auxiliar_voltage_median  = MEDIAN(auxiliar_voltage_data_H)
        auxiliar_voltage_dev     = STDDEV(auxiliar_voltage_data_H)

        PRINT, 'Medians +/- STDDEV:  H [nT]       H [mV]'
        print, ''
        PRINT, 'STATION:', station_magnetic_median, station_magnetic_dev, $
                           station_voltage_median, station_voltage_dev, $
                           FORMAT='(A, 5X, F7.2,"+/-",F7.3, 7X, F7.2, "+/-", F7.3)'
        print, 'Auxiliar:', auxiliar_magnetic_median, auxiliar_magnetic_dev, $
                            auxiliar_voltage_median, auxiliar_voltage_dev, $
                            FORMAT='(A, 4X, F7.2,"+/-",F7.3, 7X, F7.2, "+/-", F7.3)'
PRINT, '------------------------------------------------------------------------'
        print, 'Differences:', MEDIAN(station_magnetic_data_H-auxiliar_magnetic_data_H), $
                               STDDEV(station_magnetic_data_H-auxiliar_magnetic_data_H), $
                               MEDIAN(station_voltage_data_H-auxiliar_voltage_data_H), $
                               STDDEV(station_voltage_data_H-auxiliar_voltage_data_H), $
                               FORMAT='(A, X, F7.2,"+/-",F7.3, 7X, F7.2, "+/-", F7.3)'

        print, ''
PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'
        print, ''

        ;c_magnetic = MEDIAN(station_magnetic_data[magnetic_indexes].H-auxiliar_magnetic_data[magnetic_indexes].H)
        ;c_voltage  = MEDIAN(station_voltage_data[magnetic_indexes].H-auxiliar_voltage_data[magnetic_indexes].H)

        ;magneticfit = LINFIT(station_magnetic_data[magnetic_indexes].H-H_base, $
        ;                   station_voltage_data[magnetic_indexes].H)
        magneticfit = LINFIT(station_magnetic_data_H, $
                             station_voltage_data_H)
        print, 'Station fit b and m [nT/mV]:'
        print, '                            ',magneticfit

PRINT, '------------------------------------------------------------------------'

        auxiliarfit = LINFIT(auxiliar_magnetic_data_H, $
                             auxiliar_voltage_data_H)
        ;auxiliarfit = LINFIT(auxiliar_magnetic_data_H, $
        ;                     auxiliar_voltage_data_H)
        print, 'Auxiliar fit b and m [nT/mV]:'
        print, '                             ', auxiliarfit
PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'
        
        result = LINFIT(auxiliar_magnetic_data_H, $
                        station_magnetic_data_H, $;-c_voltage, $
                        MEASURE_ERRORS=measure_errors, sigma=uncertainties)
        ;result = LINFIT(auxiliar_magnetic_data_H, $
        ;                station_magnetic_data_H, $;-c_voltage, $
        ;                MEASURE_ERRORS=measure_errors, sigma=uncertainties)
                        
        print, '1st line fit:     nT Diff  &  Slope'
        print, '                  ', result, FORMAT='(A,F7.2, 2X, F7.2)'
        print, '    uncertainties:', uncertainties, FORMAT='(A,F7.2, 2X, F7.2)'
        ;print, '      correlation:', correlate(auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic,station_magnetic_data[magnetic_indexes].H-H_base)
        print, '      correlation:', correlate(station_magnetic_data_H, (auxiliar_magnetic_data_H)*result[1]+result[0])
        ;print, '      errors     :', median(auxiliar_magnetic_data[magnetic_indexes].H-H_base-(station_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0])        
        ;result1 = LINFIT((auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic+result[0]/result[1])*result[1], $

        print, ''
        result2 = LINFIT((auxiliar_magnetic_data_H)*result[1]+result[0], $
                        station_magnetic_data_H, $;-c_voltage, $
                        MEASURE_ERRORS=measure_errors, sigma=uncertainties2)
        print, 'fit confirmation:  [near 0]        [near 1]'
        print, result2[0], uncertainties2[0], result2[1], uncertainties2[1], FORMAT='("                  ",F5.2, " +/-", F5.2,2X,F5.2, " +/-", F5.2)'
        ;print, '      correlation:', correlate(auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic,station_magnetic_data[magnetic_indexes].H-H_base)



        ;tmp_auxiliar_magnetic_data = auxiliar_voltage_data[magnetic_indexes].H
        ;tmp_auxiliar_voltage_data  = (auxiliar_voltage_data[magnetic_indexes].H)

        ;tmp_station_voltage_data  = station_voltage_data[magnetic_indexes].H+
        ;tmp_station_magnetic_data = (station_voltage_data[magnetic_indexes].H+result[0]/result[1])*result[1]/auxiliar[1]-auxiliar[0]
        
        ;plot, station_magnetic_data[magnetic_indexes].H-c_magnetic-H_base, linestyle=1
        ;plot, (station_voltage_data[magnetic_indexes].H-c_voltage)
        ;oplot, auxiliar_voltage_data[magnetic_indexes].H
DEVICE, DECOMPOSED = 0
        WINDOW, 0, XSIZE=600, YSIZE=600, TITLE='H - Callibration window'
        !P.MULTI = [0, 2, 2]
        
        ;tmp_arr = station_voltage_data[magnetic_indexes].Ts-auxiliar_voltage_data[magnetic_indexes].Ts

;plot, (tmp_arr[*]), linestyle=0, NSUM=50*24;, YRange=[-600,100.]

;plot, station_voltage_data[magnetic_indexes].Ts, linestyle=1
;oplot, auxiliar_voltage_data[magnetic_indexes].Ts, linestyle=0
LOADCT, /SILENT, 39;, /silent

        Y_max = MAX([station_magnetic_data[magnetic_indexes].H-H_base,(auxiliar_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0]])
        Y_min = MIN([station_magnetic_data[magnetic_indexes].H-H_base,(auxiliar_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0]])
        plot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].H-H_base, linestyle=0, title='nT', $
              YRange=[Y_min,Y_max], /NoData, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].H-H_base, color = 254
        oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0], color = 50;, title='Station mV'


        Y_max = MAX([(station_magnetic_data[magnetic_indexes].H-H_base) , (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        Y_min = MIN([(station_magnetic_data[magnetic_indexes].H-H_base) , (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        plot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].H-H_base, linestyle=0, title='nT vs mV', $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].H-H_base, color = 254
        oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50;, title='Station mV'
        oplot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].H-H_base, color = 254, nsum=(24*60), thick=2
        oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50, nsum=(24*60), thick=2;, 




        Y_max = MAX([station_magnetic_data[magnetic_indexes].H-H_base - (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        Y_min = MIN([station_magnetic_data[magnetic_indexes].H-H_base - (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        plot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].H-H_base - (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), linestyle=0, title='nT vs mV Difference [nT]', $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].H-H_base - (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50, NSUM=60.*24.,thick=2
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].H-H_base - (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 254



        

        Y_max = MAX((station_magnetic_data[magnetic_indexes].H-H_base))
        Y_min = MIN((station_magnetic_data[magnetic_indexes].H-H_base))
        X_max = MAX((auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]))
        X_min = MIN((auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]))
        Y_max = MAX([Y_max,X_max])
        Y_min = MIN([Y_min,X_min])
        X_max = Y_max
        X_min = X_min
        plot, (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), (station_magnetic_data[magnetic_indexes].H-H_base), title='Station vs Auxiliar [nT]', YRange=[Y_min,Y_max], XRange=[X_min,X_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, Xstyle=1, Ystyle=1
        oplot,  (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), (station_magnetic_data[magnetic_indexes].H-H_base), color = 254


LOADCT, /SILENT, 0;, /SILENT

;print, Mean((auxiliar_magnetic_data[magnetic_indexes].H-H_base)/(station_magnetic_data[magnetic_indexes].H-H_base)), STDDEV((auxiliar_magnetic_data[magnetic_indexes].H-H_base)/(station_magnetic_data[magnetic_indexes].H-H_base))

        PRINT, ''
        PRINT, '------------------------------------------------------------------------'
        ;PRINT, '========================================================================'
        PRINT, '------ H component Results ---------------------------------------------'
        PRINT, ' New proportional constant [nT/mV] & new offset [mV] for AUXLIAR GMS'
        PRINT, auxiliarfit[1]/result[1], (result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]
        PRINT, '------------------------------------------------------------------------'
        PRINT, ' Differences     ......:::::......      K[nT]: 00[4.8], 03[5.5], 07[6.5]'
        PRINT, '      Medians +/- STDDEV'
        PRINT, '               [near 0]  [K <= 07]'
        print, '[nT vs mV]:',  MEDIAN((station_magnetic_data[magnetic_indexes].H-H_base) - ( (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]) )), $
                               STDDEV((station_magnetic_data[magnetic_indexes].H-H_base) - ( (auxiliar_voltage_data[magnetic_indexes].H+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]) )), $
                               FORMAT='(A, X, F7.2," +/-",F7.3)'
        print, '[nT vs nT]:',  MEDIAN((station_magnetic_data[magnetic_indexes].H-H_base)-((auxiliar_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0])), $
                               STDDEV((station_magnetic_data[magnetic_indexes].H-H_base)-((auxiliar_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0])), $
                               FORMAT='(A, X, F7.2," +/-",F7.3)'
PRINT, '------------------------------------------------------------------------'
PRINT, '========================================================================'
        PRINT, ''


LOADCT, /SILENT, 0
ENDIF







;plot, auxiliar_magnetic_data[magnetic_indexes].H-H_base, linestyle=0

;plot, station_magnetic_data[magnetic_indexes].H-H_base, linestyle=0

;print, N_ELEMENTS(magnetic_indexes), N_ELEMENTS(station_magnetic_data[*].H), N_ELEMENTS(station_voltage_data[*].H)
;for i=0, N_ELEMENTS(magnetic_indexes)-1 DO print, station_voltage_data[i].H, station_magnetic_data[i].H
;print, MAX((station_magnetic_data[*].H-H_base)*valid_indexes), MAX((station_voltage_data[*].H)*valid_indexes)
;print, MIN((station_magnetic_data[*].H-H_base)*valid_indexes), MIN((station_voltage_data[*].H)*valid_indexes)
IF keyword_set(D) THEN BEGIN
PRINT, '========================================================================'
PRINT, ' D - component analysis'
PRINT, '------------------------------------------------------------------------'

        valid_indexes       = (station_magnetic_data[*].D LE 90.0) AND (auxiliar_magnetic_data[*].D LE 90.0) AND $
                              (station_voltage_data[*].Ts GE 180.0) AND (auxiliar_voltage_data[*].Ts GE 180.0)

        
        magnetic_indexes       = WHERE(station_magnetic_data[*].D LT 90.0 AND auxiliar_magnetic_data[*].D LT 90.0 )
        magnetic_indexes0      = WHERE(valid_indexes EQ 1)
        PRINT, ''
        PRINT, '   Correlations Station vs Auxiliar:'
        PRINT, '                  D [Deg]       D [mV]'

        corr1 = CORRELATE(station_magnetic_data[magnetic_indexes].D-D_base, auxiliar_magnetic_data[magnetic_indexes].D-D_base)
        corr2 = CORRELATE(station_voltage_data[magnetic_indexes].D, auxiliar_voltage_data[magnetic_indexes].D)
        PRINT, corr1, corr2, FORMAT='(18X,F6.4,7X,F6.4, "  *** non-gap data ")'
        ;print, magnetic_indexes
        corr1 = CORRELATE(station_magnetic_data[magnetic_indexes0].D-D_base, auxiliar_magnetic_data[magnetic_indexes0].D-D_base)
        corr2 = CORRELATE(station_voltage_data[magnetic_indexes0].D, auxiliar_voltage_data[magnetic_indexes0].D)
        PRINT, corr1, corr2, FORMAT='(18X,F6.4,7X,F6.4, "  *** non-gap & temp data ")'
        ;print, TOTAL(ABS(station_magnetic_data[magnetic_indexes].H-auxiliar_magnetic_data[magnetic_indexes].H))/TOTAL(station_magnetic_data[magnetic_indexes].H)
        ;print, CORRELATE(station_magnetic_data[magnetic_indexes].H-H_base, (auxiliar_magnetic_data[magnetic_indexes].H-H_base)/corr1), $
        ;       CORRELATE((station_magnetic_data[*].H-H_base)*valid_indexes, (auxiliar_magnetic_data[*].H-H_base)*valid_indexes/corr2)
        PRINT, ''
        PRINT, ''
        station_magnetic_mean  = MEDIAN(station_magnetic_data[magnetic_indexes].D-D_base)
        station_voltage_mean   = MEDIAN(station_voltage_data[magnetic_indexes].D)
        auxiliar_magnetic_mean = MEDIAN(auxiliar_magnetic_data[magnetic_indexes].D-D_base)
        auxiliar_voltage_mean  = MEDIAN(auxiliar_voltage_data[magnetic_indexes].D)



        coeff_magnetic_station  = DETREND(station_magnetic_data[magnetic_indexes].D-D_base, station_magnetic_data_D, ORDER=2)
        coeff_magnetic_auxiliar = DETREND(auxiliar_magnetic_data[magnetic_indexes].D-D_base, auxiliar_magnetic_data_D, ORDER=2)
        coeff_voltage_station   = DETREND(station_voltage_data[magnetic_indexes].D, station_voltage_data_D, ORDER=2)
        coeff_voltage_auxiliar  = DETREND(auxiliar_voltage_data[magnetic_indexes].D, auxiliar_voltage_data_D, ORDER=2)

        station_magnetic_median  = MEDIAN(station_magnetic_data[magnetic_indexes].D-D_base)
        station_magnetic_dev     = STDDEV(station_magnetic_data[magnetic_indexes].D-D_base)
        station_voltage_median   = MEDIAN(station_voltage_data[magnetic_indexes].D)
        station_voltage_dev      = STDDEV(station_voltage_data[magnetic_indexes].D)
        auxiliar_magnetic_median = MEDIAN(auxiliar_magnetic_data[magnetic_indexes].D-D_base)
        auxiliar_magnetic_dev    = STDDEV(auxiliar_magnetic_data[magnetic_indexes].D-D_base)
        auxiliar_voltage_median  = MEDIAN(auxiliar_voltage_data[magnetic_indexes].D)
        auxiliar_voltage_dev     = STDDEV(auxiliar_voltage_data[magnetic_indexes].D)

        PRINT, 'Medians +/- STDDEV:  [Deg]                [mV]'
        print, ''
        PRINT, 'STATION:', station_magnetic_median, station_magnetic_dev, $
                           station_voltage_median, station_voltage_dev, $
                           FORMAT='(A, 5X, F7.2,"+/-",F7.3, 7X, F7.2, "+/-", F7.3)'
        print, 'Auxiliar:', auxiliar_magnetic_median, auxiliar_magnetic_dev, $
                            auxiliar_voltage_median, auxiliar_voltage_dev, $
                            FORMAT='(A, 4X, F7.2,"+/-",F7.3, 7X, F7.2, "+/-", F7.3)'
PRINT, '------------------------------------------------------------------------'
        print, 'Differences:', MEDIAN(station_magnetic_data[magnetic_indexes].D-auxiliar_magnetic_data[magnetic_indexes].D), $
                               STDDEV(station_magnetic_data[magnetic_indexes].D-auxiliar_magnetic_data[magnetic_indexes].D), $
                               MEDIAN(station_voltage_data[magnetic_indexes].D-auxiliar_voltage_data[magnetic_indexes].D), $
                               STDDEV(station_voltage_data[magnetic_indexes].D-auxiliar_voltage_data[magnetic_indexes].D), $
                               FORMAT='(A, X, F7.2,"+/-",F7.3, 7X, F7.2, "+/-", F7.3)'
        print, ''
PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'
        print, ''

        ;c_magnetic = MEDIAN(station_magnetic_data[magnetic_indexes].H-auxiliar_magnetic_data[magnetic_indexes].H)
        ;c_voltage  = MEDIAN(station_voltage_data[magnetic_indexes].H-auxiliar_voltage_data[magnetic_indexes].H)

        ;magneticfit = LINFIT(station_magnetic_data[magnetic_indexes].H-H_base, $
        ;                   station_voltage_data[magnetic_indexes].H)
        magneticfit = LINFIT(station_magnetic_data[magnetic_indexes].D-D_base, $
                           station_voltage_data[magnetic_indexes].D)
        print, 'Station fit b [mV] and m [Deg/mV]:'
        print, '                            ',magneticfit[0], magneticfit[1]/479.04

PRINT, '------------------------------------------------------------------------'

        auxiliarfit = LINFIT(auxiliar_magnetic_data[magnetic_indexes].D-D_base, $
                             auxiliar_voltage_data[magnetic_indexes].D)
        ;auxiliarfit = LINFIT(auxiliar_magnetic_data_H, $
        ;                     auxiliar_voltage_data_H)
        print, 'Auxiliar fit b [mV] and m [Deg/mV]:'
        print, '                             ', auxiliarfit[0], auxiliarfit[1]/479.04

PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'
        
        result = LINFIT(auxiliar_magnetic_data[magnetic_indexes].D-D_base, $
                        station_magnetic_data[magnetic_indexes].D-D_base, $;-c_voltage, $
                        MEASURE_ERRORS=measure_errors, sigma=uncertainties)
        ;result = LINFIT(auxiliar_magnetic_data_H, $
        ;                station_magnetic_data_H, $;-c_voltage, $
        ;                MEASURE_ERRORS=measure_errors, sigma=uncertainties)
                        
        print, '1st line fit: ArcSec Diff  &  Slope'
        print, '                  ', result, FORMAT='(A,F7.2, 2X, F7.2)'
        print, '    uncertainties:', uncertainties, FORMAT='(A,F7.2, 2X, F7.2)'
        ;print, '      correlation:', correlate(auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic,station_magnetic_data[magnetic_indexes].H-H_base)
        print, '      correlation:', correlate(station_magnetic_data[magnetic_indexes].D-D_base, (auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0])
        ;print, '      errors     :', median(auxiliar_magnetic_data[magnetic_indexes].H-H_base-(station_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0])        
        ;result1 = LINFIT((auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic+result[0]/result[1])*result[1], $

        print, ''
        result2 = LINFIT((auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0], $
                        station_magnetic_data[magnetic_indexes].D-D_base, $;-c_voltage, $
                        MEASURE_ERRORS=measure_errors, sigma=uncertainties2)
        print, 'fit confirmation:  [near 0]        [near 1]'
        print, result2[0], uncertainties2[0], result2[1], uncertainties2[1], FORMAT='("                  ",F5.2, " +/-", F5.2,2X,F5.2, " +/-", F5.2)'
        ;print, '      correlation:', correlate(auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic,station_magnetic_data[magnetic_indexes].H-H_base)





        ;tmp_auxiliar_magnetic_data = auxiliar_voltage_data[magnetic_indexes].H
        ;tmp_auxiliar_voltage_data  = (auxiliar_voltage_data[magnetic_indexes].H)

        ;tmp_station_voltage_data  = station_voltage_data[magnetic_indexes].H+
        ;tmp_station_magnetic_data = (station_voltage_data[magnetic_indexes].H+result[0]/result[1])*result[1]/auxiliar[1]-auxiliar[0]
        
        ;plot, station_magnetic_data[magnetic_indexes].H-c_magnetic-H_base, linestyle=1
        ;plot, (station_voltage_data[magnetic_indexes].H-c_voltage)
        ;oplot, auxiliar_voltage_data[magnetic_indexes].H
DEVICE, DECOMPOSED = 0
        WINDOW, 0, XSIZE=600, YSIZE=600, TITLE='D - Callibration window'
        !P.MULTI = [0, 2, 2]
        
        ;tmp_arr = station_voltage_data[magnetic_indexes].Ts-auxiliar_voltage_data[magnetic_indexes].Ts

;plot, (tmp_arr[*]), linestyle=0, NSUM=50*24;, YRange=[-600,100.]

;plot, station_voltage_data[magnetic_indexes].Ts, linestyle=1
;oplot, auxiliar_voltage_data[magnetic_indexes].Ts, linestyle=0
LOADCT, /SILENT, 39

        Y_max = MAX([station_magnetic_data[magnetic_indexes].D-D_base, ((auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0])])
        Y_min = MIN([station_magnetic_data[magnetic_indexes].D-D_base, ((auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0])])
        plot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].D-D_base, linestyle=0, title='ArcSec vs ArcSec', $
              YRange=[Y_min,Y_max], /NoData, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].D-D_base, color = 254
        oplot, time_array[magnetic_indexes]/(24*60), ((auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0]), color = 50;, title='Station mV'
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].D-D_base, color = 254, nsum=(24*60), thick=2
        oplot, time_array[magnetic_indexes]/(24*60), ((auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0]), color = 50, nsum=(24*60), thick=2;, title='Station mV'



        Y_max = MAX([(station_magnetic_data[magnetic_indexes].D-D_base) , (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        Y_min = MIN([(station_magnetic_data[magnetic_indexes].D-D_base) , (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        plot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].D-D_base, linestyle=0, title='ArcSec vs mV', $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].D-D_base, color = 254
        oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50;, title='Station mV'
        oplot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].D-D_base, color = 254, nsum=(24*60), thick=2
        oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50, nsum=(24*60), thick=2;, title='Station mV'





        Y_max = MAX([station_magnetic_data[magnetic_indexes].D-D_base - (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        Y_min = MIN([station_magnetic_data[magnetic_indexes].D-D_base - (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        plot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].D-D_base - (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), linestyle=0, title='Station-Auxiliar Difference [nT]', $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].D-D_base - (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50, NSUM=60.*24.,thick=2
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].D-D_base - (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 254




        Y_max = MAX((station_magnetic_data[magnetic_indexes].D-D_base))
        Y_min = MIN((station_magnetic_data[magnetic_indexes].D-D_base))
        X_max = MAX((auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]))
        X_min = MIN((auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]))
        Y_max = MAX([Y_max,X_max])
        Y_min = MIN([Y_min,X_min])
        X_max = Y_max
        X_min = X_min
        plot, (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), (station_magnetic_data[magnetic_indexes].D-D_base), title='Station vs Auxiliar [nT]', YRange=[Y_min,Y_max], XRange=[X_min,X_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, Xstyle=1
        oplot,  (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), (station_magnetic_data[magnetic_indexes].D-D_base), color = 254


LOADCT, /SILENT, 0

;print, Mean((auxiliar_magnetic_data[magnetic_indexes].H-H_base)/(station_magnetic_data[magnetic_indexes].H-H_base)), STDDEV((auxiliar_magnetic_data[magnetic_indexes].H-H_base)/(station_magnetic_data[magnetic_indexes].H-H_base))


        PRINT, ''
        PRINT, '------------------------------------------------------------------------'
        ;PRINT, '========================================================================'
        PRINT, '------ D component Results ---------------------------------------------'
        PRINT, ' New proportional constant [ArcSec/mV] & new offset [mV] for AUXLIAR GMS'
        PRINT, auxiliarfit[1]/result[1]/479.04, (result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]
        PRINT, '------------------------------------------------------------------------'
        PRINT, ' Differences     ......:::::......      K[nT]: 00[4.8], 03[5.5], 07[6.5]'
        PRINT, '      Medians +/- STDDEV'
        PRINT, '      [near 0]     [<= K=07]'
        print, '[nT vs mV]:',  MEDIAN((station_magnetic_data[magnetic_indexes].D-D_base) - ( (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]) )), $
                               STDDEV((station_magnetic_data[magnetic_indexes].D-D_base) - ( (auxiliar_voltage_data[magnetic_indexes].D+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]) )), $
                               FORMAT='(A, X, F7.2," +/-",F7.3)'
        print, '[nT vs nT]:',  MEDIAN((station_magnetic_data[magnetic_indexes].D-D_base)-((auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0])), $
                               STDDEV((station_magnetic_data[magnetic_indexes].D-D_base)-((auxiliar_magnetic_data[magnetic_indexes].D-D_base)*result[1]+result[0])), $
                               FORMAT='(A, X, F7.2," +/-",F7.3)'
PRINT, '------------------------------------------------------------------------'
PRINT, '========================================================================'
        PRINT, ''

ENDIF















IF keyword_set(Z) THEN BEGIN
PRINT, '========================================================================'
PRINT, ' Z - component analysis'
PRINT, '------------------------------------------------------------------------'

        IF offset_tc EQ 0. THEN BEGIN
                PRINT, 'WARNING:'
                PRINT, '        Null value of Offset-Tcontol detected!'
                PRINT, ''
                PRINT, '        Since Z component depends on unit contol temperature'
                PRINT, '        it is recomendable to calibrate tempratures before'
                PRINT, '        the calibration of Z-component.'
                PRINT, ''
        ENDIF ELSE BEGIN
                PRINT, 'WARNING:'
                PRINT, offset_tc, Tz_factor, FORMAT='("        Offset-Tcontol = ", F7.2, " and Tz_factor = ", F5.2)'
                PRINT, ''
                PRINT, offset_tc*Tz_factor, FORMAT='("        Offset_Tc and Tz_factor product (", F7.2, ") is going to be included")'
                PRINT, '        on Z-component calibration process.'
                PRINT, ''
        ENDELSE

        valid_indexes       = (station_magnetic_data[*].Z LE 99999.0) AND (auxiliar_magnetic_data[*].Z LE 99999.0) AND $
                              (station_voltage_data[*].Ts GE 180.0) AND (auxiliar_voltage_data[*].Ts GE 180.0)

        
        magnetic_indexes       = WHERE(station_magnetic_data[*].Z LT 99999.0 AND auxiliar_magnetic_data[*].Z LT 99999.0 )
        magnetic_indexes0      = WHERE(valid_indexes EQ 1)
        PRINT, ''
        PRINT, '   Correlations Station vs Auxiliar:'
        PRINT, '                  Z [nT]       Z [mV]'

        corr1 = CORRELATE(station_magnetic_data[magnetic_indexes].Z-Z_base, auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)
        corr2 = CORRELATE(station_voltage_data[magnetic_indexes].Z, auxiliar_voltage_data[magnetic_indexes].Z)
        PRINT, corr1, corr2, FORMAT='(18X,F6.4,7X,F6.4, "  *** non-gap data ")'
        ;print, magnetic_indexes
        corr1 = CORRELATE(station_magnetic_data[magnetic_indexes0].Z-Z_base, auxiliar_magnetic_data[magnetic_indexes0].Z-Z_base)
        corr2 = CORRELATE(station_voltage_data[magnetic_indexes0].Z, auxiliar_voltage_data[magnetic_indexes0].Z)
        PRINT, corr1, corr2, FORMAT='(18X,F6.4,7X,F6.4, "  *** non-gap & temp data ")'
        ;print, TOTAL(ABS(station_magnetic_data[magnetic_indexes].H-auxiliar_magnetic_data[magnetic_indexes].H))/TOTAL(station_magnetic_data[magnetic_indexes].H)
        ;print, CORRELATE(station_magnetic_data[magnetic_indexes].H-H_base, (auxiliar_magnetic_data[magnetic_indexes].H-H_base)/corr1), $
        ;       CORRELATE((station_magnetic_data[*].H-H_base)*valid_indexes, (auxiliar_magnetic_data[*].H-H_base)*valid_indexes/corr2)
        PRINT, ''
        PRINT, ''

        coeff_magnetic_station  = DETREND(station_magnetic_data[magnetic_indexes].Z-Z_base, station_magnetic_data_Z, ORDER=2)
        coeff_magnetic_auxiliar = DETREND(auxiliar_magnetic_data[magnetic_indexes].Z-Z_base, auxiliar_magnetic_data_Z, ORDER=2)
        coeff_voltage_station   = DETREND(station_voltage_data[magnetic_indexes].Z, station_voltage_data_Z, ORDER=2)
        coeff_voltage_auxiliar  = DETREND(auxiliar_voltage_data[magnetic_indexes].Z, auxiliar_voltage_data_Z, ORDER=2)


        station_magnetic_median  = MEDIAN(station_magnetic_data[magnetic_indexes].Z-Z_base)
        station_magnetic_dev     = STDDEV(station_magnetic_data[magnetic_indexes].Z-Z_base)
        station_voltage_median   = MEDIAN(station_voltage_data[magnetic_indexes].Z)
        station_voltage_dev      = STDDEV(station_voltage_data[magnetic_indexes].Z)
        auxiliar_magnetic_median = MEDIAN(auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)
        auxiliar_magnetic_dev    = STDDEV(auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)
        auxiliar_voltage_median  = MEDIAN(auxiliar_voltage_data[magnetic_indexes0].Z)
        auxiliar_voltage_dev     = STDDEV(auxiliar_voltage_data[magnetic_indexes0].Z)


        PRINT, 'Medians +/- STDDEV:  Z [nT]       Z [mV]'
        print, ''
        PRINT, 'STATION:', station_magnetic_median, station_magnetic_dev, $
                           station_voltage_median, station_voltage_dev, $
                           FORMAT='(A, 5X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
        print, 'Auxiliar:', auxiliar_magnetic_median, auxiliar_magnetic_dev, $
                            auxiliar_voltage_median, auxiliar_voltage_dev, $
                            FORMAT='(A, 4X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
PRINT, '------------------------------------------------------------------------'
        print, 'Differences:', MEDIAN(station_magnetic_data[magnetic_indexes].Z-auxiliar_magnetic_data[magnetic_indexes].Z), $
                               STDDEV(station_magnetic_data[magnetic_indexes].Z-auxiliar_magnetic_data[magnetic_indexes].Z), $
                               MEDIAN(station_voltage_data[magnetic_indexes].Z-auxiliar_voltage_data[magnetic_indexes].Z), $
                               STDDEV(station_voltage_data[magnetic_indexes].Z-auxiliar_voltage_data[magnetic_indexes].Z), $
                               FORMAT='(A, X, F7.2," +/-",F7.3, 7X, F7.2, " +/-", F7.3)'
        print, ''
        print, ''
PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'
        print, ''

        ;c_magnetic = MEDIAN(station_magnetic_data[magnetic_indexes].H-auxiliar_magnetic_data[magnetic_indexes].H)
        ;c_voltage  = MEDIAN(station_voltage_data[magnetic_indexes].H-auxiliar_voltage_data[magnetic_indexes].H)
        

        ;magneticfit = LINFIT(station_magnetic_data[magnetic_indexes].H-H_base, $
        ;                   station_voltage_data[magnetic_indexes].H)

;Z(mV) = ValCte1*[ValorADC_Z] + ValCte2   + (Temp_Ref - TC(mV))*TZ_factor + [Value OFFSET_Z in Setuplog.cfg file]


        magneticfit = LINFIT(station_magnetic_data[magnetic_indexes].Z-Z_base, $
                           station_voltage_data[magnetic_indexes].Z)
        print, 'Station fit b [mV] and m [nT/mV]:'
        print, '                            ',magneticfit[0], magneticfit[1]

PRINT, '------------------------------------------------------------------------'

        auxiliarfit = LINFIT(auxiliar_magnetic_data[magnetic_indexes].Z-Z_base, $
                             auxiliar_voltage_data[magnetic_indexes].Z-offset_tc*Tz_factor)
        ;auxiliarfit = LINFIT(auxiliar_magnetic_data_H, $
        ;                     auxiliar_voltage_data_H)
        print, 'Auxiliar fit b [mV] and m [nT/mV]:'
        print, '                             ', auxiliarfit[0], auxiliarfit[1]

PRINT, '------------------------------------------------------------------------'
PRINT, '------------------------------------------------------------------------'
        
        result = LINFIT(auxiliar_magnetic_data[magnetic_indexes].Z-Z_base, $
                        station_magnetic_data[magnetic_indexes].Z-Z_base, $;-c_voltage, $
                        MEASURE_ERRORS=measure_errors, sigma=uncertainties)
        ;result = LINFIT(auxiliar_magnetic_data_H, $
        ;                station_magnetic_data_H, $;-c_voltage, $
        ;                MEASURE_ERRORS=measure_errors, sigma=uncertainties)
                        
        print, '1st line fit:     nT Diff  &  Slope'
        print, '                  ', result, FORMAT='(A,F7.2, 2X, F7.2)'
        print, '    uncertainties:', uncertainties, FORMAT='(A,F7.2, 2X, F7.2)'
        ;print, '      correlation:', correlate(auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic,station_magnetic_data[magnetic_indexes].H-H_base)
        print, '      correlation:', correlate(station_magnetic_data[magnetic_indexes].Z-Z_base, (auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0])
        ;print, '      errors     :', median(auxiliar_magnetic_data[magnetic_indexes].H-H_base-(station_magnetic_data[magnetic_indexes].H-H_base)*result[1]+result[0])        
        ;result1 = LINFIT((auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic+result[0]/result[1])*result[1], $

        print, ''
        result2 = LINFIT((auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0], $
                        station_magnetic_data[magnetic_indexes].Z-Z_base, $;-c_voltage, $
                        MEASURE_ERRORS=measure_errors, sigma=uncertainties2)
        print, 'fit confirmation:  [near 0]        [near 1]'
        print, result2[0], uncertainties2[0], result2[1], uncertainties2[1], FORMAT='("                  ",F5.2, " +/-", F5.2,2X,F5.2, " +/-", F5.2)'
        ;print, '      correlation:', correlate(auxiliar_magnetic_data[magnetic_indexes].H-H_base+c_magnetic,station_magnetic_data[magnetic_indexes].H-H_base)








DEVICE, DECOMPOSED = 0
        WINDOW, 0, XSIZE=600, YSIZE=600, TITLE='Z - Callibration window'
        !P.MULTI = [0, 2, 2]
        
        ;tmp_arr = station_voltage_data[magnetic_indexes].Ts-auxiliar_voltage_data[magnetic_indexes].Ts

;plot, (tmp_arr[*]), linestyle=0, NSUM=50*24;, YRange=[-600,100.]

;plot, station_voltage_data[magnetic_indexes].Ts, linestyle=1
;oplot, auxiliar_voltage_data[magnetic_indexes].Ts, linestyle=0
LOADCT, /SILENT, 39

        Y_max = MAX([station_magnetic_data[magnetic_indexes].Z-Z_base, ((auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0])])
        Y_min = MIN([station_magnetic_data[magnetic_indexes].Z-Z_base, ((auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0])])
        plot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].Z-Z_base, linestyle=0, title='ArcSec vs ArcSec', $
              YRange=[Y_min,Y_max], /NoData, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].Z-Z_base, color = 254
        oplot, time_array[magnetic_indexes]/(24*60), ((auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0]), color = 50;, title='Station mV'
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].Z-Z_base, color = 254, nsum=(24*60), thick=2
        oplot, time_array[magnetic_indexes]/(24*60), ((auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0]), color = 50, nsum=(24*60), thick=2;, title='Station mV'



        Y_max = MAX([(station_magnetic_data[magnetic_indexes].Z-Z_base) , (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        Y_min = MIN([(station_magnetic_data[magnetic_indexes].Z-Z_base) , (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        plot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].Z-Z_base, linestyle=0, title='ArcSec vs mV', $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].Z-Z_base, color = 254
        oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50;, title='Station mV'
        oplot, time_array[magnetic_indexes]/(24*60),station_magnetic_data[magnetic_indexes].Z-Z_base, color = 254, nsum=(24*60), thick=2
        oplot, time_array[magnetic_indexes]/(24*60), (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50, nsum=(24*60), thick=2;, title='Station mV'





        Y_max = MAX([station_magnetic_data[magnetic_indexes].Z-Z_base - (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        Y_min = MIN([station_magnetic_data[magnetic_indexes].Z-Z_base - (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1])])
        plot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].Z-Z_base - (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), linestyle=0, title='Station-Auxiliar Difference [nT]', $
              YRange=[Y_min,Y_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, XRANGE=[X_min, X_max], Xstyle=1
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].Z-Z_base - (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 50, NSUM=60.*24.,thick=2
        oplot, time_array[magnetic_indexes]/(24*60), station_magnetic_data[magnetic_indexes].Z-Z_base - (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), color = 254




        Y_max = MAX((station_magnetic_data[magnetic_indexes].Z-Z_base))
        Y_min = MIN((station_magnetic_data[magnetic_indexes].Z-Z_base))
        X_max = MAX((auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]))
        X_min = MIN((auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]))
        Y_max = MAX([Y_max,X_max])
        Y_min = MIN([Y_min,X_min])
        X_max = Y_max
        X_min = X_min
        plot, (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), (station_magnetic_data[magnetic_indexes].Z-Z_base), title='Station vs Auxiliar [nT]', YRange=[Y_min,Y_max], XRange=[X_min,X_max], /NODATA, Color=0, background=255, CHARSIZE=1.0, Xstyle=1, Ystyle=1
        oplot,  (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]))/(auxiliarfit[1]/result[1]), (station_magnetic_data[magnetic_indexes].Z-Z_base), color = 254

LOADCT, /SILENT, 0

;print, Mean((auxiliar_magnetic_data[magnetic_indexes].H-H_base)/(station_magnetic_data[magnetic_indexes].H-H_base)), STDDEV((auxiliar_magnetic_data[magnetic_indexes].H-H_base)/(station_magnetic_data[magnetic_indexes].H-H_base))

        PRINT, ''
        PRINT, '------------------------------------------------------------------------'
        ;PRINT, '========================================================================'
        PRINT, '------ Z component Results ---------------------------------------------'
        PRINT, ' New proportional constant     [nT/mV] & new offset [mV] for AUXLIAR GMS'
        PRINT, auxiliarfit[1]/result[1], (result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]-Tz_factor*offset_tc
        PRINT, '------------------------------------------------------------------------'
        PRINT, ' Differences     ......:::::......      K[nT]: 00[4.8], 03[5.5], 07[6.5]'
        PRINT, '      Medians +/- STDDEV'
        PRINT, '              [near 0]   [K<=07]'
        print, '[nT vs mV]:',  MEDIAN((station_magnetic_data[magnetic_indexes].Z-Z_base) - ( (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]-Tz_factor*offset_tc))/(auxiliarfit[1]/result[1]) )), $
                               STDDEV((station_magnetic_data[magnetic_indexes].Z-Z_base) - ( (auxiliar_voltage_data[magnetic_indexes].Z+((result[0]*auxiliarfit[1])/result[1]-auxiliarfit[0]-Tz_factor*offset_tc))/(auxiliarfit[1]/result[1]) )), $
                               FORMAT='(A, X, F7.2," +/-",F7.3)'
        print, '[nT vs nT]:',  MEDIAN((station_magnetic_data[magnetic_indexes].Z-Z_base)-((auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0])), $
                               STDDEV((station_magnetic_data[magnetic_indexes].Z-Z_base)-((auxiliar_magnetic_data[magnetic_indexes].Z-Z_base)*result[1]+result[0])), $
                               FORMAT='(A, X, F7.2," +/-",F7.3)'
PRINT, '------------------------------------------------------------------------'
PRINT, '========================================================================'
        PRINT, ''


LOADCT, /SILENT, 0
ENDIF



RETURN


END




