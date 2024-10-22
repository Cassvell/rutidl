;+
; NAME:
;       geomagixs_voltagedata_makemagneticfile.pro
;
;
; PURPOSE:
;
;       make magnetic data file from voltage files
;
; AUTHOR:
;
;       Pedro Corona Romero
;       Insituto de Geofisica, Unidad Michoacan
;       UNAM, Campus Morelia
;       Antigua Carretera a Patzcuaro, Morelia, Michoacan
;       piter.cr@gmail.com
;       Mexico, 11.i.mmxxiii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;
;       geomagixs_voltagedata_makemagneticfile, initial_date, final_date [, STATION=GMS_name, /QUIET, /FORCE_ALL, /HRES]
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
;       HRES                                           : computes high resolution files (second sampling)
;
; DEPENDENCIES:
;       geomagixs system
;
; INPUT FILES:
;       GMSDDmmm.YYv[.hres]     [voltage data-files from variometers]
;
; OUTPUT FILES:
;       GMSDDmmm.YYm[.hres]     [mangetic data-files from variometers]
;
; HISTORIA:
;               11/01/2023      developed from geomagixs_magneticdata_transformintermagnetfile
;
;-

;##############################################################################
;##############################################################################
;##############################################################################
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




PRO undefining_variable, var0
;
; MODIFICATION HISTORY:
;       Written by David W. Fanning, 8 June 97, from an original program
;       given to me by Andrew Cool, DSTO, Adelaide, Australia.
;       Simplified program so you can pass it an undefined variable. :-) 17 May 2000. DWF
;       Simplified it even more by removing the unnecessary SIZE function. 28 June 2002. DWF.
;       Added capability to delete up to 10 variables at suggestion of Craig Markwardt. 10 Jan 2008. DWF.
;       If the variable is a pointer, object or structure reference the variable is recursively traversed
;          to free up all variables pointed to before the variable is itself destroyed. 10 June 2009. DWF.
;       Updated to allow undefining of pointer arrays. 8 October 2009. DWF.
;       Adapted for this program. 11 January 2023. PCR.
;******************************************************************************************;
;  Copyright (c) 2008 - 2009, by Fanning Software Consulting, Inc.                         ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
   
   IF N_Elements(var0) NE 0 THEN BEGIN
        dataType = Size(var0, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var0)-1 DO BEGIN
              IF Ptr_Valid(var0[j]) THEN Undefine, *var0[j]
              Ptr_Free, var0[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var0
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var0)-1 DO Undefine, var0.(j)
        var0 = 0
        dummy = Temporary(var0)
   ENDIF

   RETURN
END





FUNCTION getting_voltagedata, initial, STATION=station, $
                                       QUIET=quiet, $
                                       HRES=hres
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
        extention       = keyword_set(hres) ? 'v.hres' : 'v'
        month_txt       = getting_month_name(initial[*])

        file_name       = gms[system.gms].code+string(initial_day, FORMAT='(I02)')+month_txt+'.'+string(initial_year MOD 1000, FORMAT='(I02)')+extention
        output_path     = system.datasource_dir+gms[system.gms].name+'/'

        file = FILE_SEARCH(output_path+file_name, COUNT=opened_files)
        IF opened_files NE N_ELEMENTS(file_name) THEN MESSAGE, 'Error finding data files or directories!'
        
        number_of_lines = FILE_LINES(file[0])
        magnetic_data   = STRARR(number_of_lines)
        
;print,file_name
;print, number_of_lines

                ;tmp_data = STRARR(number_of_lines)

        OPENR, lun, file, /GET_LUN, ERROR=err
                IF err NE 0 THEN MESSAGE, 'Error opening '+file_name
                READF, lun, magnetic_data, FORMAT='(A)'
        CLOSE, lun
        FREE_LUN, lun
;help, magnetic_data
;##############################################################################
; extracting data
;##############################################################################
;02 07 2022 00 00 00 +0578.063 -0040.775 -0684.538 +0284.103 +0278.259
;02 07 2022 00 00 +0576.967 -0040.725 -0685.448 +0291.232 +0253.352

        IF keyword_set(hres) THEN BEGIN
                DataStruct  =  { day : 0, month : 0, year : 0, $
                                 HH : 0,  MM: 0, SS : 0, $
                                 H : 0., D : 0., Z : 0., Tc : 0., Ts: 0. }
                ReadingFormat = '(I2,X,I2,X,I4,X,I2,X,I2,X,I2,X,F9,X,F9,X,F9,X,F9,X,F9)'
        ENDIF ELSE BEGIN
                DataStruct  =  { day : 0, month : 0, year : 0, $
                                 HH : 0,  MM: 0, $
                                 H : 0., D : 0., Z : 0., Tc : 0., Ts:0. }
                ReadingFormat = '(I2,X,I2,X,I4,X,I2,X,I2,X,F9,X,F9,X,F9,X,F9,X,F9)'
        ENDELSE
;02 07 2022 00 00 00 +0578.063 -0040.775 -0684.538 +0284.103 +0278.259
;I2,X,I2,X,I4,X,I2,X,I2,X,I2,X,F9,X,F9,X,F9,X,F9,X,F9
;I2,X,I2,X,I4,X,I2,X,I2,X,I2,5(X,F9)
;12 3 45 6
;        7890 1 23 4 56 7 89
                         
        ;minutes_per_day = 24*60
        ;initial_minutes = initial_hour*60+initial_minute
        ;final_minutes   = final_hour*60+final_minute 
        ;data_number     = fix(total_of_data-initial_minutes-minutes_per_day+final_minutes)
        header         = 4
;help, number_of_lines
;for i = header, number_of_lines-1 DO print, i, STRLEN(magnetic_data[i])
        
        ;return, 0
        ;print, num, 0ber_of_lines-header
        resulting_data = REPLICATE(DataStruct, number_of_lines-header)

        READS, magnetic_data[header:number_of_lines-1], resulting_data, $
               FORMAT=ReadingFormat
        
        tempvar = SIZE(TEMPORARY(magnetic_data)) ; liberar memoria de la info no usada
        undefining_variable, DataStruct          ; undefines DataStruct


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
; FUNCION AUXILIAR
PRO making_magneticfile, file_date, STATION=station, $
                                    QUIET=quiet, $
                                    HRES=hres
                



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
        tmp_julday  = JULDAY(initial_month, initial_day, initial_year)
                
        CALDAT, tmp_julday, tmp_month, tmp_day, tmp_year
        
        doy              = JULDAY(initial_month,initial_day, initial_year)-JULDAY(1,0,initial_year)
        ;header           = 4
        header           = 18LL
        header_string    = STRARR(header)

        ;tmp_string       = keyword_set(hres) ? '> 1 Second. Reported data' : '> 1 Min. Reported data'
        ;header_string[0] = STRUPCASE(gms[system.gms].name)+' <'+STRING(doy, FORMAT='(I03)')+tmp_string
        ;header_string[1] = ''
        ;tmp_string       = keyword_set(hres) ? ' SS' : ''
        ;header_string[2] = 'DD MM YYYY HH MM'+tmp_string+'   D(Deg)   H(nT)   Z(nT)  I(Deg)  F(nT)'
        ;header_string[3] = ''



        header_string[0]          =' FORMAT                 IAGA-2002x (Extended IAGA2002 Format)                         |'
        header_string[1]          =' Source of Data         Space Weather National Laboratory (LANCE), UNAM               |'
        str_tmp1 = ''
        for i = 0, 61 - STRLEN(gms[system.gms].name) DO str_tmp1+=' '
        header_string[2]          =' Station Name           '+ STRUPCASE(gms[system.gms].name)+str_tmp1+'|'
        header_string[3]          =' IAGA CODE              '+STRUPCASE(gms[system.gms].code)+'                                                           |'
        header_string[4]          =' Geodetic Latitude      '+STRING(gms[system.gms].latitude,FORMAT='(F8.3)')+'                                                      |'
        header_string[5]          =' Geodetic Longitude     '+STRING(gms[system.gms].longitude,FORMAT='(F8.3)')+'                                                      |'
        header_string[6]          =' Elevation              '+STRING(gms[system.gms].elevation,FORMAT='(F6.1)')+'                                                        |'
        header_string[7]          =' Reported               DHZF                                                          |'
        header_string[8]          =' Sensor Orientation     variation:DHZIF                                               |'
        header_string[9]          =' Digital Sampling       1 second                                                      |'
        tmp_string       = keyword_set(hres) ? '1-second (00:00:00.00 - 00:00:00.99)' : '1-minute (00:00:00.00 - 00:00:59.00)'
        header_string[10]         =' Data Interval Type     Filtered '+tmp_string+'                 |'
        header_string[11]         =' Data Type              Reported                                                      |'
        header_string[12]         =' # Element              Geomagnetic field                                             |'
        header_string[13]         =' # Unit                 D(eastward+):seconds, H:nT, Z(downward+):nT, F:nT             |'
        header_string[14]         =' # Issued by            Instituto de Geofísica, UNAM, MEXICO                          |'
        header_string[15]         =' # URL                  http://www.lance.unam.mx                                      |'
        header_string[16]         =' # Last Modified        Jan 16 2023                                                   |'

        tmp_code                  = STRUPCASE(gms[system.gms].code)
        ;tmp_string                = keyword_set(hres) ? ' SS' : ''
        header_string[17]         ='DATE       TIME         DOY     '+tmp_code+'D      '+tmp_code+'H      '+tmp_code+'Z      '+tmp_code+'F                    |'









        
;        if station EQ 1 THEN BEGIN

                ;file_name = station_code+string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')+'rmin.min'
        
        extention   = keyword_set(hres) ? 'v.hres' : 'v'
        month_txt   = getting_month_name([tmp_year,tmp_month,tmp_day])
        source_file = gms[system.gms].code+string(initial_day, FORMAT='(I02)')+month_txt+'.'+string(initial_year MOD 1000, FORMAT='(I02)')+extention
        source_path = system.datasource_dir+gms[system.gms].name+'/'

        file_exists = FILE_TEST(source_path+source_file)
 
        IF NOT file_exists THEN BEGIN
                                        IF NOT keyword_set(quiet) THEN BEGIN
                                                PRINT, FORMAT="('Critical ERROR: Missing input voltage file.')"
                                                PRINT, FORMAT="('                impossible to compute magnetic-data file.')"
                                        ENDIF
                                        error.value[1] += 1
                                        error.log      += 'Missing input voltage file '+source_file+'. '
                                        RETURN
        ENDIF
        
        IF not keyword_set(quiet) THEN  print, '        Extracting data from: '+source_file
        source_data = getting_voltagedata([initial_year, initial_month, initial_day], STATION=station, QUIET=quiet, HRES=hres)

        data_number = N_ELEMENTS(source_data[*].year)
        file_string = STRARR(header+data_number)
                
        file_string[0:header-1] = header_string[*]
                
                ;indexes_gap   = WHERE(source_data[*].X GE 99990.00)
                ;indexes_nogap = WHERE(source_data[*].X LT 99990.00)
                
                
        data_H     = FLTARR(data_number) + GMS[system.gms].base_line[0]
        data_H[*] += (source_data[*].H+GMS[system.gms].scale_offset[0])/GMS[system.gms].scale_constants[0]
                                  
        data_D     = FLTARR(data_number) + GMS[system.gms].base_line[1]
        data_D[*] += (source_data[*].D +GMS[system.gms].scale_offset[1])/(GMS[system.gms].scale_constants[1]*479.04);*(!Pi/180) ;/479.04

        data_Z     = FLTARR(data_number) + GMS[system.gms].base_line[2]
        data_Z[*] += (source_data[*].Z+GMS[system.gms].scale_offset[2])/GMS[system.gms].scale_constants[2]
                                  
        ;data_I     = FLTARR(data_number)
        ;data_I[*] += ATAN(data_Z[*]/data_H[*])*180 / !Pi
                
        data_F     = FLTARR(data_number)
        data_F[*] += SQRT( data_H[*]^2 + data_Z[*]^2 )
                                  
;        IF keyword_set(hres) THEN $
;                FOR i=header, header+data_number-1 DO $
;                        file_string[i] = string(source_data[i-header].day, source_data[i-header].month, source_data[i-header].year, $
;                                                source_data[i-header].HH, source_data[i-header].MM, source_data[i-header].SS, $
;                                                data_D[i-header], data_H[i-header], data_Z[i-header], data_I[i-header], data_F[i-header], $
;                                         FORMAT='(I02,X,I02,X,I04,X,I02,X,I02,X,I02, X, F+08.4,X,F+8.1,X,F+8.1,X,F+08.4,X,F+8.1)') $
;        ELSE $
;                FOR i=header, header+data_number-1 DO $
;                        file_string[i] = string(source_data[i-header].day, source_data[i-header].month, source_data[i-header].year, $
;                                                source_data[i-header].HH, source_data[i-header].MM, $
;                                                data_D[i-header], data_H[i-header], data_Z[i-header], data_I[i-header], data_F[i-header], $
;                                         FORMAT='(I02,X,I02,X,I04,X,I02,X,I02, X, F+8.4,X,F+8.1,X,F+8.1,X,F+8.4,X,F+8.1)')

        IF keyword_set(hres) THEN $
                FOR i=header, header+data_number-1 DO $
                        file_string[i] = string(source_data[i-header].year, source_data[i-header].month, source_data[i-header].day, $
                                                source_data[i-header].HH, source_data[i-header].MM, source_data[i-header].SS, doy, $
                                                data_D[i-header]*60., data_H[i-header], data_Z[i-header], data_F[i-header], $
                                         FORMAT = '(I4,"-",I02,"-",I02,X,I02,":",I02,":",I02,".000",X,I03,5X,'+ $
                                                               'F8.2,2x,F8.2,2x,F8.2,2x,F8.2,2x,F8.2)') $
        ELSE $
                FOR i=header, header+data_number-1 DO $
                        file_string[i] = string(source_data[i-header].year, source_data[i-header].month, source_data[i-header].day, $
                                                source_data[i-header].HH, source_data[i-header].MM, doy, $
                                                data_D[i-header]*60., data_H[i-header], data_Z[i-header], data_F[i-header], $
                                         FORMAT = '(I4,"-",I02,"-",I02,X,I02,":",I02,":00.000",X,I03,5X,'+ $
                                                               'F8.2,2x,F8.2,2x,F8.2,2x,F8.2,2x,F8.2)')

;##############################################################################
; creating data file
;##############################################################################
        ;output_datafile = station_code+'_'+string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')+'.dat'
        ;extention       = keyword_set(hres) ? 'm.hres' : 'm'
        ;output_datafile = gms[system.gms].code+string(initial_day, FORMAT='(I02)')+month_txt+'.'+string(initial_year MOD 1000, FORMAT='(I02)')+extention
        extention       = keyword_set(hres) ? 'rK.min.hres' : 'rK.min'
        output_datafile = gms[system.gms].code+string(initial_year, initial_month, initial_day, FORMAT='(I4,I02,I02)')+extention

        output_path     = system.datasource_dir+gms[system.gms].name+'/'
        
        exist_dir       = FILE_TEST(output_path, /DIRECTORY)
        
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
        OPENW, lun, output_path+output_datafile, /GET_LUN, ERROR=err
                IF err EQ 0 THEN BEGIN
                        FOR i=0ll, header+data_number-1 DO PRINTF, lun, file_string[i]
                ENDIF ELSE MESSAGE, 'Error while writing data file!
                
                                     
        CLOSE, lun
        FREE_LUN, lun

        IF not keyword_set(quiet) THEN BEGIN
                print, '        - Saving: '+output_datafile
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
;SECUENCIA PRINCIPAL


PRO geomagixs_voltagedata_makemagneticfile, initial, final, STATION=station, $
                                                          QUIET=quiet, $
                                                          FORCE_ALL=force_all, $
                                                          HRES=hres

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
        file_number         = (JULDAY(final_month, final_day, final_year) - JULDAY(initial_month, initial_day, initial_year))+1
        data_file_name      = strarr(file_number)
        processed_file_name = strarr(file_number)
        string_date         = strarr(file_number)
        ;exist_file     = intarr(file_number)

        extention       = keyword_set(hres) ? 'm.hres' : 'm'

        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(initial_month, initial_day, initial_year)


                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                month_txt         = getting_month_name([tmp_year,tmp_month,tmp_day])
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                
                        ;data_file_name[i] = station_code+string_date[i]+'rmin.min'
                        ;processed_file_name[i] = station_code+'_'+string_date[i]+'.dat'
                data_file_name[i] = gms[system.gms].code+string(tmp_day, FORMAT='(I02)')+month_txt+'.'+string(tmp_year MOD 1000, FORMAT='(I02)')+extention
                ;print, i+1, '  ', string_date[i], '  ', processed_file_name[i]
        ENDFOR

        exist_data_file = FILE_TEST(system.datasource_dir+gms[system.gms].name+'/'+data_file_name) AND not(keyword_set(force_all))
        dummy           = WHERE(exist_data_file EQ 0, updating_files)

        ;IF N_ELEMENTS(where(exist_data_file EQ 0)) GT 1 THEN updating_files = N_ELEMENTS(where(exist_data_file EQ 0)) $
        ;        ELSE IF where(exist_data_file EQ 0) GE 0 THEN updating_files = N_ELEMENTS(where(exist_data_file EQ 0)) $
        ;                ELSE updating_files = 0
;print, data_file_name[where(exist_data_file EQ 0)]

        IF not keyword_set(quiet) THEN BEGIN
                IF updating_files GT 0 THEN BEGIN
                        PRINT, ''
                        PRINT, updating_files, FORMAT='("        A total of ",I," file(s) need to be updated.")' 
                ENDIF ELSE BEGIN
                        PRINT, "        No file requires to be updated."
                        RETURN
                ENDELSE
        
                IF updating_files NE N_ELEMENTS(exist_data_file) AND not(keyword_set(quiet) OR keyword_set(force_all)) THEN BEGIN
                        PRINT, N_ELEMENTS(exist_data_file)-updating_files, FORMAT='("        There are still ",I," file(s) that can be updated.")'
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

        FOR i = 0ll, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 0 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        making_magneticfile, [tmp_year, tmp_month, tmp_day], STATION=station, QUIET=quiet, HRES=hres
                        ;print, tmp_year, tmp_month, tmp_day
                        ;tmp=getting_magneticdata_intermagnet([tmp_year, tmp_month, tmp_day], QUIET=quiet, STATION=station)
                        ;print, tmp
                ENDIF
        ENDFOR

        IF not keyword_set(quiet) THEN BEGIN
                PRINT, ''
                PRINT, '        Magnetic data-file(s) already computed!'
        ENDIF

RETURN


END




