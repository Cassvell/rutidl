;+
; NAME:
;       getting_local_qdays.pro
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
;       getting_local_qdays, initial_date, final_date 
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
;
; DEPENDENCIES:
;
; INPUT FILES:
;       GMSYYYYMMDDrmin.min     [original geomagnetic service data files]
;
; OUTPUT FILES:
;       dates [YYYY, MM, DD] of the monthly Qdays
;
; HISTORIA:
;               22/03/2017      First succesfully running code
;               27/04/2017      Version 1.0 ready
;


FUNCTION getting_local_qdays, initial, idx

        On_error, 2
        COMPILE_OPT idl2, hidden
;##############################################################################
; initialize directories
;##############################################################################
;###############################################################################
;###############################################################################
	@set_up_commons
	set_up      
;##############################################################################
; initializing dates and hours
;##############################################################################
        initial_year   = initial[0]
        initial_month  = initial[1]
        initial_day    = initial[2]
              
;print, ':', initial_month 
;        days_for_qdays =  NOT keyword_set(real_time) ? JULDAY(final_month+1, 0, final_year)-JULDAY(final_month, 1, final_year) : $
 ;                                                      JULDAY(initial_month, 0, initial_year)-JULDAY(initial_month-1, 1, initial_year)
      
		days_for_qdays = JULDAY(initial_month+1, 0, initial_year) - $
		JULDAY(initial_month, 0, initial_year)+1
		
        julday_tmp = JULDAY(initial_month, 0, initial_year);NOT keyword_set(real_time) ? JULDAY(initial_month+1, 0, initial_year) : JULDAY(initial_month, 0, initial_year)
        ;julday_tmp = JULDAY(initial_month+1, 0, initial_year) 
        CALDAT, julday_tmp, tmp_m, tmp_d, tmp_y
   ;     print, tmp_y, tmp_m, tmp_d
;print, 
;IF JULDAY(gms[system.gms].dates_index[1,1],gms[system.gms].dates_index[1,2],gms[system.gms].dates_index[1,0]) LE julday_tmp THEN BEGIN

        tmp_m = 0
        tmp_d = 0
        tmp_y = 0
        
        str_tmp = { year:0, month:0, day:0, $
                    total_k : 0., total_k2 : 0., max_k : 0. $
                  }
                  
        data_qd = REPLICATE(str_tmp, days_for_qdays)
        ;print, days_for_qdays
	    station         = set_var.gms[FIX(idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
        station_code    = set_var.gms_code[FIX(idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu	       
     
        FOR i=0, days_for_qdays-1 DO BEGIN
                CALDAT, julday_tmp+i, tmp_m, tmp_d, tmp_y
                data_qd[i].year  = tmp_y
                data_qd[i].month = tmp_m
                data_qd[i].day   = tmp_d
;print, i,tmp_y,tmp_m,tmp_d
                tmp = kmex([tmp_y,tmp_m,tmp_d], idx)
                
                data_qd[i].total_k  = FLOAT(tmp.K_SUM)
                good_indexes          = WHERE(tmp.K_mex[*] LT 99, good_indexes_count)
                IF good_indexes_count LE 0 THEN BEGIN
                        data_qd[i].total_k2 = 999.0^2*8.
                        data_qd[i].max_k    = 999.0
                ENDIF ELSE BEGIN
                        data_qd[i].total_k2 = TOTAL(FLOAT(tmp.K_mex[good_indexes])^2)
                        data_qd[i].max_k    = MAX(FLOAT(tmp.K_mex[good_indexes]))
                ENDELSE
        ENDFOR

        

        sorting1 = SORT(data_qd[*].total_k)
;        print, '*', data_qd[sorting1].month
        data_qd[*] = data_qd[sorting1]
;        print, '**', data_qd[*].month
        
        FOR i=0, days_for_qdays-1 DO BEGIN
                
                indexes_equals1 = WHERE( data_qd[*].total_k EQ data_qd[i].total_k )
                ;PRINT, 'i=', i, N_ELEMENTS(indexes_equals1)
                ;PRINT, '     ', data_qd[i].total_k, data_qd[i].total_k2, data_qd[i].max_k
                
                ;print, indexes_equals1, data_qd[indexes_equals1].total_k
                ;print,'???'
                IF N_ELEMENTS(indexes_equals1) GT 1 THEN BEGIN
                        tmp_struct1 = data_qd[indexes_equals1]
                        sorting2 = SORT(tmp_struct1[*].total_k2)
                        tmp_struct1 = tmp_struct1[sorting2]
                        
                        FOR j = 0, N_ELEMENTS(sorting2)-1 DO BEGIN
                                indexes_equals2 = WHERE( tmp_struct1[*].total_k2 EQ tmp_struct1[j].total_k2 )
                                ;print, '!!!', indexes_equals2, '!!!'
                                ;PRINT, data_qd[indexes_equals1[sorting2]].total_k2, data_qd[indexes_equals1[sorting2[j]]].total_k2
                                IF N_ELEMENTS(indexes_equals2) GT 1 THEN BEGIN
                                        tmp_struct2 = tmp_struct1[indexes_equals2]
                                        sorting3 = SORT(tmp_struct2[*].max_k)
                                        tmp_struct1[indexes_equals2]=tmp_struct2[sorting3]
                                        j += N_ELEMENTS(indexes_equals2)-1
                                ENDIF
                        ENDFOR
                        data_qd[indexes_equals1] = tmp_struct1[*]
                                
                ;PRINT, '    nuevo i=', i, data_qd[indexes_equals1].total_k2, data_qd[indexes_equals1].max_k
                                ;print, data_qd[indexes_equals2[sorting3]].max_k, data_qd[indexes_equals2[sorting3]].max_k
                        i += N_ELEMENTS(indexes_equals1)-1
                        ;print, tmp_struct0
                        ;IF N_ELEMENTS(indexes_equals2) GT 1 THEN print, '!'
                ENDIF
        ENDFOR

        valid_days = WHERE(data_qd[*].total_k LT 990. AND data_qd[*].total_k2 LT 990.^2*8. AND data_qd[*].max_k LT 999., valid_days_count)

;print, N_ELEMENTS(valid_days)
;FOR i=0, N_ELEMENTS(valid_days)-1 DO print, data_qd[valid_days[i]].total_k, data_qd[valid_days[i]].total_k2, data_qd[valid_days[i]].max_k

IF valid_days_count GE 10 THEN BEGIN ;MESSAGE, 'Critial error: Less than 10 local Q-days found!'


        IF NOT keyword_set(quiet) THEN BEGIN
;print, data_qd[0].month
;print, valid_days[0]
;print, data_qd[valid_days[0]].month
                tmp_month = data_qd[valid_days[0]].month

                CASE 1 OF
                        tmp_month EQ 1  : tmp_string = 'Jan'
                        tmp_month EQ 2  : tmp_string = 'Feb'
                        tmp_month EQ 3  : tmp_string = 'Mar'
                        tmp_month EQ 4  : tmp_string = 'Apr'
                        tmp_month EQ 5  : tmp_string = 'May'
                        tmp_month EQ 6  : tmp_string = 'Jun'
                        tmp_month EQ 7  : tmp_string = 'Jul'
                        tmp_month EQ 8  : tmp_string = 'Aug'
                        tmp_month EQ 9  : tmp_string = 'Sep'
                        tmp_month EQ 10 : tmp_string = 'Oct'
                        tmp_month EQ 11 : tmp_string = 'Nov'
                        tmp_month EQ 12 : tmp_string = 'Dec'
                        ELSE: MESSAGE, 'Critical error'
                ENDCASE

                IF N_ELEMENTS(valid_days) LT 15 THEN $
                        str_result = ' '+tmp_string+' '+STRING(data_qd[valid_days[0]].year, $
                                                                data_qd[valid_days[0:4]].day, $
                                                                data_qd[valid_days[5:9]].day, $
                                                                FORMAT='(I4,X,5(2X,I2),2X,5(2X,I2))') $
                ELSE $
                        str_result = ' '+tmp_string+' '+STRING(data_qd[valid_days[0]].year, $
                                                                data_qd[valid_days[0:4]].day, $
                                                                data_qd[valid_days[5:9]].day, $ 
                                                                data_qd[valid_days[N_ELEMENTS(valid_days)-5:N_ELEMENTS(valid_days)-1]].day, $
                                                                FORMAT='(I4,X,5(2X,I2),2X,5(2X,I2),2X,5(2X,I2))')
                
                tmp_string = keyword_set(real_time) ? ' [early]' : ' [final]'
               ; PRINT, 'Local Qday selected by Kmex'
               ; PRINT, ' * Local'+tmp_string+' Q & D days for '+station_code+' GMS.'
               ; PRINT, ' MMM YYYY   Q1  Q2  Q3  Q4  Q5    Q6  Q7  Q8  Q9  Q10   D1  D2  D3  D4  D5'
              ;  PRINT, str_result
                PRINT, ''

        ENDIF

		
        resultado = { year: INTARR(5), $
        			  month: INTARR(5), $
        			  day : INTARR(5) }
        			  

        resultado.day[*] = data_qd[0:4].day
        resultado.year[*]   = data_qd[0:4].year
        resultado.month[*]  = data_qd[0:4].month		

ENDIF ELSE BEGIN
        resultado = { year:INTARR((5)), $
        			  month:INTARR((5)), $
        			  day : INTARR((5)) }
        resultado.day[*] = 99
        resultado.year   = initial_year
        resultado.month  = initial_month
       
ENDELSE
;FOR i=0, 10-1 DO print, data_qd[valid_days[i]].total_k, data_qd[valid_days[i]].day, data_qd[valid_days[i]].month, data_qd[valid_days[i]].year
;print, ''
;FOR i=N_ELEMENTS(valid_days)-5, N_ELEMENTS(valid_days)-1 DO print, data_qd[valid_days[i]].total_k, data_qd[valid_days[i]].day, data_qd[valid_days[i]].month, data_qd[valid_days[i]].year

RETURN, resultado
END
