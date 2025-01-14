;
;Name:
;	sel_qdayV2.pro
;purpose:
;	analyse and plot the standard deviation of H component of geomagnetic field obs
;   in order to select quiet days for Solar quiet diurnal baseline
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data analysis
;
;calling sequence:
;   .r sel_qday
;   sel_qday, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;   idate/fdate: initial date/ final date
;
;dependencies:
;
;
;input files
;   H component of geomagnetic observations
;
;output files:
;   .PNG figure of Standard deviation of H component in 1H resolution.
;   imported to: output/qdays/DH_stdesv_V'n'_yyyymmdd_yyyymmdd.png
;
;version
;   sel_qday V2. March, 2023
;
;
;note
;   This routine must be run in the following order
;   1. sel_qday.pro, to select QD
;
;   2. bsq_plot.pro, to plot and analyse the resulting BSQ base line before using it 
;       in the following study
;
;   3. bsq_V2.pro,   to generate Bsq files for the following analysis
;
function sel_qday, date_i, date_f
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

;###############################################################################                                  
;###############################################################################        
;###############################################################################
ndays = N_ELEMENTS(H)/1440

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
    tot_days= FINDGEN(file_number*1440)/1440.0    
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

    plot, tot_days,H, ystyle=1
    ;print, min(H, /nan)
  
    daily_picks = picks(H, 60, 24)

    time_w = TIMEGEN(N_ELEMENTS(daily_picks), final=JULDAY(mh_f, dy_f, yr_f, 23), $
    start=JULDAY(mh_i, dy_i, yr_i, 0) , units='D')
    
    CALDAT, time_w, m, d, y, hr

    ;selección de los días quietos con base en el día con menor desviación en IQR  
    ;	PRINT, "Five Qdays according to IQR criteria"
    ;  PRINT, format='(2X, "Fecha", 4X, "QD", 4X, "Indice IQR(H)")'
    ; PRINT, '---------------------------------'

    picks_sorted = SORT(daily_picks)
    iqr_sort = daily_picks[picks_sorted]

    d_sort = d[picks_sorted[0:9]]
    m_sort = m[picks_sorted[0:9]]
    y_sort = y[picks_sorted[0:9]]

    result = {day : INTARR(10), month : INTARR(10), year : INTARR(10), iqr : FLTARR(10)}	               
    ;###############################################twmin################################   
    result.day[*] = d_sort
    result.month[*] = m_sort
    result.year[*] = y_sort
    result.iqr[*] = iqr_sort[0:9]

	PRINT, "Five Qdays according to IQR criteria"
    PRINT, format='(2X, "Fecha", 4X, "QD", 4X, "Indice IQR(H)")'
    PRINT, '#######################################################'


    for i=0, n_elements(result.day)-1 do begin
        print, string(result.year[i], result.month[i], result.day[i], result.iqr[i], format = '(I4,"-",I02,"-",I02,4X,F8.4 )')
    endfor    
    return, result
END	
