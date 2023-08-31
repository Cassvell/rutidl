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
FUNCTION sel_qdayV2, date_i, station_idx
	On_error, 2
	COMPILE_OPT idl2, HIDDEN


        @set_up_commons
        set_up
        	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= 1

	fday = ''
	CASE mh_i of
		1	: fday=31
		2	: fday=28
		3	: fday=31
		4	: fday=30
		5	: fday=31
		6	: fday=30
		7	: fday=31
		8	: fday=31
		9	: fday=30
		10	: fday=31
		11	: fday=30
		12	: fday=31
	ENDCASE	  

	yr_f	= yr_i
	mh_f	= mh_i
	dy_f 	= fday

    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu
        
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, 1, yr_i))+1
        data_file_name = STRARR(file_number)
        string_date     = STRARR(file_number) 
       
; Generate the time series variables 
; define H variables                  
    H  = rawH_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx)
    PRINT, '#######################################################################'
    H = nanpc(H, 99999.0, 'gequal')
    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 99999.0, 'equal')

;Cálculo de las desviaciones estándar en formato horario.
	    
    PRINT, '#######################################################################'
    PRINT, '#######################################################################'       
    twmin = FINDGEN(file_number*1440)/1440.    
    tw = FINDGEN(file_number*24)/24.    
    H_IQR = FINDGEN(N_ELEMENTS(H)/60)
    FOR i=0, N_ELEMENTS(H_IQR)-1 DO BEGIN
        ;PRINT, cgPercentiles(H[i*60:(i+1)*60-1], Percentiles=[0.75])
        QR1= cgPercentiles(H[i*60:(i+1)*60-1], Percentiles=[0.25])
        QR3= cgPercentiles(H[i*60:(i+1)*60-1], Percentiles=[0.75])
        H_IQR[i] = QR3-QR1                    
    ENDFOR        
    ;PRINT, cgPercentiles(H, Percentiles=[0.25])
    n = 1.5
    H_IQR_n =  H_IQR*n
    IQR_hr = FINDGEN(N_ELEMENTS(H_IQR_n)/24)
            
    FOR i=0, N_ELEMENTS(IQR_hr)-1 DO BEGIN
        IQR_hr[i] = MAX(H_IQR_n[i*24:(i+1)*24-1])    
    ENDFOR

    
    time_w = TIMEGEN(N_ELEMENTS(H_IQR), final=JULDAY(mh_f, dy_f, yr_f, 23), $
                start=JULDAY(mh_i, dy_i, yr_i, 0) , units='D')
                
    CALDAT, time_w, m, d, y, hr
    
;selección de los días quietos con base en el día con menor desviación en IQR  
;	PRINT, "Five Qdays according to IQR criteria"
  ;  PRINT, format='(2X, "Fecha", 4X, "QD", 4X, "Indice IQR(H)")'
   ; PRINT, '---------------------------------'
    
    IQR_sorted = SORT(IQR_hr)
    iqr_sort = IQR_hr[IQR_sorted]
       
    d_sort = d[IQR_sorted[0:4]]
    m_sort = m[IQR_sorted[0:4]]
    y_sort = y[IQR_sorted[0:4]]
	
	result = {day : INTARR(5), month : INTARR(5), year : INTARR(5), iqr : FLTARR(5)}	               
;###############################################twmin################################   
	result.day[*] = d_sort
	result.month[*] = m_sort
	result.year[*] = y_sort
	result.iqr[*] = iqr_sort[0:4]
 ;   DEVICE
     
 ;   WINDOW, 0,  XSIZE=1600, YSIZE=800, TITLE='H'
 ;   PLOT, tw, H_IQR_n, xstyle=1, XTICKS=file_number, XTICKNAME=X_label, THICK=1
    
 ;   WINDOW, 1,  XSIZE=1600, YSIZE=800, TITLE='H'
 ;   PLOT, twmin, H, xstyle=1, ystyle=1, XTICKS=file_number, XTICKNAME=X_label, THICK=1    
    ;OPLOT, tw, k_mex, LINESTYLE=3
    RETURN, result
END	
