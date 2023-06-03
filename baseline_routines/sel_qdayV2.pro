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
PRO sel_qdayV2, date_i, date_f
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]

        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = STRARR(file_number)
        string_date     = STRARR(file_number) 
       
; Generate the time series variables 
; define H variables                  
    H  = rawH_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'coeneo')
    PRINT, '#######################################################################'
    H = nanpc(H, 99999.0, 'gequal')
    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 99999.0, 'equal')

;Cálculo de las desviaciones estándar en formato horario.
	    
    PRINT, '#######################################################################'
    PRINT, '#######################################################################'       
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
    PRINT, format='(2X, "Fecha", 4X,"Indice IQR(H)")'
    PRINT, '---------------------------------'
    PRINT, '                                                                    '
         
    FOR i=0, N_ELEMENTS(IQR_hr)-1 DO BEGIN
        PRINT, y[i], m[i], d[i], IQR_hr[i], $
        FORMAT = '(I4, "-", I02, "-", I02, 4X, F6.2)'                                 
    ENDFOR        
;###############################################################################   
 ;   DEVICE
   ; WINDOW, 0,  XSIZE=1600, YSIZE=800, TITLE='kmex vx kp'
   ;; PLOT, tw, H
    ;OPLOT, tw, k_mex, LINESTYLE=3
END	
