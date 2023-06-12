;
;Name:
;	bsq_plot.pro
;purpose:
;   Plot the BSQ base line based on two Qdays selected	
;
;
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data plot analysis
;
;calling sequence:
;   .r bsq_plot
;   bsq_plot, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;
;
;dependencies:
;
;
;input files
;   geomagnetic field measurements from a certain observatory or geomagnetic station.
;
;output files:
;   .dat files
;   imported to: /output/Bsq_baselines/Bsq_yyyymmddh.dat    -> for hourly resolution data
;                /output/Bsq_baselines/Bsq_yyyymmddm.dat    -> for minut resolution data
;
;version
;   Dec, 2022
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


PRO bsq_V2, date_i, date_f, resolution, MAKE_FILE=make_file
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
;###############################################################################
        @set_up_commons
        set_up
        
	station_idx = ''
	PRINT, 'Enter GMS idx: 0:coe, 1:teo, 2:tuc, 3:bsl, 4:itu'
	READ, station_idx, PROMPT = '> '


    station         = set_var.gms[FIX(station_idx)]        ;0:coeneo, 1:teoloyuca, 2:tucson, 3:bsl, 4:iturbide
    station_code    = set_var.gms_code[FIX(station_idx)]   ;0;coe, 1:teo, 2:tuc, 3:bsl, 4:itu	
		
	dat1    = struct_H([yr_i, mh_i, dy_i], station, station_code, 'min')	
	H1      = dat1.H

    dat2    = struct_H([yr_f, mh_f, dy_f], station, station_code, 'min')
    H2      = dat2.H
    
    ndays   = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    td      = FINDGEN(ndays*1440)/1440.0
    td_h    = FINDGEN(ndays*24)/24.0
    datetime= TIMEGEN(N_ELEMENTS(td), FINAL=JULDAY(mh_f, dy_f, yr_f, 23), $
                START=JULDAY(mh_i, dy_i, yr_i, 0), UNITS='H')
    CALDAT, datetime, mh, dy, yr, hr
;###############################################################################                        
;identifying NAN percentage values in the Time Series    
    H1 = nanpc(H1, 99999.0, 'gequal')
    H2 = nanpc(H2, 99999.0, 'gequal')   
        
    H1 = add_nan(H1, 99999.0, 'gequal')        
    H2 = add_nan(H2, 99999.0, 'gequal')            
;###############################################################################        
    ;implementar una función de interpolación en caso de que el porcentaje de 
    ;nan sea muy bajo       
    H1_tmp   = H1
    H1_exist = WHERE(finite(H1_tmp), ngooddata1, complement=baddata1, $
    ncomplement=nbaddata1)

    H2_tmp   = H2
    H2_exist = WHERE(finite(H2_tmp), ngooddata2, complement=baddata2, $
    ncomplement=nbaddata2)
    
    H = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_code, 'min')
    H_h = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_code, 'H')    
;###############################################################################                        
;identifying NAN percentage values in the Time Series    
    H = nanpc(H, 99999.0, 'gequal')
    H = add_nan(H, 99999.0, 'gequal') 

    H_h = nanpc(H_h, 99999.0, 'gequal')
    H_h = add_nan(H_h, 99999.0, 'gequal') 
;###############################################################################        
    ;implementar una función de interpolación en caso de que el porcentaje de 
    ;nan sea muy bajo       
    H = fillnan(H)
    H_h = fillnan(H_h)       
    ; interpolate at the locations of the bad data using the good data    
    IF nbaddata1 GT 0 THEN H1_tmp[baddata1] = INTERPOL(H1_tmp[H1_exist], $
    H1_exist, baddata1, /QUADRATIC)
    H1 = H1_tmp  
    
    IF nbaddata1 GT 0 THEN H2_tmp[baddata2] = INTERPOL(H2_tmp[H2_exist], $
    H2_exist, baddata2, /QUADRATIC)
    H2 = H2_tmp 
;###############################################################################
;Extend QDS data ndays for a quadratic interpolation
    QDS1 = REFORM(REBIN(H1, 1440, ndays), N_ELEMENTS(td))
    QDS2 = REFORM(REBIN(H2, 1440, ndays), N_ELEMENTS(td))
;###############################################################################
;Interpolate between the QDS
    slope1   = (td - td[719])
    slope2   = (td[N_ELEMENTS(td)-721]-td[719])
    slope    = slope1/slope2

    Bsq1     = (QDS2-QDS1)*slope
    Bsq     = QDS1+Bsq1
    Bsq = SMOOTH(Bsq, 60, /EDGE_TRUNCATE, /NAN); consultar   
    
;Applying a detrend function
    Bsq_24h=FINDGEN(N_ELEMENTS(Bsq)/1440)   
    FOR i=0, N_ELEMENTS(Bsq_24h)-1 DO BEGIN
        Bsq_24h[i] = MEDIAN(Bsq[i*1440:(i+1)*1440-1])    
    ENDFOR
    x = FINDGEN(N_ELEMENTS(Bsq))/1440

;plotting to check data
    Bsq_det = FLTARR(N_ELEMENTS(Bsq))
    coeff =  DETREND(Bsq, Bsq_det); remove linear trend.
    
    Bsq_trend = INTERPOLATE(Bsq_24h, x, CUBIC=-0.5,  /GRID)
    Bsq_det2 =  Bsq-Bsq_trend 
                                    
;###############################################################################
    ;Generate a QD file in days
    outfile = STRARR(ndays) 
    
    string_date        = STRARR(ndays)                       
    data_file_name_h  = STRARR(ndays)

    Bsq_H = FINDGEN(N_ELEMENTS(Bsq)/60)
    FOR i=0, N_ELEMENTS(Bsq_H)-1 DO BEGIN
        ;print, Bsq[i*60:(i+1)*60-1]
       ; Bsq_H[i] = MEDIAN(Bsq_det[i*60:(i+1)*60-1])        
        Bsq_H[i] = MEDIAN(Bsq[i*60:(i+1)*60-1]) 
    ENDFOR

      
    time = FINDGEN(N_ELEMENTS(Bsq_H))

    Bsq_trendH = INTERPOLATE(Bsq_24h, time, CUBIC=-0.5,  /GRID)
    Bsq_detH =  Bsq_H-Bsq_trend  
    
    WINDOW, 2, XSIZE=800, YSIZE=400, TITLE='Bsq [m]'
    PLOT, x,Bsq, YRANGE=[MIN(Bsq_det, /NAN),MAX(Bsq_det, /NAN)], XSTYLE=1, COLOR=255
    OPLOT, x, Bsq_det, LINESTYLE=0
    OPLOT, x, Bsq_det2, LINESTYLE=2
    
    WINDOW, 1, XSIZE=800, YSIZE=400, TITLE='H Bsq(trended) [m]' 
    PLOT, x, H, YRANGE=[MIN(H, /NAN),MAX(H, /NAN)], XSTYLE=1, COLOR=255
  ;  OPLOT, x, H-Bsq_det, LINESTYLE=0
    OPLOT, x, H-Bsq, LINESTYLE=0 

    WINDOW, 0, XSIZE=800, YSIZE=400, TITLE='Bsq [H]' 
    PLOT, time,Bsq_H, YRANGE=[MIN(Bsq_H, /NAN),MAX(Bsq_H,/NAN)], XSTYLE=1


    WINDOW, 3, XSIZE=800, YSIZE=400, TITLE='H [m]'
    PLOT, x, H, YRANGE=[MIN(H, /NAN),MAX(H, /NAN)], XSTYLE=1, COLOR=255
  ;  OPLOT, x, H-Bsq_det, LINESTYLE=0
    OPLOT, x, H-Bsq_det2, LINESTYLE=0    

    WINDOW, 4, XSIZE=800, YSIZE=400, TITLE='H [H]'
    PLOT, time, H_h, YRANGE=[MIN(H, /NAN),MAX(H, /NAN)], XSTYLE=1, COLOR=255
  ;  OPLOT, x, H-Bsq_det, LINESTYLE=0
    OPLOT, time, H_h-Bsq_detH, LINESTYLE=0

IF KEYWORD_SET(make_file) THEN BEGIN
;Generación de archivo en muestreo de horas  

    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        

        outfile[i] = '/home/isaac/geomstorm/rutidl/output/Bsq_baselines/Bsq_'+string_date[i]+'h.dat'
        ;PRINT, Bsq_H[i*24:(i+1)*24-1]     
        OPENW, LUN, outfile[i], /GET_LUN        
        PRINTF, LUN, Bsq_H[i*24:(i+1)*24-1], format='(F9.4)'
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR 


   
    ;archivo en muestreo de minutos
    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        

        outfile[i] = '/home/isaac/geomstorm/rutidl/output/Bsq_baselines/Bsq_'+string_date[i]+'m.dat'    
        OPENW, LUN, outfile[i], /GET_LUN        
        PRINTF, LUN, Bsq[i*1440:(i+1)*1440-1], format='(F9.4)'
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR 
   
    PRINT, 'Se generaron archivos BSQ'
ENDIF 


END
