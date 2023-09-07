;
;Name:
;	bsq.pro
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


FUNCTION bsq, H1, H2, ndays, MAKE_FILE=make_file
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
;###############################################################################
        @set_up_commons
        set_up

    td      = FINDGEN(ndays*1440)/1440.0
    td_h    = FINDGEN(ndays*24)/24.0
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
    
;###############################################################################        
    ;implementar una función de interpolación en caso de que el porcentaje de 
    ;nan sea muy bajo       
    
    H1 = fillnan(H1)
    H2 = fillnan(H2)          
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

 ;   file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1  
    time = FINDGEN(N_ELEMENTS(Bsq_H))

    Bsq_trendH = INTERPOLATE(Bsq_24h, time, CUBIC=-0.5,  /GRID)
    Bsq_detH =  Bsq_H-Bsq_trend  
;###############################################################################    



	Bsq_res = {H : FLTARR(N_ELEMENTS(Bsq_H)), m : FLTARR(N_ELEMENTS(Bsq))}
	
	Bsq_res.H[*] = Bsq_H
	Bsq_res.m[*] = Bsq_det2

RETURN, Bsq_res

END
