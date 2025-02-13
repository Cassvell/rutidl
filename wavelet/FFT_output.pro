;Name:
;	FFT_output.pro
;purpose:
;   plot geomagnetic and ionospheric response due to Dst and Ddyn magnetic trace
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
;   .r iono_resp
;   iono_resp, idate, fdate
;parameters:
;   idate/fdate: format ([yyyy,mm,dd])
;
;dependencies:
;
;
;input files
;   Dst files, H obs, Bsq baseline, TEC data files
;
;output files:
;   .PNG figure
;   imported to /output/eventos_tgm/iono_resp_V9_yyyy-mm-dd.png
;
;version
;   Dec, 2022
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;
PRO FFT_output, date_i, date_f, station_code, PS=ps, Bsq=Bsq 
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
    
    IF station_code EQ '' THEN BEGIN 
    
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
    IF station_class EQ 'regmex' THEN  BEGIN    
    station = set_var.gms[FIX(station_idx)]
    station_code = set_var.gms_code[FIX(station_idx)]  
    ENDIF 
    
    IF station_class EQ 'intermagnet' THEN  BEGIN 
    station = set_var.gmsi[FIX(station_idx)] 
    station_code = set_var.gmsi_code[FIX(station_idx)] 
    ENDIF
	PRINT,  'GMS selected: '+station+' IAGA code: '+station_code   
	ENDIF
;###############################################################################	
;###############################################################################  
    	
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
;###############################################################################
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_n = event_case([yr_i,mh_i,dy_i])  
;###############################################################################   
    time= findgen(file_number*1440)/1440.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################

;###############################################################################
; Generate the time series variables 
; define H variables                  
  ;  dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
  ;  dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    data   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = data.H
    SQ = data.SQ
    idx = sym_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH = idx.symH
    asymH = idx.asyH
    idx2 = sym0_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    
    rc = dst_0([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    Q = rc.Q
    ;symH0 = idx2.symH0
    ip   = ip_arraym([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    Bt = ip.Bt
    Bz = ip.Bz
    P = ip.n_p
    V = ip.vx

    a = ae_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    AE = a.AE
    ;correctedsymH = dst_0([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    ;print, correctedsymH.symH_0
    ;symH_0 = correctedsymH.symH_0
; define Bsq 
 ;   Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx, 'min')    

; Generate the time variables to plot TEC time series         
;###############################################################################
;identifying NAN percentage values in the Time Series                
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       

H = add_nan(H, 99999.0, 'equal')  
H = add_nan(H, 200.0, 'greater')  

;symH0 = fillnan(symH0)
H = fillnan(H)

;    DEVICE, true=24, retain=2, decomposed=0
  ;  TVLCT, R_bak, G_bak, B_bak, /GET        
   ; LOADCT, 39, /SILENT    
        
 ;   X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')
    new_month = month_name(mh_f, 'english')
    IF mh_i NE mh_f THEN BEGIN
    	time_name = 'days of '+old_month+' and '+ new_month
    ENDIF ELSE BEGIN 
    	time_name = 'days of '+old_month
    ENDELSE
    set_plot, 'x'   
       time = FINDGEN(N_ELEMENTS(H))/1440.0   
       
 
    class = gms_class(station_code)
    info = stationlist(class, station_code)
    mlat = info.mlat 

    l            = mlat
    mlat         = l*!pi
    ld           = cos(mlat/180)
    p_a          = symH*ld
    baseline     = p_a
    Bdiono       = H-baseline

;###############################################################################
;###############################################################################     
    ;print, Q
    path='/home/isaac/longitudinal_studio/fig/magdata/'
    path2 = '/home/isaac/longitudinal_studio/fig/'
    ;ts_plots, symH, H, SQ, Bdiono, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code
    ip_plots, symH, Q, P, V, Bz, Bt, AE,[yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path2

END
