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
    data   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = data.H
    sym = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'sym')


; define Bsq 
 ;   Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, station_idx, 'min')    

; Generate the time variables to plot TEC time series         
;###############################################################################
;identifying NAN percentage values in the Time Series                
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
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
       
 ;  	WINDOW, 2, XSIZE=1000, YSIZE=400, TITLE='H component'
  ;  PLOT, time, H, XSTYLE=1,YSTYLE=2, XRANGE=[0,file_number], $
  ;  YRANGE=[MIN(H),MAX(H)], CHARSIZE = 1.8, background=255, color=0, $
  ;  CHARTHICK=2.0, YTITLE = 'H [nT]',XTITLE = time_name, XTICKS=file_number, $
  ;  XTICKNAME=X_label    


; define frequencies
    l            = 28.06  
    mlat         = l*!pi
    ld           = cos(mlat/180)
    p_a          = sym*ld
    baseline     = p_a;Bsq + p_a Desde la actualización, Bsq ya está restado de H
    Bdiono       = H-baseline
   ; Bdiono2	     = H-p_a+Bsq	; Al no tener Bsq desde el inicio, aquí se suma
    n            = N_ELEMENTS(Bdiono) 
    
    time_res    = 'm'
    time        = 0.

    
    CASE time_res of 
        'h'     : time = 3600.0
        'm'     : time = 60.0
    ENDCASE

    fny      = FLOAT(1.0/(2.0*time)) ; frecuencia de Nyquist
    y        = FFT(Bdiono)            ; Compute Fast Fourie Transform from diono time series
;###############################################################################



;###############################################################################    
	;PRINT, "Press value for Z thershold (100 by default)"
	;Z_Hthreshold = ''
	; = '100'
	;READ, Z_Xthreshold, PROMPT = '> '	
	;Hspikes = whitaker_hayer(H, FIX(Z_Hthreshold), 'H')
	;H_det = fillnan(H_det)	

	wave_test, Bdiono,[yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, PS="ps"
END
