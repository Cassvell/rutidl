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


pro bsq, date_i, date_f, MAKE_FILE=make_file
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
;###############################################################################
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
  ;Z = add_nan(Z, 3.04e4, 'greater')	
  ;H = add_nan(H, 2.76e4, 'greater')
  ;  
  ;plot, tot_days,H, ystyle=1
  ;print, min(H, /nan)

  daily_picks = picks(H, 60, 24)
  
  picks = picks(H, 60, 6)
  hist = threshold(picks)
  
END
