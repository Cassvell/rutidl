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


FUNCTION bsq, H, MAKE_FILE=make_file
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
;###############################################################################
        @set_up_commons
        set_up
        
;###############################################################################                                  
;###############################################################################        
;###############################################################################
ndays = N_ELEMENTS(H)/1440

FOR  i = 0, ndays-1 DO BEGIN
    stacked_data = H[i*1440:(i+1)*1440-1]
    ; If the current day is not the first day, stack the data from the previous day
    IF i NE 0 THEN BEGIN
        stacked_data = [data[i-1], stacked_data]
    ENDIF
    
    ; If the current day is not the last day, stack the data from the next day
    IF i NE n_days - 1 THEN BEGIN
        stacked_data = [stacked_data, data[i+1]]
    ENDIF    
ENDFOR

;
;###############################################################################    
    ;DEVICE, true=24, retain=2, decomposed=0
    ;TVLCT, R_bak, G_bak, B_bak, /GET        
    ;LOADCT, 39, /SILENT
    

   ; WINDOW, 5, XSIZE=1000, YSIZE=400, TITLE='Bsq [m]'
 ;   PLOT, x_30min, qdl_mean, XSTYLE=1, background=255, color=0, XTICKS=ndays, $
  ;  CHARSIZE = 1.8, CHARTHICK=2.0, YTITLE = 'Bsq [nT]', XTITLE = time_name,THICK=2
;	OPLOT, x_30min, avr_H2, color=254
	;OPLOT, x_1min, H1, color=254
;	OPLOT, x_1min, avr_H1, color=76

;    WINDOW, 1, XSIZE=1000, YSIZE=400, TITLE='Bsq(mean) [30m]' 
    ;PLOT, td_h, QDL_avr, YRANGE=[MIN(QDL_avr, /NAN),MAX(QDL_avr, /NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0 , $
   ; CHARTHICK=2.0, YTITLE = 'Bsq [nT]', XTITLE = time_name, XTICKS=ndays, XTICKNAME=X_label, THICK=2
 
  ;  OPLOT, td, qdl, LINESTYLE=0, color=240
 ;   OPLOT, x, H-Bsq, LINESTYLE=0, color=240, THICK=2 

  ;  WINDOW, 0, XSIZE=1000, YSIZE=400, TITLE='Bsq [H]' 
 ;   PLOT, time,Bsq_H, YRANGE=[MIN(Bsq_H, /NAN),MAX(Bsq_H,/NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0 ,$
;    CHARTHICK=2.0, YTITLE = 'Bsq [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label, THICK=2


  ;  WINDOW, 3, XSIZE=1000, YSIZE=400, TITLE='H [m]'
 ;   PLOT, x, H, YRANGE=[MIN(H, /NAN),MAX(H, /NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0 ,$
;    CHARTHICK=2.0, YTITLE = 'BH [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label, THICK=2
  ;  OPLOT, x, H-Bsq_det, LINESTYLE=0
;    OPLOT, x, H-Bsq_det2, LINESTYLE=0, color=240, THICK=2    

  ;  WINDOW, 4, XSIZE=1000, YSIZE=400, TITLE='H [h]'
 ;   PLOT, time, H_h, YRANGE=[MIN(H, /NAN),MAX(H, /NAN)], XSTYLE=1, CHARSIZE = 1.8, background=255, color=0 ,$
;    CHARTHICK=2.0, YTITLE = 'BH [nT]', XTITLE = time_name, XTICKS=file_number, XTICKNAME=X_label, THICK=2
  ;  OPLOT, x, H-Bsq_det, LINESTYLE=0
 ;   OPLOT, time, H_h-Bsq_detH, LINESTYLE=0, color=240, THICK=2



	Bsq_res = {H : FLTARR(N_ELEMENTS(qdl_mean)), m : FLTARR(N_ELEMENTS(qdl))}
	
	Bsq_res.H[*] = qdl_mean
	Bsq_res.m[*] = qdl
IF KEYWORD_SET(make_file) THEN BEGIN
;Generación de archivo en muestreo de horas  

    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        

        outfile[i] = '/home/isaac/geomstorm/rutidl/output/Bsq_baselines/'+STRLOWCASE(station_code)+$
        '/Bsq_'+station_code+string_date[i]+'h.dat'
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

        outfile[i] = '/home/isaac/geomstorm/rutidl/output/Bsq_baselines/'+STRLOWCASE(station_code)+$
        '/Bsq_'+station_code+string_date[i]+'m.dat'    
        OPENW, LUN, outfile[i], /GET_LUN        
        PRINTF, LUN, Bsq[i*1440:(i+1)*1440-1], format='(F9.4)'
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR 
   
    PRINT, 'Se generaron archivos BSQ'
ENDIF 

RETURN, Bsq_res

END
