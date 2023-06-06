;
;Name:
;	tgm_file.pro
;purpose:
;	generate a txt file with Dst and dH data in order to correlate their data
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data writting
;
;calling sequence:
;   .r tgm_file
;   tgm_file, date_i, date_f
;parameters:
;   date(_i,_f): format = [yyyy,mm,dd]
;
;dependencies:
;
;
;input files
;   dH and Dst data files
;
;output files:
;   txt files containing dH and Dst data with a certain format set to generate a global events
;   correlation and scatter analysis
;   imported to /master_thesis/datos/tgm/dir1/ssubdir/tgmdatayyyymmdd_yyyymmdd.txt
;
;   dir and subdir refer to directories which depend on the study and classification of event
;
;version
;   Dec, 2022
;

PRO tgm_file, date_i, date_f

	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]      
	
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 

    idate    = string(yr_i, mh_i, dy_i, FORMAT='(I4, I02,I02)')
    fdate    = string(yr_f, mh_f, dy_f, FORMAT='(I4, I02,I02)')
;###############################################################################     
; Generate the time series variables 
; define H variables                  
    H  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]) 
      
; Generate the time variables to plot TEC time series         
    tec  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'tec')
    med  = tec_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'med')
    
;###############################################################################
;Identifying the NAN values         
    tec = add_nan(tec, 999.0, 'equal')            
    med = add_nan(tec, 999.0, 'equal')
    H = add_nan(H, 999999.0, 'equal')
    H = add_nan(H, 99999.0, 'equal')    
;############################################################################################################
    outfile= 'home/isaac/MEGAsync/datos/tgm/article_events/list1.1/tgmdata'+idate+'_'+fdate+'.txt'
    OPENW, lun, outfile, /GET_LUN

    PRINTF, lun, 'DOY', 'hora', 'Dst', 'DH', FORMAT = '(3A,6A,6A,6A)'
    FOR i=0, N_ELEMENTS(dst)-1 DO BEGIN

      ;  print, doy[i], hour[i], dst[i], H[i], tec[i], med[i], FORMAT = '(I03, F5.1, I5, I5, F6.1, F5.2)'   
        PRINTF, lun, doy[i], hour[i], dst[i], H[i], FORMAT = '(I03, F5.1, I5, F6.1)'                                
    ENDFOR
    
    close,lun
    FREE_LUN, lun

END
