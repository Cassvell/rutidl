;
;Name:
;	k_plot.pro
;purpose:
;	plot kmex and kp data in line format
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
;   .r kmx_plot
;   kmx_plot, date_i, date_f
;parameters:
;   date(_i,_f): format = [yyyy,mm,dd]
;
;dependencies:
;
;
;input files
;   kmex data files
;
;output files:
;   kp and kmex in an IDL GUI window
;
;version
;   Dec, 2022
;

PRO k_plot, date_i, date_f
	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]      
;###############################################################################    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    tot_days= findgen(file_number*8)/8.0    
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
;###############################################################################
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
    k_mex   = add_nan(k_mex, 9.0, 'greater')   


    print, FORMAT='(6X,"kp",10X,"k_mex")'
    print, MAX(kp), MAX(k_mex)
   ; DEVICE
   ; WINDOW, 0,  XSIZE=800, YSIZE=800, TITLE='kmex vx kp'

   ; PLOT, tot_days, kp, YRANGE=[0.0,9.0]
   ; OPLOT, tot_days, k_mex, LINESTYLE=3





END















