;
;Name:
;	index_plot.pro
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

PRO index_plot, date_i, date_f
	On_error, 2
	compile_opt idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]  
;##############################################################################
 ;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
        @set_up_commons
        set_up	    
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 	
    ;###############################################################################
        idate0 = string(yr_i, mh_i, format='(I4,I02)')
        TGM_n = event_case([yr_i,mh_i,dy_i])  
    ;###############################################################################   
        time= findgen(file_number*1440)/1440.0
        ;time_h = findgen(file_number*24)/24.0    
        Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
    ;###############################################################################
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
; define time variable  

    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), $
    FINAL=JULDAY(mh_f, dy_f, yr_f, 23,59), UNITS='Minutes')




    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])	  
    ;Date    = STRING(yr_i, mh_i, 17, yr_f, mh_f, 18, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
;###############################################################################
; Generate the time series DH and Dst                                


    data   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = data.H
    SQ = data.SQ
    idx = sym_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH = idx.symH
    ;asymH = idx.asyH
    H = add_nan(H, 99999.0, 'equal')  
    H = add_nan(H, 200.0, 'greater')  

    class = gms_class(station_code)

	;print, symH


    info = stationlist(class, station_code)

    mlat = info.mlat 
	l = mlat * (!PI/180)
	H_I = H - (symH * cos(l)) ;+ SQ 

;###############################################################################                
;identifying NAN percentage values in the Time Series

	DEVICE, true=24, retain=2, decomposed=0
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT    

	WINDOW, 0, XSIZE=900, YSIZE=500, TITLE='H data: '+ station_code

	up 		= max(H_I)
	down 	= min(H)

	plot, date_time, H, XTICKS=file_number, xminor=8, xstyle=1, ystyle=1, background=255, $
	color=0, CHARTHICK=2.0,     POSITION=[0.1,0.1,0.9,0.9], XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'],$
    XTICKLAYOUT = 2, XTICKINTERVAL = 1, YRANGE=[down,up]
	
	oplot, date_time, SQ, color = 250, thick = 2
	oplot, date_time, H_I, color = 70, thick = 3

	;path = set_var.local_dir+'output/indexplot/'+station_code	
	;test = FILE_TEST(path, /DIRECTORY) 
	;IF test EQ 0 THEN BEGIN
	;	FILE_MKDIR, path
	;	PRINT, 'PATH directory '+path
	;	PRINT, 'created'
	;ENDIF ELSE BEGIN
;		PRINT, ''
		
;	ENDELSE    
    ;psfile =  path+'idx_'+Date+'.eps'
   ;makepsfigure, kp, kmex, dst, dH, psfile, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]
END



pro makepsfigure, kp, kmex, dst, dH, psfile, date_i, date_f
On_error, 2
compile_opt idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
	file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1

    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	;X_label = xlabel([yr_i, mh_i, dy_i], file_number)


	tot_days = FINDGEN(file_number*8)/8.
	tot_days2       = FINDGEN(file_number*24)/24.
        
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10

	date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), $
	FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Hours')

	xtick_values = [date_time[0], date_time[N_ELEMENTS(date_time)-1]]  ; First and last tick
	date_label = LABEL_DATE(xtick_values, DATE_FORMAT=['%D %M', '%Y'])  ; Format them	

	CHARSIZE = 1.8
    CHARTHICK=2.0

	chr_size1 = 0.9
	chr_thick1= 1.0
	space     = 0.015
	rojo      = 248
	amarillo  = 190
	verde     = 150
	negro     = 0
	azul      = 90
	blanco    = 255
	gris      = 110
	morado    = 16
	naranja  = 220

    cgPolygon, [0.70,0.74,0.74,0.70], [0.80,0.80,0.806,0.806], color = 'green', /NORMAL, /fill
    cgPolygon, [0.79,0.83,0.83,0.79], [0.80,0.80,0.806,0.806], color = 'blue', /NORMAL  , /fill

    CGTEXT, 0.745, 0.802 , /NORMAL, $
    'Kp,            Kmex'
	
	cgPLOT, tot_days, Kp, PSYM=6, /NODATA, MAX_VALUE=9., XTICKS=file_number, XMINOR=8, $
	BACKGROUND = 'white', COLOR='black', YRANGE=[0,9], YTICKS=9, $
	YMINOR=0, CHARSIZE = 1.8, CHARTHICK=1.2, $
	POSITION=[0.15, 0.51, 0.9, 0.9]  , XSTYLE = 1, YSTYLE = 1,$
	XTICKFORMAT= '(A1)' , XRANGE=[0, file_number],$
	/NOERASE
	j = N_ELEMENTS(Kp)

	FOR i = 0, j-1 DO BEGIN
		; Default color for bars
		;color_final = 'blue'   ; Default color (for when both values are not even)
	
		; Check if both values are even
		IF Kp[i] EQ kmex[i] THEN BEGIN
			color_final = 'yellow'   ; Color the bar yellow if both are even
		ENDIF
	
		; Determine which value is higher and which is lower
		IF Kp[i] GT kmex[i] THEN BEGIN			
			bottom_value = kmex[i]  ; kmex is the bottom bar
			top_value = Kp[i]      ; Kp is the top bar
			top_color = 'green'     ; Color for the top bar (Kp)
			
		ENDIF ELSE BEGIN

			bottom_value = Kp[i]   ; Kp is the bottom bar
			top_value = kmex[i]    ; kmex is the top bar
			top_color = 'blue'     ; Color for the top bar (kmex)
		endelse
		
		IF Kp[i] LT kmex[i] THEN BEGIN
			bottom_value = kp[i]  ; kmex is the bottom bar
			top_value = Kmex[i]      ; Kp is the top bar
			bottom_color = 'green'     ; Color for the top bar (Kp)
		ENDIF ELSE BEGIN
			bottom_value = kmex[i]   ; Kp is the bottom bar
			top_value = kp[i]    ; kmex is the top bar
			bottom_color = 'blue'     ; Color for the top bar (kmex)
		endelse


		

		; Small offset for zero values
		step = (bottom_value EQ 0) ? 0.1 : 0.
	
		; Draw the bottom part (the smaller value)
		cgPolygon, [0.+space, 0.125-space, 0.125-space, 0.+space]+tot_days[i], $
				  [0, 0, bottom_value+step, bottom_value+step], COLOR=bottom_color, /fill
	
		; Draw the top part (the larger value)
		cgPolygon, [0.+space, 0.125-space, 0.125-space, 0.+space]+tot_days[i], $
				  [bottom_value, bottom_value, top_value+step, top_value+step], COLOR=top_color, /fill
		;print, Kp[i], kmex[i]
	ENDFOR
	

	
	FOR i = 0, file_number-1 DO BEGIN
	;cgOPLOT, [i,i], [0.,9.], LINESTYLE=1, COLOR=negro
	ENDFOR


    IF MAX(dst, /NAN) GT MAX(dH, /NAN) THEN up = MAX(dst, /NAN) ELSE up = MAX(dH, /NAN)
    IF MIN(dst, /NAN) LT MIN(dH, /NAN) THEN down = MIN(dst, /NAN) ELSE down = MIN(dH, /NAN)

    cgPLOT, date_time, dst, YRANGE=[down, up], CHARSIZE = 1.8, background='white', color='black', CHARTHICK=2.0, THICK=3,$
    XTICKS=file_number, 	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'year'], XTICKLAYOUT = 2,  $
	position = [0.15, 0.15, 0.9, 0.49], /noerase, XRANGE=[0, file_number], xstyle = 5, /nodata
	
	cgOPLOT, tot_days2, dst, LINESTYLE=0, color='black', THICK=3
    cgOPLOT, tot_days2, dH, LINESTYLE=0, color='red', THICK=3

    threshold = FLTARR(N_ELEMENTS(dH))
    threshold[*] = -50	
    ;print, 	threshold
    cgOPLOT, tot_days2, threshold, LINESTYLE=2, color='blue', THICK=2

    cgPolygon, [0.70,0.74,0.74,0.70], [0.4,0.4,0.406,0.406], color = 'black', /NORMAL, /fill
    cgPolygon, [0.79,0.83,0.83,0.79], [0.4,0.4,0.406,0.406], color = 'red', /NORMAL  , /fill

    CGTEXT, 0.745, 0.402 , /NORMAL, $
    'Dst,            dH', COLOR=0, $
    CHARSIZE = 1.8, $
    CHARTHICK=2.0


	CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
	COLOR='black', $
	XSTYLE=1,$ 
	XMINOR=8,$
	XTICKS=file_number,$
	;xTITLE = 'Time [days]',$ 
	CHARSIZE = 1.4, $
	TICKLEN=0.04,$
	CHARTHICK=1.5,$
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'],$
	XTICKUNITS=['day', 'year'] ,$
	XTICKLAYOUT = 2
					  
	CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)    
	COLOR='black', $
	XSTYLE=1,$
	XTICKS=file_number,$
	XMINOR=8,$   
	XTICKFORMAT='(A1)'
	
	cgPS_Close, density = 300, width = 1600 ;, /PNG 



end











