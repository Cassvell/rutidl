;;
;Name:
;	get_data_date
;purpose:
;	this routine will call read a certain number of files containing DST 
;   magnetic index.
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   Getting the data magnetic, date and time within several files. 
;
;calling sequence:
;
;
;parameters:
;
;
;dependencies:
;
;
;input files
;
;
;output files:
;
;



function kp_data, in, tp

	On_error, 2
	compile_opt idl2, HIDDEN

	;iyear	= initial[0]
	;imonth	= initial[1]
	;iday 	= initial[2]	

        header = 36             ; Defining number of lines of the header 
	;	file = DIALOG_PICKFILE(FILTER='*.dat')

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        year = string(in, format = '(I4)')
	    type_data = string(tp, format = '(A)')
		file_name = '../rutidl/kp/Kp_'+ year+'-01-01_'+ $
		year+'-12-31_'+type_data+'.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        DataStruct = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
                      DOY : 0, Kp: 0, Kp_str: '', Ap: 0}

		resulting_data0 = REPLICATE(DataStruct, number_of_lines-header)	
        ;print, number_of_lines-header-1, number_of_lines-header+1
        
        
		READS, data[header:number_of_lines-1], resulting_data0, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,8X,I3,X,I1,A1,X,I3)'
		
		indexes_07 = WHERE(resulting_data0[*].Kp_str EQ '-')
		indexes_03 = WHERE(resulting_data0[*].Kp_str EQ '+')
		indexes_00 = WHERE(resulting_data0[*].Kp_str EQ 'o')
		
		Kp_tmp = resulting_data0[*].Kp*10
		
		Kp_tmp[indexes_07] = Kp_tmp[indexes_07]-3
		Kp_tmp[indexes_03] = Kp_tmp[indexes_03]+3


                DataStruct2 = {year : 0, month : 0, day : 0, hour : 0, minute: 0, $
                      DOY : 0, Kp: 0, Ap: 0}

		resulting_data = REPLICATE(DataStruct2, number_of_lines-header)	

		resulting_data[*].year   = resulting_data0[*].year
		resulting_data[*].month  = resulting_data0[*].month
		resulting_data[*].day    = resulting_data0[*].day
		resulting_data[*].hour   = resulting_data0[*].hour
		resulting_data[*].minute = resulting_data0[*].minute
		resulting_data[*].DOY    = resulting_data0[*].DOY
		resulting_data[*].Kp     = Kp_tmp[*]
		resulting_data[*].Ap     = resulting_data0[*].AP
		
		
		return, resulting_data

end

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;creating output files for the geomagnetic storm events

pro list, resulting_data, in, tp
    
    On_error, 2
	compile_opt idl2, HIDDEN
	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;calling the get_data_date function
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	a = in
	b = tp
	
	dat = kp_data(a, b) 
    
        ;initial_year   = initial[0]
        ;initial_month  = initial[1]
        ;initial_day    = initial[2]

        ;final_year     = final[0]
        ;final_month    = final[1]
        ;final_day      = final[2]    
    
    
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Defining variables to write output files indicating the events where Dst index
; got lower than -150 nT.
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	;tmp = [string(dat.year)+string(dat.month)+string(dat.day)+string(dat.hour)]
	tmp = n_elements(dat.year)
	;print, tmp
	indexes = WHERE(dat.Kp GE 70, count)
	IF count LE 0 THEN MESSAGE, 'ERROR'
	
	
	kp0      = dat.Kp
    doy0     = dat.DOY
    year0    = dat.year
    month0   = dat.month
    day0     = dat.day
    hour0    = dat.hour 
    
    idx_kp  = kp0[indexes]
	doy     = doy0[indexes]
	year    = year0[indexes]
	month   = month0[indexes]
	day     = day0[indexes]
	hour    = hour0[indexes]
	;print, tmp[0:100]

    ;print, dat.DOY[indexes]


	idx_kp_tmp   = idx_kp*0
	doy_tmp      = doy*0
	year_tmp     = year*0
	month_tmp    = month*0
	day_tmp      = day*0
	hour_tmp     = hour*0



    ;i_pos   = where(idx_kp GT 6, count) ; i_pos are the positions where the Kp 
    ;criterium is true
    ;print, i_pos
    Date    = strmid(string(dat.year[0]), 8, 4)
    outfile = '../rutidl/output/'+'kp_'+Date[0]+'TGM.txt'; defining the 
    ;output file names and path directories
    
    j=0l
    idx_kp_tmp[j] = idx_kp_tmp[0]
	year_tmp[j]   = year[0]
	month_tmp[j]  = month[0]
    day_tmp[j]    = day[0]
	hour_tmp[j]   = hour[0]
    doy_tmp[j]    = doy[0]  

    FOR i=1l, N_ELEMENTS(idx_kp)-1 DO BEGIN
	IF doy_tmp[j] EQ doy[i] THEN BEGIN
	        IF idx_kp[i] GT idx_kp_tmp[j] THEN BEGIN
        	        idx_kp_tmp[j] = idx_kp[i]
		            year_tmp[j]   = year[i]
	                month_tmp[j]  = month[i]
        	        day_tmp[j]    = day[i]
	                hour_tmp[j]   = hour[i]
	                doy_tmp[j]    = doy[i]
	        ENDIF
	ENDIF ELSE BEGIN
                j+=1
                
	        idx_kp_tmp[j] = idx_kp[i]
	        year_tmp[j]   = year[i]
	        month_tmp[j]  = month[i]
        	day_tmp[j]    = day[i]
	        hour_tmp[j]   = hour[i]
	        doy_tmp[j]    = doy[i]
	ENDELSE
    ENDFOR
    
    ;print, i, idx_mag[i]
    ;print, outfile
    ;print, tmp[i], idx_mag[i]
    ;print, idx_mag[i], N_ELEMENTS(i)
   
    OPENW, lun, outfile, /GET_LUN

    ;PRINTF,lun, tmp[i], i, format = "(A50, 2X, I5)"
    ;idx   = where(idx_kp GT 6, count) 

    ;idx = findgen(n_elements(idx_kp))
 
    print, '###################################################################'
    print, 'lista de eventos de tormenta geomagnética '
    print, '###################################################################'
    print, '                                                                    '
    print, 'Descripción: Identificación de la fecha y hora en formato '
    print, 'fecha yy/mm/dd hh, día del año de cuando el índice Kp ascendió por'
    print, 'encima de 7, significando un evento de tormenta geomagnética intensa.'
    print, '                                                                    '
    
    print, format='(2X, "Fecha", 4X, "Hora", 3X, "DOY", 2X, "Indice Kp")'
    print, '---------------------------------'
   
    
    for i=0, N_ELEMENTS(indexes)-1 do begin
        ;idx   = where(idx_kp GT 6, count)
        
            if doy_tmp[i] ne 0 then begin

            ;idx_kp[idx] = idx_kp[i]

                print, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_kp_tmp[i], $
                             FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 4X, I03, 5X, I02)'  
                printf, lun, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_kp_tmp[i], $
                            FORMAT = '(I4, X, I02, X, I02, 2X, I02, 4X, I03, X, I02)'             
         
        endif
        
    endfor
    
    close,lun
    FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
   print, '                                                                    '
    print, '###################################################################'
    print, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    print, 'límites de ventana del tiempo en formato DOY entorno a cada evento'

end 


;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; procedure to plot each TGM event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

    
pro plotting, resulting_data, in, tp, ini, fn, PNG = png, JPEG = jpeg, DIR = dir

	On_error, 2
	compile_opt idl2, HIDDEN
	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Define a time window
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-    
	;inicio = long(in, format = '(I4)')
	;fin = long(fn, format = '(I4)')

    yr  = in
    dt  = tp  
    
    dat = kp_data(yr, dt)
    t = n_elements(dat.year)
    ;print, t
    
    ;initial_year   = initial[0]
    ;initial_month  = initial[1]
    ;initial_day    = initial[2]

    ;final_year     = final[0]
    ;final_month    = final[1]
    ;final_day      = final[2]
        
  ; year = string(iyear, format = '(I4)')
  ;	month = string(imonth, format = '(I2)')
  ;	day = string(iday, format = '(I2)')
        
    ;file_number    = (JULDAY(final_month, final_day, final_year) - $
    ;JULDAY(initial_month, initial_day, initial_year))+1
    ;data_file_name = strarr(file_number)
    ;print, file_number
    ;string_date     = strarr(file_number)
    
    

    i_kp = dat.Kp
    
    ;date1    = strmid(string(dat.year[0]), 8, 4)
    ;date2   = strmid(string(dat.month[0]), 11, 2)
    ;date3     = strmid(string(dat.day[0]), 11, 2)
    ;Date    = Date1[0]+'-'+Date2[0]+'-'+Date3[0]
    
    DUM = LABEL_DATE( DATE_FORMAT= ['%N/%D'])    ; time laber format for the 
    ;Dst graphics
    
    year    = dat.year
    month   = dat.month
    day     = dat.day
    hour    = dat.hour
    doy     = dat.DOY
    
    ;print, n_elements(doy)    
    ventana_t   = timegen(t, START=julday(1, doy[0], year[0], dat.hour[0]), units='H')


;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

    ;print, fn, in
    time_w  = ventana_t[ini:fn]
    tw = n_elements(time_w)
    tot_days= findgen(tw*8)/8.0
    ;print, n_elements(ventana_t)
    caldat, julday(1, ini, year[0]), ini_month, dy
    
    Kp      = i_kp[(ini*8)-8:fn*8]
    ;Kp_idx = i_kp[ini:fn]    
    ;PRINT, Kp
    
    ;kp_arr      = intarr(tw*8)
    ;print, n_elements(kp), tw*8
   ; for i = 0, tw-1 do begin
        ;print, (i+1)*8
        ;print, i*8
    ;    kp_arr[i*8:(i+1)*8] = Kp_idx[*]
     ;   kp[i*8] = Kp[*]
    ;endfor

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ;mydevice = !D.NAME
    ;SET_PLOT, 'ps'
    path = '../rutidl/output/globfig_to_reg/'
    date = string(year[0], ini_month, dy, FORMAT='(I4, "-", I02, "-", I02)')
    ;psname = path+'kp_'+date+'TGM.ps'
    ;page_height = 27.94
    ;page_width = 21.59
    ;plot_left = 5.
    ;plot_bottom = 5
    ;xsize = 7
    ;ysize = 5
    
    ;print, psname
    ;DEVICE, FILENAME=psname
    
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        IF tw GT 7 THEN tmp_spam = 1.5
        IF tw GT 15 THEN tmp_spam = 2.
        
        Xsize=fix(800*tmp_spam)
        Ysize=400
        ;DEVICE, decompose=0
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;definición de color
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        naranja   = 220
        amarillo  = 198
        verde     = 160
        negro     = 0
        gris_o    = 100
        blanco    = 255
        gris      = 130
        morado    = 248
        
       ; device, decomposed = 0
        
                        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ;doy_to_date = timegen(t, START=julday(1, doy[330*8], year[330*8]), FINAL = julday(1, doy[360*8], year[360*8]), units='H')
    ;print, tw, n_elements(doy_to_date), n_elements(tot_days)
    
     X_label   = STRARR(tw+1)+' '
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        old_month = ini_month
        ;print, old_month
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(1, ini, in)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR    
    
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; PLOTTING
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-


IF dt eq 'D' then plot_title = 'Definitive Planetary K index'
 
if dt eq 'P' then plot_title = 'Provisional Planetary K index'

if dt eq 'Q' then plot_title = 'Quick look Planetary K index'


time_title = 'Time window begin: '+string(year[0], ini_month, dy, $
                FORMAT='(I4, "/", I02, "/", I02)')+' 00:00 UTC'
                
MAG_source = 'Source: International Service of Geomagnetic Indices'                
    
    plot, tot_days, Kp, psym=6, /NoDATA, MAX_VALUE=9., XTICKS=tw, xminor=8, $
                    TITLE = plot_title, SUBTITLE = time_title, XTITLE = 's', YTITLE = 's', $
                    BACKGROUND = blanco, COLOR=negro, YRANGE=[0,90], YTICKS=9, $
                    YMINOR=0, CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
                    POSITION=[0.07,0.35,0.95,0.9], XSTYLE = 5, ystyle = 5,$
                    XTICKNAME=REPLICATE(' ', tw+1), XRANGE=[0, tw]

;,


        j = N_ELEMENTS(Kp)
        ;print, Kp , n_elements(tot_days); , n_elements(ventana_t[0:100])
        ;WHILE k_mex_data[j] GT 9 AND j GT 0 DO j--
        
        ;POLYFILL, [0.,0.125,0.125,0.]+tiempo[20], [0,0,Kp[20],Kp[20]], color=16
        
        ;for i = 0l, j-1 do begin
         ;   step  = (Kp[i] EQ 0) ? 0.1 : 0. ,
         ;   POLYFILL, [0.+space,0.125-space,0.125-space,0.+space]+tiempo[i], [0,0,Kp[i]+step,Kp[i]+step], color=verde
        
       ; endfor
        
        ;FOR i = 0, N_ELEMENTS(k_mex_data)-1 DO BEGIN
        FOR i = 0, j-1 DO BEGIN
                IF Kp[i] LE 90 THEN BEGIN
                                        ;LOADCT, 13, /SILENT
                                        color = 0
                                        step  = (Kp[i] EQ 0) ? 0.1 : 0.
                                        CASE 1 OF
                                                Kp[i] EQ 40 : color = amarillo
                                                ;Kp[i] GT 40 && Kp[i] LE 6 : color = naranja
                                                Kp[i] GE 40 : color = rojo
                                                ELSE       : color = verde
                                        ENDCASE
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], [0,0,Kp[i]$
                                        +step,Kp[i]+step], color=color
                                      ENDIF $
                                      ELSE BEGIN
                                        ;LOADCT, 0, /SILENT
                                        ;POLYFILL, [0.+space,0.125-space,0.125-$
                                       ; space,0.+space]+time[i], [0,0,9.,9.], $
                                        ;color=gris, /LINE_FILL, ORIENTATION=45.
                                        
                                        
                                        ;POLYFILL, [0.+space,0.125-space,0.125-$
                                        ;space,0.+space]+time[i], [0,0,9.,9.], $
                                        ;color=gris, /LINE_FILL, ORIENTATION=-45.
                                        
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], $
                                        [0,0,9.,9.], color=morado, /LINE_FILL, $
                                        ORIENTATION=45., linestyle = 0
                                         
                                         
                                        POLYFILL, [0.+space,0.125-space,0.125-$
                                        space,0.+space]+tot_days[i], $
                                        [0,0,9.,9.],color=morado, /LINE_FILL, $
                                         ORIENTATION=-45., linestyle = 0
                                      ENDELSE
        ENDFOR

 FOR i = 0, tw-1 DO BEGIN
                OPLOT, [i,i], [0.,90.], linestyle=1, COLOR=negro
                ;print, [i,i], [0.,9.]
        ENDFOR

        FOR i=50, 80, 10 DO OPLOT, [0,tw], [i,i], linestyle=1, COLOR=negro
        ;OPLOT, [0,file_number], [6.,6.], linestyle=1, COLOR=negro

        AXIS, XAXIS = 0, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.7, $
                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04


        AXIS, YAXIS = 0, YRANGE=[0,90], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTITLE = 'Kp index', $
                         COLOR=negro, $
                         CHARSIZE = 0.7, $
                         CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[0,90], $
                         YTICKS=9, $
                         YMINOR=1, $
                         YTICKNAME=[' ', ' ', ' ', ' ', ' ', 'G1', 'G2', 'G3', 'G4', 'G5'], $
                         COLOR=negro, $
                         CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1;, $


       ; XYOUTS, 0.01, 0.015 , /NORMAL, $
       ;         LANCE_banner, COLOR=negro, $
       ;         CHARSIZE = chr_size1, $
       ;                  CHARTHICK=chr_thick1

        XYOUTS, 0.01, 0.070 , /NORMAL, $
                MAG_source, COLOR=negro, $
                CHARSIZE = chr_size1, $
                         CHARTHICK=chr_thick1


    if tw gt 15 then begin
    
        XYOUTS, 0.01, 0.165 , /NORMAL, $
                'Color Code:        quiet,            disturbed,           storm,                   data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
       
        POLYFILL, [0.07,0.10,0.10,0.07], [0.165,0.165,0.195,0.195], color = verde, /NORMAL
        POLYFILL, [0.15,0.18,0.18,0.15], [0.165,0.165,0.195,0.195], color = amarillo, /NORMAL
        ;POLYFILL, [0.29,0.32,0.32,0.29], [0.165,0.165,0.195,0.195], color = naranja, /NORMAL
        POLYFILL, [0.25,0.28,0.28,0.25], [0.165,0.165,0.195,0.195], color = rojo, /NORMAL
        POLYFILL, [0.37,0.40,0.40,0.37], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        POLYFILL, [0.37,0.40,0.40,0.37], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0
    
    endif else begin
        XYOUTS, 0.01, 0.165 , /NORMAL, $
                'Color Code:       quiet,          disturbed,           storm,                 data not available.', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
      
        POLYFILL, [0.09,0.12,0.12,0.09], [0.165,0.165,0.195,0.195], color = verde, /NORMAL
        POLYFILL, [0.19,0.22,0.22,0.19], [0.165,0.165,0.195,0.195], color = amarillo, /NORMAL
        ;POLYFILL, [0.29,0.32,0.32,0.29], [0.165,0.165,0.195,0.195], color = naranja, /NORMAL
        POLYFILL, [0.32,0.35,0.35,0.32], [0.165,0.165,0.195,0.195], color = rojo, /NORMAL
        POLYFILL, [0.47,0.50,0.50,0.47], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=45., linestyle = 0
        POLYFILL, [0.47,0.50,0.50,0.47], [0.165,0.165,0.195,0.195], color = morado, /NORMAL, /LINE_FILL, ORIENTATION=-45., linestyle = 0
    endelse
    
        ;XYOUTS, 0.65, 0.015 , /NORMAL, $
        ;        UPDATE_banner, COLOR=negro, $
        ;        CHARSIZE = chr_size1, $
        ;                 CHARTHICK=chr_thick1

    Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak
       
    ;cgps2pdf, psname
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; open the post stript device
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-


        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'kp_'+date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'kp_'+date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'kp_'+date
                print, ''
        ENDIF

;print, file_number
;print, k_mex_data

        RETURN

    
end

    
    
    
    

