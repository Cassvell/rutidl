;
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



function dst_data, initial, tp

	On_error, 2
	compile_opt idl2, HIDDEN

;	iyear	= initial[0]
;	imonth	= initial[1]
;	iday 	= initial[2]	

        header = 25             ; Defining number of lines of the header 
	;	file = DIALOG_PICKFILE(FILTER='*.dat')

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

	year = string(initial, format = '(I4)')
	type_data = string(tp, format = '(A)')
		file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+ $
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
        DOY : 0, Dst: 0}

		resulting_data = REPLICATE(DataStruct, number_of_lines-header)	
        ;print, number_of_lines-header-1, number_of_lines-header+1
        
        
		READS, data[header:number_of_lines-1], resulting_data, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,8X,I3,X,I6)'
		
		
		;print, resulting_data[25:100]
		;print, DOY;, Dst[0:1000], FORMAT = '(A24, X, A24)'
		;plot, resulting_data.year, resulting_data.Dst
		
		return, resulting_data

end

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;creating output files for the geomagnetic storm events

pro list, resulting_data, initial, tp
    
    On_error, 2
	compile_opt idl2, HIDDEN
	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;calling the get_data_date function
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	a = initial
	b = tp
	
	dat = dst_data(a, b) 

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Defining variables to write output files indicating the events where Dst index
; got lower than -150 nT.
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	;tmp_y = strmid(string(dat.year), 8, 4)
	;tmp_m = strmid(string(dat.month), 11, 2)
	;tmp_d = strmid(string(dat.day), 11, 2)
	;tmp_h = strmid(string(dat.hour), 11, 2)
	
	;tmp = tmp_y+'-'+tmp_m+'-'+tmp_d
	tmp = n_elements(dat.year)
	
	indexes = WHERE(dat.Dst LE -150, count)
	IF count LE 0 THEN MESSAGE, 'ERROR'
	
	
	dst0      = dat.Dst
    doy0     = dat.DOY
    year0    = dat.year
    month0   = dat.month
    day0     = dat.day
    hour0    = dat.hour 
    
    idx_dst  = dst0[indexes]
	doy     = doy0[indexes]
	year    = year0[indexes]
	month   = month0[indexes]
	day     = day0[indexes]
	hour    = hour0[indexes]
	;print, tmp[0:100]
    
	idx_dst_tmp   = idx_dst*0
	doy_tmp      = doy*0
	year_tmp     = year*0
	month_tmp    = month*0
	day_tmp      = day*0
	hour_tmp     = hour*0   

    j=0l
    idx_dst_tmp[j] = idx_dst_tmp[0]
	year_tmp[j]   = year[0]
	month_tmp[j]  = month[0]
    day_tmp[j]    = day[0]
	hour_tmp[j]   = hour[0]
    doy_tmp[j]    = doy[0]   
    
    Date    = strmid(string(dat.year[0]), 8, 4)
    outfile = '../rutidl/output/'+'dst_'+Date[0]+'TGM.txt'   ; defining the output file
    ;names and path directories

    FOR i=1l, N_ELEMENTS(idx_dst)-1 DO BEGIN
	IF doy_tmp[j] EQ doy[i] THEN BEGIN
	        IF idx_dst[i] LT idx_dst_tmp[j] THEN BEGIN
        	        idx_dst_tmp[j] = idx_dst[i]
		            year_tmp[j]   = year[i]
	                month_tmp[j]  = month[i]
        	        day_tmp[j]    = day[i]
	                hour_tmp[j]   = hour[i]
	                doy_tmp[j]    = doy[i]
	        ENDIF
	ENDIF ELSE BEGIN
                j+=1
                
	        idx_dst_tmp[j] = idx_dst[i]
	        year_tmp[j]   = year[i]
	        month_tmp[j]  = month[i]
        	day_tmp[j]    = day[i]
	        hour_tmp[j]   = hour[i]
	        doy_tmp[j]    = doy[i]
	ENDELSE
    ENDFOR
     
    
    ;print, i, idx_mag[i]
    ;print, outfile
    ;print, tmp
    ;print, idx_mag[i], N_ELEMENTS(i)
   
 ;   OPENW, lun, Outfile, /GET_LUN
    
    
    
    print, '###################################################################'
    print, 'lista de eventos de tormenta geomagnética '
    print, '###################################################################'
    print, '                                                                    '
    print, 'Descripción: Identificación de la fecha y hora en formato '
    print, 'fecha yy/mm/dd hh, día del año de cuando el índice Dst descendió por'
    print, 'debajo de -150 nT, lo que es un indicativo de la ocurrencia de un '
    print, 'un evento de tormenta geomagnética intensa y de interés  '
    print, '                                                                   '
    print, format='("Fecha", 6X, "Hora", X, "DOY", 8X, "Indice Dst")'
    print, '------------------------------'
    ;PRINTF,lun, tmp[i], i, format = "(A11, 2X, I5)"
   
    for i=0, N_ELEMENTS(indexes)-1 do begin

           if doy_tmp[i] ne 0 then begin

            ;idx_kp[idx] = idx_kp[i]

                print, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_dst_tmp[i], $
                             FORMAT = '(I4, "-", I02, "-", I02, 2X, I02, 2X, I03, X, I)'  
               ; printf, lun, year_tmp[i], month_tmp[i], day_tmp[i], hour_tmp[i], doy_tmp[i], idx_dst_tmp[i], $
                ;            FORMAT = '(I4, X, I02, X, I02, 2X, I02, 2X, I03, X, I4)'             
            endif 
   
        ;printf, lun, year[i], month[i], day[i], hour[i], doy[i], idx_kp[i], $
        ;FORMAT = '(I4, "/", I02, "/", I02, X, I02, 2X, I03, 4X, I)'       
    endfor
    
  ;  close,lun
   ; FREE_LUN, lun
   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
    print, '                                                                    '
    print, '###################################################################'
    print, 'A continuación, ejecutar el procedimiento plotting eligiendo dos'
    print, 'límites de ventana del tiempo en formato DOY entorno a cada evento'

   ;PRINT, DATETIME[i], Dst[i], format = "(A29)"
    
    
end 

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; procedure to plot each TGM event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

    
pro plotting, resulting_data, initial, tp, ini, fn

	On_error, 2
	compile_opt idl2, HIDDEN
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of Dst Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

    yr  = initial
    dt  = tp  

    dat = dst_data(yr, dt)
    t = n_elements(dat.year)
    ;print, t
    
    i_dst = dat.Dst
    
    ;Date1   = strmid(string(dat.year[0]), 8, 4)
    ;Date2   = strmid(string(dat.month[0]), 11, 2)
;    Date3   = strmid(string(dat.day[0]), 11, 2)

    
    DUM = LABEL_DATE( DATE_FORMAT= ['%Y/%N/%D'])    ; time laber format for the 
    ;Dst graphics

    
    tiempo = TIMEGEN(t, START=julday(dat.month[0], dat.day[0],  $
                     dat.year[0], dat.hour[0]), UNITS='Hours')

    time_w  = tiempo[ini:fn]
    tw = n_elements(time_w)
    tot_days= findgen(tw*24)/24.0
                         
    year = dat.year                 
    caldat, julday(1, ini, year[0]), ini_month, dy
    
    Date = string(year[0], ini_month, dy, FORMAT='(I4, "-", I02, "-", I02)')   
        
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
             
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        amarillo  = 220
        verde     = 180
        negro     = 0
        gris_o    = 100
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ;mydevice = !D.NAME
    ;SET_PLOT, 'ps'
    path = '../rutidl/output/globfig_to_reg/'
    ;psname = path+Date+'TGM.ps'
    ;page_height = 27.94
    ;page_width = 21.59
    ;plot_left = 5.
    ;plot_bottom = 5
    ;xsize = 7
    ;ysize = 5
    ;print, psname
    ;DEVICE, FILENAME=psname

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ;print, fn, in

    ;print, n_elements(ventana_t)
        
    ;print, year
     ;print, tw
    
    dst      = i_dst[(ini*24)-23:fn*24]
    ;Kp_idx = i_kp[ini:fn]    
    ;PRINT, n_elements(dst)
    print, n_elements(tot_days), n_elements(dst)
    ;kp_arr      = intarr(tw*8)
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
     X_label   = STRARR(tw+1)+' '
    ; print, n_elements(x_label)
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        old_month = ini_month
       ; print, old_month
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(1, ini, initial)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR    

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-


IF dt eq 'D' then plot_title = 'Definitive Dst index'
 
if dt eq 'P' then plot_title = 'Provisional Dst index'

if dt eq 'Q' then plot_title = 'Quick look Dst index'

    
    up      = MAX(dst, ind)
    down    = MIN(dst, idx) 
;print, up, down
time_title = 'Time window begin: '+string(year[0], ini_month, dy, $
                FORMAT='(I4, "/", I02, "/", I02)')+' 00:00 UTC'
                
MAG_source = 'Source: International Service of Geomagnetic Indices'  
        
    plot, tot_days, dst, XTICKS=tw, xminor = 8, POSITION=[0.07,0.35,0.95,0.9],$
    XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=6,$
    title = plot_title,  SUBTITLE = time_title,$
	ytitle = 'Indice DST [nT]',  XTICKNAME=REPLICATE(' ', tw+1), XRANGE=[0, tw], $
	YRANGE=[down,up], BACKGROUND = blanco, COLOR=negro             
    ;print, t


    
    for i = -150., 0., 50. do oplot, [0,tw], [i,i], linestyle=1, COLOR=negro
    ;OPLOT, [0,tw], [0.,0.], linestyle=1, COLOR=negro
    ;OPLOT, [0,tw], [-50.,-50.], linestyle=1, COLOR=negro
    ;OPLOT, [0,tw], [-100.,-100.], linestyle=1, COLOR=negro
    ;OPLOT, [0,tw], [-150.,-150.], linestyle=1, COLOR=negro    

    
        AXIS, XAXIS = 0, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.5, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,tw], $
                         XTICKS=tw, $
                         XMINOR=8, $
                         XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = 'Dst index [nT]', $
                         COLOR=negro, $
                         CHARSIZE = 0.5;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[down,up], $
                         ;YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         CHARSIZE = 0.5;, $
                        ; CHARTHICK=chr_thick1;, $    
   
    LOADCT, 0, /SILENT
    
    ;DEVICE, /CLOSE
    ;SET_PLOT, mydevice
    ;cgps2pdf, psname
     ;Image=TVRD()  
    ;pngname = path+Date+'TGM.png'
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   
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
                write_jpeg, path+'dst_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'dst_'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'dst_'+Date
                print, ''
        ENDIF

;print, file_number
;print, k_mex_data

        RETURN


end
    

    
    
    
    

