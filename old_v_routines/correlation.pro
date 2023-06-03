;
;
;
;
;Name:
;	correlation
;purpose:
;	this routine will make correlations between every time when Kp and Dst index 
;   satisfied the conditions of an intense geomagnetic storm.
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data analysis. 
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
function dst_data, initial

	On_error, 2
	compile_opt idl2, HIDDEN

	year = string(initial, format = '(I4)')
	;type_data = string(tp, format = '(A)')
		file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_D.dat'
		
        header = 25             ; Defining number of lines of the header 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-	
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		
	    IF opened_files NE N_ELEMENTS(file) THEN begin
	        file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_P.dat'
	        file = FILE_SEARCH(file_name, COUNT=opened_files) 
    	    IF opened_files NE N_ELEMENTS(file) THEN begin
    	        file_name = '../rutidl/dst/Dst_'+ year+'-01-01_'+year+'-12-31_Q.dat'
	            file = FILE_SEARCH(file_name, COUNT=opened_files)    	        
    	        IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+'not found'  
    	    ENDIF    	    	    
	    ENDIF

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

		res_dat = REPLICATE(DataStruct, number_of_lines-header)	        
        
		READS, data[header:number_of_lines-1], res_dat, $
		FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,8X,I3,X,I6)'
		
		return, res_dat
end


pro magdat, res_dat, year, doy_i, doy_f

	On_error, 2
	compile_opt idl2, HIDDEN
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of Dst Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    year = ['2003', '2004', '2005', '2015', '2017', '2017']
    ;dt  = tp 
   
    for i=0, 5 do begin
    
    t = n_elements(dat.year[i])
    ;print, t
    
    i_dst = dat.Dst[i]
    ;print, t
    year = dat.year[i]
    month= dat.month[i]
    day  = dat.day[i]
    hour = dat.hour[i]
    tiempo = TIMEGEN(t, START=julday(dat.month[0], dat.day[0],  $
                     year[0], dat.hour[0]), UNITS='Hours')                                     
        
    ini_time  = JULDAY(1, ini, initial)
    fnl_time  = JULDAY(1, fn, initial)
    
    time_w  = tiempo[ini:fn]
    tw = n_elements(time_w)
    tot_days= findgen(tw*24)/24.0
    tot_days_2= (findgen(tw*24)/24.0)-6
                                             
    caldat, ini_time, ini_month, ini_day, ini_year
    caldat, fnl_time, fnl_month, fnl_day, fnl_year
    
    Date = string(year[0], ini_month, ini_day, FORMAT='(I4, "-", I02, "-", I02)')    
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of DH Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
	yr_i	= ini_year
	mh_i	= ini_month
	dy_i 	= ini_day	

	yr_f	= fnl_year
	mh_f	= fnl_month
	dy_f 	= fnl_day
		
;##############################################################################
; reading data files
;##############################################################################
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = strarr(file_number)
        string_date     = strarr(file_number)
       
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        fecha = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4,I02,I02,"_",I4,I02,I02)')

        H    = FLTARR(file_number*24)
        D    = FLTARR(file_number*24)
        Z    = FLTARR(file_number*24)
        F    = FLTARR(file_number*24)
        N    = FLTARR(file_number*24)
        
        H_STDESV    = FLTARR(file_number*24)
        D_STDESV    = FLTARR(file_number*24)
        Z_STDESV    = FLTARR(file_number*24) 
        F_STDESV    = FLTARR(file_number*24)
        N_STDESV    = FLTARR(file_number*24)
                       
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        ;print, tmp_year, tmp_month, tmp_day
                        dat = DH_teo([tmp_year, tmp_month, tmp_day])
                        
                        H[i*24:(i+1)*24-1] = dat.H[*]
                        D[i*24:(i+1)*24-1] = dat.D[*]
                        Z[i*24:(i+1)*24-1] = dat.Z[*]
                        F[i*24:(i+1)*24-1] = dat.F[*]
                        N[i*24:(i+1)*24-1] = dat.N[*]
                                                
                        H_STDESV[i*24:(i+1)*24-1] = dat.H_stdesv[*]
                        D_STDESV[i*24:(i+1)*24-1] = dat.D_stdesv[*]
                        Z_STDESV[i*24:(i+1)*24-1] = dat.Z_stdesv[*]  
                        F_STDESV[i*24:(i+1)*24-1] = dat.F_stdesv[*]
                        N_STDESV[i*24:(i+1)*24-1] = dat.N_stdesv[*]
                                                                                              
                ENDIF ELSE BEGIN
                         H[i*24:(i+1)*24-1] = 999999.0
                         D[i*24:(i+1)*24-1] = 999999.0
                         Z[i*24:(i+1)*24-1] = 999999.0
                         F[i*24:(i+1)*24-1] = 999999.0
                         N[i*24:(i+1)*24-1] = 999999.0

                         N_STDESV[i*24:(i+1)*24-1] = 999999.0                        
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                         N_STDESV[i*24:(i+1)*24-1] = 999999.0
                ENDELSE                
        ENDFOR
        
        for i=0, n_elements(H)-1 do begin
            ;idx = where(H eq 999999.0, inan)
            if H[i] eq 999999.0 then begin
                H[where(H[*] eq 999999.0)] = !Values.F_NAN          
            endif
        endfor
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Generate the time variables to plot time series of Dst Index
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        IF tw GT 7 THEN tmp_spam = 1.5
        IF tw GT 15 THEN tmp_spam = 2.
        
        Xsize=fix(800*tmp_spam)
        Ysize=800
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
        azul      = 30
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Write a post Script
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    path = '../rutidl/output/globfig_to_reg/'

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    
    dst      = i_dst[(ini*24)-24:fn*24]
 
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
;print, up, down
time_title = '2005/05/14 to 2005/05/18'
                
MAG_source = 'Source: International Service of Geomagnetic Indices'  
        
   ; plot, tot_days, dst, XTICKS=tw, xminor = 8, POSITION=[0.07,0.55,0.95,0.9],$
   ; XTICKFORMAT='LABEL_DATE', xstyle = 5, ystyle=6,$
   ; title = time_title, ytitle = 'Indice DST [nT]',  XTICKNAME=REPLICATE(' ', tw+1), $
   ; XRANGE=[0, tw], YRANGE=[down,up], BACKGROUND = blanco, COLOR=negro;, /noerase             
    ;print, t
   ; oplot, tot_days, H, color=rojo
     
;     down  = min(dst)
;     up    = max(dst)    
   
    
    plot, dst, H, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = chr_size1, CHARTHICK=chr_thick1, POSITION=[0.20,0.20,0.80,0.80], $
     XSTYLE = 5, ySTYLE = 6, psym=2

     
        AXIS, XAXIS = 0, XRANGE=[-500,100], $
                         ;XTICKS=tw, $
                       ;  XMINOR=8, $
                         XTITLE = 'Dst [nT] ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                       ;  XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.5, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[-500,100], $
                     ;    XTICKS=tw, $
                      ;   XMINOR=8, $
                   ;      XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[-500,100], $
                         YTITLE = 'DH [nT]', $
                         COLOR=negro, $
                         CHARSIZE = 0.5;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[-500,100], $
                         YTITLE = 'DH [nT]', $
                       ;  YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         CHARSIZE = 0.5;, $
                        ; CHARTHICK=chr_thick1;, $   


    ;if tw gt 15 then begin
    ;    XYOUTS, 0.05, 0.170 , /NORMAL, $
     ;           '            Dst index,               DH index', COLOR=negro, $
     ;           CHARSIZE = chr_size1, $
     ;           CHARTHICK=chr_thick1     
     
  ;      POLYFILL, [0.07,0.10,0.10,0.07], [0.178,0.178,0.180,0.180], color = negro, /NORMAL
  ;      POLYFILL, [0.19,0.22,0.22,0.19], [0.178,0.178,0.180,0.180], color = rojo, /NORMAL
   ; endif else begin
    ;    XYOUTS, 0.03, 0.168 , /NORMAL, $
      ;          '            Dst index,         DH index', COLOR=negro, $
       ;         CHARSIZE = chr_size1, $
        ;        CHARTHICK=chr_thick1     
     
  ;      POLYFILL, [0.07,0.10,0.10,0.07], [0.178,0.178,0.180,0.180], color = negro, /NORMAL
   ;     POLYFILL, [0.19,0.22,0.22,0.19], [0.178,0.178,0.180,0.180], color = rojo, /NORMAL
  ;  endelse
   ; LOADCT, 0, /SILENT
    
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
                write_jpeg, path+'dst_DH_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'dst_DH_'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'dst_DH_'+Date
                print, ''
        ENDIF
        RETURN
end

function get_kp, year, type_data2
	On_error, 2
	compile_opt idl2, HIDDEN


    return, r_data_kp


end

pro correlation, r_data_dst, r_data_kp, year, type_data1, type_data2

    	On_error, 2
	compile_opt idl2, HIDDEN

    yr      = year
    typ1    = type_data1
    typ2    = type_data2
    
    dst_dat = get_dst(yr, typ1)
    kp_dat  = get_kp(yr, typ2)
    
    

    dst     = dst_dat.dst
    kp      = kp_dat.kp
    
    doy_d   = dst_dat.doy
    doy_k   = kp_dat.doy
    
    hour_d  = dst_dat.hour
    hour_k  = kp_dat.hour

    yr_d    = dst_dat.year
    yr_k    = kp_dat.year
    
    mt_d    = dst_dat.month
    mt_k    = kp_dat.month
    
    dy_d    = dst_dat.day
    dy_k    = kp_dat.day
   ; i = 0
   ; j = 0
; imprimir y escribir un archivo con prinf y /append para todas las coincidencias de todos los años disponibles
    outfile = '../rutidl/output/'+'TGM_Intensas_list2.txt'
    
    OPENW, lun, Outfile, /GET_LUN, /append


print, '#######################################################################'
print, 'Lista de fechas donde tanto el índice Kp como el Dst cumplieron con el '
print, 'criterio que indica la ocurrencia de una tormenta geomagnética intensa '
print, '#######################################################################'
print, '                                                                       '
print, format = '(8X, "año", 11X, "mes", 9X, "día", 8X, "Dst", X, "Kp")'
print, '                                                                       '

    for i=0, n_elements(dst)-1 do begin
        for j = 0, n_elements(kp)-1 do begin
        
            if doy_d[i] eq doy_k[j] then begin
                mt_d[i]     = mt_k[j]
                dy_d[i]     = dy_k[j]
                doy_d[i]    = doy_k[j]
               ; hour_d[i]   = hour_k[j]
                
                print, yr_d[i], mt_d[i], dy_d[i], dst[i], kp[j], $
                format = '(4I, X, I02, X, I02, 2X, I4, 2X, I02)'
                
                printf, lun, yr_d[i], mt_d[i], dy_d[i], dst[i], kp[j], $
                format = '(4I, X, I02, X, I02, 2X, I4, 2X, I02)'
            endif
        
        endfor
 
    
    endfor



    close,lun
    FREE_LUN, lun












end



    for i=0, n_elements(dh_dat.DH)-1 do begin
        for j = 0, n_elements(km_dat.km)-1 do begin
        
            if fecha_km[j] eq fecha_dh[i] then begin
                dh_dat.year[i]     = km_dat.year[j]
                dh_dat.month[i]    = km_dat.month[j]
                dh_dat.day[i]      = km_dat.day[j]
               ; hour_d[i]   = hour_k[j]
                
                print, km_dat.year[j], km_dat.month[j], km_dat.day[j], dh_dat.DH[i], $
                km_dat.km[j], format = '(4I, X, I02, X, I02, 2X, I4, 2X, I02)'
                
                printf, lun, km_dat.year[j], km_dat.month[j], km_dat.day[j], $
                dh_dat.DH[i], km_dat.km[j],$
                format = '(4I, X, I02, X, I02, 2X, I4, 2X, I02)'
            endif        
        endfor    
    endfor









