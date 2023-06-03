;
;Name:
;	get_data_date
;purpose:
;	this routine will call read a certain number of files containing 
;   interplanetary data measurements
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
pro lista, file
file = '../rutidl/lista_tgmi.txt'
readcol, file, date, hour, DST, F='A,A,I'

year    = strmid(date, 0, 4)
month   = strmid(date, 5, 2)
day     = strmid(date, 8, 2)
fecha   = year+'/'+month+'/'+day
print, fecha

end

function ip_dat, date

	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	

        header = 60      ; Defining number of lines of the header 
	;	file = DIALOG_PICKFILE(FILTER='*.dat')

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        date = string(year, month, day, format = '(I4, "-", I02, "-", I02)')
        ;yr = string(year, format = '(I4)')
        ;mt = string(month, format = '(I02)')
        ;dy = string(day, format = '(I02)')
		
		file_name = '../rutidl/ip/'+date+'.dat'
       ; file_name = '../rutidl/ip/'+yr+'-'+mt+'-'+dy+'.csv'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
	   ; print, number_of_lines
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;extracting data and denfining an structure data
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        DStruct = {YEAR : 0, DOY : 0, hour : 0, bartels : 0, id_ifm : 0, $
                      id_sw : 0, n_points_ifm: 0, n_points_plasna : 0, B_esc : 0., $
                      B_vec : 0., B_lat : 0., B_long : 0., Bx : 0., By : 0., Bz : 0., $
                      By0 : 0., Bz0 : 0., B_rms_esc : 0., B_rms_vec : 0., Bx_rms : 0., $
                      By_rms : 0., Bz_rms : 0., Temp : 0., density_p : 0., v_p : 0., $
                      flow_long : 0., flow_lat : 0., alfa_prot : 0., sigma_T : 0., $
                      sigma_n : 0., sigma_v : 0., sigma_phi : 0., sigma_theta : 0., $
                      sigma_ratio : 0., flow_P : 0., E : 0., beta_p : 0., alfven_M : 0., $
                      mag_M : 0., q_invariant : 0., kp : 0, R : 0, dst : 0, ap : 0, $
                      f10_idx : 0., AE : 0, AL : 0, AU : 0, pc_idx : 0., lyman_alfa : 0.,$
                      p_flux_1MeV : 0., p_flux_2MeV : 0., p_flux_4MeV : 0., p_flux_10MeV : 0.,$
                      p_flux_30MeV : 0., p_flux_60MeV : 0., flux_FLAG : 0}
                                      

		resulting_data = REPLICATE(DStruct, number_of_lines-header)	
		READS, data[header:number_of_lines-1], resulting_data, $
FORMAT='(I4,I4,I3,I5,I3,I3,I4,I4,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,'+$
'F9,F6,F6,F6,F6,F6,F9,F6,F6,F6,F6,F6,F6,F7,F7,F6,F5,F7,I3,I4,I6,I4,F6,I5,'+$
'I6,I6,F6,F9,F10,F9,F9,F9,F9,F9,I3)'
		
		return, resulting_data

end

pro plotting, resulting_data, date, PNG = png, JPEG = jpeg, DIR = dir

	On_error, 2
	compile_opt idl2, HIDDEN

	yr	= date[0]
	mh	= date[1]
	dy 	= date[2]	

    dat = ip_dat([yr, mh, dy])
    
    year = dat.year
    doy  = dat.DOY
    hour = dat.hour
    
    time = timegen(n_elements(year), START=julday(1, doy[0], year[0]))
    
    fecha= string(yr, mh, dy, FORMAT='(I4, "-", I02, "-", I02)')
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- 
    time_w = n_elements(time)/24
    tot_days= findgen(time_w*24)/24.0 
    ;print, n_elements(tot_days)
    
    X_label   = STRARR(time_w+1)+' '
    ; print, n_elements(x_label)
    
        months    = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        caldat, julday(1, doy[0], year[0]), ini_month, ini_day
        old_month = ini_month
       ; print, old_month
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(1, doy[0], year[0])

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? string(tmp_day, FORMAT='(I02)') : months[tmp_month-1]+' '+string(tmp_day, FORMAT='(I02)')
                old_month = tmp_month
        ENDFOR      

    path = '../rutidl/output/ip_fig/'

    
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        tmp_spam = 1
        IF time_w GT 7 THEN tmp_spam = 1.5
        IF time_w GT 15 THEN tmp_spam = 2.
        
        Xsize=fix(800*tmp_spam)
        Ysize=800
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
        azul      = 80
        blanco    = 255
        gris      = 150
        morado    = 16
       ; device, decomposed = 0
        
                        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
 
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; plot the Dst time series for each event
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

MAG_source = 'Omniweb'  

B_vec   = dat.B_vec
Bz      = dat.Bz 
v_p     = dat.v_p
flow_p  = dat.flow_P
beta_p  = dat.beta_p  
alfven_M= dat.alfven_M
mag_M   = dat.mag_M
Ey      = dat.E
 
        for i=0, n_elements(B_vec)-1 do begin
            if B_vec[i] eq 999.900 then begin
                B_vec[where(B_vec[*] eq 999.900)] = !Values.F_NAN          
            endif
        endfor    

        for i=0, n_elements(dat.Bz)-1 do begin
            if Bz[i] eq 999.900 then begin
                Bz[where(Bz[*] eq 999.900)] = !Values.F_NAN          
            endif
        endfor    

        for i=0, n_elements(dat.v_p)-1 do begin
            if v_p[i] eq 9999.00 then begin
                v_p[where(v_p[*] eq 9999.00)] = !Values.F_NAN          
            endif
        endfor 
        
        for i=0, n_elements(dat.flow_P)-1 do begin
            if flow_P[i] eq 99.99 then begin
                flow_P[where(flow_P[*] eq 99.99)] = !Values.F_NAN          
            endif
        endfor  

        for i=0, n_elements(dat.beta_p)-1 do begin
            if beta_p[i] eq 999.99 then begin
                beta_p[where(beta_p[*] eq 999.99)] = !Values.F_NAN          
            endif
        endfor           

        for i=0, n_elements(dat.alfven_M)-1 do begin
            if alfven_M[i] eq 999.900 then begin
                alfven_M[where(alfven_M[*] eq 999.900)] = !Values.F_NAN          
            endif
        endfor 

        for i=0, n_elements(dat.alfven_M)-1 do begin
            if mag_M[i] eq 99.9000 then begin
                mag_M[where(mag_M[*] eq 99.9000)] = !Values.F_NAN          
            endif
        endfor         

        for i=0, n_elements(dat.E)-1 do begin
            if Ey[i] eq 999.990 then begin
                Ey[where(Ey[*] eq 999.990)] = !Values.F_NAN          
            endif
        endfor 
        print, max(Ey), min(Ey)
    if max(B_vec) gt max(Bz) then up = max(B_vec) else up = max(Bz)
    if min(B_vec) lt min(Bz) then down = min(B_vec) else down = min(Bz)

    plot, tot_days, B_vec, xminor = 8, xstyle = 5, ystyle = 6, XTICKS=time_w,$
    BACKGROUND = blanco, YRANGE=[down,up], XTICKNAME=REPLICATE(' ', time_w+1),$
    ytitle = 'nT', POS=[0.1, 0.81, 0.95, 0.95], thick=1, PSym=0, COLOR=rojo
    
    oplot, tot_days, Bz, color = azul
        AXIS, XAXIS = 0, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         ;XTICKNAME='', $
                         XTICKFORMAT="(A1)",$
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = 'B [nT]', $
                         COLOR=negro, $
                         CHARSIZE = 0.6
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[down,up], $
                         ;YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         YTITLE = 'Bz [nT]', $
                         CHARSIZE = 0.6
                        ; CHARTHICK=chr_thick1;, $  

    up2     = MAX(v_p, ind1)
    down2   = MIN(v_p, idx1)  
    ;print, up1, down1            
    plot, tot_days, v_p, xminor = 8, xstyle = 5, ystyle = 6,  XTICKNAME=REPLICATE(' ', time_w+1), $
    ytitle = 'km/s' , /noerase, POS=[0.1, 0.66, 0.95, 0.80], thick=1, PSym=0, $
    BACKGROUND = blanco, COLOR=negro, YRANGE=[down2,up2]

        AXIS, XAXIS = 0, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         ;XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"
                         
        AXIS, XAXIS = 1, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"

        AXIS, YAXIS = 0, YRANGE=[down2,up2], $
                         YTITLE = 'V [Km/s]', $
                         COLOR=negro, $
                         CHARSIZE = 0.6
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[down2,up2], $
                         ;YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         ;YTITLE = 'Bz [nT]', $
                         CHARSIZE = 0.6
                        ; CHARTHICK=chr_thick1;, $    
 
    up3 = max(flow_p)
    down3 = min(flow_p)        
    plot, tot_days, flow_P , xminor = 8, xstyle = 5, ystyle = 6,  XTICKNAME=REPLICATE(' ', time_w+1),$
    ytitle = 'nPa' , /noerase, POS=[0.1, 0.51, 0.95, 0.65], thick=1, PSym=0, $
    BACKGROUND = blanco, COLOR=morado,  YRANGE=[down3,up3]
    
        AXIS, XAXIS = 0, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         ;XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"
                         
        AXIS, XAXIS = 1, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTICKNAME=REPLICATE(' ', tw+1), $
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"

        AXIS, YAXIS = 0, YRANGE=[down3,up3], $
                         YTITLE = 'P [nPa]', $
                         COLOR=negro, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[down3,up3], $
                         ;YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         YTITLE = 'Beta', $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $  
   
    up4     = max(beta_p)
    down4   = min(beta_p)
    plot, tot_days, beta_p , xminor = 8, xstyle = 5, ystyle = 6,$
    ytitle = 'Magnetosonic Mach' , /noerase, POS=[0.1, 0.36, 0.95, 0.50], thick=1, PSym=0, $
    BACKGROUND = blanco, COLOR=negro, YRANGE=[down4,up4],  XTICKNAME=REPLICATE(' ', time_w+1)    
    
    
        AXIS, XAXIS = 0, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         XTICKFORMAT="(A1)", $
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTICKNAME=REPLICATE(' ', time_w+1), $
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"
                        

        AXIS, YAXIS = 0, YRANGE=[down4,up4], $
                         YTITLE = 'beta', $
                         COLOR=negro, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[down4,up4], $
                         ;YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         ;YTITLE = 'Mach', $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $  
    up5     = max(mag_M)
    down5   = min(mag_M)
    plot, tot_days, mag_M , xminor = 8, xstyle = 5, ystyle = 6,$
    ytitle = 'Magnetosonic Mach' , /noerase, POS=[0.1, 0.21, 0.95, 0.35], thick=1, PSym=0, $
    BACKGROUND = blanco, COLOR=negro, YRANGE=[down4,up4],  XTICKNAME=REPLICATE(' ', time_w+1)    
    
    
        AXIS, XAXIS = 0, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         XTICKFORMAT="(A1)", $
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"
                        

        AXIS, YAXIS = 0, YRANGE=[down5,up5], $
                         YTITLE = 'Mach', $
                         COLOR=negro, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[down5,up5], $
                         ;YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         ;YTITLE = 'Mach', $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $  

    up6     = max(Ey)
    down6   = min(Ey)
    plot, tot_days, Ey , xminor = 8, xstyle = 5, ystyle = 6,$
    ytitle = 'Magnetosonic Mach' , /noerase, POS=[0.1, 0.06, 0.95, 0.20], thick=1, PSym=0, $
    BACKGROUND = blanco, COLOR=negro, YRANGE=[down6,up6],  XTICKNAME=REPLICATE(' ', time_w+1)    
    
    
        AXIS, XAXIS = 0, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         ;XTITLE = ' ' , $;Time_title, $; XTICKUNITS = 'Time', XTICKFORMAT='LABEL_DATE',$
                         XTICKNAME=X_label, $, $
                         COLOR=negro, $
                         CHARSIZE = 0.6, $
;                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,time_w], $
                         XTICKS=time_w, $
                         XMINOR=8, $
                         COLOR=negro, $
                         TICKLEN=0.04,$
                         XTICKFORMAT="(A1)"
                        

        AXIS, YAXIS = 0, YRANGE=[down6,up6], $
                         YTITLE = 'Ey', $
                         COLOR=negro, $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $
                         ;TICKLEN=0.00

        AXIS, YAXIS = 1, YRANGE=[down6,up6], $
                         ;YTICKNAME=['', '', 'STGM', 'TGM I', 'TGM M', 'QUIET'], $
                         COLOR=negro, $
                         ;YTITLE = 'Mach', $
                         CHARSIZE = 0.6;, $
                        ; CHARTHICK=chr_thick1;, $     
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; Making True color and setting png file
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-   
    
    Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak

        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+fecha+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+fecha+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+fecha
                print, ''
        ENDIF

;print, file_number
;print, k_mex_data

        RETURN 
    
    
end





