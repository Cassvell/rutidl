;
;Name:
;	magdat_iono.pro
;
;purpose:
;	Este programa está diseñado para retornar una gráfica de comparación entre 
;   la diferencia de los índices geomagnéticos Dst-DH y la diferencia del índice
;   ionosférico TEC - <TEC>. La gráfica resultante servirá para comparar la
;   respuesta ionosférica y la respuesta geomagnética con respecto del tiempo.
;
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   Obtención, graficado y comparación de datos.
;
;calling sequence:
;>   .r mag_iono
;>   mag_iono, [idate], [fdate]
;parameters:
;
;
;dependencies:
;
;
;input files
;   archivos de DH
;   archivos de Dst
;   archivos de TEC
;output files:
;   Gráfica Dst-DH y TEC-<TEC>. figura en formato .PNG de nombre 'idxVn_yyyy-mm-dd.png'
;   n = número de versión
;   figura es importada en el dir output/eventos_tgm/
;
;versión
;   Dic, 2022
;

PRO mag_iono, idate, fdate

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= idate[0]
	mh_i	= idate[1]
	dy_i 	= idate[2]	

	yr_f	= fdate[0]
	mh_f	= fdate[1]
	dy_f 	= fdate[2]	
;###############################################################################
; Generate the time variables to plot time series of Dst Index
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    days_dst= findgen(file_number*24)/24.0    
;###############################################################################
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_n     TGM_n = event_case([yr_i,mh_i,dy_i])     
;##############################################################################     
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
       
    tec_days= findgen(file_number*12)/12.0   
;############################################################################### 
    tec_diff = tec-med
    
    new_tecdays = findgen(file_number*48)/48.0 ;se genera un arreglo de tiempo con 
;    muestreo cada 15 min. para mejorar la resolución de las gráficas    
    
    new_tecdiff = FLTARR(N_ELEMENTS(new_tecdays))     	
    
;    print, N_ELEMENTS(new_tecdays), N_ELEMENTS(new_tecdiff)
    tmp_tecdif  = INTERPOL(tec_diff, N_ELEMENTS(new_tecdays))
    new_tecdiff = tmp_tecdif
;############################################################################### 
    i_diff = dst-H
    
    new_dstdays = findgen(file_number*96)/96.0 ;se genera un arreglo de tiempo con 
;    muestreo cada 15 min. para mejorar la resolución de las gráficas    
    
    new_idiff = FLTARR(N_ELEMENTS(new_dstdays))     	
    
;    print, N_ELEMENTS(new_tecdays), N_ELEMENTS(new_tecdiff)
    tmp_idiff  = INTERPOL(i_diff, N_ELEMENTS(new_dstdays))
    new_idiff = tmp_idiff    
;###############################################################################
; initiate the figure Device, defining colors and figure dim
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        Xsize=fix(1000)
        Ysize=200
        
    ;    DEVICE, DECOMPOSED = 0
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=0
        DEVICE, set_character_size = [10, 12]
             
        chr_size1 = 0.9
        chr_thick1= 1.5
        space     = 0.015
        rojo      = 248
        naranja   = 220
        amarillo  = 198
        verde     = 160
        negro     = 0
        azul      = 100
        blanco    = 255
        gris      = 100
        morado    = 50
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
;###############################################################################
; Create a time label based on the DOY initial and DOY final inputs
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)

    window_title = 'GS'+ STRING(TGM_n, format='(I01)')+', '+ $
    STRING(old_month, yr_i, format='(A, X, I4)')
;#####################################################################################    
    med_idx = MEDIAN(new_idiff)
    std_idx = stddev(new_idiff, /NAN)
    
    i_out = WHERE(new_idiff GE med_idx+std_idx OR new_idiff LE med_idx-std_idx)
    i_in  = WHERE(new_idiff LE med_idx+std_idx AND new_idiff GE med_idx-std_idx)
    id_diff_out = new_idiff
    id_diff_out[i_in]=!Values.F_NAN
    
    id_diff_in  = new_idiff
    id_diff_in[i_out]=!Values.F_NAN

    sup0 = med_idx+std_idx
    inf0 = med_idx-std_idx
    
    lim_sup = fltarr(n_elements(new_idiff))
    lim_sup[*] = sup0

    lim_inf = fltarr(n_elements(new_idiff))
    lim_inf[*] = inf0   
                    
    up  = max(dst-H)
    down= min(dst-H)
    up2      = max(tec-med)
    down2    = min(tec-med)
    
    if up2 gt up then up0 = up2 else up0 = up
    if down2 lt down then down0 = down2 else down0 = down
       dH = TeXtoIDL('\DeltaH')   
;#####################################################################################  
       days = intarr(file_number+1)
       for i=0, n_elements(days)-1 do begin
            days[i] = dy_i+i
       endfor
       days = days*24/24. 
      day_time = findgen(24)            
      tot_days = fltarr(n_elements(days_dst))      
      for i=0, file_number-1 do begin  
      for j=0, 23 do begin       
      tot_days = days[i]+(day_time[j]/24)                       
      endfor
      endfor
;#####################################################################################         
    plot, new_dstdays, new_idiff, XTICKS=file_number, xminor = 12, POSITION=[0.1,0.18,0.9,0.7],$
    xstyle = 5, ystyle=6, YRANGE=[down,up], XRANGE=[0, file_number],$
    title = '', BACKGROUND = blanco, COLOR=negro,$
	ytitle = 'Indice DST [nT]',  XTICKNAME=REPLICATE(' ', file_number+1), CHARSIZE = 1.2,$
	thick=2, /NODATA

          f_a   = N_ELEMENTS(new_dstdays)
    POLYFILL, [new_dstdays[0], new_dstdays[f_a-1], new_dstdays[f_a-1], new_dstdays[0]],$
    [inf0, inf0, sup0, sup0], COLOR=azul, /LINE_FILL, SPACING=0.001, linestyle=1, THICK=0.1  		
    POLYFILL, [new_dstdays[0], new_dstdays[f_a-1], new_dstdays[f_a-1], new_dstdays[0]],$
    [inf0, inf0, sup0, sup0], COLOR=azul, /LINE_FILL, SPACING=0.001, linestyle=1, ORIENTATION=90, THICK=0.1        

    oplot, new_dstdays, id_diff_in, color=negro, linestyle=3        
    oplot, new_dstdays, id_diff_out, color=negro, linestyle=0, thick=4        
;#####################################################################################        
 med_tec = MEDIAN(new_tecdiff)
    std_tec = stddev(new_tecdiff)
    
    index_out = WHERE(new_tecdiff GE med_tec+std_tec OR new_tecdiff LE med_tec-std_tec)
    index_in  = WHERE(new_tecdiff LE med_tec+std_tec AND new_tecdiff GE med_tec-std_tec)
    tec_diff_out = new_tecdiff
    tec_diff_out[index_in]=!Values.F_NAN
    
    tec_diff_in  = new_tecdiff
    tec_diff_in[index_out]=!Values.F_NAN

    sup = med_tec+std_tec
    inf = med_tec-std_tec
    
    l_sup = fltarr(n_elements(new_tecdiff))
    l_sup[*] = sup

    l_inf = fltarr(n_elements(new_tecdiff))
    l_inf[*] = inf    
;#####################################################################################
        plot, new_tecdays, new_tecdiff, color=rojo, XTICKS=file_number, xminor = 8,$
        xstyle = 5, ystyle=6, YRANGE=[down2,up2], POSITION=[0.1,0.18,0.9,0.7],$
        BACKGROUND = blanco, XRANGE=[0, file_number], /noerase,$
        XTICKNAME=REPLICATE(' ', file_number+1), CHARSIZE = 0.8, thick=3, /NODATA

    POLYFILL, [new_dstdays[0], new_dstdays[f_a-1], new_dstdays[f_a-1], new_dstdays[0]],$
    [inf, inf, sup, sup], COLOR=amarillo, /LINE_FILL, SPACING=0.001, linestyle=1,$
    ORIENTATION=-45  

    POLYFILL, [new_dstdays[0], new_dstdays[f_a-1], new_dstdays[f_a-1], new_dstdays[0]],$
    [inf, inf, sup, sup], COLOR=amarillo, /LINE_FILL, SPACING=0.001, linestyle=1,$
    ORIENTATION=45  	
    
        oplot, new_tecdays, tec_diff_in, color=rojo, linestyle=0
        oplot, new_tecdays, tec_diff_out, color=rojo, linestyle=0, thick=4

    oplot, new_tecdays, l_sup, color=rojo, linestyle=1, thick=1
    oplot, new_tecdays, l_inf, color=rojo, linestyle=1, thick=1


    plot, new_dstdays, new_idiff, XTICKS=file_number, POSITION=[0.1,0.18,0.9,0.7], xminor = 12,$
    xstyle = 5, ystyle=6, YRANGE=[down,up], XRANGE=[0, file_number],$
    title = '', BACKGROUND = blanco, COLOR=negro,$
	ytitle = 'Indice DST [nT]',  XTICKNAME=REPLICATE(' ', file_number+1), CHARSIZE = 1.2,$
	thick=2, /NODATA, /NOERASE

    oplot, new_dstdays, id_diff_in, color=negro, linestyle=0, THICK=1        
    oplot, new_dstdays, id_diff_out, color=negro, linestyle=0, thick=5  
             
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         xstyle=1, $
                         XTICKS=file_number, $
                         XMINOR=12, $
                         xtitle='Tiempo Universal [Dias]', $
                         XTICKNAME=X_label ,$
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=chr_thick1, $
                         TICKLEN=0.1
                             
        AXIS, XAXIS = 1, XRANGE = (!X.CRANGE+dy_i-0.25),$
                         XTICKS=file_number, $ 
                         XTICKV=days,$
                         xstyle=1, $
                         XMINOR=12, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=chr_thick1, $
                         COLOR=negro, $
                         TICKLEN=0.1

        AXIS, YAXIS = 0, YRANGE=[down0,up0], $
                         YTITLE = 'Dst-'+dH+' [nT]', $                          
                         COLOR=negro, $
                         ystyle=2, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=chr_thick1;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down2,up2], $
                         YTITLE = 'TEC-<TEC> [TECu]', $   
                         COLOR=rojo, $
                         ystyle=2, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=chr_thick1;, $    
                        
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.91
   XYOuts, X, y, window_title, /Normal, color=negro, Alignment=0.5, $
    Charsize=1.25, CHARTHICK=1.5

   XYOuts, X, 0.82, 'LT', /Normal, color=negro, Alignment=0.5, $
    Charsize=0.8, CHARTHICK=1.5   
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   
;###############################################################################
; open the post stript device
    path = '../rutidl/output/eventos_tgm/'   
    Date    = string(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'idxV2_'+Date+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'idxV2_'+Date+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'idxV2_'+Date+'_V1.png'
                print, ''
        ENDIF
        RETURN
end    
