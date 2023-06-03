;
;Name:
;	ip_plot.pro
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
;   .r ip_plot
;   ip_plot([yyyy,mm,dd], [yyyy,mm,dd])
;
;parameters:
;   
;
;dependencies:
;   Omniweb NASA
;
;input files
;   ip daily data files from Omniweb and with formated changed
;
;output files:
;
;   imported to dir /output/ip_fig/
;
;

PRO ip_plot, date_i, date_f

	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2] 
	
;###############################################################################    
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    tot_days= findgen(file_number*24)/24.0    
    Date    = STRING(yr_i,mh_i,dy_i,yr_f,mh_f,dy_f, FORMAT='(I4,"-",I02,"-",I02,"_",I4,"-",I02,"-",I02)')
    
; help = help_data(HELP_ip='help_ip')
; Define the IP time series
    
    Bz   = ip_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'Bz')
    B    = ip_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'B_vec')
    Bz   = add_nan(Bz, 999.90, 'equal') ;declare certain values as NAN  
    B    = add_nan(B, 999.90, 'equal') 
    
    v_p = ip_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'v_p')
    v_p = add_nan(v_p, 9999.0, 'equal') ;declare certain values as NAN 
        
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    H   = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
;setting certain values as NaN        
    H = add_nan(H, 999999.0, 'equal')        
    H = add_nan(H, 100.0, 'greater')                 
   ; dst    = add_nan(dst, 999999.0, 'equal') 

    p_dyn = ip_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'p_dyn')
    Ey = ip_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'E')    
    p_dyn = add_nan(p_dyn, 99.990, 'equal');declare certain values as NAN   
    Ey    = add_nan(Ey, 999.990, 'equal')   	
;###############################################################################
; define device and color parameters       
        Device_bak = !D.Name 
        SET_PLOT, 'Z'
        
        Xsize=fix(800)
        Ysize=1000
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=0     
        DEVICE, set_character_size = [10, 12]
        DEVICE, DECOMPOSED=1     
        chr_size1 = 1.5
        chr_thick1= 1.5
        space     = 0.015
        rojo      = 248
        amarillo  = 220
        verde     = 150
        negro     = 0
        azul      = 70
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET
        
    LOADCT, 39, /SILENT
;###############################################################################
; Time label    
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english')                        ;Function to convert month from mm format to string   
;###############################################################################
    idate0 = STRING(yr_i, mh_i, FORMAT='(I4,I02)')
    TGM_n = event_case([yr_i, mh_i,dy_i])     
;###############################################################################
       days = intarr(file_number+1)
       FOR i=0, N_ELEMENTS(days)-1 DO BEGIN
            days[i] = dy_i+i
       ENDFOR
       days = days*24/24. 
       day_time = FINDGEN(24)
;############################################################################### 
    window_title = 'SGS'+ STRING(TGM_n, FORMAT='(I2)')+', '+ $
                STRING(old_month, yr_i, FORMAT='(A, X, I4)')
;###############################################################################
    dst_min = MIN(dst,i)    
    t0 = 0
    CASE TGM_n of
        1   :   t0 = 11
        2   :   t0 = 2
        3   :   t0 = 8
        4   :   t0 = i
        5   :   t0 = 17
        6   :   t0 = i
        7   :   t0 = 3
        8   :   t0 = i
        9   :   t0 = 3
        10  :   t0 = 7
        11  :   t0 = 12
        12  :   t0 = 9
        13  :   t0 = 17
        14  :   t0 = 19
        15  :   t0 = 20
        16  :   t0 = 6
        17  :   t0 = 18
        18  :   t0 = 10
        19  :   t0 = i
        20  :   t0 = 14        
        ELSE: PRINT, 'evento no disponible'   
    ENDCASE
    tgm_nat =  ''
    CASE TGM_n of    
        4   :   tgm_nat = 'complex'
        6   :   tgm_nat = 'complex'          
        8   :   tgm_nat = 'complex'
        19  :   tgm_nat = 'complex'            
        ELSE:   tgm_nat = 'no'
    ENDCASE
    print, tgm_nat
    mainphase_end = i
    mainphase_beg = i-t0
;###############################################################################
; Plot data
    up_B = MAX(B+5) 
    down_B = MIN(B-5) 
    
    PLOT, tot_days, B, XTICKS=file_number, xminor=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.1,0.76,0.9,0.93], $
     XSTYLE = 5, XRANGE=[0, file_number],  XTICKNAME=REPLICATE(' ', file_number+1), YSTYLE = 5,$
     YRANGE=[down_B, up_B], THICK=3  

 	;OPLOT, tot_days, B, LINESTYLE=3, THICK=3, COLOR=negro  

    IF tgm_nat NE 'complex' THEN BEGIN     
    OPLOT, [tot_days[mainphase_end],tot_days[mainphase_end]], $ ;referencia para el mínimo en dst
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro

    OPLOT, [tot_days[mainphase_beg],tot_days[mainphase_beg]], $ ;referencia para el inicio de la fase principal
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    ENDIF            
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='UT [days]', $                           
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down_B, up_B], $
                         YSTYLE=1, $  
                         YTITLE = 'B [nT]', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 1.0;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down_B,up_B], $
                         YSTYLE=1, $  
                         ;YTITLE = '', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.0, $
                         CHARTHICK=1.5                          
;###############################################################################    
    up_p    = MAX(p_dyn)
    down_p  = MIN(p_dyn) 
    
    up_v    = MAX(v_p)
    down_v  = MIN(v_p)

     PLOT, tot_days, v_p, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.1,0.28,0.9,0.45], $
     XSTYLE = 5, XRANGE=[0, file_number], YRANGE=[down_v,up_v], $
     XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 5, /NOERASE, THICK=3

     PLOT, tot_days, p_dyn, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.1,0.28,0.9,0.45], $
     XSTYLE = 5, XRANGE=[0, file_number], YRANGE=[down_p,up_p], $
     XTICKNAME=REPLICATE(' ', file_number+1), ySTYLE = 5, /NOERASE, THICK=3, /NODATA
     
     OPLOT, tot_days, p_dyn, LINESTYLE=0, THICK=3, COLOR=azul

    IF tgm_nat NE 'complex' THEN BEGIN  
    OPLOT, [tot_days[mainphase_end],tot_days[mainphase_end]], $
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro

    OPLOT, [tot_days[mainphase_beg],tot_days[mainphase_beg]], $ ;referencia para el inicio de la fase principal
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    ENDIF        
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='UT [days]', $                           
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down_v, up_v], $
                         YSTYLE=1, $  
                         YTITLE = 'V [m s!U-1!N]', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 1.0;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down_p,up_p], $
                         YSTYLE=1, $  
                         YTITLE = 'P [nPa]', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.0, $
                         CHARTHICK=1.5                               
;###############################################################################
    up_B     = MAX(Bz+5)
    down_B   = MIN(Bz-5)
        
    PLOT, tot_days, Bz, XTICKS=file_number, XMINOR=8, BACKGROUND = blanco, COLOR=negro,$
     CHARSIZE = 0.9, CHARTHICK=chr_thick1, POSITION=[0.1,0.52,0.9,0.69], $
     XSTYLE = 5, XRANGE=[0, file_number],  XTICKNAME=REPLICATE(' ', file_number+1), YSTYLE = 5,$
     YRANGE=[down_B,up_B], THICK=3 , /NOERASE
     
    OPLOT, tot_days, Bz*0, LINESTYLE=1, THICK=3, COLOR=negro

    IF tgm_nat NE 'complex' THEN BEGIN      
    OPLOT, [tot_days[mainphase_end],tot_days[mainphase_end]], $ ;referencia para el mínimo en dst
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro  
    
    OPLOT, [tot_days[mainphase_beg],tot_days[mainphase_beg]], $ ;referencia para el inicio de la fase principal
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    ENDIF          
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='UT [days]', $                           
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$   
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down_B, up_B], $
                         ystyle=2, $  
                         YTITLE = 'Bz [nT]', $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 1.0;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down_B, up_B], $
                         ystyle=2, $  
                         YTITLE = '', $                          
                         COLOR=negro, $
                         CHARSIZE = 1.0, $
                         CHARTHICK=1.5           
;###############################################################################
    IF max(dst) GT max(H) THEN up = max(dst+5) ELSE up = max(H+5)
    IF min(dst) LT min(H) THEN down = min(dst-5) ELSE down = min(H-5)    
    
    PLOT, tot_days, dst, XTICKS=file_number, xminor = 8, POSITION=[0.1,0.04,0.9,0.21],$
    XTICKFORMAT='LABEL_DATE', XSTYLE = 5, YSTYLE=5, YRANGE=[down, up],$
    title = '', BACKGROUND = blanco, COLOR=negro, XRANGE=[0, file_number],$
	ytitle = '',  XTICKNAME=REPLICATE(' ', file_number+1), /NOERASE, THICK=3

    OPLOT, tot_days, H, COLOR=rojo, LINESTYLE=0, THICK=3

    IF tgm_nat NE 'complex' THEN BEGIN  
    OPLOT, [tot_days[mainphase_end],tot_days[mainphase_end]], $ ;referencia para el mínimo en dst
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro

    OPLOT, [tot_days[mainphase_beg],tot_days[mainphase_beg]], $ ;referencia para el inicio de la fase principal
    [!Y.CRANGE[0], !Y.CRANGE[1]], LINESTYLE=2, $
    THICK=2, COLOR=negro
    ENDIF    
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTITLE='UT [days]', $  
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 0.9, $                         
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = 'Dst [nT]', $
                         YSTYLE=1, $                          
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         CHARSIZE = 1.0;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $       
                         COLOR=negro, $
                         YSTYLE=1, $
                         CHARSIZE = 1.0, $
                         CHARTHICK=1.5
;###############################################################################   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.97
   XYOuts, X, y, window_title, /Normal, color=negro, Alignment=0.5,$
   Charsize=2, CHARTHICK = 1.5                  
;###############################################################################
   XYOuts, 0.14, 0.9, '(a)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3    
      
   XYOuts, 0.14, 0.55, '(b)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3 
   
   XYOuts, 0.14, 0.42, '(c)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3    
      
   XYOuts, 0.14, 0.17, '(d)', /Normal, $
   color=negro, Alignment=0.5, Charsize=1.2, CHARTHICK= 3    
;############################################################################### 
     Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    SET_PLOT, Device_bak   
;###############################################################################
; open the post stript device
    path = '../rutidl/output/article1events/ip_obs/'
       ; IF keyword_set(jpeg) THEN BEGIN
        info = SIZE(Image)
        nx = info[1]
        ny = info[2]
        true_image = BYTARR(3,nx,ny)
        true_image[0,*,*] = reds[image]
        true_image[1,*,*] = greens[image]
        true_image[2,*,*] = blues[image]

        PRINT, '        Setting PNG as default file type.'
        WRITE_PNG, path+'ip_obsV1_'+Date+'.png', Image, reds,greens,blues
        
        PRINT, '        Saving: '+path+'ip_obsV1_'+Date+'.png'
        PRINT, ''	                                                	
END	
