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
;   .PNG figure
;   imported to /output/eventos_tgm/Bsq_yyyy-mm_V3.png
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

PRO bsq_plot, teo_mag, date_i, date_f
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
;###############################################################################	
	dat1    = rawH([yr_i, mh_i, dy_i])	
	H1      = dat1.TEOH

    dat2    = rawH([yr_f, mh_f, dy_f])
    H2      = dat2.TEOH
    
    ndays   = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    td      = FINDGEN(ndays*1440)/1440.0
    td_h    = FINDGEN(ndays*24)/24.0
    datetime= TIMEGEN(N_ELEMENTS(td), FINAL=JULDAY(mh_f, dy_f, yr_f, 23), $
                START=JULDAY(mh_i, dy_i, yr_i, 0), UNITS='H')
    CALDAT, datetime, mh, dy, yr, hr
;###############################################################################                        
;identifying NAN percentage values in the Time Series    
    H1 = nanpc(H1, 99999.0, 'gequal')
    H2 = nanpc(H2, 99999.0, 'gequal')   
        
    H1 = add_nan(H1, 99999.0, 'gequal')        
    H2 = add_nan(H2, 99999.0, 'gequal')            
;###############################################################################        
    ;implementar una función de interpolación en caso de que el porcentaje de 
    ;nan sea muy bajo       
    H1_tmp   = H1
    H1_exist = WHERE(finite(H1_tmp), ngooddata1, complement=baddata1, $
    ncomplement=nbaddata1)

    H2_tmp   = H2
    H2_exist = WHERE(finite(H2_tmp), ngooddata2, complement=baddata2, $
    ncomplement=nbaddata2)
       
    ; interpolate at the locations of the bad data using the good data    
    IF nbaddata1 GT 0 THEN H1_tmp[baddata1] = INTERPOL(H1_tmp[H1_exist], $
    H1_exist, baddata1, /QUADRATIC)
    H1 = H1_tmp  
    
    IF nbaddata1 GT 0 THEN H2_tmp[baddata2] = INTERPOL(H2_tmp[H2_exist], $
    H2_exist, baddata2, /QUADRATIC)
    H2 = H2_tmp 
;###############################################################################
;Extend QDS data ndays for a quadratic interpolation
    QDS1 = REFORM(REBIN(H1, 1440, ndays), N_ELEMENTS(td))
    QDS2 = REFORM(REBIN(H2, 1440, ndays), N_ELEMENTS(td))
;###############################################################################
;Interpolate between the QDS
    slope1   = (td - td[719])
    slope2   = (td[N_ELEMENTS(td)-721]-td[719])
    slope    = slope1/slope2

    Bsq1     = (QDS2-QDS1)*slope
    Bsq     = QDS1+Bsq1                           
;###############################################################################
    ;Generate a QD file in days
    outfile = STRARR(ndays) 
    
    string_date        = STRARR(ndays)                       
    data_file_name_h  = STRARR(ndays)

    Bsq_H = FINDGEN(N_ELEMENTS(Bsq)/60)
    FOR i=0, N_ELEMENTS(Bsq_H)-1 DO BEGIN
        ;print, Bsq[i*60:(i+1)*60-1]
        Bsq_H[i] = MEDIAN(Bsq[i*60:(i+1)*60-1])        
    
    ENDFOR
;###############################################################################
;Generación de la figura de Bsq
    path = '../rutidl/output/eventos_tgm/'
    Date = string(yr_i, mh_i, FORMAT='(I4, "-", I02)')

        Device_bak = !D.Name 
        SET_PLOT, 'Z'    
        
        Xsize=FIX(1600)
        Ysize=300
        ;DEVICE, decompose=0
        DEVICE, SET_RESOLUTION = [Xsize,Ysize]
        DEVICE, z_buffering=O
        DEVICE, set_character_size = [10, 12]

    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT
             
        chr_size1 = 0.9
        chr_thick1= 1.0
        space     = 0.015
        rojo      = 248
        amarillo  = 190
        verde     = 170
        negro     = 0
        azul      = 70
        blanco    = 255
        gris      = 130
        morado    = 16
        
    TVLCT, R_bak, G_bak, B_bak, /GET   
    LOADCT, 39, /SILENT
;###############################################################################    
     X_label   = STRARR(ndays+1)+' '
        months    = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', $
                     'Sep', 'Oct', 'Nov', 'Dic']
        old_month = mh_i
        FOR i =0,  N_ELEMENTS(X_label)-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                
                IF i LT N_ELEMENTS(X_label)-1 THEN $
                        X_label[i]  = (tmp_month EQ old_month) ? STRING(tmp_day, $
                        FORMAT='(I02)') : STRING(tmp_day, FORMAT='(I02)')
                        old_month = tmp_month
        ENDFOR 
;##############################################################################
;Declaración de fechas
;##############################################################################
;mes del primer día quieto
mh_i2 = mh_i
CASE mh_i2 of
    1: mh_i2 = 'Enero'
    2: mh_i2 ='Febrero'
    3: mh_i2 ='Marzo'
    4: mh_i2 ='Abril'
    5: mh_i2 ='Mayo'
    6: mh_i2 ='Junio'
    7: mh_i2 ='Julio'
    8: mh_i2 ='Agosto'
    9: mh_i2 ='Septiembre'
    10:mh_i2 ='Octubre'
    11:mh_i2 ='Noviembre'
    12:mh_i2 ='Diciembre'
    ELSE: PRINT, 'fuera de rango'
ENDCASE
;###############################################################################
;mes del segundo día quieto
mh_f2 = mh_f
CASE mh_f2 of
    1: mh_f2 = 'Enero'
    2: mh_f2 ='Febrero'
    3: mh_f2 ='Marzo'
    4: mh_f2 ='Abril'
    5: mh_f2 ='Mayo'
    6: mh_f2 ='Junio'
    7: mh_f2 ='Julio'
    8: mh_f2 ='Agosto'
    9: mh_f2 ='Septiembre'
    10:mh_f2 ='Octubre'
    11:mh_f2 ='Noviembre'
    12:mh_f2 ='Diciembre'
    ELSE: PRINT, 'fuera de rango'
ENDCASE   
;###############################################################################
;fecha en que inició la respectiva TGM 
idate0 = string(yr_i, mh_i, format='(I4,I02)')
TGM_i = idate0
case TGM_i of
    '200310' : TGM_i = td_h[360]
    '200411' : TGM_i = td_h[24]
    '200505' : TGM_i = td_h[240]
    '201503' : TGM_i = td_h[144]
    '201705' : TGM_i = td_h[24]
    '201708' : TGM_i = td_h[216]
    else: print, 'fuera de rango'
endcase  
;###############################################################################
;fecha en que terminó la respectiva TGM 
fdate0 = string(yr_i, mh_f, format='(I4,I02)')
TGM_f = fdate0
case TGM_f of
    '200311' : TGM_f = td_h[456]
    '200411' : TGM_f = td_h[144]
    '200505' : TGM_f = td_h[288]
    '201504' : TGM_f = td_h[216]
    '201706' : TGM_f = td_h[72]
    '201709' : TGM_f = td_h[288]
    else: print, 'fuera de rango'
endcase                   
;###############################################################################
       days = intarr(ndays+1)
       for i=0, n_elements(days)-1 do begin
            days[i] = dy_i+i
       endfor
       tot_days = days*24/24. 
       day_time = findgen(24)                       
;###############################################################################                 
IF mh_i EQ mh_f THEN BEGIN
    time_title = STRING(mh_i2, yr_i, FORMAT='((A, X, I4))')
ENDIF ELSE BEGIN
    time_title = STRING(mh_i2, FORMAT='((A))')+' y '+$
    STRING(mh_f2, ',', yr_i, FORMAT='((A,A,X,I4))')   
ENDELSE
;###############################################################################
;ylimits
IF MAX(Bsq_H) GT MAX(QDS1) THEN up = MAX(Bsq_H) ELSE up = MAX(QDS1)
IF MIN(Bsq_H) LT MIN(QDS1) THEN down=MIN(Bsq_H) ELSE down=MIN(QDS1)
;###############################################################################
;xlimits
ini = td[0]
fin = td[N_ELEMENTS(td)-1]
;###############################################################################                                 
    PLOT, td_h, Bsq_H, position = [0.09, 0.14, 0.91, 0.9], XRANGE=[ini,fin], $
    YRANGE=[down,up], YSTYLE=6, XSTYLE=5, BACKGROUND = blanco, COLOR=negro,$
    XTICKNAME=REPLICATE(' ', ndays+1)

    POLYFILL, [TGM_i, TGM_f, TGM_f, TGM_i], $
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=amarillo
              
    POLYFILL, [td_h[0], td_h[23], td_h[23], td_h[0]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, $
              SPACING=0.3  
              
    POLYFILL, [td_h[0], td_h[23], td_h[23], td_h[0]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, $
              SPACING=0.3                                  

    POLYFILL, [td_h[N_ELEMENTS(td_h)-1], td_h[N_ELEMENTS(td_h)-24], $
               td_h[N_ELEMENTS(td_h)-25], td_h[N_ELEMENTS(td_h)-1]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, $
              SPACING=0.3     
              
    POLYFILL, [td_h[N_ELEMENTS(td_h)-1], td_h[N_ELEMENTS(td_h)-24], $
               td_h[N_ELEMENTS(td_h)-25], td_h[N_ELEMENTS(td_h)-1]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, $
              SPACING=0.3        
              
              
    OPLOT, [td_h[23], td_h[23]], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, $
    color=negro, THICK=2                
    OPLOT, [td_h[N_ELEMENTS(td_h)-25], td_h[N_ELEMENTS(td_h)-25]], $
    [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2
              
    OPLOT, [TGM_i, TGM_i], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5, $
    color=negro, THICK=2                   
    OPLOT, [TGM_f, TGM_f], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5, $
    color=negro, THICK=2     
              
IF idate0 EQ '200310' THEN BEGIN
    POLYFILL, [TGM_i+22, TGM_f+22, TGM_f+22, TGM_i+22], $
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], COLOR=amarillo 
                
    OPLOT, [tgm_i+22, tgm_i+22], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5, color=negro, THICK=2 
    OPLOT, [tgm_f+22, tgm_f+22], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5, color=negro, THICK=2                
ENDIF                          
    OPLOT, td_h, Bsq_H, COLOR=negro, THICK=4   
;###############################################################################
        AXIS, XAXIS = 0, XRANGE=[0,ndays], $
                         XTICKS=ndays, $                     
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 1.1 , $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,ndays], $     
                         XTICKFORMAT="(A1)", $                                            
                         XMINOR=8, $ 
                         CHARSIZE = 1.1 , $ 
                         CHARTHICK=1.5,$                      
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $                     
                         COLOR=negro, $
                         CHARTHICK=1.5,$
                         YSTYLE=2, $
                         CHARSIZE = 1.1 ;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                         COLOR=negro, $
                         CHARTHICK=1.5,$                         
                         YSTYLE=2, $
                         CHARSIZE = 1.1 ;, $  
;###############################################################################                         
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.02, y, '[nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.2                            
;###############################################################################
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.925   
   XYOUTS, X, y, time_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.75, CHARTHICK=1.2 
;###############################################################################  
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.01, 'Tiempo Universal en dias', /Normal, $
   color=negro, Alignment=0.5, Charsize=1, CHARTHICK=1.2   
;###############################################################################
    IF idate0 EQ '200310' THEN BEGIN
    ;first panel legend                          
            POLYFILL, [0.80,0.82,0.82,0.80], [0.832,0.832,0.862,0.862], color = verde, $
            /NORMAL, /LINE_FILL, ORIENTATION=45, THICK=3, LINESTYLE=0, SPACING=0.3   
            
            POLYFILL, [0.80,0.82,0.82,0.80], [0.832,0.832,0.862,0.862], color = verde, $
            /NORMAL, /LINE_FILL, ORIENTATION=-45, THICK=3, LINESTYLE=0, SPACING=0.3  

            POLYFILL, [0.80,0.82,0.82,0.80], [0.764,0.764,0.794,0.794], color = amarillo, /NORMAL         
                                 
            XYOUTS, 0.83, 0.832 , /NORMAL, $
                    'DGS', COLOR=negro, $
                    CHARSIZE = 1.2, $
                    CHARTHICK=2
                    
            XYOUTS, 0.83, 0.76 , /NORMAL, $
                    'TGM', COLOR=negro, $
                    CHARSIZE = 1.2, $
                    CHARTHICK=2                                
    ENDIF                   
;###############################################################################                
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
                true_image = BYTARR(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'Bsq_'+Date+'_V3.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                WRITE_PNG, path+'Bsq_'+Date+'_V3.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                print, '        Saving: '+path+'Bsq_'+Date+'_V3.png'
                print, ''
        ENDIF
        RETURN                 
END
