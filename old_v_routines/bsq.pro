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

function DH_teo, date

	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]
	day 	= date[2]	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        date = string(year, month, day, format = '(I4, I02, I02)')
       ; sts  = string(stats, format = '(A5)')
		
		file_name = '../rutidl/dH_teo/'+'teo_'+date+'.dst.early'
		
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
        DStruct = {hora : 0, D_stdesv : 0., D : 0., H_stdesv : 0., H : 0., $
        Z_stdesv : 0., Z : 0., N_stdesv : 0., N : 0., F_stdesv : 0., F : 0.}

		teo_mag = REPLICATE(DStruct, number_of_lines)	
  
		READS, data[0:number_of_lines-1], teo_mag, $
		FORMAT='(I2, F10, F8, F10, F10, F10, F10, F10, F10, F10, F10)'
		
		return, teo_mag		
end


pro sq, teo_mag, date_i, date_f
	On_error, 2
	compile_opt idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
;###############################################################################	
	dat1    = DH_teo([yr_i, mh_i, dy_i])	
	H1      = dat1.H

    dat2    = DH_teo([yr_f, mh_f, dy_f])
    H2      = dat2.H
    
    ndays = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    td = FINDGEN(ndays*24)/24.
    datetime = TIMEGEN(N_ELEMENTS(td), FINAL=JULDAY(mh_f, dy_f, yr_f, 23), $
                START=JULDAY(mh_i, dy_i, yr_i, 0), UNITS='H')
    CALDAT, datetime, mh, dy, yr, hr
;###############################################################################                        
        i_nan1 = WHERE(H1 eq 999999.0, ncount)
        i_nan2 = WHERE(H1 gt 100.0, n2count)
        
        prcent_nan = FLOAT(ncount+n2count)*100.0
        PRINT,'porcentaje de valores NaN:', prcent_nan/N_ELEMENTS(H1),'%'
        
        i_nan11 = WHERE(H2 eq 999999.0, ncount2)
        i_nan22 = WHERE(H2 gt 100.0, n2count2)
        
        prcent_nan = FLOAT(ncount2+n2count2)*100.0
        PRINT,'porcentaje de valores NaN:', prcent_nan/N_ELEMENTS(H1),'%'
;###############################################################################
; define device and color parameters
;###############################################################################  
time     = FINDGEN(24)+1
 
fft_H1    = fft(H1)
n        = N_ELEMENTS(H1)

pws1      = abs(fft_H1[0:n/2])^2

fn       = FLOAT(1.0/(2.0*84000))
f_k      = (1+findgen(n))/FLOAT(n*3600)
;print, abs(fft_H)
;plot, f_k, pws, /YLOG;, /XLOG;, Xrange=[1e-5, fn], xstyle=1
;plot, f_k, temp
;###############################################################################
time     = FINDGEN(24)+1
 
fft_H2    = FFT(H2)
n        = N_ELEMENTS(H2)

pws2      = ABS(fft_H2[0:n/2])^2
;###############################################################################
; define device and color parameters
;###############################################################################
Harm1 = ABS(fft_H1[0])*COS((2*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H1[0]), $
REAL_PART(fft_H1[0])))

Harm2 = ABS(fft_H1[1])*COS((4*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H1[1]), $
REAL_PART(fft_H1[1])))

Harm3 = ABS(fft_H1[2])*COS((6*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H1[2]), $
REAL_PART(fft_H1[2])))

Harm4 = ABS(fft_H1[3])*COS((8*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H1[3]), $
REAL_PART(fft_H1[3])))

Harm5 = ABS(fft_H1[4])*COS((10*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H1[4]), $
REAL_PART(fft_H1[4])))

Harm6 = ABS(fft_H1[5])*COS((12*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H1[5]), $
REAL_PART(fft_H1[5])))

Harm7 = ABS(fft_H1[6])*COS((14*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H1[6]), $
REAL_PART(fft_H1[6])))

temp1 = Harm1+Harm2+Harm3+Harm4+Harm5+Harm6+Harm7

T1 = REFORM(REBIN(temp1, 24, ndays), N_ELEMENTS(td))
;print, n_elements(T1), T1
;###############################################################################
Harm1_2 = abs(fft_H2[0])*COS((2*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H2[0]), $
REAL_PART(fft_H2[0])))

Harm2_2 = abs(fft_H2[1])*COS((4*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H2[1]), $
REAL_PART(fft_H2[1])))

Harm3_2 = abs(fft_H2[2])*COS((6*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H2[2]), $
REAL_PART(fft_H2[2])))

Harm4_2 = abs(fft_H2[3])*COS((8*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H2[3]), $
REAL_PART(fft_H2[3])))

Harm5_2 = abs(fft_H2[4])*COS((10*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H2[4]), $
REAL_PART(fft_H2[4])))

Harm6_2 = abs(fft_H2[5])*COS((12*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H2[5]), $
REAL_PART(fft_H2[5])))

Harm7_2 = abs(fft_H2[6])*COS((14*!PI*time*3600/86400)+ATAN(IMAGINARY(fft_H2[6]), $
REAL_PART(fft_H2[6])))

temp2 = Harm1_2+Harm2_2+Harm3_2+Harm4_2+Harm5_2+Harm6_2+Harm7_2
T2 = REFORM(REBIN(temp2, 24, ndays), N_ELEMENTS(td))
;print, temp
;###############################################################################
slope1   = (td - td[11])
slope2   = (td[N_ELEMENTS(td)-13]-td[11])
slope    = slope1/slope2

Bsq1     = (T2-T1)*slope
Bsq     = T1+Bsq1
;print, Bsq, '######', n_elements(Bsq)
;###############################################################################
    fecha = STRING(yr_i, mh_i, FORMAT='(I4, "-", I02 )')
    outfile = STRARR(ndays) 
    
    year = STRMID(STRING(yr, format='(I4)'),2,2)
    doy = INTARR(N_ELEMENTS(td))
    string_date        = STRARR(ndays)                       
    data_file_name_dh  = STRARR(ndays) 
FOR i=0, ndays-1 DO BEGIN
    tmp_year    = 0
    tmp_month   = 0
    tmp_day     = 0
    tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
    string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        

    outfile[i] = '../rutidl/output/Bsq_baselines/BsqV2_'+string_date[i]+'.txt'    
    OPENW, LUN, outfile[i], /GET_LUN        
    PRINTF, LUN, Bsq[i*24:(i+1)*24-1], format='(F08.4)'
    
    CLOSE, LUN
    FREE_LUN, LUN    
    ENDFOR   
;###############################################################################    
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
    '200310' : TGM_i = td[360]
    '200411' : TGM_i = td[24]
    '200505' : TGM_i = td[240]
    '201503' : TGM_i = td[144]
    '201705' : TGM_i = td[24]
    '201708' : TGM_i = td[216]
    else: print, 'fuera de rango'
endcase  
;###############################################################################
;fecha en que terminó la respectiva TGM 
fdate0 = string(yr_i, mh_f, format='(I4,I02)')
TGM_f = fdate0
case TGM_f of
    '200311' : TGM_f = td[456]
    '200411' : TGM_f = td[144]
    '200505' : TGM_f = td[288]
    '201504' : TGM_f = td[216]
    '201706' : TGM_f = td[72]
    '201709' : TGM_f = td[288]
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
IF MAX(Bsq) GT MAX(T2) THEN up = MAX(Bsq) ELSE up = MAX(T2)
IF MIN(Bsq) LT MIN(T2) THEN down=MIN(Bsq) ELSE down=MIN(T2)
;###############################################################################
;xlimits
ini = td[0]
fin = td[N_ELEMENTS(td)-1]
;###############################################################################                                 
    PLOT, td, Bsq, position = [0.05, 0.12, 0.95, 0.9], XRANGE=[ini,fin], $
    YRANGE=[down,up], YSTYLE=6, XSTYLE=5, BACKGROUND = blanco, COLOR=negro,$
    XTICKNAME=REPLICATE(' ', ndays+1)

    POLYFILL, [TGM_i, TGM_f, TGM_f, TGM_i], $
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=amarillo
              
    POLYFILL, [td[0], td[23], td[23], td[0]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3  
              
    POLYFILL, [td[0], td[23], td[23], td[0]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3                                  

    POLYFILL, [td[N_ELEMENTS(td)-1], td[N_ELEMENTS(td)-24], $
               td[N_ELEMENTS(td)-25], td[N_ELEMENTS(td)-1]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3     
              
    POLYFILL, [td[N_ELEMENTS(td)-1], td[N_ELEMENTS(td)-24], $
               td[N_ELEMENTS(td)-25], td[N_ELEMENTS(td)-1]],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3                 
    ;OPLOT, [td[23],td[23]], [!Y.CRANGE[0], !Y.CRANGE[1]], COLOR=negro, THICK=3, $
    ;LINESTYLE=2   
        
    OPLOT, td, T1, COLOR=azul, THICK=2
    OPLOT, td, T2, COLOR=rojo, THICK=2   
    OPLOT, td, Bsq, COLOR=negro, THICK=4   
;###############################################################################
        AXIS, XAXIS = 0, XRANGE=[0,ndays], $
                         XTICKS=ndays, $
                         ;XTITLE=time_title, $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 1.1 , $
                         TICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[0,ndays], $
                         ;XTICKS=ndays, $
                         ;XTICKV=FIX(tot_days), $       
                         XTICKFORMAT="(A1)", $                                            
                         XMINOR=8, $ 
                         CHARSIZE = 1.1 , $                       
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                        ; YTITLE = '[nT]', $                          <
                         COLOR=negro, $
                         CHARTHICK=1,$
                         YSTYLE=2, $
                         CHARSIZE = 1.2 ;, $
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                         COLOR=negro, $
                         CHARTHICK=1,$                         
                         YSTYLE=2, $
                         CHARSIZE = 1.2 ;, $  
;###############################################################################                         
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.02, y, '[nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90                           
;###############################################################################
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.92   
   XYOUTS, X, y, time_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.75 
;###############################################################################  
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOuts, X, 0.01, 'Tiempo Universal en dias', /Normal, $
   color=negro, Alignment=0.5, Charsize=1  
;###############################################################################
IF idate0 EQ '200310' THEN BEGIN
;first panel legend                   
        POLYFILL, [0.08,0.11,0.11,0.08], [0.862,0.862,0.866,0.866], color = azul, /NORMAL
        POLYFILL, [0.08,0.11,0.11,0.08], [0.827,0.827,0.830,0.830], color = rojo, /NORMAL        
        POLYFILL, [0.08,0.11,0.11,0.08], [0.794,0.794,0.797,0.797], color = negro, /NORMAL 
       
        POLYFILL, [0.16,0.18,0.18,0.16], [0.832,0.832,0.862,0.862], color = verde, $
        /NORMAL, /LINE_FILL, ORIENTATION=45, THICK=3, LINESTYLE=0, SPACING=0.3   
        
        POLYFILL, [0.16,0.18,0.18,0.16], [0.832,0.832,0.862,0.862], color = verde, $
        /NORMAL, /LINE_FILL, ORIENTATION=-45, THICK=3, LINESTYLE=0, SPACING=0.3  

        POLYFILL, [0.16,0.18,0.18,0.16], [0.794,0.794,0.824,0.824], color = amarillo, /NORMAL         
        
        T1 = TexToIDL('T_1')
        XYOUTS, 0.12, 0.854 , /NORMAL, $
                T1, COLOR=negro, $
                CHARSIZE = 0.9, $
                CHARTHICK=2 
                
        T2 = TexToIDL('T_2')
        XYOUTS, 0.12, 0.822 , /NORMAL, $
                T2, COLOR=negro, $
                CHARSIZE = 0.9, $
                CHARTHICK=2   

        Bsq = TexToIDL('B_{SQ}')                
        XYOUTS, 0.12, 0.785 , /NORMAL, $
                Bsq, COLOR=negro, $
                CHARSIZE = 0.9, $
                CHARTHICK=2
                             
        XYOUTS, 0.19, 0.832 , /NORMAL, $
                'Dia Quieto', COLOR=negro, $
                CHARSIZE = 0.9, $
                CHARTHICK=2
                
        XYOUTS, 0.19, 0.795 , /NORMAL, $
                'TGM', COLOR=negro, $
                CHARSIZE = 0.9, $
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


   ; plot, f_k, pws1, position = [0.55, 0.56, 0.93, 0.90], xstyle=1, $
    ;/noerase, /ylog, title = 'PWS of H component for quiet days', $
    ;subtitle='Frequencies [Hz]', CHARSIZE = chr_size1;, xrange=[min(f_k), fn];, yrange=[down,up]
    
    ;oplot, f_k, pws2   
    
   ; plot, time, H, position = [0.07, 0.12, 0.45, 0.46], /noerase, 
     
    ;oplot, time, Bsq

    ;plot, time, H-Bsq, position = [0.55, 0.12, 0.93, 0.46], /noerase, /ylog, $
    ;xstyle=1, title = 'PWS of H component for Qday 2', subtitle='Frequencies [Hz]', $
    ; CHARSIZE = chr_size1;, xrange=[min(f_k), fn]     
  ;   print, temp
;###############################################################################
; saving png
;###############################################################################     
    ; Image=TVRD() 
    ;TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
   ; TVLCT, R_bak, G_bak, B_bak
        
    ;DEVICE, /CLOSE
    ;SET_PLOT, Device_bak2  
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; open the post stript device
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

;device,/close
    
END






