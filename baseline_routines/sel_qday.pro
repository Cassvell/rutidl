;
;Name:
;	sel_qday.pro
;purpose:
;	analyse and plot the standard deviation of H component of geomagnetic field obs
;   in order to select quiet days for Solar quiet diurnal baseline
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
;   .r sel_qday
;   sel_qday, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;   idate/fdate: initial date/ final date
;
;dependencies:
;
;
;input files
;   H component of geomagnetic observations
;
;output files:
;   .PNG figure of Standard deviation of H component in 1H resolution.
;   imported to: output/qdays/DH_stdesv_V'n'_yyyymmdd_yyyymmdd.png
;
;version
;   Dec, 2022
;
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
PRO sel_qday, date_i, date_f
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]

        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
        data_file_name = STRARR(file_number)
        string_date     = STRARR(file_number)
       
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = string(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                data_file_name[i] = '../rutidl/teoloyucan/teoloyucan/'+'teo_'+string_date[i]+'.clean.dat'
                
		        data_file_name = FILE_SEARCH(data_file_name[i], COUNT=opened_files)
		        		        
	            IF opened_files NE N_ELEMENTS(data_file_name) THEN begin
	                data_file_name[i] = '../rutidl/dH_teo/'+'teo_'+string_date[i]+'.dst.early'    
	            ENDIF                
        ENDFOR

        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(where(exist_data_file EQ 1))

        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.k_index.',A,' impossible to plot all data.')"              
        ENDIF

        H    = FLTARR(file_number*1440)                               
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        dat = teo([tmp_year, tmp_month, tmp_day])
                        
                        H[i*1440:(i+1)*1440-1] = dat.TEOH[*]                                                
                ENDIF ELSE BEGIN
                         H[i*1440:(i+1)*1440-1] = 999999.0
                ENDELSE                
        ENDFOR
;identifying NAN percentage values in the Time Series
    H = nanpc(H, 99999.0, 'gequal')
PRINT, '#######################################################################'
    H = add_nan(H, 999999.0, 'equal')       
;Cálculo de las desviaciones estándar en formato horario.
    H_STDESV = FINDGEN(N_ELEMENTS(H)/60)  
        	
    FOR i=0, N_ELEMENTS(H_STDESV)-1 DO BEGIN 
        ;PRINT, STDDEV(H[i*60:(i+1)*60-1], /NAN) 
        H_STDESV[i] = STDDEV(H[i*60:(i+1)*60-1], /NAN)
    ENDFOR       	
;Determinación de las máximas desviaciones estándar por cada día
    H_std_max = FINDGEN(N_ELEMENTS(H_STDESV)/24)
    FOR i=0, file_number-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        
     ;  PRINT, string_date[i], max(H_hr[i*24:(i+1)*24-1]), FORMAT='(8A, 4X, I03)'   
    ENDFOR 	    
PRINT, '#######################################################################'
PRINT, '#######################################################################'       
tw = FINDGEN(file_number*24)/24.    
;###############################################################################   
;print figures
    time = FINDGEN(file_number *24)/24.0
        Device_bak = !D.Name 
        SET_PLOT, 'Z'

        
        Xsize=fix(1600)
        Ysize=250
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
                     
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT

    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = mh_i      
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
;fecha del primer dia quieto seleccionado
    dy_i2 = dy_i
    idate1 = string(yr_i, mh_i, format='(I4,I02)')
    dq1 = idate1

    case dq1 of
        '200310' : dq1 = 6
        '200411' : dq1 = 5
        '200505' : dq1 = 4
        '201503' : dq1 = 6
        '201705' : dq1 = 6
        '201708' : dq1 = 18
        else: print, 'fuera de rango'
    endcase  
        
    IF idate1 EQ '200505' THEN BEGIN
        H_STDESV = add_nan(H_STDESV, 1000.0, 'greater')       
    ENDIF

    IF idate1 EQ '201705' THEN BEGIN
        H_STDESV = add_nan(H_STDESV, 40.0, 'greater')         
    ENDIF

    IF idate1 EQ '201708' THEN BEGIN
        H_STDESV = add_nan(H_STDESV, 100.0, 'greater')         
    ENDIF
;###############################################################################
;fecha del n1 dia quieto 
    dy_i01 = dy_i
    fdate1 = string(yr_i, mh_f, format='(I4,I02)')
    dq01 = idate1
    case dq01 of
        '200310' : dq01 = 5
        '200411' : dq01 = 1
        '200505' : dq01 = 99999
        '201503' : dq01 = file_number-5    
        '201705' : dq01 = 5
        '201708' : dq01 = 5
        else: print, 'fuera de rango'
    endcase                   
;###############################################################################
;fecha del n2 dia quieto 
    dy_i02 = dy_i
    fdate1 = string(yr_i, mh_f, format='(I4,I02)')
    dq02 = idate1
    case dq02 of
        '200310' : dq02 = 7
        '200411' : dq02 = 99999
        '200505' : dq02 = 99999
        '201503' : dq02 = 22
        '201705' : dq02 = 4
        '201708' : dq02 = 16
        else: print, 'fuera de rango'
    endcase    
;###############################################################################
;fecha del n3 dia quieto 
    dy_i02 = dy_i
    fdate1 = string(yr_i, mh_f, format='(I4,I02)')
    dq03 = idate1
    case dq03 of
        '200310' : dq03 = 18
        '200411' : dq03 = 99999
        '200505' : dq03 = 99999
        '201503' : dq03 = file_number-6
        '201706' : dq03 = 19
        '201708' : dq03 = 18
        else: print, 'fuera de rango'
    endcase                 
;###############################################################################
;día del segundo día quieto seleccionado
    dy_f2 = dy_f
    fdate1 = string(yr_i, mh_f, format='(I4,I02)')
    dq2 = fdate1
    case dq2 of
        '200311' : dq2 = file_number-3
        '200411' : dq2 = file_number-7
        '200505' : dq2 = file_number-7
        '201504' : dq2 = file_number-5
        '201706' : dq2 = 15
        '201709' : dq2 = file_number-9
        else: print, 'fuera de rango'
    endcase 
;###############################################################################
;día del segundo día n1
    dy_f2 = dy_f
    fdate1 = string(yr_i, mh_f, format='(I4,I02)')
    dq04 = fdate1
    case dq04 of
        '200311' : dq04 = file_number-4
        '200411' : dq04 = file_number-4
        '200505' : dq04 = file_number-8
        '201504' : dq04 = 31
        '201706' : dq04 = 15
        '201709' : dq04 = file_number-5
        else: print, 'fuera de rango'
    endcase   
;###############################################################################
;día del segundo día n2
    dy_f2 = dy_f
    fdate1 = string(yr_i, mh_f, format='(I4,I02)')
    dq05 = fdate1
    case dq05 of
        '200311' : dq05 = file_number-2
        '200411' : dq05 = 99999
        '200505' : dq05 = file_number-4
        '201504' : dq05 = 35
        '201706' : dq05 = file_number-11
        '201709' : dq05 = file_number-8
        else: print, 'fuera de rango'
    endcase 
;###############################################################################
;día del segundo día n3
    dy_f2 = dy_f
    fdate1 = string(yr_i, mh_f, format='(I4,I02)')
    dq06 = fdate1
    case dq06 of
        '200311' : dq06 = 99999
        '200411' : dq06 = 99999
        '200505' : dq06 = file_number-6
        '201504' : dq06 = file_number-2
        '201706' : dq06 = file_number-1
        '201709' : dq06 = file_number-6
        else: print, 'fuera de rango'
    endcase  
;###############################################################################
;fecha en que inició la respectiva TGM 
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_i = idate0
    case TGM_i of
        '200310' : TGM_i = 23
        '200411' : TGM_i = 6
        '200505' : TGM_i = 14
        '201503' : TGM_i = 8
        '201705' : TGM_i = 11
        '201708' : TGM_i = file_number-25
        else: print, 'fuera de rango'
    endcase  
;###############################################################################
;fecha en que terminó la respectiva TGM 
    fdate0 = string(yr_i, mh_f, format='(I4,I02)')
    TGM_f = fdate0
    case TGM_f of
        '200311' : TGM_f = 27
        '200411' : TGM_f = 12
        '200505' : TGM_f = 16
        '201504' : TGM_f = 16
        '201706' : TGM_f = 7
        '201709' : TGM_f = file_number-20
        else: print, 'fuera de rango'
    endcase                   
;###############################################################################
    days = intarr(file_number+1)
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
    FOR i=0, file_number-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        

       ;OPENW, LUN, outfile[i], /GET_LUN        
       ;PRINT, string_date[i], max(H_STDESV[i*24:(i+1)*24-1]), $
       FORMAT='(8A, 4X, I03)' 
        
       ; CLOSE, LUN
       ; FREE_LUN, LUN    
    ENDFOR   
;###############################################################################
    sigma = TeXtoIDL('\sigmaH')
    dH    = TeXtoIDL('\DeltaH')
    time_title = 'Desviacion estandar de '+dH+' para '+ time_title
;###############################################################################    
    down0 = min(H_STDESV)   
    up0   = max(H_STDESV)  

    plot, time, H_STDESV, XTICKS=file_number, xminor=8, BACKGROUND = blanco, COLOR=rojo,$
    CHARSIZE = 1.0, CHARTHICK=chr_thick1, POSITION=[0.05,0.15,0.95,0.90], $
    XSTYLE = 5, XTICKNAME=REPLICATE(' ', file_number+1), XRANGE=[0, file_number], $
    ySTYLE = 6, YRANGE=[down0,up0], THICK=4         
;###############################################################################    
    POLYFILL, [dq01, dq01+1, dq01+1, dq01],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3             

    POLYFILL, [dq01, dq01+1, dq01+1, dq01],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3   
                            
    POLYFILL, [dq02, dq02+1, dq02+1, dq02],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3   

    POLYFILL, [dq02, dq02+1, dq02+1, dq02],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3  
              
    POLYFILL, [dq03, dq03+1, dq03+1, dq03],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3   

    POLYFILL, [dq03, dq03+1, dq03+1, dq03],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3       
              
    POLYFILL, [dq04, dq04+1, dq04+1, dq04],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3   

    POLYFILL, [dq04, dq04+1, dq04+1, dq04],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3      

    POLYFILL, [dq05, dq05+1, dq05+1, dq05],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3   

    POLYFILL, [dq05, dq05+1, dq05+1, dq05],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3      
              
    POLYFILL, [dq06, dq06+1, dq06+1, dq06],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3, SPACING=0.3   

    POLYFILL, [dq06, dq06+1, dq06+1, dq06],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=3, SPACING=0.3                                                                   
;###############################################################################    
    ;días quietos seleccionados
    POLYFILL, [TGM_i, TGM_f, TGM_f, TGM_i], $
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=amarillo
              
    POLYFILL, [dq1, dq1+1, dq1+1, dq1],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde;, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3           
              
    POLYFILL, [dq2, dq2+1, dq2+1, dq2],$
              [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
              COLOR=verde;, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=3                              

    IF idate0 EQ '200310' THEN BEGIN
        POLYFILL, [TGM_i+22, TGM_f+22, TGM_f+22, TGM_i+22], $
                  [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], $
                  COLOR=amarillo   
        OPLOT, [tgm_i+22, tgm_i+22], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5,$
        color=negro, THICK=3 
        OPLOT, [tgm_f+22, tgm_f+22], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5, $
        color=negro, THICK=3                 
    ENDIF             
;###############################################################################           
;días quietos    
    OPLOT, [dq01,dq01], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2    
    OPLOT, [dq01+1,dq01+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2  

    OPLOT, [dq02,dq02], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2    
    OPLOT, [dq02+1,dq02+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2 
    
    OPLOT, [dq03,dq03], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2    
    OPLOT, [dq03+1,dq03+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2 
    
    OPLOT, [dq04,dq04], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2    
    OPLOT, [dq04+1,dq04+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2 
    
    OPLOT, [dq05,dq05], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2    
    OPLOT, [dq05+1,dq05+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2         
 
    OPLOT, [dq06,dq06], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2    
    OPLOT, [dq06+1,dq06+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=2        
;###############################################################################       
;días quietos seleccionados              
    OPLOT, [dq1,dq1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=3    
    OPLOT, [dq1+1,dq1+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=3   
        
    OPLOT, [dq2,dq2], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=3
    OPLOT, [dq2+1,dq2+1], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=3, color=negro, THICK=3   
    
    OPLOT, [tgm_i, tgm_i], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5, color=negro, THICK=3 
    OPLOT, [tgm_f, tgm_f], [!Y.CRANGE[0], !Y.CRANGE[1]], linestyle=5, color=negro, THICK=3  
    
    OPLOT, time, H_STDESV, LINESTYLE=0, THICK=4 , COLOR=rojo                                                              
;###############################################################################       
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         COLOR=negro, $
                         CHARSIZE = 1.0, $
                         xtitle='',$
                         CHARTHICK=2
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XMINOR=8, $
                         XTICKNAME=REPLICATE(' ', file_number+1), $
                         COLOR=negro, $
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down0,up0],$
                         Ystyle=2, $
                         COLOR=negro, $
                         CHARSIZE = 1.0, $
                         CHARTHICK=1.5;, $

        AXIS, YAXIS = 1, YRANGE=[down0,up0],$
                         Ystyle=2, $
                         COLOR=negro, $
                         CHARSIZE = 1.0, $
                         CHARTHICK=1.5;, $
;###############################################################################                         
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.02, y, sigma+' [nT]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90, CHARTHICK=1.5                             
;###############################################################################
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.92   
   XYOUTS, X, y, time_title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65, CHARTHICK=2 
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.02   
   XYOUTS, X, y, 'Tiempo universal [dias]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.0, CHARTHICK=1.0
;###############################################################################    
    IF idate0 EQ '200310' THEN BEGIN
    ;first panel legend                   

           
            POLYFILL, [0.07,0.1,0.1,0.07], [0.832,0.832,0.872,0.872], color = verde, $
            /NORMAL, /LINE_FILL, ORIENTATION=45, THICK=3, LINESTYLE=0, SPACING=0.3
            
            POLYFILL, [0.07,0.1,0.1,0.07], [0.832,0.832,0.862,0.862], color = verde, $
            /NORMAL, /LINE_FILL, ORIENTATION=-45, THICK=3, LINESTYLE=0, SPACING=0.3 

            POLYFILL, [0.07,0.1,0.1,0.07], [0.752,0.752,0.802,0.802], color = verde, $
            /NORMAL         
            
            
            POLYFILL, [0.07,0.1,0.1,0.07], [0.672,0.672,0.722,0.722], color = amarillo, $
            /NORMAL
            

                                 
            XYOUTS, 0.105, 0.83 , /NORMAL, $
                    'DQ', COLOR=negro, $
                    CHARSIZE = 1.1, $
                    CHARTHICK=2
                    
            XYOUTS, 0.105, 0.75, /NORMAL, $
                    'DQS', COLOR=negro, $
                    CHARSIZE = 1.1, $
                    CHARTHICK=2                
                    
            XYOUTS,  0.105, 0.67 , /NORMAL, $
                    'TGM', COLOR=negro, $
                    CHARSIZE = 1.1, $
                    CHARTHICK=2                                
    ENDIF
;###############################################################################                     
    Image=TVRD() 
    TVLCT, reds, greens, blues, /get                          ; reads Z buffer !!
    
    TVLCT, R_bak, G_bak, B_bak
        
    SET_PLOT, Device_bak
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
; open the post stript device
    path = '../rutidl/output/eventos_tgm/'
    fecha = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT = '(I4,I02,I02,"_",I4,I02,I02)')
        IF keyword_set(jpeg) THEN BEGIN
                info = size(Image)
                nx = info[1]
                ny = info[2]
                true_image = bytarr(3,nx,ny)
                true_image[0,*,*] = reds[image]
                true_image[1,*,*] = greens[image]
                true_image[2,*,*] = blues[image]
                write_jpeg, path+'DH_stdesv_V2_'+fecha+'.jpg', True_Image, true=1
        ENDIF ELSE BEGIN
                IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN PRINT, '        Setting PNG as default file type.'
                WRITE_PNG, path+'DH_stdesv_V2_'+fecha+'.png', Image, reds,greens,blues
        ENDELSE

        IF NOT keyword_set(quiet) THEN BEGIN
                PRINT, '        Saving: '+path+'DH_stdesv_V2_'+fecha
                PRINT, ''
        ENDIF
        RETURN   
	
END	
