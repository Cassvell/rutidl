;Name:
;	iono_respV2.pro
;purpose:
;   plot geomagnetic and ionospheric response due to Dst and Ddyn magnetic trace
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
;   .r iono_resp
;   iono_resp, idate, fdate, PNG='png', PS='ps'
;parameters:
;   idate/fdate: format ([yyyy,mm,dd])
;
;dependencies:
;
;
;input files
;   Dst files, H obs, Bsq baseline, TEC data files
;
;output files:
;   .PNG figure
;   imported to /output/eventos_tgm/iono_resp_V9_yyyy-mm-dd.png
;
;version
;   apr, 2023
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;

PRO iono_resp_pws, date_i, date_f, PNG = png, PS=ps 

	On_error, 2
	COMPILE_OPT idl2, HIDDEN

	@set_up_commons
	set_up    

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
	
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
;###############################################################################  
; Generate the time series variables 
; define H variables                  

    idx = sym_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH = idx.symH
    ;dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    data   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = data.H
    H = add_nan(H, 120, 'greater')

    H = fillnan(H)
    print, max(H)
 ;   PRINT, H;N_ELEMENTS(H), N_ELEMENTS(Bsq)
;###############################################################################    
;###############################################################################             
;###############################################################################
; Import the structure of diono generated variables   

    dionstr = gen_diono(symH, H, 28.06, 'm', TGM_n, station_code, DIG_FILTER = 'dig_filter')
 ;   PRINT, Bsq
; compute frequencies 
    f_k   = dionstr.f_k
    fn    = dionstr.fn

; compute and define Power Spectrum
    pws = dionstr.pws
	;pws = pws/SQRT(TOTAL(pws^2))
; compute diono variables    
    diono = dionstr.diono
  ;  PRINT, dst
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn

;###############################################################################         
;###############################################################################
; define device and color parameters 
;###############################################################################

    IF keyword_set(ps) THEN BEGIN
        path = set_var.local_dir+'output/diono_recons/'+station_code+'/'	
        test = FILE_TEST(path, /DIRECTORY) 
        IF test EQ 0 THEN BEGIN
            FILE_MKDIR, path
            PRINT, 'PATH directory '+path
            PRINT, 'created'
        ENDIF ELSE BEGIN
            PRINT, ''
            
        ENDELSE   

    make_psfig, f_k, fn, pws, symH, H, diono, ddyn, dp2, time,$
        path, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code   
    ENDIF

    IF keyword_set(png) THEN BEGIN        
    make_pngfig, f_k, fn, pws, new_Bz, new_Ey, new_vp, new_pdyn, new_idiff, new_ddyn, new_dp2, time, $
        SG_beg, SG_end, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]               
    ENDIF 

END


PRO make_psfig, f_k, fn, pws, new_dst, new_dH, new_idiff, new_ddyn, new_dp2, time,$
        path, date_i, date_f, station_code

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    @set_up_commons
    set_up			
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])    	
;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)')

    psfile =  path+'iono_PI_'+Date+'_'+station_code+'.eps'    
   ; LOADCT, 39
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10
    X_label = xlabel([yr_i, mh_i, dy_i], file_number)
    old_month = month_name(mh_i, 'english') 
    
    class = gms_class(station_code)
    info = stationlist(class, station_code)

    time_title = ' UT [days]'
    title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
    FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, ", ", "magnetic lon: ", F7.2," ", A)')

    periodo = 'Period [h]'        
;###############################################################################     
    chr_size1 = 0.9
    chr_thick1= 1.5          
    cgPLOT, f_k, pws, /XLOG, /YLOG, POSITION=[0.1,0.11,0.95,0.89],$
    BACKGROUND = 'white', COLOR='black', $
    CHARSIZE = chr_size1, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=4, /NODATA    

   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   y = 0.95   
   XYOUTS, X, y, title, /NORMAL, $
   ALIGNMENT=0.5, CHARSIZE=2.8, CHARTHICK=1.5               
;###############################################################################               
    freqs = [1.0/(96.0*3600.0), 1.0/(48.0*3600.0), 1.0/(24.0*3600.0), $
              1.0/(12.0*3600.0), 1.0/(6.0*3600.0), 1.0/(3.0*3600.0), 1.0/3600.0]
    
    i = WHERE((f_k GE freqs[1]) AND (f_k LE freqs[6]), count)
    
    fny=WHERE(f_k EQ fn)

    ysup = 50;max(pws[i])
    yinf = 1e-5;min(pws[i])
    
    print, 'min pws: ', yinf
    print, 'max pws: ', ysup
    
    print, 'min DP2: ', min(new_dp2)
    print, 'max DP2: ', max(new_dp2)

    print, 'min Ddyn: ', min(new_ddyn)
    print, 'max Ddyn: ', max(new_ddyn)



    periods = [96.0, 48.0, 24.0, 12.0, 6.0, 3.0, 1.0]
    
    cgPLOT, f_k, pws, /XLOG, /YLOG, XRANGE = [freqs[1], freqs[6]], POSITION=[0.08,0.11,0.4,0.89],$
    YRANGE=[yinf, ysup], BACKGROUND ='white', COLOR='black', $
    CHARSIZE = 1.4, XSTYLE=5, YSTYLE=5, SUBTITLE='', THICK=2, /NODATA,$
    /NOERASE

    i = freq_cuts(TGM_n, station_code)

    passband_l = freq_band(TGM_n, 'passband_l')
    passband_u = freq_band(TGM_n, 'passband_u')
    highpass_l = freq_band(TGM_n, 'highpass_l')

    cgPolygon, [passband_l, passband_u ,passband_u, passband_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='BLK2', /FILL

    cgPolygon, [highpass_l, freqs[6], freqs[6], highpass_l], $
              [!Y.CRANGE[0], !Y.CRANGE[0], ysup, ysup], COLOR='BLK2', /FILL
                                             
              
    cgOPLOT, f_k, pws, COLOR='black', THICK=5   
;###############################################################################    
        AXIS, XAXIS = 0, XRANGE=[freqs[1], freqs[6]], $
                         /XLOG,$
                         XSTYLE=1,$
                         xTITLE = '',$
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5
                                           
        AXIS, XAXIS = 1, XRANGE=[freqs[1], freqs[6]], $;.0/(!X.CRANGE), $
                         /XLOG,$
                         XTICKS=6,$
                         XMINOR=4,$
                         XTICKV=freqs,$                         
                         XTICKN=STRING(periods, FORMAT='(F4.1)'),$
                         XSTYLE=1,$
                         CHARSIZE = 1.2,$
                       ;  COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5                     

        AXIS, YAXIS = 0, yrange=[yinf, ysup], $
                         YTITLE = '', $
                         ystyle=1,$                          
                        ; COLOR=negro, $
                         /ylog,$
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5
                        
        AXIS, YAXIS = 1, yrange=[yinf, ysup], $
                        ; COLOR=negro, $
                         /ylog,$
                         YTICKFORMAT='(A1)',$ 
                         ystyle=1, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5

   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.922, periodo, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5   
   
   x = (!X.Window[1] - !X.Window[0]) / 2. + !X.Window[0]
   XYOUTS, X, 0.065, 'Frequence [Hz]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, CHARTHICK=1.5      
   
   
   y = (!Y.Window[1] - !Y.Window[0]) / 2. + !Y.Window[0] 
   XYOUTS, 0.025, y, 'Power Spectral Density', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.6, ORIENTATION=90, CHARTHICK=1.5
   
   ;XYOUTS, 0.16, .17, 'Ddyn', /NORMAL, $
   ;COLOR=negro, ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5     
   
   ;XYOUTS, 0.34, .82, 'DP2', /NORMAL, $
   ;COLOR=negro, ALIGNMENT=0.5, CHARSIZE=2, CHARTHICK=1.5    
;###############################################################################       
     d_H = TeXtoIDL('\DeltaH_{' + STRUPCASE(station_code) + '}') 
;###############################################################################   [0.5,0.7,0.95,0.9]   
     ; up = MAX(new_dH)
     ;down=MIN(new_dH)
    if max(new_dH) eq max(new_dst) then up = max(new_dH) else up = max(new_dst)
    if min(new_dH) eq min(new_dst) then down = min(new_dH) else down = min(new_dst)

     CGPLOT, time, new_dH, XTICKS=file_number, XMINOR=8, BACKGROUND = 'white', $
     COLOR='black', CHARSIZE = 0.9, CHARTHICK=chr_thick1, $
     POSITION=[0.5,0.71,0.95,0.89], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, THICK=2, /NODATA       
     
     cgOPlot, time, new_dH, color = 'black', thick=3, linestyle=0
     cgOPlot, time, new_dst, color = 'GRN5', thick=3, linestyle=0     
   
        AXIS, XAXIS = 0, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                        ; COLOR=negro, $
                         CHARSIZE = 3.0 , $
                         TICKLEN=0.04,$
                         CHARTHICK=3.5 
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                        ; COLOR=negro, $
                         CHARSIZE = 0.9, $
                         CHARTHICK=1.5,$
                         TICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down,up], $
                         YTITLE = 'G. Indices [nT]', $                          
                         ;COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6 
                        
        AXIS, YAXIS = 1, YRANGE=[down,up], $
                        ; COLOR=negro, $                                                                      
                         YSTYLE=2, $       
                         YTICKFORMAT='(A1)',$
                         CHARSIZE = 1.4 ,$
                         CHARTHICK=1.6                                                                                            
;###############################################################################                                 
;############################################################################### 
     up_diono=max(new_idiff)
     down_diono=min(new_idiff)
    ; print,  new_idiff
     cgPLOT, time, new_idiff, XTICKS=file_number, XMINOR=8, BACKGROUND = 'white', $
     COLOR='black', CHARSIZE = 0.6, CHARTHICK=chr_thick1, $
     POSITION=[0.5,0.51,0.95,0.68], XSTYLE = 5, XRANGE=[0, file_number], ySTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down_diono,up_diono], /NOERASE,$
     THICK=2, /NODATA   

    cgOPLOT, time, new_idiff, THICK=3, LINESTYLE=0, COLOR='black'     

    ppi = TexToIDL('D_{I}')        
        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='', $                         
                         XMINOR=8, $
                         XTICKFORMAT='(A1)',$
               ;          COLOR=negro, $
                         CHARSIZE = 1.6, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6
                         
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$                     
                      ;   COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $
                         YTITLE = ppi+' [nT]', $                          
                   ;      COLOR=negro, $
                         YSTYLE=2, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, YRANGE=[down_diono,up_diono], $
                         YTICKS=4, $      
                     ;    COLOR=negro, $
                         YSTYLE=2, $       
                         YTICKFORMAT='(A1)',$
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6                
;###############################################################################                
    ;IF max(new_ddyn) GT max(new_dp2) THEN up = max(new_ddyn) ELSE up = max(new_dp2)
    ;IF min(new_ddyn) LT min(new_dp2) THEN down = min(new_ddyn) ELSE down = min(new_dp2)
;###############################################################################
    up  = 50
    down= -50
    ;IF downddyn LT downdp2 THEN down = downddyn ELSE down=downdp2 
                               
     cgPLOT, time, new_ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND ='white', $
     COLOR='black', CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.5,0.11,0.95,0.48], XSTYLE = 5, XRANGE=[0, file_number], YSTYLE = 6,$
     XTICKNAME=REPLICATE(' ', file_number+1), YRANGE=[down,up], /NOERASE, /NODATA
    

    cgOPLOT, time, new_ddyn, COLOR='black' , LINESTYLE=0, THICK=1  
  
;###############################################################################     
    cgOPLOT, time, new_dp2, COLOR='red', THICK=3    
    
    cgOPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0.], LINESTYLE=1, THICK=4,COLOR='black'
    med_dp2 = MEDIAN(new_dp2)   
 

        AXIS, XAXIS = 0, XRANGE=[0,file_number], $
                         XTICKS=file_number, $
                         XTITLE='', $                         
                         XMINOR=8, $
                         XTICKNAME=X_label, $
                         ;XTICKFORMAT='(A1)',$
                         COLOR=negro, $
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6
                         
                                                 
        AXIS, XAXIS = 1, XRANGE=[0,file_number],$
                         ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
                         XTICKS=file_number, $
                         XMINOR=8, $
                         ;XTICKV=FIX(days), $       
                         XTICKFORMAT='(A1)',$
                         CHARSIZE = 0.8, $                                                
                    ;     COLOR=negro, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.6

        AXIS, YAXIS = 0, yrange=[down,up], $ 
                         ystyle=1, $  
                         YTITLE = 'I. Currents [nT]', $                          
                    ;     COLOR=negro, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6
                        
        AXIS, YAXIS = 1, yrange=[down,up], $ 
                         ystyle=1, $        
                         YTICKFORMAT='(A1)',$
                  ;       COLOR=negro, $
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.6          
;###############################################################################
;first panel legend 
        cgPolygon, [0.90,0.93,0.93,0.90], [0.804,0.804,0.807,0.807], color = 'black', /NORMAL, /FILL    
        cgPolygon, [0.90,0.93,0.93,0.90], [0.767,0.767,0.770,0.770], color = 'GRN5', /NORMAL , /FILL  
        
        XYOUTS, 0.85, 0.8 , /NORMAL, d_H, CHARSIZE = 1.2, CHARTHICK=chr_thick1                 
                
        XYOUTS, 0.85, 0.762 , /NORMAL, 'Sym-H', CHARSIZE = 1.2, CHARTHICK=chr_thick1  
;###############################################################################                     
;second panel legend                   
        cgPolygon, [0.91,0.94,0.94,0.91], [0.461,0.461,0.464,0.464], color = 'black', /NORMAL, /FILL    
        cgPolygon, [0.91,0.94,0.94,0.91], [0.424,0.424,0.427,0.427], color = 'red', /NORMAL , /FILL  
        
        XYOUTS, 0.87, 0.456 , /NORMAL, 'Ddyn', CHARSIZE = 1.2, CHARTHICK=chr_thick1                 
                
        XYOUTS, 0.87, 0.42 , /NORMAL, 'DP2', CHARSIZE = 1.2, CHARTHICK=chr_thick1     
                
;###############################################################################                                                            
  !P.Font = 1
  XYOuts, 0.53, 0.735, '(a)', /Normal, $
    Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font= 3 
   
   XYOuts, 0.91, 0.54, '(b)', /Normal, $
   Alignment=0.5, Charsize=3.2, CHARTHICK= 10, font=1  
   
   XYOuts, 0.53, 0.41, '(d)', /Normal, $
   Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font=1  
   
   XYOuts, 0.12, 0.2, '(c)', /Normal, $
   Alignment=0.5, Charsize=3.8, CHARTHICK= 10;, font=1  
   
   ;XYOuts, 0.91, 0.235, '(e)', /Normal, $
   ;Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font=1    
   ;!P.Font = 0
;###############################################################################   

    cgPS_Close, density = 300, width = 1600;, /PNG  
    RETURN  
END 