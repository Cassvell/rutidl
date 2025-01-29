PRO make_psfig_composed, H, power, xwt, ddyn, period, coi, date_i, date_f, path, station_code	
        @set_up_commons
        set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])  

;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
	
    psfile =  path+'composed_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

    cgLOADCT,40

    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
                        FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

    up = max(H)
    down = min(H)
    cgplot, date_time, H, background='white', color='black', XMINOR=8, XTICKFORMAT='(A1)', XTICKUNITS=['day', 'month'],$
    XTICKLAYOUT = 2, XTICKINTERVAL = 1, POSITION=[.1, .74, .8, .92], xstyle=1, ystyle = 5
                      
    ytitle = TeXtoIDL('H_{loc} [nT]')
cgAxis,YAxis=0, yrange=[down, up], $
    YTITLE = ytitle, $
    ystyle=1,$  
    COLOR='black', $  
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

   
cgAxis, YAxis=1, yrange=[down, up], $                        
    COLOR='black', $
    ystyle=1, $
    CHARSIZE = 1.2,$
    CHARTHICK=1.5


; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
;minPower =  min(power)
;maxPower =  max(power)
    period2 = FIX(ALOG(period)/ALOG(2))
    minPower =  min(power)
    maxPower =  max(power)

    nLevels = 48
    levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower

; Generate tick names based on levels

    CGCONTOUR,power,date_time,period, XSTYLE=1,YTITLE='', title='', POSITION=[.1, .55, .8, 0.73],$
	YSTYLE=5,C_COLORS=colors, XMINOR=8,YTICKFORMAT='exponent',$ 
	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
	XTICKFORMAT='(A1)', XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1, /noerase ;,  xTITLE = 'Time [days]'

    ;ticknames: in, optional, type=string
    ;A string array of names or values for the color bar tick marks. 
    nColors = !D.TABLE_SIZE

    title = Textoidl('Power [nT^{2} Hz^{-1}]')
    tickNames = STRING(levels, FORMAT='(E8.1)')
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.55, 0.87, 0.73, 0.89], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title, vertical=1, right=1 ; Moves title and labels to the right


    ;l1 = 1600.0
    ;l2 = 1100.0
    ;l3 = 675.0
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l1,l1], color='yellow', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l2,l2], color='yellow', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l3,l3], color='yellow', thick=2

    ;print, 'frequency ranges of significant Ddyn [Hz]:'
    ;print,  string(1.0/l1, 1.0/l2, 1.0/l3,  FORMAT='(E12.5, X, E12.5, X, E12.5)')

    ;print, 'period ranges of significant Ddyn [Hz]:'
    ;print,  string(l1/60.0, l2/60.0, l3/60.0, FORMAT='(F7.1, X, F7.1, X, F7.1)')

    ;p2 = 1400.0
    ;p1 = 1100.0
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [p1,p1], color='red', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [p2,p2], color='red', thick=2

    ;print, 'frequency ranges of Ddyn peak energy:'
    ;print,  string(1.0/p2, 1.0/p1, FORMAT='(E12.5, X, E12.5)')
    ;print, 'period ranges of Ddyn peak energy:'
    ;print,  string(p2/60.0, p1/60.0, FORMAT='(F7.1, X, F7.1)')
; Check that colors and levels are properly defined before passing to cgColorbar

;print, 'pico de potencia, rango de frecuencia, rango de periodo'

    freq_series = 1/(period*60)
    ;print, n_elements(freq_series), n_elements(period2)
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

	CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 720, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 240, PSYM=8, COLOR='white', thick=4
  	CGPLOTS, max(date_time), 60, PSYM=8, COLOR='white', thick=4

   CGTEXT, MAX(date_time), 2880  , ' 48',$
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2;, ORIENTATION=90   

   CGTEXT, MAX(date_time), 1440  , ' 24', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2
   
   CGTEXT, MAX(date_time), 720  , ' 12', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2
   
   CGTEXT, MAX(date_time), 240  , '  4', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2
   
	CGTEXT, MAX(date_time), 60  , '  1', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2 
;##################################################

	x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='white', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='white', /FILL


        CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
                         COLOR='white', $
                         XSTYLE=1,$ 
                         XMINOR=8,$
                         XTICKS=file_number,$
                         ;xTITLE = 'Time [days]',$ 
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5,$
                         XTICKFORMAT='(A1)'
                                           
        CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)    
                         COLOR='white', $
                         XSTYLE=1,$
                         XTICKS=file_number,$
                         XMINOR=8,$
                         XTICKFORMAT='(A1)',$
                         XTICKUNITS=['day']                         

        cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
                         YTITLE = 'Freq [Hz]', $
                         ystyle=1,$  
                         COLOR='black', $                
                         /ylog,$
                         CHARSIZE = 1.2,$
                         CHARTHICK=1.5

                        
        cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
                         /ylog,$ 
                         YTITLE = 'Period [h]', $                         
                         COLOR='black', $
                         YTICKFORMAT='(A1)',$ 
                         ystyle=5, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                                            
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .55, .8, 0.73],$
    color = 'white',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
    ;print, n_elements(period2), n_elements(date_time)
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT TICK LAYERS         

    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='white', $                
    /ylog,$
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='white', $
    YTICKFORMAT='(A1)',$ 
    ystyle=5, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5

    CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
    COLOR='white', $
    XSTYLE=1,$ 
    XMINOR=8,$
    XTICKS=file_number,$
    ;xTITLE = 'Time [days]',$ 
    CHARSIZE = 1.4, $
    TICKLEN=0.04,$
    CHARTHICK=1.5,$
    XTICKFORMAT='(A1)'
                      
CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)    
    COLOR='white', $
    XSTYLE=1,$
    XTICKS=file_number,$
    XMINOR=8,$
    XTICKFORMAT='(A1)',$
    XTICKUNITS=['day']   

    ;###############################################################################
;###############################################################################
   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.855
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90
;###############################################################################   
;###############################################################################
;###############################################################################
;###############################################################################                          
;###############################################################################   
    minPower =  min(xwt)
    maxPower =  max(xwt)

    nLevels = 36
    levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower 

    cgCONTOUR,xwt,date_time,period, $
    XSTYLE=1,YTITLE='', title='', POSITION=[.1, .36, .8, .54],$
    YSTYLE=5,C_COLORS=colors, XMINOR=8,	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
    XTICKFORMAT='(A1)', XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
    XTICKINTERVAL = 1, /noerase;,  xTITLE = 'Time [days]'

    nColors = !D.TABLE_SIZE

    ; Reduce tick labels to 10
    title = 'Cross Wavelet [nT]'
    nTicks = 6  
    tickIndices = ROUND(FINDGEN(nTicks) * (nLevels - 1) / (nTicks - 1))
    tickValues = levels[tickIndices]
    tickNames = STRING(tickValues, FORMAT='(F6.1)')

    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.36, 0.87, 0.54, 0.89], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
        Charsize=1.0, title=title, vertical=1, right=1
    
    ;cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    ;Charsize= 1.0,  title=title


;###############################################################################
;###############################################################################
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

    CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='red', thick=4
    CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='red', thick=4
    CGPLOTS, max(date_time), 720, PSYM=8, COLOR='red', thick=4
    CGPLOTS, max(date_time), 240, PSYM=8, COLOR='red', thick=4
    CGPLOTS, max(date_time), 60, PSYM=8, COLOR='red', thick=4

    CGTEXT, MAX(date_time), 2880  , ' 48',$
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2;, ORIENTATION=90   

    CGTEXT, MAX(date_time), 1440  , ' 24', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2

    CGTEXT, MAX(date_time), 720  , ' 12', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2

    CGTEXT, MAX(date_time), 240  , '  4', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2

        CGTEXT, MAX(date_time), 60  , '  1', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2  
;##################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .8, .54],$
    color = 'black',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
    ;print, n_elements(period2), n_elements(date_time)
;###############################################################################
;###############################################################################
;###############################################################################
    x = [date_time[0],date_time,MAX(date_time)]
    y = [MAX(period),coi,MAX(period)]

    cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='white', /FILL
    cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='white', /FILL
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT TICK LAYERS         

    freq_series = 1/(period*60)
    ; print, n_elements(freq_series), n_elements(period2)
    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = 'Freq [Hz]', $
    ystyle=1,$  
    COLOR='black', $                
    /ylog,$
    ;YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='black', $
    YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5


    ;###############################################################################
;###############################################################################
   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.855
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90
;###############################################################################   
;###############################################################################
;###############################################################################
;###############################################################################                          
;###############################################################################    
minPower = min(ddyn)
maxPower = max(ddyn)

ddyn = (maxPower - ddyn) / (maxPower - minPower)


minPower = 0
maxPower = 1

nLevels = 36

levels = FINDGEN(nLevels) / (nLevels)
   ; print, levels

; Generate tick names based on levels
  
cgCONTOUR,ddyn,date_time,period, $
XSTYLE=1,YTITLE='', title='', POSITION=[.1, .17, .8, .35],$
YSTYLE=5,C_COLORS=colors, XMINOR=8,	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
XTICKINTERVAL = 1, charsize=1.2, /noerase;,  xTITLE = 'Time [days]'

nColors = !D.TABLE_SIZE

title = Textoidl('ddyn correlation')
tick_vals = [0, 0.2, 0.4, 0.6, 0.8, 1]
tickNames = STRING(tick_vals, FORMAT='(F4.1)')

cgCOLORBAR, NCOLORS=nColors, POSITION=[0.17, 0.87, 0.35, 0.89], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
Charsize= 1.0,  title=title, vertical=1, right=1 ; Moves title and labels to the right
;cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
;Charsize= 1.0,  title=title


;###############################################################################
;###############################################################################
usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='red', thick=4
CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='red', thick=4
CGPLOTS, max(date_time), 720, PSYM=8, COLOR='red', thick=4
CGPLOTS, max(date_time), 240, PSYM=8, COLOR='red', thick=4
  CGPLOTS, max(date_time), 60, PSYM=8, COLOR='red', thick=4

CGTEXT, MAX(date_time), 2880  , ' 48',$
COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2;, ORIENTATION=90   

CGTEXT, MAX(date_time), 1440  , ' 24', $
COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2

CGTEXT, MAX(date_time), 720  , ' 12', $
COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2

CGTEXT, MAX(date_time), 240  , '  4', $
COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2
    CGTEXT, MAX(date_time), 60  , '  1', $
COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.2
;##################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT LAYER

cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .17, .8, .35],$
color = 'black',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
;print, n_elements(period2), n_elements(date_time)
;###############################################################################
;###############################################################################
;###############################################################################
x = [date_time[0],date_time,MAX(date_time)]
y = [MAX(period),coi,MAX(period)]

cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='white', /FILL
cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='white', /FILL
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT TICK LAYERS         

freq_series = 1/(period*60)

    CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
    COLOR='white', $
    XSTYLE=1,$ 
    XMINOR=8,$
    XTICKS=file_number,$
    ;xTITLE = 'Time [days]',$ 
    CHARSIZE = 1.4, $
    TICKLEN=0.04,$
    CHARTHICK=1.5,$
    XTICKFORMAT='(A1)'
                    
    CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)    
    COLOR='white', $
    XSTYLE=1,$
    XTICKS=file_number,$
    XMINOR=8,$
    XTICKFORMAT='(A1)',$
    XTICKUNITS=['day']                         

    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = 'Freq [Hz]', $
    ystyle=1,$  
    COLOR='black', $                
    /ylog,$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5


    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='black', $
    YTICKFORMAT='(A1)',$ 
    ystyle=5, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5
                   
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .8, .92],$
    color = 'white',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
;print, n_elements(period2), n_elements(date_time)
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT TICK LAYERS         

    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='white', $                
    /ylog,$
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='white', $
    YTICKFORMAT='(A1)',$ 
    ystyle=5, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5

;###############################################################################   
;###############################################################################
;###############################################################################
;###############################################################################                          
;###############################################################################   

 
   class = gms_class(station_code)
   info = stationlist(class, station_code)
   title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
   FORMAT='(A, ", mlat: ", F7.2, " ", A, ", ", "mlon: ", F7.2," ", A)')
  
  
  x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.93   
   XYOUTS, X, y, title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65     

   x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.02   

   xtitle = 'Time [UT h]'

   XYOUTS, X, y, xtitle, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2  

;###############################################################################
;###############################################################################
   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.855
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90
;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END 



PRO make_psfig1, power, period, coi, date_i, date_f, path, station_code	
        @set_up_commons
        set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])  

;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
	
    psfile =  path+'power_'+Date+'.scaled.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

 cgLOADCT,40

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

period2 = FIX(ALOG(period)/ALOG(2))
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
;minPower =  min(power)
;maxPower =  max(power)

minPower =  min(power)
maxPower =  max(power)

nLevels = 48
levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower

; Generate tick names based on levels
      
CGCONTOUR,power,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.1, .36, .92, .92],$
	YSTYLE=5,C_COLORS=colors, XMINOR=8,YTICKFORMAT='exponent',$ 
	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1;,  xTITLE = 'Time [days]'

    ;ticknames: in, optional, type=string
    ;A string array of names or values for the color bar tick marks. 
    nColors = !D.TABLE_SIZE

    title = Textoidl('Power [nT^{2} Hz^{-1}]')
    tickNames = STRING(levels, FORMAT='(E12.2)')
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title


    l1 = 1600.0
    l2 = 1100.0
    l3 = 675.0
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l1,l1], color='yellow', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l2,l2], color='yellow', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [l3,l3], color='yellow', thick=2

    print, 'frequency ranges of significant Ddyn [Hz]:'
    print,  string(1.0/l1, 1.0/l2, 1.0/l3,  FORMAT='(E12.5, X, E12.5, X, E12.5)')

    print, 'period ranges of significant Ddyn [Hz]:'
    print,  string(l1/60.0, l2/60.0, l3/60.0, FORMAT='(F7.1, X, F7.1, X, F7.1)')

    p2 = 1400.0
    p1 = 1100.0
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [p1,p1], color='red', thick=2
    ;cgoplot, [!X.CRANGE[0], !X.CRANGE[1]], [p2,p2], color='red', thick=2

    print, 'frequency ranges of Ddyn peak energy:'
    print,  string(1.0/p2, 1.0/p1, FORMAT='(E12.5, X, E12.5)')
    print, 'period ranges of Ddyn peak energy:'
    print,  string(p2/60.0, p1/60.0, FORMAT='(F7.1, X, F7.1)')
; Check that colors and levels are properly defined before passing to cgColorbar

;print, 'pico de potencia, rango de frecuencia, rango de periodo'

    freq_series = 1/(period*60)
    ;print, n_elements(freq_series), n_elements(period2)
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

	CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 720, PSYM=8, COLOR='white', thick=4
	CGPLOTS, max(date_time), 240, PSYM=8, COLOR='white', thick=4
  	CGPLOTS, max(date_time), 60, PSYM=8, COLOR='white', thick=4

   CGTEXT, MAX(date_time), 2880  , ' 48',$
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65;, ORIENTATION=90   

   CGTEXT, MAX(date_time), 1440  , ' 24', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
   
   CGTEXT, MAX(date_time), 720  , ' 12', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
   
   CGTEXT, MAX(date_time), 240  , '  4', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
   
	CGTEXT, MAX(date_time), 60  , '  1', $
   COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65    
;##################################################

	x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='gray', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='gray', /FILL


        CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
                         COLOR='white', $
                         XSTYLE=1,$ 
                         XMINOR=8,$
                         XTICKS=file_number,$
                         ;xTITLE = 'Time [days]',$ 
                         CHARSIZE = 1.4, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5,$
                         XTICKFORMAT='(A1)'
                                           
        CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)    
                         COLOR='white', $
                         XSTYLE=1,$
                         XTICKS=file_number,$
                         XMINOR=8,$
                         XTICKFORMAT=['LABEL_DATE'],$
                         XTICKUNITS=['day']                         

        cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
                         YTITLE = '', $
                         ystyle=1,$  
                         COLOR='black', $                
                         /ylog,$
                         CHARSIZE = 1.65,$
                         CHARTHICK=1.5

                        
        cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
                         /ylog,$                          
                         COLOR='black', $
                         YTICKFORMAT='(A1)',$ 
                         ystyle=5, $
                         CHARSIZE = 1.4,$
                         CHARTHICK=1.5
                                            
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT LAYER

cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .92, .92],$
color = 'white',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
;print, n_elements(period2), n_elements(date_time)
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT TICK LAYERS         

    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='white', $                
    /ylog,$
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='white', $
    YTICKFORMAT='(A1)',$ 
    ystyle=5, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5
;###############################################################################   
;###############################################################################
;###############################################################################
;###############################################################################                          
;###############################################################################   
   class = gms_class(station_code)
   info = stationlist(class, station_code)
   title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
   FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, ", ", "magnetic lon: ", F7.2," ", A)')
  
  
  x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.93   
   XYOUTS, X, y, title, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65     

   x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.18   

   xtitle = 'Time [UT h]'

   XYOUTS, X, y, xtitle, /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4  

;###############################################################################
;###############################################################################

   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.025   
   CGTEXT, x, y,'Freq [Hz]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90

   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.985   
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90
   print, '/min power: ', min(power), '     max power: ',max(power)

;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END 


PRO make_psfig2, ddyn, period, coi, date_i, date_f, path, station_code	
        @set_up_commons
        set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])  

;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
	
    psfile =  path+'xwt_'+Date+'.scaled.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

 cgLOADCT,40

date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

period2 = FIX(ALOG(period)/ALOG(2))
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
index = (where((period GE 60) and (period LT 2880 )))

print, 'min xwt: ', min(ddyn)
print, 'max xwt: ', max(ddyn)

minPower = min(ddyn)
maxPower = max(ddyn)

nLevels = 48
levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower


; Generate tick names based on levels
      
    cgCONTOUR,ddyn,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.1, .36, .92, .92],$
	YSTYLE=5,C_COLORS=colors, XMINOR=8,	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1;,  xTITLE = 'Time [days]'

    nColors = !D.TABLE_SIZE

    title = Textoidl('crosswavelet [nT]')
    tickNames = STRING(levels, FORMAT='(F12.2)')
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title


    ;###############################################################################
    ;###############################################################################
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

	CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 720, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 240, PSYM=8, COLOR='red', thick=4
  	CGPLOTS, max(date_time), 60, PSYM=8, COLOR='red', thick=4

    CGTEXT, MAX(date_time), 2880  , ' 48',$
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65;, ORIENTATION=90   

    CGTEXT, MAX(date_time), 1440  , ' 24', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 720  , ' 12', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 240  , '  4', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
        CGTEXT, MAX(date_time), 60  , '  1', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65    
    ;##################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .92, .92],$
    color = 'black',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
    ;print, n_elements(period2), n_elements(date_time)
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]
    
    cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='gray', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='gray', /FILL
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT TICK LAYERS         
    
    freq_series = 1/(period*60)
   ; print, n_elements(freq_series), n_elements(period2)
    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='black', $                
    /ylog,$
    ;YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='black', $
    YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5
    ;###############################################################################   
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################                          
    ;###############################################################################   
    class = gms_class(station_code)
    info = stationlist(class, station_code)
    title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
    FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, ", ", "magnetic lon: ", F7.2," ", A)')


    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.93   
    XYOUTS, X, y, title, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65     

    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.18   

    xtitle = 'Time [UT h]'

    XYOUTS, X, y, xtitle, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4  

    ;###############################################################################
    ;###############################################################################

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.025   
    CGTEXT, x, y,'Freq [Hz]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.985   
    CGTEXT, x, y,'Period [h]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90
;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END 


PRO make_psfig3, ddyn, period, coi, date_i, date_f, path, station_code	
        @set_up_commons
        set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    TGM_n = event_case([yr_i,mh_i,dy_i])  

;############################################################################### 
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
	X_label = xlabel([yr_i, mh_i, dy_i], file_number)
	
    psfile =  path+'ddyn_'+Date+'.scaled.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
 ;###############################################################################  

    cgLOADCT,40

    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
                        FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Minutes')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					

    period2 = FIX(ALOG(period)/ALOG(2))
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels


    index = (where((period GE 60) and (period LT 2880 )))


    print, 'min ddyn: ', min(ddyn)
    print, 'max ddyn: ', max(ddyn)

    minPower = min(ddyn)
    maxPower = max(ddyn)

    ddyn = (maxPower - ddyn) / (maxPower - minPower)


    minPower = 0
    maxPower = 1

    nLevels = 48

    levels = FINDGEN(nLevels) / (nLevels)
       ; print, levels

; Generate tick names based on levels
      
    cgCONTOUR,ddyn,date_time,period, $
	XSTYLE=1,YTITLE='', title='', POSITION=[.1, .36, .92, .92],$
	YSTYLE=5,C_COLORS=colors, XMINOR=8,	/YTYPE, LEVELS=levels, yrange=[30,4000], NLEVELS=nLevels,/FILL, $
	XTICKFORMAT=['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS=['day', 'month'], XTICKLAYOUT = 2,  $
	XTICKINTERVAL = 1;,  xTITLE = 'Time [days]'

    nColors = !D.TABLE_SIZE

    title = Textoidl('Semblance [nT]')
    tick_vals = [0, 0.2, 0.4, 0.6, 0.8, 1]
    tickNames = STRING(tick_vals, FORMAT='(F6.1)')

    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.15, 0.12, 0.9, 0.14], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title


    ;###############################################################################
    ;###############################################################################
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill

	CGPLOTS, max(date_time), 2880, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 1440, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 720, PSYM=8, COLOR='red', thick=4
	CGPLOTS, max(date_time), 240, PSYM=8, COLOR='red', thick=4
  	CGPLOTS, max(date_time), 60, PSYM=8, COLOR='red', thick=4

    CGTEXT, MAX(date_time), 2880  , ' 48',$
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65;, ORIENTATION=90   

    CGTEXT, MAX(date_time), 1440  , ' 24', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 720  , ' 12', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
    CGTEXT, MAX(date_time), 240  , '  4', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65
    
        CGTEXT, MAX(date_time), 60  , '  1', $
    COLOR='black', ALIGNMENT=0.0, CHARSIZE=1.65    
    ;##################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.1, .36, .92, .92],$
    color = 'black',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
    ;print, n_elements(period2), n_elements(date_time)
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='gray', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='gray', /FILL
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT TICK LAYERS         
    
    freq_series = 1/(period*60)
    cgAxis,YAxis=0, yrange=[max(freq_series),min(freq_series)], $
    YTITLE = '', $
    ystyle=1,$  
    COLOR='white', $                
    /ylog,$
    ;YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5

    cgAxis, YAxis=1, yrange=[max(freq_series),min(freq_series)], $
    /ylog,$                          
    COLOR='white', $
    YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.5
    ;###############################################################################   
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################                          
    ;###############################################################################   
    class = gms_class(station_code)
    info = stationlist(class, station_code)
    title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
    FORMAT='(A, ", magnetic lat: ", F7.2, " ", A, ", ", "magnetic lon: ", F7.2," ", A)')


    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.93   
    XYOUTS, X, y, title, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.65     

    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.18   

    xtitle = 'Time [UT h]'

    XYOUTS, X, y, xtitle, /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4  

    ;###############################################################################
    ;###############################################################################

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.025   
    CGTEXT, x, y,'Freq [Hz]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
    x = 0.985   
    CGTEXT, x, y,'Period [h]', /NORMAL, $
    COLOR=negro, ALIGNMENT=0.5, CHARSIZE=1.4, ORIENTATION=90
;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 ;, /PNG  

    RETURN  
END