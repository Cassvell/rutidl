
PRO make_psfig_composed, asymH, diono, H, H_sq, SQ, power, xwt, ddyn, period, coi, date_i, date_f, station_code	
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
	
    psfile =  '/home/isaac/rutidl/output/wavelet/'+station_code+'/'+station_code+'_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=7

   
;###############################################################################               
;###############################################################################  

    cgLOADCT,40

    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,1), $
                        FINAL=JULDAY(mh_f, dy_f, yr_f, 24,0), UNITS='Hours')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])					
;################################################################################
;################################################################################
;################################################################################
;################################################################################
; panel a

nLevels = 36
    minPower =  min(real_part(SQ))
    maxPower =  max(real_part(SQ))
    levels = FINDGEN(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower

    CGCONTOUR,SQ,date_time,period, XSTYLE=5,YTITLE='', title='', POSITION=[.07, .66, .77, .92],$
	YSTYLE=5,C_COLORS=colors, yrange=[480,2880],XMINOR=8,YTICKFORMAT='exponent',$ 
	/YTYPE, LEVELS=levels, NLEVELS=nLevels,/FILL, $
	XTICKFORMAT='(A1)', XTICKUNITS=['day', 'month'], XTICKLAYOUT = 0,  $
	XTICKINTERVAL = 1, /noerase ;,  xTITLE = 'Time [days]'


    nColors = !D.TABLE_SIZE

    title = Textoidl('Amplitude [nT h]')
    tickNames = STRING(levels, FORMAT='(F8.1)')
    
    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.66, 0.87, 0.92, 0.89], TICKNAMES=tickNames, RANGE=[minPower, maxPower], $
    Charsize= 1.0,  title=title, vertical=1, right=1 ; Moves title and labels to the right

 
    
    ; Create a half rectangle (right half)
    x_coords = [0, 2, 2, 0, 0]  ; X coordinates: left, right, right, left, left
    y_coords = [0, 0, 0.2, 0.2, 0]  ; Y coordinates: bottom, bottom, top, top, bottom

    ; Register the custom symbol
    usersym, x_coords, y_coords, /fill


    CGPLOTS, min(date_time), 1440, PSYM=8, COLOR='black', thick=4
    CGPLOTS, min(date_time), 720, PSYM=8, COLOR='black', thick=4

    ; Create a half rectangle (right half)
    x_coords = [0, 1, 1, 0, 0]  ; X coordinates: left, right, right, left, left
    y_coords = [0, 0, 0.2, 0.2, 0]  ; Y coordinates: bottom, bottom, top, top, bottom

    ; Register the custom symbol
    usersym, x_coords, y_coords, /fill        

    CGPLOTS, min(date_time), 2520, PSYM=8, COLOR='black', thick=1
    CGPLOTS, min(date_time), 2160, PSYM=8, COLOR='black', thick=1
    CGPLOTS, min(date_time), 1800, PSYM=8, COLOR='black', thick=1
    CGPLOTS, min(date_time), 1080, PSYM=8, COLOR='black', thick=1    

    CGTEXT, min(date_time), 1380  , '24 ', $
    COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2

    CGTEXT, min(date_time), 680  , '12 ', $
    COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2
;##################################################
    
        CGAXIS, XAXIS = 0, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $                       
                         COLOR='black', $
                         XSTYLE=1,$ 
                         XMINOR=8,$
                         XTICKS=file_number,$
                         ;xTITLE = 'Time [days]',$ 
                         CHARSIZE = 1.2, $
                         TICKLEN=0.04,$
                         CHARTHICK=1.5,$
                         XTICKFORMAT='(A1)'
                                           
        CGAXIS, XAXIS = 1, XRANGE=[date_time[0],date_time[N_ELEMENTS(date_time)-1]], $;.0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)    
                         COLOR='black', $
                         XSTYLE=1,$
                         XTICKS=file_number,$
                         XMINOR=8,$
                         XTICKFORMAT='(A1)',$
                         XTICKUNITS=['day']            

        cgAxis,YAxis=0,  yrange=[480, 2880], $
            ;YTITLE = 'Period [Hr]', $

            
            YTICKFORMAT='(A1)',$ 
            ystyle=5,$  
            COLOR='black', $
            CHARSIZE = 1.2,$
            CHARTHICK=1.5


    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.03
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90

    y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.845
   
   sqlabel = Textoidl('H_{SQ} & D_I [nT]')
   CGTEXT, x, y,sqlabel, /NORMAL, $
   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90   
;###############################################################################               
;###############################################################################
;################################################################################ 
;################################################################################
    ; OVERPLOT LAYER

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[0.66, 0.87, 0.92, 0.89],$
    color = 'black',  xstyle=1, ystyle=1, /overplot,  axiscolor='white', /nodata
    ;print, n_elements(period2), n_elements(date_time)

	x = [date_time[0],date_time,MAX(date_time)]
	y = [MAX(period),coi,MAX(period)]

	cgPolygon,x,y,ORIEN=+45,SPACING=0.5,NOCLIP=0, LINESTYLE=0, FCOLOR='black', /FILL
	cgPolygon,x,y,ORIEN=-45,SPACING=0.5,NOCLIP=0, LINESTYLE=0,FCOLOR='black', /FILL
;###############################################################################               
;###############################################################################
    cgplot, date_time, H_sq, POSITION=[.07, .66, .77, .92],$
    color = 'black',  xstyle=5, ystyle=5, /nodata, /noerase, yrange=[-75, 150]

    cgoplot, date_time, H_sq, color='black', thick=3, linestyle=0
    cgoplot, date_time, diono, color='hot pink', thick=4, linestyle=0    

    cgAxis, YAxis=1, yrange=[-75, 150], $             
    COLOR='black', $
    ;YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.2,$
    CHARTHICK=1.5    

   ; cgtext, 0.68, 0.69, '(a)', color='black', /normal, TT_FONT='Helvetica Bold', charsize = 2       
;################################################################################ 
;################################################################################
   
; Define the levels and colors used in CGCONTOUR
          ; Data range for the colorbar

; Define the range of the power series and compute levels
minPower =  min((power))
maxPower =  max((power))
    period2 = FIX(ALOG(period)/ALOG(2))
print, maxPower,   minPower
    
    nLevels = 36

    levels_arr = FINDGEN(nLevels)
    levels = levels_arr * ((maxPower - minPower) / (nLevels - 1.0)) + minPower

; Generate tick names based on levels

; ###############################################################################
; ###############################################################################
; panel b
    CGCONTOUR,power,date_time,period, XSTYLE=1,YTITLE='', title='', POSITION=[.07, .38, .77, 0.64],$
	YSTYLE=5,C_COLORS=colors, yrange=[480,2880], XMINOR=8,YTICKFORMAT='exponent',$ 
	/YTYPE, LEVELS=levels, NLEVELS=nLevels,/FILL, $
	XTICKFORMAT='(A1)', XTICKUNITS=['day', 'month'], XTICKLAYOUT = 0,  $
	XTICKINTERVAL = 1, /noerase ;,  xTITLE = 'Time [days]'

    nColors = !D.TABLE_SIZE

    title = Textoidl('Energy Density [nT^2 h^{2}]')
    
    tick_indices = [0, nLevels/4, nLevels/2, 3*nLevels/4, nLevels-1]
    tickValues = levels[tick_indices]

    tickNames = STRING(tickValues, FORMAT='(F8.1)')

    cgCOLORBAR, NCOLORS=nColors, POSITION=[0.38, 0.87, 0.64, 0.89], TICKNAMES=tickNames, RANGE=[0, maxPower], $
    divisions=n_elements(tickValues)-1,  Charsize= 1.0,  title=title, vertical=1, right=1 

   ; cgtext, 0.6775, 0.418, '(b)', color='black', /normal, TT_FONT='Helvetica Bold', charsize = 2.3
    ;cgtext, 0.68, 0.42, '(b)', color='white', /normal, TT_FONT='Helvetica Bold', charsize = 2

    ; Create a half rectangle (right half)
    x_coords = [0, 2, 2, 0, 0]  ; X coordinates: left, right, right, left, left
    y_coords = [0, 0, 0.2, 0.2, 0]  ; Y coordinates: bottom, bottom, top, top, bottom

    ; Register the custom symbol
    usersym, x_coords, y_coords, /fill


    CGPLOTS, min(date_time), 1440, PSYM=8, COLOR='white', thick=4
    CGPLOTS, min(date_time), 720, PSYM=8, COLOR='white', thick=4

    ; Create a half rectangle (right half)
    x_coords = [0, 1, 1, 0, 0]  ; X coordinates: left, right, right, left, left
    y_coords = [0, 0, 0.2, 0.2, 0]  ; Y coordinates: bottom, bottom, top, top, bottom

    ; Register the custom symbol
    usersym, x_coords, y_coords, /fill        

    CGPLOTS, min(date_time), 2520, PSYM=8, COLOR='white', thick=1
    CGPLOTS, min(date_time), 2160, PSYM=8, COLOR='white', thick=1
    CGPLOTS, min(date_time), 1800, PSYM=8, COLOR='white', thick=1
    CGPLOTS, min(date_time), 1080, PSYM=8, COLOR='white', thick=1    

    CGTEXT, min(date_time), 1380  , '24 ', $
    COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2

    CGTEXT, min(date_time), 680  , '12 ', $
    COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2
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
                         CHARSIZE = 1.2, $
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

        cgAxis,YAxis=0,  yrange=[480,2880], $
            YTITLE = 'Freq [Hz]', $
            ystyle=5,$  
            COLOR='black', $                
            /ylog,$
            CHARSIZE = 1.2,$
            CHARTHICK=1.5


            cgAxis, YAxis=1, yrange=[480,2880], $
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

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.07, .38, .77, 0.64],$
    color = 'white',  xstyle=5, ystyle=5, /overplot,  axiscolor='white', /nodata
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ;###############################################################################
    ; OVERPLOT TICK LAYERS         

    cgAxis,YAxis=0, yrange=[480,2880], $
    YTITLE = '', $
    ystyle=5,$  
    COLOR='white', $                
    /ylog,$
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
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
   x = 0.03
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90

   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.845
   
   d_h = Textoidl('H_{D} [nT]') 
   
   CGTEXT, x, y,d_H, /NORMAL, $
   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90
;###############################################################################   
;###############################################################################
;###############################################################################
;###############################################################################   
asymHmin = min(asymH)
asymHmax = max(asymH)

cgplot, date_time, H, POSITION=[.07, .38, .77, 0.64],$
color = 'black',  xstyle=5, ystyle=5, /nodata, /noerase, yrange=[min(H),max(H)]
 
cgoplot, date_time, H, color='orange', thick=5, linestyle=3

   cgAxis, YAxis=1, yrange=[min(H),max(H)], $                    
    COLOR='black', $
    ;YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.2,$
    CHARTHICK=0.05,$
    ticklen = 0.0

cgplot, findgen(n_elements(asymH)), asymH, POSITION=[.07, .38, .77, 0.64],$
color = 'black',  xstyle=5, ystyle=5, /nodata, /noerase, yrange=[min(H),max(H)]

   cgAxis, YAxis=1, yrange=[min(H),max(H)], $                    
    COLOR='white', $
    YTICKFORMAT='(A1)',$ 
    ystyle=1, $
    CHARSIZE = 1.2,$
    CHARTHICK=2.5

;###############################################################################   
;###############################################################################   
;###############################################################################
;###############################################################################
;###############################################################################                          
;###############################################################################
;panel c


ddyn = ddyn * (-1) ; invert the scale of Ddyn
minPower = min(ddyn)
maxPower = max(ddyn)

ddyn = (ddyn - minPower) / (maxPower - minPower) ; Normalize to [0, 1]

nLevels = 36
; Levels from -1 to 1

;for i = 0, n_elements(ddyn)-1 do begin
;  if ddyn[i] LT 0 then ddyn[i] = 0
;endfor


minPower = min(ddyn)
maxPower = max(ddyn)

  step = (maxPower - minPower) / (nLevels - 1.0)  ; Floating-point division
  levels = minPower + FINDGEN(nLevels) * step  ; High precision


cgCONTOUR, ddyn, date_time, period, $
    XSTYLE=1, YSTYLE=5, $
    YTITLE='Period [min]', $
    POSITION=[0.07, 0.1, 0.77, 0.36], C_COLORS=colors, LEVELS=levels, YRANGE=[480, 2880], /FILL, /YTYPE, $
    XTICKFORMAT=['LABEL_DATE'], XTICKUNITS=['day'], XTICKLAYOUT=1, XTICKINTERVAL=1, $
    C_LABELS=levels, c_charsize = 1.0,$  ; Show labels for all levels  
    CHARSIZE=1.2, /NOERASE

nColors = !D.TABLE_SIZE

title = Textoidl('DDEF correlation')
tick_indices = [0, nLevels/4, nLevels/2, 3*nLevels/4, nLevels-1]
tickValues = levels[tick_indices]
tickNames = STRING(tickValues, FORMAT='(F4.1)')

cgCOLORBAR, NCOLORS=nColors, POSITION=[0.1, 0.87, 0.36, 0.89], $
RANGE=[minPower, maxPower], Charsize= 1.0,  title=title, vertical=1, right=1 ;

;###############################################################################
;###############################################################################
    ; Create a half rectangle (right half)
    x_coords = [0, 2, 2, 0, 0]  ; X coordinates: left, right, right, left, left
    y_coords = [0, 0, 0.2, 0.2, 0]  ; Y coordinates: bottom, bottom, top, top, bottom

    ; Register the custom symbol
    usersym, x_coords, y_coords, /fill


    CGPLOTS, min(date_time), 1440, PSYM=8, COLOR='white', thick=4
    CGPLOTS, min(date_time), 720, PSYM=8, COLOR='white', thick=4

    ; Create a half rectangle (right half)
    x_coords = [0, 1, 1, 0, 0]  ; X coordinates: left, right, right, left, left
    y_coords = [0, 0, 0.2, 0.2, 0]  ; Y coordinates: bottom, bottom, top, top, bottom

    ; Register the custom symbol
    usersym, x_coords, y_coords, /fill        

    CGPLOTS, min(date_time), 2520, PSYM=8, COLOR='white', thick=1
    CGPLOTS, min(date_time), 2160, PSYM=8, COLOR='white', thick=1
    CGPLOTS, min(date_time), 1800, PSYM=8, COLOR='white', thick=1
    CGPLOTS, min(date_time), 1080, PSYM=8, COLOR='white', thick=1    

    CGTEXT, min(date_time), 1380  , '24 ', $
    COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2

    CGTEXT, min(date_time), 680  , '12 ', $
    COLOR='black', ALIGNMENT=1.0, CHARSIZE=1.2

;##################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT LAYER

cgplot, date_time, H, POSITION=[.07, .1, .77, .36],$
color = 'black',  xstyle=5, ystyle=5, /overplot,  axiscolor='white', /nodata, /noerase

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

    cgAxis,YAxis=0,  yrange=[480,2880], $
    YTITLE = 'Freq [Hz]', $
    ystyle=5,$  
    COLOR='black', $                
    /ylog,$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5


    cgAxis, YAxis=1, yrange=[480,2880], $
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

    cgplot, date_time, findgen(n_elements(date_time)), POSITION=[.07, .36, .77, .92],$
    color = 'white',  xstyle=1, ystyle=5, /overplot,  axiscolor='white', /nodata
;print, n_elements(period2), n_elements(date_time)
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
; OVERPLOT TICK LAYERS         

    cgAxis,YAxis=0, yrange=[480,2880], $
    YTITLE = '', $
    ystyle=5,$  
    COLOR='white', $                
    /ylog,$
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2,$
    CHARTHICK=1.5
;###############################################################################   
;###############################################################################
;###############################################################################

cgplot, findgen(n_elements(asymH)), asymH, POSITION=[0.07, 0.1, 0.77, 0.36], yrange=[asymHmin, asymHmax],$
color = 'black',  xstyle=5, ystyle=6, /nodata, /noerase
 
cgoplot, findgen(n_elements(asymH)), asymH, color='yellow', thick=5, linestyle=3


cgAxis, YAxis=1, yrange=[asymHmin, asymHmax], $                       
COLOR='white', $
ystyle=1, $
CHARSIZE = 1.2,$
CHARTHICK=1.5


cgAxis, YAxis=1, yrange=[asymHmin, asymHmax], $                       
COLOR='black', $
ystyle=1, $
CHARSIZE = 1.2,$
TIcklen = 0.0, $
CHARTHICK=0.01

cgplot, date_time, H, POSITION=[0.07, 0.1, 0.77, 0.36], yrange=[asymHmin, asymHmax],$
color = 'black',  xstyle=5, ystyle=6, /nodata, /noerase

cgAxis, YAxis=1, yrange=[asymHmin, asymHmax], $                       
COLOR='white', $
ystyle=1, $
YTICKFORMAT='(A1)',$
CHARSIZE = 1.2,$
CHARTHICK=1.5


;cgtext, 0.68, 0.13, '(c)', color='yellow', /normal, TT_FONT='Helvetica Bold', charsize = 2
;###############################################################################                          
;###############################################################################   

   class = gms_class(station_code)
   info = stationlist(class, station_code)
   title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
   FORMAT='(A, ", mlat: ", F7.2, " ", A, ", ", "mlon: ", F7.2," ", A)')
  
  
  x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.93   
   XYOUTS, X, y, title, /NORMAL, $
   ALIGNMENT=0.5, CHARSIZE=1.65     

   x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
   y = 0.03   
    

   month = month_name(mh_i, 'english')
   xtitle = Textoidl('Universal Time [days], ' + month + ' ' + STRING(yr_i, FORMAT='(I04)'))

   XYOUTS, X, y, xtitle, /NORMAL, $
   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2  


;###############################################################################
;###############################################################################
   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.03
   CGTEXT, x, y,'Period [h]', /NORMAL, $
   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90


   y = (!Y.Window[1] - !Y.Window[0]) /  2. + !Y.Window[0]
   x = 0.845
   CGTEXT, x, y,'AE [nT]', /NORMAL, $
   COLOR='black', ALIGNMENT=0.5, CHARSIZE=1.2, ORIENTATION=90   
;###############################################################################
;###############################################################################
;###############################################################################    
;spawn, 'evice psfile'
    cgPS_Close, density = 300, width = 1600 , /PNG  

    RETURN  
END 

