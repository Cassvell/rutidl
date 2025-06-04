

pro ts_plots, asymH, symH, H, SQ, Bdiono, date_i, date_f, path, station_code
    On_error, 2
    COMPILE_OPT idl2, HIDDEN
    yr_i	= date_i[0]
    mh_i	= date_i[1]
    dy_i 	= date_i[2]	

    yr_f	= date_f[0]
    mh_f	= date_f[1]
    dy_f 	= date_f[2]
    ;###############################################################################
    @set_up_commons
    set_up	

    date_time = TIMEGEN(START=JULDAY(03, 16, 2015, 0,0), $
    FINAL=JULDAY(mh_f, 26, yr_f, 23,59), UNITS='Minutes')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])	
    Date    = STRING(2015, 03, 16, 2015, 03, 26, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
    file_number    = (JULDAY(03, 26, 2015) - JULDAY(03, 16, 2015))+1
    psfile =  path+station_code+'_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=4

    class = gms_class(station_code)
    info = stationlist(class, station_code)

    print, 'UTC: ', info.utc
    ;DEVICE, true=24, retain=2, decomposed=0
    ;TVLCT, R_bak, G_bak, B_bak, /GET     
    ;LOADCT, 39
    ;WINDOW, 1, XSIZE=800, YSIZE=500, TITLE='GS'

    down = -170
    up = 100
    cgplot, date_time, Bdiono, background='white', color='black', position=[.1, .16, .9, .90], XTICKFORMAT=['LABEL_DATE'], $
    xminor=8,XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, $
    yrange=[down,up], /nodata

    ndata = n_elements(symH)-1
    
    ;mlt = mlt(station_code, date_time)
    jul_conv = abs((0.1/2.4)*info.utc)

    ;print, 'magnetic local time zone: ', -mlt[0]
    print, jul_conv
    if info.utc LT 0 then begin
        
        local_ini = date_time[0] + (jul_conv)
        local_fin = date_time[n_elements(date_time) - 1] + jul_conv
    endif else begin 
        local_ini = date_time[0] - jul_conv
        local_fin = date_time[n_elements(date_time) - 1] - jul_conv
    endelse    

    local_time = TIMEGEN(START=local_ini, FINAL=local_fin, UNITS='Minutes')


    midday = fltarr(n_elements(local_time)/720)

    midddays = n_elements(symH)/720

    if info.utc LT 0 then begin
        if local_time[(0*720)]-0.25 GE date_time[0] then begin
        cgPolygon, [local_time[(0*720)]-0.25, local_time[((1)*720)]-0.25, local_time[((1)*720)]-0.25, local_time[(0*720)]-0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
        endif else begin
            cgPolygon, [date_time[0], local_time[((1)*720)]-0.25, local_time[((1)*720)]-0.25, date_time[0]], $
            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
        endelse

    if date_time[ndata]-(local_time[(midddays*719)]-0.25) LE 0.5 and date_time[ndata] - (local_time[(midddays*719)]-0.25)GE 0 then begin
        cgPolygon, [local_time[(midddays*719)]-0.25, date_time[ndata], date_time[ndata], local_time[(midddays*719)]-0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill

    endif 


endif else begin
    if local_time[(0*720)]-0.25 GE date_time[0] then begin
        cgPolygon, [local_time[(0*720)]+0.25, local_time[((1)*720)]+0.25, local_time[((1)*720)]+0.25, local_time[(0*720)]+0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
        endif else begin
            cgPolygon, [date_time[0], local_time[((1)*720)]+0.25, local_time[((1)*720)]+0.25, date_time[0]], $
            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
        endelse    


    if date_time[ndata]-(local_time[(midddays*719)]-0.25) LE 0 then begin
        cgPolygon, [local_time[((midddays-1)*720)]+0.25, date_time[ndata], date_time[ndata], local_time[((midddays-1)*720)]+0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
    endif else begin
        cgPolygon, [local_time[((midddays-1)*720)]+0.25, date_time[ndata], date_time[ndata], local_time[((midddays-1)*720)]+0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
    endelse  
endelse

    
    for i = 0, n_elements(midday)-1 do begin
        
        ; Define color based on even/odd index
        if info.utc LT 0 then begin
                if (i mod 2) eq 0 then color_shade = 'white' else color_shade = 'light gray'

                if i LT n_elements(midday)-1 and i GT 0 then begin
            
                    cgPolygon, [local_time[(i*720)]+0.25, local_time[((i+1)*720)]+0.25, local_time[((i+1)*720)]+0.25, local_time[(i*720)]+0.25], $
                            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = color_shade, /fill
                        
                endif               
        endif else begin
                if (i mod 2) eq 0 then color_shade = 'light gray' else color_shade = 'white'
                if i LT n_elements(midday)-1 and i GT 0 then begin

                    cgPolygon, [local_time[(i*720)]-0.25, local_time[((i+1)*720)]-0.25, local_time[((i+1)*720)]-0.25, local_time[(i*720)]-0.25], $
                            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = color_shade, /fill
                    
                endif 
        endelse    

       
    endfor


    cgoplot,date_time, Bdiono, color='black', thick=3, linestyle=0 

    cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
    xtitle='',$
    xstyle=1,$
    xminor=8,$
    XTICKUNITS=['day'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 1, $       
    XTICKFORMAT='(A1)',$
   ; COLOR=negro, $
    CHARSIZE = 1.0 , $
    TICKLEN=0.04,$
    CHARTHICK=3.5 
    
    cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
    xminor=8,$
    XTICKUNITS=['day'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 1, $      
    XTICKFORMAT='(A1)',$
; COLOR=negro, $
    CHARSIZE =1.0, $
    CHARTHICK=1.5,$
    TICKLEN=0.04

    h_sq = Textoidl('H_{I}')

    AXIS, YAXIS = 0, yrange=[down,up],$
    YTITLE = h_sq+' [nT]', $                          
    YSTYLE=1, $
    CHARSIZE = 1.2,$
    CHARTHICK=1.6 

    cgAXIS, YAXIS = 1,  yrange=[down,up],$
; COLOR=negro, $                                                                      
    YSTYLE=5, $       
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2 ,$
    CHARTHICK=1.6    

;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################

    down = -30
    up = 70

    cgplot, date_time, SQ, background='white', color='black', position=[.1, .16, .9, .9], XTICKFORMAT=['LABEL_DATE'], $
    xminor=8,XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, $
    yrange=[down,up], /nodata,/noerase


    cgOPlot, date_time, SQ, color=' BLK5', thick=2

    

    ;XYOUTS, 0.822, 0.657 , /NORMAL, 'ASYM-H', CHARSIZE = 1.2, CHARTHICK=chr_thick1    

    ;XYOUTS, 0.822, 0.615 , /NORMAL, 'SYM-H', CHARSIZE = 1.2, CHARTHICK=chr_thick1                 
            
    ;XYOUTS, 0.822, 0.573 , /NORMAL, d_H, CHARSIZE = 1.2, CHARTHICK=chr_thick1  

    cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
    xtitle='',$
    xstyle=1,$
    xminor=8,$
    XTICKUNITS=['day'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 1, $       
    XTICKFORMAT=['LABEL_DATE'],$
   ; COLOR=negro, $
    CHARSIZE = 1.2 , $
    TICKLEN=0.04,$
    CHARTHICK=3.5 
    
    cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
    xminor=8,$
    XTICKUNITS=['day'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 1, $      
    XTICKFORMAT='(A1)',$
; COLOR=negro, $
    CHARSIZE =1.2, $
    CHARTHICK=1.5,$
    TICKLEN=0.04

    h_i = Textoidl('H_{SQ}')
    cgAXIS, YAXIS = 0, yrange=[down,up],$
          
    ;COLOR=negro, $
    YSTYLE=5, $
    CHARSIZE = 1.2,$
    CHARTHICK=1.6 

    cgAXIS, YAXIS = 1,  yrange=[down,up],$
; COLOR=negro, $      
    YTITLE = h_i+' [nT]', $                                                                                    
    YSTYLE=1, $       
    ;YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2 ,$
    CHARTHICK=1.6    

;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################  
;##################################################################################################################
;##################################################################################################################  
    title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
    FORMAT='("Obs: ", A, ", mlat: ", F7.2, " ", A, ",    ", "mlon: ", F7.2," ", A)')
   
   
    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.93   
    XYOUTS, X, y, title, /NORMAL, $
    ALIGNMENT=0.5, CHARSIZE=1.65     
 
    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.02   

    cgPS_Close, density = 300, width = 1600 ;, /PNG  
end


pro ip_plots, symH, Q, P, V, T, E, Bz, Bt, asymH, date_i, date_f, path
    On_error, 2
    COMPILE_OPT idl2, HIDDEN
    yr_i	= date_i[0]
    mh_i	= date_i[1]
    dy_i 	= date_i[2]	

    yr_f	= date_f[0]
    mh_f	= date_f[1]
    dy_f 	= date_f[2]
    ;###############################################################################
    @set_up_commons
    set_up	

    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), $
    FINAL=JULDAY(mh_f, dy_f, yr_f, 23,59), UNITS='Minutes')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])	
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    psfile =  path+'ip_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=10
    ;DEVICE, true=24, retain=2, decomposed=0
    ;TVLCT, R_bak, G_bak, B_bak, /GET     
    ;LOADCT, 39
    ;WINDOW, 1, XSIZE=800, YSIZE=500, TITLE='GS'

    if min(Bz) LT min(Bt) then down = min(Bz) else down = min(Bt)
    if max(Bz) GT max(Bt) then up = max(Bz) else up = max(Bt)
    cgplot, date_time, Bz, background='white', color='black', position=[.1, .76, .92, .98], XTICKFORMAT=['LABEL_DATE'], $
    xminor=8,XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, yrange=[down,up];, /nodata

    cgtext, 0.12, 0.95, '(a)', charthick=3, charsize=2, /normal, font = 1, TT_FONT='Helvetica Bold'

    cgOPlot, date_time, Bz, color='red', thick=3, linestyle=0   
    cgoplot,date_time, Bt, color='black', thick=2, linestyle=0   

    cgOPlot, [date_time[3150], date_time[3150]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
    linestyle=2, thick=2, color='black' ;IP shock

    cgOPlot, [date_time[3600], date_time[3600]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
    linestyle=2, thick=2, color='black' ;IP shock

    cgOPlot, [date_time[4320], date_time[4320]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
    linestyle=2, thick=2, color='black' ;IP shock

    cgOPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0.], LINESTYLE=1, THICK=2,COLOR='black' 
    ;first panel legend 
    cgPolygon, [0.72,0.75,0.75,0.72], [0.793,0.793, 0.796,0.796], color = 'black', /NORMAL, /FILL    
    cgPolygon, [0.81,0.84,0.84,0.81], [0.793,0.793, 0.796,0.796], color = 'red', /NORMAL , /FILL  
    
    B_z = Textoidl('B_Z')
    B_T = Textoidl('B_T, ')
    XYOUTS, 0.76, 0.79 , /NORMAL, B_T, CHARSIZE = 1.2, CHARTHICK=chr_thick1                 
            
    XYOUTS, 0.852, 0.79 , /NORMAL, B_z, CHARSIZE = 1.2, CHARTHICK=chr_thick1  

    cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
    xtitle='',$
    xstyle=1,$
    xminor=8,$
    XTICKUNITS=['day'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 1, $       
    XTICKFORMAT='(A1)',$
   ; COLOR=negro, $
    CHARSIZE = 1.0 , $
    TICKLEN=0.04,$
    CHARTHICK=3.5 
    
    cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
    xminor=8,$
    XTICKUNITS=['day'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 1, $      
    XTICKFORMAT='(A1)',$
; COLOR=negro, $
    CHARSIZE =1.0, $
    CHARTHICK=1.5,$
    TICKLEN=0.04

    cgAXIS, YAXIS = 0, yrange=[down,up],$
    YTITLE = 'IMF [nT]', $                          
    ;COLOR=negro, $
    YSTYLE=1, $
    CHARSIZE = 1.2,$
    CHARTHICK=1.6 

    cgAXIS, YAXIS = 1,  yrange=[down,up],$
; COLOR=negro, $                                                                      
    YSTYLE=1, $       
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2 ,$
    CHARTHICK=1.6    
        
;##################################################################################################################
;##################################################################################################################

cgplot, date_time, V*(-1), background='white', color='black', position=[.1, .53, .92, .75], XTICKFORMAT=['LABEL_DATE'], $
XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, /noerase, /nodata

cgtext, 0.12, 0.72, '(b)', charthick=3, charsize=2, /normal, font = 1, TT_FONT='Helvetica Bold'


cgoplot,date_time, V*(-1), color='black', thick = 4


cgOPlot, [date_time[3150], date_time[3150]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
linestyle=2, thick=2, color='black' ;IP shock

cgOPlot, [date_time[3600], date_time[3600]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
linestyle=2, thick=2, color='black' ;IP shock

cgOPlot, [date_time[4320], date_time[4320]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
linestyle=2, thick=2, color='black' ;IP shock

cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
xtitle='',$
xstyle=1,$
xminor=8,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $       
XTICKFORMAT='(A1)',$
; COLOR=negro, $
CHARSIZE = 1.0 , $
TICKLEN=0.04,$
CHARTHICK=3.5 

cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
xminor=8,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $      
XTICKFORMAT='(A1)',$
; COLOR=negro, $
CHARSIZE =1.0, $
CHARTHICK=1.5,$
TICKLEN=0.04

V_x = Textoidl('V_X [km s^{-1}]')

cgAXIS, YAXIS = 0, $
YTITLE = V_x, $                          
;COLOR=negro, $
YSTYLE=1, $
CHARSIZE = 1.2,$
CHARTHICK=1.6 

 
;##################################################################################################################
;##################################################################################################################  
;##################################################################################################################

cgplot, date_time, E, background='white', color='black', position=[.1, .53, .92, .75], XTICKFORMAT=['LABEL_DATE'], $
XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, /noerase, /nodata
cgOPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0.], LINESTYLE=1, THICK=2,COLOR='black' 
cgoplot, date_time, E, color='YGB5', thick=1
E_Y = Textoidl('E [mV m^{-1}]')
cgAXIS, YAXIS = 1, $
YTITLE = E_Y, $       
 COLOR='YGB5', $                                                                      
YSTYLE=1, $       
CHARSIZE = 1.2 ,$
CHARTHICK=1.8   
;##################################################################################################################  
;##################################################################################################################
;##################################################################################################################  



cgplot, date_time, T, background='white', color='black', position=[.1, .30, .92, .52], XTICKFORMAT=['LABEL_DATE'], $
XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, /noerase, /nodata

cgtext, 0.12, 0.49, '(c)', charthick=3, charsize=2, /normal, font = 1, TT_FONT='Helvetica Bold'


cgoplot,date_time, T, color='blue', thick = 2

cgOPlot, [date_time[3150], date_time[3150]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
linestyle=2, thick=2, color='black' ;IP shock

cgOPlot, [date_time[3600], date_time[3600]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
linestyle=2, thick=2, color='black' ;IP shock

cgOPlot, [date_time[4320], date_time[4320]], [!Y.CRANGE[0],  !Y.CRANGE[1]], $
linestyle=2, thick=2, color='black' ;IP shock

cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
xtitle='',$
xstyle=1,$
xminor=8,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $       
XTICKFORMAT='(A1)',$
; COLOR=negro, $
CHARSIZE = 1.0 , $
TICKLEN=0.04,$
CHARTHICK=3.5 

cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
xminor=8,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $      
XTICKFORMAT='(A1)',$
; COLOR=negro, $
CHARSIZE =1.0, $
CHARTHICK=1.5,$
TICKLEN=0.04

T_p = Textoidl('T_P')
cgAXIS, YAXIS = 0, yrange=[min(T), max(T)],$
/ylog,$
YTITLE = T_p+' [K]', $                          
COLOR='blue', $
YSTYLE=1, $
CHARSIZE = 1.2,$
CHARTHICK=1.6 

;##################################################################################################################
;################################################################################################################## 
;##################################################################################################################
;################################################################################################################## 

cgplot, date_time, P, background='white', color='black', position=[.1, .3, .92, .52], XTICKFORMAT=['LABEL_DATE'], $
XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, /noerase, /nodata

cgoplot,date_time, P, color='ORG4', thick = 1
proton = Textoidl('n_P [cm^{-1}]')
cgAXIS, YAXIS = 1,  yrange=[min(P), max(P)],$
/ylog,$
YTITLE = proton, $       
 COLOR='ORG4', $                                                                      
YSTYLE=1, $       
;YTICKFORMAT='(A1)',$
CHARSIZE = 1.2 ,$
CHARTHICK=1.8    
;##################################################################################################################
;##################################################################################################################  
;##################################################################################################################
;##################################################################################################################  
;##################################################################################################################
;##################################################################################################################  



cgplot, date_time, symH, background='white', color='black', position=[.1, .07, .92, .29], XTICKFORMAT=['LABEL_DATE'], $
XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, /noerase, /nodata



cgtext, 0.12, 0.1, '(e)', charthick=3, charsize=2, /normal, font = 1, TT_FONT='Helvetica Bold'


for i = 0, n_elements(Q)-1 do begin
    if Q[i] eq 0 then Q[i] = !VALUES.F_NAN
endfor    

cgPolygon, [date_time[3150], date_time[3300], date_time[3300], date_time[3150]], $
                            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill

cgPolygon, [date_time[3300], date_time[3600], date_time[3600], date_time[3300]], $
                            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'gray', /fill

cgPolygon, [date_time[3600], date_time[4260], date_time[4260], date_time[3600]], $
                            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'gray', /fill                            


cgoplot,date_time, symH, color='GRN5', thick = 3


cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
xminor=8,$
xtitle='Universal Time [days], March 2015',$
xstyle=1,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $       
XTICKFORMAT=['LABEL_DATE'],$
; COLOR=negro, $
CHARSIZE = 1.2 , $
TICKLEN=0.04,$
CHARTHICK=3.5 


cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
xminor=8,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $      
XTICKFORMAT='(A1)',$
; COLOR=negro, $
CHARSIZE =1.0, $
CHARTHICK=1.5,$
TICKLEN=0.04

cgAXIS, YAXIS = 0, $
YTITLE = 'SYMH [nT]', $                          
;COLOR=negro, $
YSTYLE=1, $
CHARSIZE = 1.2,$
CHARTHICK=1.6 

cgAXIS, YAXIS = 1, $
; COLOR=negro, $                                                                      
YSTYLE=5, $       
;YTICKFORMAT='(A1)',$
CHARSIZE = 1.2 ,$
CHARTHICK=1.6    
;##################################################################################################################
;##################################################################################################################  
;##################################################################################################################
;##################################################################################################################  
;##################################################################################################################
;##################################################################################################################  

cgplot, date_time, asymH, background='white', color='black', position=[.1, .07, .92, .29], XTICKFORMAT=['LABEL_DATE'], $
XTICKUNITS=['day'], XTICKLAYOUT = 1, XTICKINTERVAL = 1, charsize=1.1, xstyle=5, ystyle=5, /noerase, /nodata



cgtext, 0.12, 0.1, '(e)', charthick=3, charsize=2, /normal, font = 1, TT_FONT='Helvetica Bold'                          

cgOPlot, date_time, asymH, color='orange', thick = 3
cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
xminor=8,$
xtitle='Universal Time [days], March 2015',$
xstyle=1,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $       
XTICKFORMAT=['LABEL_DATE'],$
; COLOR=negro, $
CHARSIZE = 1.2 , $
TICKLEN=0.04,$
CHARTHICK=3.5 


cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
xminor=8,$
XTICKUNITS=['day'], $
XTICKLAYOUT = 0, $
XTICKINTERVAL = 1, $      
XTICKFORMAT='(A1)',$
; COLOR=negro, $
CHARSIZE =1.0, $
CHARTHICK=1.5,$
TICKLEN=0.04

cgAXIS, YAXIS = 0, $
;COLOR=negro, $
YSTYLE=5, $
CHARSIZE = 1.2,$
CHARTHICK=1.6 

cgAXIS, YAXIS = 1, $
YTITLE = 'ASYMH [nT]', $                          
; COLOR=negro, $                                                                      
YSTYLE=1, $       
;YTICKFORMAT='(A1)',$
CHARSIZE = 1.2 ,$
CHARTHICK=1.6    


    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.93   
    XYOUTS, X, y, '', /NORMAL, $
    ALIGNMENT=0.5, CHARSIZE=1.65     
 
    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.02   

    cgPS_Close, density = 300, width = 1600 ;, /PNG  
end