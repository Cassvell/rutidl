pro diono_recons, date_i, date_f, station_code
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


    ; If no station_code is provided or it's an empty string, ask for user input
    IF N_PARAMS() LT 3 THEN BEGIN 
        station_class = ''
        PRINT, 'Enter GMS class code: 0: regmex or 1: intermagnet'
        READ, station_class, PROMPT = '> '

        CASE station_class OF
            '0': station_class = 'regmex'
            '1': station_class = 'intermagnet'
            ELSE: BEGIN
                PRINT, 'Non-available GMS class. Exiting.'
                RETURN
            END
        ENDCASE

        PRINT, 'Enter GMS idx: If you do not know the GMS idx, please run PRO gms_code_table'
        READ, station_idx, PROMPT = '> '

        ;###############################################################################
        ; Assign station code based on selected class
        IF station_class EQ 'regmex' THEN BEGIN    
            station = set_var.gms[FIX(station_idx)]
            station_code = set_var.gms_code[FIX(station_idx)]  
        ENDIF 

        IF station_class EQ 'intermagnet' THEN BEGIN 
            station = set_var.gmsi[FIX(station_idx)] 
            station_code = set_var.gmsi_code[FIX(station_idx)] 
        ENDIF

        PRINT, 'GMS selected: ' + station + ' IAGA code: ' + station_code   
    ENDIF

    
;###############################################################################	
;###############################################################################  
    	
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_n = event_case([yr_i,mh_i,dy_i])  
;###############################################################################   
    date_time = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), $
    FINAL=JULDAY(mh_f, dy_f, yr_f, 23,59), UNITS='Minutes')
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])	  
    Date    = STRING(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, FORMAT='(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
;###############################################################################

    data   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
    H = data.H
    SQ = data.SQ
    idx = sym_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH = idx.symH
    asymH = idx.asyH
    H = add_nan(H, 99999.0, 'equal')  
    H = add_nan(H, 200.0, 'greater')  
    a = ae_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    AE = a.AE
    AL = a.AL
    ;symH0 = fillnan(symH0)
    H = fillnan(H)

    class = gms_class(station_code)
    info = stationlist(class, station_code)
    mlat = info.mlat 

    dionstr = gen_diono(symH, H, 28.06, 'm', '13', station_code, DIG_FILTER = 'dig_filter')
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
    prc   = dionstr.prc
    prc2   = dionstr.prc2
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
    rc = dst_0([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    Q = rc.Q
    
    plot, date_time[3260:4880], dp2[3260:4880], yrange=[min(dp2), max(dp2)], XTICKFORMAT=['LABEL_DATE'], XTICKUNITS=['day'],$
    XTICKLAYOUT = 2, XTICKINTERVAL = 1, xminor=24
    ;oplot, date_time[3260:4320], AL[3260:4320], color=60
    ;oplot, date_time[3260:4320], asymH[3260:4320], color=100
    ;oplot, date_time[3260:4320], H[3260:4320], color=200
    threshold= 23.37
    OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0.], LINESTYLE=1
    OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [threshold,threshold], LINESTYLE=3, thick=2
    OPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [threshold*(-1),threshold*(-1)], LINESTYLE=3, thick=2
    oplot, [date_time[3685], date_time[3685]], [!Y.CRANGE[0],  !Y.CRANGE[1]], linestyle=2
    oplot, [date_time[3950], date_time[3950]], [!Y.CRANGE[0],  !Y.CRANGE[1]], linestyle=2
    oplot, [date_time[3930], date_time[3930]], [!Y.CRANGE[0],  !Y.CRANGE[1]], linestyle=1
    oplot, [date_time[4255], date_time[4255]], [!Y.CRANGE[0],  !Y.CRANGE[1]], linestyle=1
    ;dp2vsQ, diono[3260:4400], asymH[3260:4400], dp2[3260:4400], Q[3260:4400], prc[3260:4400],H[3260:4400],station_code
;###############################################################################
;###############################################################################
;###############################################################################
    path='/home/isaac/longitudinal_studio/fig/diono_recons/'
    psfile = path+station_code+'_'+Date+'.eps'

    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=4
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
   y = 0.85   
   XYOUTS, X, y, title, /NORMAL, $
   ALIGNMENT=0.5, CHARSIZE=2.1, CHARTHICK=1.5               

;###############################################################################       
     d_H = TeXtoIDL('\DeltaH_{' + STRUPCASE(station_code) + '}') 

    up  = 65
    down= -65
    ;IF downddyn LT downdp2 THEN down = downddyn ELSE down=downdp2 
                               
     cgPLOT, date_time, ddyn, XTICKS=file_number, XMINOR=8, BACKGROUND ='white', $
     COLOR='black', CHARSIZE = chr_size1, CHARTHICK=chr_thick1, $
     POSITION=[0.1,0.2,0.9,0.8], XSTYLE = 5, YSTYLE = 5, XTICKFORMAT=['LABEL_DATE'], XTICKUNITS=['day'],$
     XTICKLAYOUT = 1, XTICKINTERVAL = 1, YRANGE=[down,up], /NOERASE, /NODATA

    ;cgPolygon, [date_time[(0)], date_time[(600)], date_time[(600)], date_time[(0)]], $
    ;                        [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill

    ;cgPolygon, [date_time[(19380)], date_time[(20100)], date_time[(20100)], date_time[(19380)]], $
    
    ;                        [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
                            
    jul_conv = abs((0.1/2.4)*info.utc)
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
    
    if info.utc LT 0 then begin
        if local_time[(0*720)]-0.25 GE date_time[0] then begin
        cgPolygon, [local_time[(0*720)]-0.25, local_time[((1)*720)]-0.25, local_time[((1)*720)]-0.25, local_time[(0*720)]-0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
        endif else begin
            cgPolygon, [date_time[0], local_time[((1)*720)]-0.25, local_time[((1)*720)]-0.25, date_time[0]], $
            [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
        endelse

    if date_time[20159]-(local_time[(28*719)]-0.25) LE 0.5 and date_time[20159] - (local_time[(28*719)]-0.25)GE 0 then begin
        cgPolygon, [local_time[(28*719)]-0.25, date_time[20159], date_time[20159], local_time[(28*719)]-0.25], $
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



    if date_time[20159]-(local_time[(28*719)]-0.25) LE 0 then begin
        cgPolygon, [local_time[(27*720)]+0.25, date_time[20159], date_time[20159], local_time[(27*720)]+0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
    endif else begin
        cgPolygon, [local_time[(27*720)]+0.25, local_time[(28*719)]+0.25, local_time[(28*719)]+0.25, local_time[(27*720)]+0.25], $
    [!Y.CRANGE[0], !Y.CRANGE[0], !Y.CRANGE[1], !Y.CRANGE[1]], color = 'light gray', /fill
    endelse  
endelse

    


    print, date_time[20159]-(local_time[(27*720)]-0.25) 
    
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

print, 'max DP2: ', max(dp2)
print, 'min DP2: ', min(dp2)


;###############################################################################
;###############################################################################
;###############################################################################
    cgOPLOT, date_time, ddyn, COLOR='black' , LINESTYLE=0, THICK=3       
    cgOPLOT, date_time, dp2, COLOR='red', THICK=2    
    ;cgOPLOT, date_time, SQ, color='blue', THICK=3  


    cgOPLOT, [!X.CRANGE[0], !X.CRANGE[1]], [0.,0.], LINESTYLE=1, THICK=4,COLOR='black' 
 

    cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
    xminor=8,$
    xtitle='Universal Time [h], March 2015',$
    xstyle=1,$
    XTICKUNITS=['day'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 1, $       
    XTICKFORMAT=['LABEL_DATE'],$
    ; COLOR=negro, $
    CHARSIZE = 1.4 , $
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
    YTITLE = 'I. Currents [nT]', $                          
    ;COLOR=negro, $
    YSTYLE=1, $
    CHARSIZE = 1.4,$
    CHARTHICK=1.6 
    
    cgAXIS, YAXIS = 1, $
    ; COLOR=negro, $                                                                      
    YSTYLE=1, $       
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2 ,$
    CHARTHICK=1.6    
;###############################################################################
;###############################################################################                     
;second panel legend                   
        cgPolygon, [0.79,0.82,0.82,0.79], [0.391,0.391,0.394,0.394], color = 'black', /NORMAL, /FILL    
        cgPolygon, [0.79,0.82,0.82,0.79], [0.324,0.324,0.327,0.327], color = 'red', /NORMAL , /FILL  
        
        ddyn = Textoidl('H_{DDEF}')
        dp2 = Textoidl('H_{PPEF}')
        XYOUTS, 0.83, 0.39 , /NORMAL, Ddyn, CHARSIZE = 1.2, CHARTHICK=chr_thick1                 
                
        XYOUTS, 0.83, 0.32 , /NORMAL, DP2, CHARSIZE = 1.2, CHARTHICK=chr_thick1     
                
;###############################################################################                                                            
  !P.Font = 1
  ;XYOuts, 0.53, 0.735, '(a)', /Normal, $
  ;  Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font= 3 
;###############################################################################   

    cgPS_Close, density = 300, width = 1600;, /PNG 
end