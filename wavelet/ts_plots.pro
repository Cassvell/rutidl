

pro ts_plots, symH, H, SQ, Bdiono, date_i, date_f, path, station_code
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
    psfile =  path+station_code+'_'+Date+'.eps'    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=6

    class = gms_class(station_code)
    info = stationlist(class, station_code)

    print, 'UTC: ', info.utc
    ;DEVICE, true=24, retain=2, decomposed=0
    ;TVLCT, R_bak, G_bak, B_bak, /GET     
    ;LOADCT, 39
    ;WINDOW, 1, XSIZE=800, YSIZE=500, TITLE='GS'


    cgplot, date_time, H, background='white', color='black', position=[.1, .56, .92, .92],XMINOR=8, charsize=1.2, xstyle = 1, ystyle=5,$
    XTICKFORMAT='(A1)', /nodata

    cgOPlot, date_time, H, color='black', thick=2, linestyle=0   
    cgoplot,date_time, symH, color='GRN5', thick=2, linestyle=0   


    cgAXIS, XAXIS = 0, XRANGE=[0,file_number],$
    ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
    XTICKS=file_number, $
    XMINOR=8, $
    ;XTICKV=FIX(days), $       
    XTICKFORMAT='(A1)',$
   ; COLOR=negro, $
    CHARSIZE = 1.2 , $
    TICKLEN=0.04,$
    CHARTHICK=3.5 
    
    cgAXIS, XAXIS = 1, XRANGE=[0,file_number],$
    ;XRANGE=(!X.CRANGE+dy_i-0.25), $      
    XTICKS=file_number, $
    XMINOR=8, $
    ;XTICKV=FIX(days), $       
    XTICKFORMAT='(A1)',$
; COLOR=negro, $
    CHARSIZE =1.2, $
    CHARTHICK=1.5,$
    TICKLEN=0.04

    cgAXIS, YAXIS = 0, $
    YTITLE = 'G. Indices [nT]', $                          
    ;COLOR=negro, $
    YSTYLE=2, $
    CHARSIZE = 1.2,$
    CHARTHICK=1.6 

    cgAXIS, YAXIS = 1, $
; COLOR=negro, $                                                                      
    YSTYLE=2, $       
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.2 ,$
    CHARTHICK=1.6    
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
;##################################################################################################################
cgplot, date_time, Bdiono, background='white', color='black', position=[.1, .18, .92, .54], XTICKFORMAT=['LABEL_DATE'], $
    XTICKUNITS=['hours'], XTICKLAYOUT = 0, XTICKINTERVAL = 24, charsize=1.1, xstyle=5, ystyle=5, $
    /nodata, /noerase
    
    
    cgoplot,date_time, Bdiono, color='red', thick = 2
    cgoplot,date_time, SQ, color='blue', thick = 2

    cgAXIS, XAXIS = 0, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$   
    xstyle=1,$
    XTICKUNITS=['hours'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 24, $       
    XTICKFORMAT=['LABEL_DATE'],$
   ; COLOR=negro, $
    CHARSIZE = 1.0 , $
    TICKLEN=0.04,$
    CHARTHICK=3.5 
    
    cgAXIS, XAXIS = 1, xrange = [date_time[0], date_time[n_elements(date_time)-1]],$
    XTICKUNITS=['hours'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 24, $      
    XTICKFORMAT='(A1)',$
; COLOR=negro, $
    CHARSIZE =1.0, $
    CHARTHICK=1.5,$
    TICKLEN=0.04

    cgAXIS, YAXIS = 0, $
    YTITLE = 'ionopheric fluctuations [nT]', $                          
    ;COLOR=negro, $
    YSTYLE=2, $
    CHARSIZE = 1.0,$
    CHARTHICK=1.6 

    cgAXIS, YAXIS = 1, $
; COLOR=negro, $                                                                      
    YSTYLE=2, $       
    YTICKFORMAT='(A1)',$
    CHARSIZE = 1.0 ,$
    CHARTHICK=1.6    




    title = STRING(STRUPCASE(station_code), info.mlat, info.mhem, info.mlon, info.mhem2,$
    FORMAT='("Obs: ", A, ", mlat: ", F7.2, " ", A, ",    ", "mlon: ", F7.2," ", A)')
   
   
    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.93   
    XYOUTS, X, y, title, /NORMAL, $
    ALIGNMENT=0.5, CHARSIZE=1.65     
 
    x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
    y = 0.02   

    local_ini = date_time[0]-0.25
    local_fin = date_time[n_elements(date_time)-1]-0.25

    local_time = TIMEGEN(START=local_ini, FINAL=local_fin, UNITS='Minutes')

    cgplot, local_time, Bdiono, background='white', color='black', position=[.1, .12, .92, .92], $
    charsize=1.0, xstyle=5,ystyle=5,/nodata, /noerase
    
    cgAXIS, XAXIS = 0,$  
    XTICKUNITS=['hours'], $
    XTICKLAYOUT = 0, $
    XTICKINTERVAL = 24, $   
    XTICKFORMAT=['LABEL_DATE'],$
    CHARSIZE = 1.0 , $
    TICKLEN=0.02,$
    CHARTHICK=3.5 
    
    cgPS_Close, density = 300, width = 1600 ;, /PNG  
end