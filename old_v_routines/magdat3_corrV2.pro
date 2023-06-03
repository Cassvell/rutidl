
function tec_data, i_date, f_date

	On_error, 2
	compile_opt idl2, HIDDEN

	iyear	= i_date[0]
	imonth	= i_date[1]
	iday 	= i_date[2]	

	fyear	= f_date[0]
	fmonth	= f_date[1]
	fday 	= f_date[2]	

        header = 1      ; Defining number of lines of the header 
	;	file = DIALOG_PICKFILE(FILTER='*.dat')

;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

        idate = string(iyear, imonth, iday, format = '(I4, "-", I02, "-", I02)')
        fdate = string(fyear, fmonth, fday, format = '(I4, "-", I02, "-", I02)')

		file_name = '../rutidl/tec/tec_newformat/'+'tec_'+idate+'_'+fdate+'.txt'

		
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
        DStruct = {doy : 0., tec : 0., med : 0.}                                   

		r_tec = REPLICATE(DStruct, number_of_lines-header)	
        ;print, number_of_lines-header-1, number_of_lines-header+1

       
		READS, data[header:number_of_lines-1], r_tec, $
	format='(F5, X, F5, X, F6)'	
	
		return, r_tec

end

    
pro magdat3_corrV2, r_tec, PNG = png, JPEG = jpeg

	    On_error, 2
	    compile_opt idl2, HIDDEN
	    
	    yr_i1	= 2003
	    mh_i1	= 11
	    dy_i1 	= 19	

	    yr_f1	= 2003
	    mh_f1	= 11
	    dy_f1 	= 24	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        t_data1 = tec_data([yr_i1,mh_i1,dy_i1], [yr_f1,mh_f1,dy_f1])
        t1 = n_elements(t_data1.doy)

        tiempo1 = TIMEGEN(t1, START=julday(t1, t_data1.doy[0],  $
                         yr_i1, 0), UNITS='Hours')                                     
                 
        tw1 = n_elements(tiempo1)
        tec_days= findgen(tw1*12)/12.0  
        tec1     = t_data1.tec
        med1     = t_data1.med                                                                         		                   
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
	    yr_i2	= 2004
	    mh_i2	= 11
	    dy_i2 	= 6	

	    yr_f2	= 2004
	    mh_f2	= 11
	    dy_f2 	= 14	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        t_data2 = tec_data([yr_i2,mh_i2,dy_i2], [yr_f2,mh_f2,dy_f2])
        t2 = n_elements(t_data2.doy)

        ;print, t
        tiempo2 = TIMEGEN(t2, START=julday(t2, t_data2.doy[0],  $
                         yr_i2, 0), UNITS='Hours')                                     
                 
        tw2 = n_elements(tiempo2)
        tec_days2= findgen(tw2*12)/12.0 
        tec2     = t_data2.tec
        med2     = t_data2.med                    
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;############################################################################### 
	    yr_i3	= 2005
	    mh_i3	= 5
	    dy_i3 	= 14	

	    yr_f3	= 2005
	    mh_f3	= 5
	    dy_f3 	= 18	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        t_data3 = tec_data([yr_i3,mh_i3,dy_i3], [yr_f3,mh_f3,dy_f3])
        t3 = n_elements(t_data3.doy)

        ;print, t
        tiempo3 = TIMEGEN(t3, START=julday(t3, t_data3.doy[0],  $
                         yr_i3, 0), UNITS='Hours')                                     
                 
        tw3 = n_elements(tiempo3)
        tec_days3= findgen(tw3*12)/12.0
        tec3     = t_data3.tec
        med3     = t_data3.med                
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
	    yr_i4	= 2015
	    mh_i4	= 3
	    dy_i4 	= 15	

	    yr_f4	= 2015
	    mh_f4	= 3
	    dy_f4 	= 20	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        t_data4 = tec_data([yr_i4,mh_i4,dy_i4], [yr_f4,mh_f4,dy_f4])
        t4 = n_elements(t_data4.doy)

        ;print, t
        tiempo4 = TIMEGEN(t4, START=julday(t4, t_data4.doy[0],  $
                         yr_i4, 0), UNITS='Hours')                                     
                 
        tw4 = n_elements(tiempo4)
        tec_days4= findgen(tw4*12)/12.0 
        tec4     = t_data4.tec
        med4     = t_data4.med                       
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
	    yr_i5	= 2017
	    mh_i5	= 5
	    dy_i5 	= 26	

	    yr_f5	= 2017
	    mh_f5	= 5
	    dy_f5 	= 30    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        t_data5 = tec_data([yr_i5,mh_i5,dy_i5], [yr_f5,mh_f5,dy_f5])
        t5 = n_elements(t_data5.doy)

        ;print, t
        tiempo5 = TIMEGEN(t5, START=julday(t5, t_data5.doy[0],  $
                         yr_i5, 0), UNITS='Hours')                                     
                 
        tw5 = n_elements(tiempo5)
        tec_days5= findgen(tw5*12)/12.0 
        tec5     = t_data5.tec
        med5     = t_data5.med                   
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;############################################################################### 
	    yr_i6	= 2017
	    mh_i6	= 9
	    dy_i6 	= 6	

	    yr_f6	= 2017
	    mh_f6	= 9
	    dy_f6 	= 10	    	    
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Generate the time variables to plot time series of Dst Index
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        t_data6 = tec_data([yr_i6,mh_i6,dy_i6], [yr_f6,mh_f6,dy_f6])
        t6 = n_elements(t_data6.doy)

        ;print, t
        tiempo6 = TIMEGEN(t6, START=julday(t6, t_data6.doy[0],  $
                         yr_i6, 0), UNITS='Hours')                                     
                 
        tw6 = n_elements(tiempo6)
        tec_days6= findgen(tw6*12)/12.0  
        tec6     = t_data6.tec
        med6     = t_data6.med                              
;############################################################################### 
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################
;###############################################################################                               
            Device_bak = !D.Name 
            SET_PLOT, 'Z'
            
            Xsize=800
            Ysize=800
            ;DEVICE, decompose=0
            DEVICE, SET_RESOLUTION = [Xsize,Ysize]
            DEVICE, z_buffering=O
            DEVICE, set_character_size = [10, 12]
                 
            chr_size1 = 0.9
            chr_thick1= 1.0
            space     = 0.015
            rojo      = 248
            naranja  = 220
            verde     = 150
            negro     = 0
            azul      = 60
            blanco    = 255
            gris      = 130
            morado    = 30
            amarillo  = 200
            
        TVLCT, R_bak, G_bak, B_bak, /GET
            
        LOADCT, 39, /SILENT
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    ; Write a post Script
    ;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
    path = '../rutidl/output/eventos_tgm/'
    window_title = 'Correlacion of Ionospheric TEC Index under TGM conditions'    
                         
    up0  =  120
    down0=  0
    plot, med1, tec1, POSITION=[0.1,0.05,0.95,0.95],$
    xstyle = 5, ystyle=5, YRANGE=[down0,up0], psym=1,$
    BACKGROUND = blanco, COLOR=negro, XRANGE=[down0,up0]
;###############################################################################
X = [-1, 0, 1, 0, -1]
Y = [0, 1, 0, -1, 0]
USERSYM, X, Y, /fill

    oplot, med2, tec2, psym=8, color=rojo
    
A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL
    oplot, med3, tec3, psym=8, color=amarillo   
     
X = [-1,0,1,-1]
Y = [-1,1,-1,-1]
USERSYM, X, Y, /fill    
    oplot, med4, tec4, psym=8, color=morado
    
X = [-1,-1,1, 1,-1]
Y = [-1, 1,1,-1,-1]
USERSYM, X, Y, /fill        
    oplot, med5, tec5, psym=8, color=verde
    oplot, med6, tec6, psym=2, color=azul
    oplot, med1, tec1, psym=1, color=negro
;###############################################################################
;###############################################################################
;###############################################################################    
    oplot, [17,17], [110,110], color=negro, linestyle=0, psym=1
X = [-1, 0, 1, 0, -1]
Y = [0, 1, 0, -1, 0]
USERSYM, X, Y, /fill    
    oplot, [17,17], [106, 106], color=rojo, linestyle=0, psym=8
    
A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL    
    oplot, [17,17], [102, 102], color=amarillo, linestyle=0, psym=8
    
X = [-1,0,1,-1]
Y = [-1,1,-1,-1]
USERSYM, X, Y, /fill      
    oplot, [17,17], [98, 98], color=morado, linestyle=0, psym=8
    
X = [-1,-1,1, 1,-1]
Y = [-1, 1,1,-1,-1]
USERSYM, X, Y, /fill     
    oplot, [17,17], [94, 94], color=verde, linestyle=0, psym=8
    oplot, [17,17], [90, 90], color=azul, linestyle=0, psym=2                

    ;print, n_elements(H1), n_elements(dst1)

;###############################################################################
;###############################################################################
;###############################################################################     
   slope = findgen(601)-500
   x1= slope
   y1=slope
   plot, x1,y1, xstyle=5, ystyle=5, color=negro, background=blanco, $
   position=[0.1,0.05,0.95,0.95], title = window_title, /noerase, CHARSIZE = 1.0
   
        AXIS, XAXIS = 0, XRANGE=[down0,up0], $
                         COLOR=negro, $
                         xstyle=1, $
                         CHARSIZE = 0.9, $
                         XTITLE = 'TEC esperado  [TECu]', $
;                         CHARTHICK=chr_thick1, $
                         XTICKLEN=0.04
                         
        AXIS, XAXIS = 1, XRANGE=[down0,up0], $
                         COLOR=negro, $
                         xstyle=1, $
                         XTICKFORMAT='(A1)',$
                         XTICKLEN=0.04

        AXIS, YAXIS = 0, YRANGE=[down0,up0], $
                         YTITLE = 'TEC medido [TECu]', $                          
                         COLOR=negro, $
                         ystyle=1, $
                         CHARSIZE = 0.9;, $

                        
        AXIS, YAXIS = 1, YRANGE=[down0,up0], $
                         ystyle=1, $
                         COLOR=negro, $
                         yTICKFORMAT='(A1)',$
                         CHARSIZE = 0.9;, $
                        ; CHARTHICK=chr_thick1;, $   
    
        XYOUTS, 0.192, 0.89 , /NORMAL, $
                'EVENT CODE:', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1        
    
        XYOUTS, 0.24, 0.869 , /NORMAL, $
                'TGM 1', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1

        XYOUTS, 0.24, 0.838 , /NORMAL, $
                'TGM 2', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1
                
        XYOUTS, 0.24, 0.809 , /NORMAL, $
                'TGM 3', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1                

        XYOUTS, 0.24, 0.779 , /NORMAL, $
                'TGM 4', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1 

        XYOUTS, 0.24, 0.749 , /NORMAL, $
                'TGM 5', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1 

        XYOUTS, 0.24, 0.719 , /NORMAL, $
                'TGM 6', COLOR=negro, $
                CHARSIZE = chr_size1, $
                CHARTHICK=chr_thick1 

        POLYFILL, [0.185,0.315,0.315,0.185], [0.713,0.713,0.714,0.714], color = negro, /NORMAL
        POLYFILL, [0.185,0.315,0.315,0.185], [0.91,0.91,0.911,0.911], color = negro, /NORMAL
        
        POLYFILL, [0.185,0.18505,0.18505,0.185], [0.713,0.713,0.911,0.911], color = negro, /NORMAL
        POLYFILL, [0.315,0.31505,0.31505,0.315], [0.713,0.713,0.911,0.911], color = negro, /NORMAL                                     
;###############################################################################    
               

    ;print, n_elements(H1), n_elements(dst1)

;###############################################################################
;###############################################################################
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
                    true_image = bytarr(3,nx,ny)
                    true_image[0,*,*] = reds[image]
                    true_image[1,*,*] = greens[image]
                    true_image[2,*,*] = blues[image]
                    write_jpeg, path+'tec_corr.jpg', True_Image, true=1
            ENDIF ELSE BEGIN
                    IF NOT (keyword_set(quiet) OR keyword_set(png)) THEN print, '        Setting PNG as default file type.'
                    WRITE_PNG, path+'tec_corr.png', Image, reds,greens,blues
            ENDELSE

            IF NOT keyword_set(quiet) THEN BEGIN
                    print, '        Saving: '+path+'tec_corr'
                    print, ''
            ENDIF
            RETURN
end
;############################################################################################################   
;############################################################################################################
;############################################################################################################
;############################################################################################################
;############################################################################################################

                                                      
