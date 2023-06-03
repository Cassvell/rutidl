PRO draw_map
	On_error, 2
	COMPILE_OPT idl2, HIDDEN


; Create a plotting window
;WINDOW, 0, TITLE='Mollweide Contour'

;DEVICE, DECOMPOSED = 0
;   TVLCT, 255, 255, 255, 254 ; White color
 ;  TVLCT, 0, 0, 0, 253       ; Black color
  ; !P.Color = 253
  ; !P.Background = 254

;MAP_SET, /MOLLWEIDE, 20, -20, /ISOTROPIC, $
 ;  /HORIZON, /GRID, /CONTINENTS, $
  ; TITLE='Mollweide Contour'
;CONTOUR, F, lon, lat, NLEVELS=7, $
 ;  /OVERPLOT, /DOWNHILL, /FOLLOW

;Next, create the Stereographic plot:

; Create another plotting window


;WINDOW, 1, TITLE='Conic projection'
;MAP_SET, /AZIM, 32, -100, LIMIT=[10,-130,35,-70],$
 ;  /ISOTROPIC, /CONTINENTS, /GRID, GLINESTYLE = 0, /ADVANCE, $
  ; TITLE='Mexico Contour'
; Display points in the northern hemisphere only:
;CONTOUR, F(*,10:*), lon(*,10:*), lat(*,10:*), $
 ;  /OVERPLOT, NLEVELS=5
;MAP_GRID, /LABEL, LATLAB=-120, LONLAB=12, COLOR=255
;MAP_CONTINENTS, COLOR=255

;CURSOR, LON, LAT &PRINT, LAT,LON

  ;  make_psfig
    make_psfig2
  ;  make_pngfig
END


PRO make_psfig

    path = '../rutidl/'
    psfile =  path+'map.eps'
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=3., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10
    
    cgMap_set, 10, -90, /CONTINENTS, /FILL_CONTINENTS, /HORIZON, $
    ISOTROPIC=ISOTROPIC, /MOLLWEIDE, /GRID, CON_COLOR  = cgColor("GRN3")
    
    CGPLOTS, -99.19325, 19.74586111, PSYM = 4, SYMSIZE=1.4, COLOR = 'Blue', THICK=6      
    cgPS_Close, density = 300, width = 1000
    RETURN
END

PRO make_psfig2
    LOADCT, 39, /SILENT
    path = '../rutidl/'
    psfile =  path+'map_mex.eps'
    n = 1000
    X = FINDGEN(n)
    
    Y = COS(X)
        
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=3., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10
    
    
    CGPlot, x, y, Color='red', Thick=2, XRANGE=[0,2000], YRANGE=[-20,20]
    ;cgMap_set, 32, -100, /CONTINENTS, /FILL_CONTINENTS,  LIMIT=[14,-120,33,-80],$
    ;ISOTROPIC=ISOTROPIC, /AZIM, /GRID, CON_COLOR  = cgColor("GRN3"), GLINESTYLE = 0, /ADVANCE, $
    ;/COUNTRIES

    ;MAP_GRID, /LABEL, LATLAB=-120, LONLAB=15, COLOR=0.
    ;CGPLOTS, -99.19325, 19.74586111, PSYM = 4, SYMSIZE=1.4, COLOR = 'Blue', THICK=6    
     
    ;CGTEXT, .495, .28,  'TEO', /NORMAL, CHARSIZE=2.0, COLOR = 'Navy';, CHARTHICK=3.5
       
    cgPS_Close, density = 300, width = 1000;, /PNG
    
   
    RETURN
END



PRO make_pngfig

    ;cgDisplay, 1750, 500, Title='Filled Area Under a Curve'
    set_plot, 'z'
    cgMap_set, 32, -100, /CONTINENTS, /FILL_CONTINENTS,  LIMIT=[14,-120,33,-80],$
    ISOTROPIC=ISOTROPIC, /AZIM, /GRID, CON_COLOR  = cgColor("GRN3"), GLINESTYLE = 0, /ADVANCE, $
    /COUNTRIES

    MAP_GRID, /LABEL, LATLAB=-120, LONLAB=15;, COLOR=0.
    CGPLOTS, -99.19325, 19.74586111, PSYM = 4, SYMSIZE=1.4, COLOR = cgColor('Blue'), THICK=6    
     
    CGTEXT, .495, .28,  'TEO', /NORMAL, CHARSIZE=2.0, COLOR = cgColor("Navy");, CHARTHICK=3.5

    write_png,'map.png',tvrd(/true,/order) 
END
