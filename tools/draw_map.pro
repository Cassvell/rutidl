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
On_error, 2
COMPILE_OPT idl2, HIDDEN
    path = '../rutidl/'
    psfile =  path+'map.eps'


    
    
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=3., font=0, /encapsulated, $
    /nomatch, XSize=16, YSize=10
    
    cgMap_set, 10, 30, /CONTINENTS, /FILL_CONTINENTS, /HORIZON, $
    ISOTROPIC=ISOTROPIC, /MOLLWEIDE, /GRID, CON_COLOR  = cgColor("gray"), BACKGROUND = "cyan"
   
    x = [0, 0.5, 0, -0.5]
    y = [0.5, 0, -0.5, 0]
    
    ; Load the symbol into IDL
    USERSYM, x, y, /FILL

    teo = obscoord('teo')
    CGPLOTS, -teo.longeo, teo.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Red', THICK=12     
    
    sjg = obscoord('sjg')
    CGPLOTS, -sjg.longeo, sjg.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'red', THICK=12         
    
    gui = obscoord('gui')
    CGPLOTS, -gui.longeo, gui.latgeo, PSYM =8, SYMSIZE=4, COLOR = 'Blue', THICK=12  

    tam = obscoord('tam')
    CGPLOTS, tam.longeo, tam.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Blue', THICK=12   

    jai = obscoord('jai')
    CGPLOTS, jai.longeo, jai.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Green', THICK=12  

    lzh = obscoord('lzh')
    CGPLOTS, lzh.longeo, lzh.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12  

    bmt = obscoord('bmt')
    CGPLOTS, bmt.longeo, bmt.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12  

    cyg = obscoord('cyg')
    CGPLOTS, cyg.longeo, cyg.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12 

    kny = obscoord('kny')
    CGPLOTS, kny.longeo, kny.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12  

    kak = obscoord('kak')
    CGPLOTS, kak.longeo, kak.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12  

    hon = obscoord('hon')
    CGPLOTS, hon.longeo, hon.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'orange', THICK=12  




    cgPS_Close, density = 300, width = 1000

  
    RETURN
END


