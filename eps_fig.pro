PRO eps_fig
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
        @set_up_commons
        set_up
        
    path = set_var.local_dir
    psfile =  path+'map_REGEMEX.eps' 
    cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
    /nomatch, XSize=10, YSize=6	
   CGMap_Set, /HAMMER, 28, -100, /Grid, /CONTINENTS, /COUNTRIES, Limit=[12, -120, 32, -80], $
   COLOR='black', CON_COLOR='gray', /FILL_CONTINENTS, /LABEL, LATLAB = -116, LONLAB = 14, LATDEL=5, $
   LONDEL=10, /COAST
  
    usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill
  
    CGPLOTS, -99.181, 19.746, PSYM=8, THICK=3,  SYMSIZE = 1, COLOR='blue' ;teo
	CGTEXT, -99.181, 18.5  , 'TEO', COLOR='black', ALIGNMENT=0.5, CHARSIZE=1
    
    CGPLOTS, -101.694, 19.81, PSYM=8, THICK=3,  SYMSIZE = 1, COLOR='red'	;coe
	CGTEXT, -101.694, 18.7  , 'COE', COLOR='black', ALIGNMENT=0.5, CHARSIZE=1
	
    CGPLOTS, -99.98, 24.36, PSYM=8, THICK=3,  SYMSIZE = 1, COLOR='red'   ;itu
	CGTEXT, -99.98, 23.16  , 'ITU', COLOR='black', ALIGNMENT=0.5, CHARSIZE=1    
    
    CGPLOTS, -106.41, 23.19, PSYM=4, THICK=3,  SYMSIZE = 1, COLOR='red' ;mzt
	CGTEXT, -106.41, 21.99  , 'MZT', COLOR='black', ALIGNMENT=0.5, CHARSIZE=1    
    
    CGPLOTS, -100.5, 20.70, PSYM=4, THICK=3,  SYMSIZE = 1, COLOR='red' 	;qro
	CGTEXT, -100.5, 19.50  , 'QRO', COLOR='black', ALIGNMENT=0.5, CHARSIZE=1        
    
    CGPLOTS, -91.99, 15.85, PSYM=4, THICK=3,  SYMSIZE = 1, COLOR='red' ;chp
    CGTEXT, -91.99, 14.55  , 'CHP', COLOR='black', ALIGNMENT=0.5, CHARSIZE=1
    
    CGPLOTS, -89.73, 21.00, PSYM=4, THICK=3,  SYMSIZE = 1, COLOR='red' ;mer
    CGTEXT, -89.73, 19.80  , 'MER', COLOR='black', ALIGNMENT=0.5, CHARSIZE=1    
    
    cgPS_Close, density = 300, width = 1600 ;, /PNG  
       
END

