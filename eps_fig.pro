PRO eps_fig
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
		
    data = data_intro(1000)
    x = data.x0
    y = data.y0
    
    dir_name = '/home/isaac/geomstorm/rutidl/test.eps'	
   ; print, y
    RETURN
    ;SET_PLOT, 'PS'
    ;DEVICE, /ENCAPSULATED, FILENAME = dir_name

    ;cgDisplay, 1750, 500, Title='Filled Area Under a Curve'
    ;CGPlot, x, y, Color='yellow', Thick=2, XRANGE=[0,2000], YRANGE=[-20,20]
    
    
   ; write_png,'test.png',tvrd() 
  ;  DEVICE, /CLOSE
    
END

FUNCTION data_intro, n
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    
    
    X = FINDGEN(n)
    
    Y = COS(X)    
    
    str = {x0 : FLTARR(N_ELEMENTS(X)), y0 : FLTARR(N_ELEMENTS(X))}

    str.x0     = X[*]     
    str.y0     = Y[*]
    RETURN, str
    ;gen_psfig = eps_fig(X, Y, dir_name)
END

