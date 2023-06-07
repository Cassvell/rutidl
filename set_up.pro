


PRO set_up
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
    
    @set_up_commons
    set_var = {local_dir : '/home/isaac/geomstorm/rutidl/', $
               Mega_dir  : '/home/isaac/MEGAsync/datos/'}
    
    print,     set_var
    RETURN
END
