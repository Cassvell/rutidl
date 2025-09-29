FUNCTION gms_class, gms_code

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
;        @set_up_commons
;        set_up
	class = ''
CASE gms_code of
        'coe'   : class = 'regmex'
        'teo'   : class = 'regmex'
		'itu'   : class = 'regmex'
		'jur'	: class = 'regmex'
		'bmt'   : class = 'intermagnet'
		'bou'   : class = 'intermagnet'
		'brd'   : class = 'intermagnet'
		'bsl'   : class = 'intermagnet'
		'cki'   : class = 'intermagnet'
		'cyg'   : class = 'intermagnet'
		'gui'   : class = 'intermagnet'
		'hbk'   : class = 'intermagnet'
		'ipm'   : class = 'intermagnet'
		'kak'   : class = 'intermagnet'
		'kdu'   : class = 'intermagnet'
		'kmh'   : class = 'intermagnet'
		'pil'   : class = 'intermagnet'
		'sjg'   : class = 'intermagnet'
		'tam'   : class = 'intermagnet'
		'tdc'	: class = 'intermagnet'
        'tuc'   : class = 'intermagnet'
		'hon'   : class = 'intermagnet'
		'kny'   : class = 'intermagnet'
		'jai'   : class = 'intermagnet'
		'lzh'   : class = 'intermagnet'
		'mlt'   : class = 'intermagnet'
		'abg'   : class = 'intermagnet'
		'qsb'   : class = 'intermagnet'
		'izn'   : class = 'intermagnet'
        ELSE : PRINT, 'non avaiable gms data'
    ENDCASE

RETURN, class
END
