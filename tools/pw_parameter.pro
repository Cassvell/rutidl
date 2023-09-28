FUNCTION pw_parameter, date
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
        @set_up_commons
        set_up


	yr	= date[0]
	mh	= date[1]
	dy 	= date[2]    

    TGM_n = STRING(yr, mh, dy, FORMAT='(I4,I02,I02)')    

    CASE TGM_n of
        '20230225' : pw_n = 1
        '20230322' : pw_n = 2
        '20230422' : pw_n = 3
        ELSE: pw_n = 'fuera de rango'
    ENDCASE 	

		data_dir = set_var.local_dir+'/tools/'
		file_name = data_dir+'powerlaw_table.dat'
       ; print, file_name
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)
	pw_n = pw_n-1
	OPENR, LUN, file, /GET_LUN, ERROR=err
	READF, LUN, data, FORMAT = '(A)'

	CLOSE, LUN
	FREE_LUN, LUN
;###############################################################################
;extracting data and denfining an structure data
;###############################################################################; col=event a1		a2		a1_Bsq	a2_Bsq
	;DStruct = {ev:0, a1:0.0, a2:0.0, a1_bsq:0.0, a2_bsq:0.0}
	DStruct = {ev: 0, a1:0., a2:0.}
	power_table = REPLICATE(DStruct, number_of_lines)	
	header = 1             ; Defining number of lines of the header 

	;READS, data[0:number_of_lines-1], power_table, FORMAT='(1I,3X,F5,3X,F5,3X,F5,3X,F5)'
	READS, data[0:number_of_lines-1], power_table, FORMAT='(I2,F8,F7)'
	;print, power_table[pw_n]
	RETURN, power_table[pw_n]
END


