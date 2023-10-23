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
        '20030526' : pw_n = 1
        '20031011' : pw_n = 2
        '20031119' : pw_n = 3
        '20040721' : pw_n = 4
        '20040827' : pw_n = 5
        '20041106' : pw_n = 6
        '20050514' : pw_n = 7
        '20050611' : pw_n = 8
        '20050821' : pw_n = 9
        '20050830' : pw_n = 10
        '20060817' : pw_n = 11
        '20061212' : pw_n = 12        
        '20150315' : pw_n = 13
        '20151005' : pw_n = 14
        '20151217' : pw_n = 15
        '20160304' : pw_n = 16
        '20161011' : pw_n = 17        
        '20170526' : pw_n = 18
        '20170906' : pw_n = 19
        '20180824' : pw_n = 20
        '20230225' : pw_n = 21
        '20230322' : pw_n = 22
        '20230422' : pw_n = 23
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


