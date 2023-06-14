FUNCTION gic_data, idate, fdate, node_idx
	On_error, 2
	compile_opt idl2, HIDDEN

	iyear	= idate[0]
	imonth	= idate[1]	
    iday     = idate[2]
    
	fyear	= fdate[0]
	fmonth	= fdate[1]	
    fday     = fdate[2]    

        @set_up_commons
        set_up	
        
        header = 5      ; Defining number of lines of the header 
;###############################################################################
;reading data files

        idate = string(iyear, imonth, iday, format = '(I4, I02, I02)')
        fdate = string(fyear, fmonth, fday, format = '(I4, I02, I02)')
        
        idx = FIX(node_idx)
        station_node    = set_var.nod_gic[idx]
                
        path=set_var.gic_dir+'gic_model_data/'				
		file_name = path+'I_'+station_node+'_'+idate+'_'+fdate+'.dat'
		print, file_name
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

        DStruct = {I : 0.0}
                                      

		r_I = REPLICATE(DStruct, number_of_lines-header)	
		READS, data[header:number_of_lines-1], r_I, $
FORMAT='(F24.22)'		
		RETURN, r_I
END

PRO gic_fragm, date_i, date_f

	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
    dy_i    = date_i[2]
    
    
	yr_f	= date_f[0]
	mh_f	= date_f[1]
    dy_f    = date_f[2]
    
        @set_up_commons
        set_up	    

    node_idx = INTARR(1)
	PRINT, 'Enter node idx: 0:lav, 1:qro, 2:maz'
	READ, node_idx, PROMPT = '> 

    node         = set_var.nod_gic[(node_idx)]        ;0:lav, 1:qro, 2:maz  
   ; print, node    
    I_gic = gic_data([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], node_idx) 
    plot, FINDGEN(N_ELEMENTS(I_gic.I)), I_gic.I
    
END    
