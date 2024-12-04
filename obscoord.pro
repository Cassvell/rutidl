
FUNCTION obscoord, station_code
	On_error, 2
	COMPILE_OPT idl2, HIDDEN    
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        @set_up_commons
        set_up	
    	
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;reading data files
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
        @set_up_commons
        set_up		

        file_name = set_var.local_dir+'tools/coord_obs.dat'
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		READCOL, file_name, code,latgeo, hemgeo, longeo, mergeo, latmag, hemag, lonmag, mermag, UTC,  DELIMITER= ' ',$
		FORMAT='A,F,A,F,A,F,A,F,A,A'
		
		idx = WHERE(strupcase(station_code) EQ code)
		
		info = {latgeo : 0.0, hemgeo : '', longeo : 0.0, mergeo : '', latmag : 0.0, hemag : '', lonmag : 0.0, mermag : '', utc : 0}
		
		info.latgeo = latgeo[idx]
		info.hemgeo  = hemgeo[idx]
        info.longeo = longeo[idx]
		info.mergeo  = mergeo[idx]
		info.latmag = latmag[idx]
		info.hemag  = hemag[idx]       
        info.lonmag = lonmag[idx]
		info.mermag  = mermag[idx]                
		info.utc  = utc[idx]
		
	RETURN, info	
END