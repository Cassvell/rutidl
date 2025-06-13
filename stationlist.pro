;
;Name:
;	set_up.pro
;purpose:
;	set declared variables in set_up_commons.pro
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   common variables
;
;calling sequence:
;   @set_up_commons
;   set_up
;
;parameters:
;   not apply
;
;dependencies:
;
;
;input files
;   declared variables in set_up_commons
;
;output files:
;   common variables for geomstorm routines
;
;version
;   set_up june, 2023
;
;
;note
;   

FUNCTION stationlist, class, station_code
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

        file_name = set_var.local_dir+class+'_stations.csv'

		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		READCOL, file_name, index, station, code, glat, ghem, glon, ghem2, mlat,  mhem, mlon, mhem2, UTC,  DELIMITER= ',',$
		FORMAT='I,A,A,F,A,F,A,F,A,F,A,A'
		

		idx = WHERE(station_code EQ code)
		
		info = {glat : 0.0, ghem : '', glon : 0.0 , ghem2 : '', mlat : 0.0, mhem : '', mlon : 0.0, mhem2 : '', utc : 0}
		
		info.glat = glat[idx]
		info.ghem = ghem[idx]
		info.glon = glon[idx]
		info.ghem2= ghem2[idx]
		info.mlat = mlat[idx]
		info.mhem = mhem[idx]
		info.mlon = mlon[idx]
		info.mhem2= mhem2[idx] 
		info.utc  = utc[idx]
		
	RETURN, info	
END
