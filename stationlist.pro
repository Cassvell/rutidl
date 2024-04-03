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

FUNCTION stationlist, class,station_code
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

        file_name = set_var.local_dir+class+'_stations.dat'
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		READCOL, file_name, index, station, code, mlat,  hem, UTC,  DELIMITER= ' ',$
		FORMAT='I,A,A,F,A,F'
		
		idx = WHERE(station_code EQ code)
		
	;	print, mlat[idx],  hem[idx],  utc[idx]
		;FOR i=0, N_elements(index)-1 DO BEGIN		
		;	PRINT, i, code[i], mlat[i], FORMAT='(I02,4X, A, F8.2)'
		;ENDFOR
		
		info = {mlat : 0.0, hem : '', utc : 0}
		
		info.mlat = mlat[idx]
		info.hem  = hem[idx]
		info.utc  = utc[idx]
		
	RETURN, info	
END
