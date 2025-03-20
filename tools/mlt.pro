;
;Name:
;	mlt.pro
;purpose:
;	compute magnetic local time
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   time array
;
;calling sequence:
;   mlt = mlt(station_code, UT)
; 
;parameters:
;   station_code: station or observatory
;   UT: array of time in UT
;
;dependencies:
;
;input files
;   declared variables in set_up_commons
;
;output files:
;   common variables for geomstorm routines
;
;version
;   mlt january, 2025
;
;
;note
;   


function mlt, station_code, ut
    On_error, 2
    COMPILE_OPT idl2, HIDDEN
    
    class = gms_class(station_code)
    info = stationlist(class, station_code)



    glon = info.glon
    ghem = info.ghem2

    mlon = info.mlon
    mhem = info.mhem2

    if ghem eq 'E' then glon_tmp = glon else glon_tmp = glon - 180
    if mhem eq 'E' then mlon_tmp = mlon else mlon_tmp = mlon -180 
    
    

    caldat, ut, mh, dy, yr, ut_h
    mlt = ut_h + (glon_tmp / 15 ) + (mlon_tmp / 15)
    


    return, mlt
end