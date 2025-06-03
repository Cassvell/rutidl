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
    COMPILE_OPT idl2, HIDDEN
    
    ; Get station information
    class = gms_class(station_code)
    info = stationlist(class, station_code)

    ; Extract longitudes and hemispheres
    glon = info.glon
    ghem = info.ghem2
    mlon = info.mlon
    mhem = info.mhem2

    ; Adjust longitudes based on hemisphere
    glon_tmp = (ghem eq 'E') ? fix(glon) : fix(glon - 180)
    mlon_tmp = (mhem eq 'E') ? fix(mlon) : fix(mlon - 180)

    ; Convert UT to hours and calculate MLT
    caldat, ut, mh, dy, yr, ut_h
    mlt = ut_h + (glon_tmp / 15) + (mlon_tmp / 15)

    ; Ensure MLT is within 0-24 range
    mlt = mlt mod 24
    for i = 0, n_elements(mlt)-1 do begin
        if mlt[i] lt 0 then mlt[i] = mlt[i] + 24
    end
    
    return, mlt
end