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
    mlon_tmp = 0
    if mhem EQ 'W' then mlon_tmp = 360-mlon else mlon_tmp = mlon


    ; Convert UT to hours and calculate MLT
    caldat, ut, mh, dy, yr, ut_h, mn, sc
    mlt = ut_h + (fix(mlon_tmp) / 15)    
    mlt = mlt mod 24 ; mlt forced to be in 0-24 range

    for i = 0, n_elements(mlt) - 1 do begin
        if mlt[i] LT 0 then mlt[i] = mlt[i] + 24
        if mlt[i] GE 24 then mlt[i] = mlt[i] - 24

    endfor    

    ;get mlt julday array

    if mlt[0] LE 12 then begin
        mlt_julday = JULDAY(mh, dy, yr, mlt, mn)
    endif else begin
        mlt_julday = JULDAY(mh, dy-1, yr, mlt, mn)
    endelse


    utc_mlt  = 0
    if mlt[0] LE 12 then utc_mlt = mlt[0] else utc_mlt = mlt[0] - 24
    
    info = {mlt : mlt, utc_mlt : utc_mlt}

    return, info
end