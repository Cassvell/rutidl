;
;Name:
;	mpausa.pro
;purpose:
;	calcular la contribución magnética asociada con la magnetopausa
;      
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   magnetic data generator
;
;calling sequence:
;   .r mpausa
;   mpausa, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;   KEYWORD = 
;
;dependencies:
;
;
;input files
;   planetary geomagnetic indices 
;   geomagnetic indices SYM-H, ASY-H
;   Induced Electric field Ey
;   Dynamic Preassure P
;
;output files:
;   mpausa array for an specific time window
;
;   imported to: 
;version
;   oct, 2024
;
;note
;   para futuras referencias, es necesario mejorar el cálculo del error para cada regresión
;

function parameters, date_i, date_f
On_error, 2
COMPILE_OPT idl2, HIDDEN

;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
    @set_up_commons
    set_up 

    yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]

    idx = sym_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH = idx.symH
    asyH = idx.asyH

    ip   = e_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    E = ip.Ey
    P = ip.Pdyn
    Pdyn = sqrt(P)
    Bz = ip.Bz
    n = ip.n_P
    v = ip.Vx

    timeip = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), FINAL=JULDAY(mh_f, dy_f, yr_f, 23,55), UNITS='Minutes', STEP_SIZE=5)
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])
    CALDAT, timeip, mh, dy, yr, hr, min

    doy = INTARR(N_ELEMENTS(hr))
    yy  = INTARR(N_ELEMENTS(hr))   
    ;print, N_ELEMENTS(Y)
    FOR i=0ll, N_ELEMENTS(hr)-1 DO BEGIN
    
        yy[i] = STRMID(STRING(yr[i], format='(I4)'),2,2)
       ; print, yy[i]
        doy[i]    = date2doy(STRING(yy[i], mh[i], dy[i], FORMAT='(I02,I02,I02)'))
    ENDFOR


;compute parameters subsolar point distance: R1    
    factor1 = 10.22 + 1.29*TANH([0, 184*(Bz + 8.14)])
    factor2 = (P)^(-1/6.6)

    R1 = factor1*factor2

;compute parameters distance to the earthward edge of the geotail current sheet: R2    
    ;phi = 74.9° – 8.6° log 10( – Dst ) 
    ang1 = 74.9 * (!PI/180)
    ang2 = 8.6 * (!PI/180)


; compute dipole tilr angle psi
    alfa    = 11.43 * (!PI/180) ;angle between rotation axis of earth and dipolar moment
    alfa2   = 23.5 * (!PI/180)  ;angle between earth's axis and normal to ecliptic

    fi_se   = 0.9856263 * (172 - doy) ; angle between the Sun-Earth line and the projection of the Earth's axis on the ecliptic plane

    beta = 0    ;declination of the sun
    
    sin_beta = sin(alfa2)*cos(fi_se)
    ;beta = 
    fi_m = hr * (15 * (!PI/180)) - (69.76 * (!PI/180))

    sin_psi = -(sin_beta*cos(alfa))+(cos(beta)*sin(alfa)*cos(fi_se))


    parameters = {R1 : R1}

    return, parameters
end