;Name:
;	gen_diono.pro
;purpose:
;   generate magnetic effects from ionospheric disturbances (diono) and derive Ddyn and DP2
;   separated magnetic effects throught Fourier analysis filters
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data analysis
;   fourier analysis
;
;calling sequence:
;   .r gen_diono
;   gen_diono(f1, f2, f3, t, DIG_FILTER = dig_filter, SIMPLE_FILTER = simple_filter)
;
;parameters:
;   f1: planetary index time series (Dst or SYM-H)
;   f2: local H component magnetic observations (cleaned data and with no NaN values)
;   f3: baseline variation time series. By default it's only incluided SQ diurna variation
;       but in future updates it's gonna be incluided longterm and midterm time baseline variations   
;   
;   l: 
;   Keywords:
;   DIG_FILTER   : set this to filter signal based on DIGITAL_FILTER and CONVOL procedures
;   SIMPLE_FILTER: set this to filter signal based on Fourier simple filtering
;
;dependencies:
;
;
;input files
;   Planetary geomagnetic indices:
;       -Dst
;       -SYM-H
;  
;   Local geomagnetic observations
;       -H observations
;       -Bsq baseline time series
;   
;   magnetic latitude [°]
;
;output files:
;   Time series:
;       -Diono
;       -Ddyn
;       -DP2
;
;   frequencies
;       -frequencies fk
;       -power spectrum
;       -Nyquits frequency
;
;version
;   Dec, 2022
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;
FUNCTION n_terms, wdiff, A, n
    M = ROUND((42/(2.285*wdiff))+1)
    N_terms = (M*2)+1
    
;    print, "threshold = ",threshold
    M_over = 0
    IF N_terms GT n THEN BEGIN
    	M_over = N_terms - n
		N_terms2 = N_terms-M_over 
		M2 = (N_terms2-1)/2
    	M = M2
    ENDIF
    
    RETURN, M
END

FUNCTION gen_diono, f1, f2, f3, l, time_res, case_event, DIG_FILTER = dig_filter, $
                                            SIMPLE_FILTER = simple_filter
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
; define frequencies  
    mlat         = l*!pi
    ld           = cos(mlat/180)
    p_a          = f1*ld
    baseline     = f3 + p_a             
    Bdiono       = f2-baseline
    n           = N_ELEMENTS(Bdiono) 
    
    time        = 0.

    
    CASE time_res of 
        'h'     : time = 3600.0
        'm'     : time = 60.0
    ENDCASE

    fny      = FLOAT(1.0/(2.0*time)) ; frecuencia de Nyquist
    
    hann_w = HANNING(n)
    w_ss = (TOTAL((hann_w)^2))/n
        
    y        = FFT(Bdiono*hann_w)            ; Compute Fast Fourie Transform from diono time series
 ;   y        = FFT(Bdiono)

    
    
    power_s = (ABS(y[0:n/2])^2)/w_ss
  ;  power_s = (ABS(y[0:n/2])^2)
   ; pws_s   = SMOOTH(pws, 1)

    fk     = (1+FINDGEN(n))/(n*time)
    PRINT, 'Nyquist freq: ', fny, 'Hz'
    
 ;   N_terms = 
; define pass band frequencies  
    passband_l = freq_band(case_event, 'passband_l')
    passband_u = freq_band(case_event, 'passband_u')

;define high band frequencies
    highpass_l = freq_band(case_event, 'highpass_l')
	
	f_s = 1/time
    fr_wdif_pb = passband_l*((2*!PI)*time)
    fr_wdif_hp = (highpass_l)*((2*!PI)*time)
		print, 'pasabandas es: ', passband_l
		print, 'pasa altas es: ', highpass_l
     ;   print, 'fr_wdif_pb para pasabandas es: ', fr_wdif_pb
     ;   print, 'fr_wdif_hp para pasa altas es: ', fr_wdif_hp
    
    M_pb = n_terms(fr_wdif_pb, 50, n)
    M_hp = n_terms(fr_wdif_hp, 50, n)
        
    IF KEYWORD_SET(dig_filter) THEN BEGIN
; define filtering   
	top=0.0 
        CASE time of
        3600.0     : top = 1.0
        60.0       : top = 1.0;(1/(0.5*3600.0))/fny
   		ENDCASE
       ; coeff_ddyn  = DIGITAL_FILTER(passband_l/fny, passband_u/fny, 50, M_pb)
       coeff_ddyn  = DIGITAL_FILTER(passband_l/fny, passband_u/fny, 50, (M_pb))
       ; coeff_dp2   = DIGITAL_FILTER(highpass_l/fny, top, 50, M_hp)
        coeff_dp2   = DIGITAL_FILTER(highpass_l/fny, top, 50, M_hp)
        print,'top: ', top
       ; coeff_dp2   = DIGITAL_FILTER(highpass_l/fny, (1.0/(0.5*3600.0))/fny, 50, M_hp)
        print, 'M para pasabandas es: ', M_pb
        print, 'M para pasa altas es: ', M_hp
        
        print, 'coeff_ddyn para pasabandas es: ', N_ELEMENTS(coeff_ddyn)
        print, 'coeff_dp2 para pasa altas es: ', N_ELEMENTS(coeff_dp2)
        
        print, 'N elements Diono', N_ELEMENTS(Bdiono)
; define disturbing effects 
        Bddyn        = CONVOL(Bdiono, coeff_ddyn, /edge_wrap)
        Bdp2         = CONVOL(Bdiono, coeff_dp2, /edge_wrap)  
    ENDIF
    
    IF KEYWORD_SET(simple_filter) THEN BEGIN
        Bddyn        = passband_filter(n, Bdiono, fk, passband_l, passband_u)
        Bdp2         = highpass_filter(n, Bdiono, fk, highpass_l) 
      ;  PRINT,  Bddyn  
    ENDIF
    
    structure = {diono: FLTARR(n), ddyn : FLTARR(n), dp2 : FLTARR(n), $
                 p_a : FLTARR(n), baseline : FLTARR(n), $
                 f_k : FLTARR(n), pws : FLTARR(n), fn : 0.}

    structure.diono     = Bdiono[*] 
    structure.ddyn      = Bddyn[*]
    structure.dp2       = BDP2[*]
    structure.f_k       = fk[*]
    structure.pws       = power_s[*]
    structure.fn        = fny
    structure.p_a       = p_a[*]
    structure.baseline = baseline[*]
    RETURN, structure
    
END



