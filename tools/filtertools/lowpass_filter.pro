; Name:
;   lowpass_filter
;
; Purpose:
;   Filter lowpass band of a signal 
;
; Calling sequence:
;   lowpass_filter(n, f, k, lf) 
;
; Inputs:
;   n : number of data vector points
;   f : signal to get filtered
;   lf: low frequency of the band
;
; Output:
;   ff: the signal filtered according to the set band of frequencies 
;       
; Keywords:
;   None.
;
; Author and history:
;   V.1 (base): Kenneth P. Bowman, 2004.
;   V.2:  C. Isaac Castellanos Velazco, 2022   

FUNCTION lowpass_filter, n, f, k, lf

	On_error, 2
	COMPILE_OPT idl2, HIDDEN	    
    ;f is a function
    ;k are the freq vector
    ;   
    filter = FLTARR(n)                                                                              ;Define filter array

    i = WHERE(ABS(k) LT lf, count)                                                                  ;Find low frequencies
    IF (count EQ 0) THEN MESSAGE, 'Error creating filter'
    filter[i] = 1.0
         
    ;###############################################################################
    ; filtering  the desired signal 
    ff = FLOAT(FFT(filter*FFT(f), /INVERSE))
    return, ff      ;retorna la señal filtrada

END
