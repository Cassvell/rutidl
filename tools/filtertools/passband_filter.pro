; Name:
;   passband_filter
;
; Purpose:
;   Filtering by passband 
;
; Calling sequence:
;   passband_filter(n, f, k, lf, hf) 
;
; Inputs:
;   n : number of data vector points
;   f : signal to get filtered
;   lf: low frequency of the band
;   hf: high frequencu of the band
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


FUNCTION passband_filter, n, f, k, lf, hf

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	    
    ;f is a signal function
    ;k are the freq vector    
    
    filter = FLTARR(n)                                                                              ;Define filter array

i = WHERE((ABS(k) GT lf) AND $
 ;Find band−pass frequencies
 (ABS(k) LT hf), count)

    IF (count EQ 0) THEN MESSAGE, 'Error creating filter'
    filter[i] = 1.0         
    ;###############################################################################
    ; filtering the desired signal        
    ff = FLOAT(FFT(filter*FFT(f), /INVERSE))                                                       
    return, ff      ;retorna la señal filtrada                                                                                   ;retorna la señal filtrada

END
