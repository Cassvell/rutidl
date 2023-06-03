; Name:
;   highpass_filter
;
; Purpose:
;   Filter highpass band of a signal 
;
; Calling sequence:
;   highpass_filter(n, f, k, hf) 
;
; Inputs:
;   n : number of data vector points
;   f : signal to get filtered
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

FUNCTION highpass_filter, n, f, k, hf

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
    
    ;f is a function
    ;k are the freq vector        
    
    filter = FLTARR(n)                                                                              ;Define filter array
    i = WHERE(ABS(k) GT hf, count)                                                                  ;Create highpass filter based on a freq range

    IF (count EQ 0) THEN MESSAGE, 'Error creating filter'
    filter[i] = 1.0
         
    ;###############################################################################
    ; filtering the desired signal
        
    ff = FLOAT(FFT(filter*FFT(f), /INVERSE))                                                       
    return, ff      ;retorna la señal filtrada

END
