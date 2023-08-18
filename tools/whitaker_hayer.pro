FUNCTION whitaker_hayer, x_diff, threshold
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	xd_med = MEDIAN(x_diff)
	MAD    = MEDIAN(ABS(x_diff-xd_med))
	
	nfact  = 0.6745
	z  	   = nfact*(x_diff-xd_med)/MAD
	
	spikes = WHERE(abs(z) GT threshold)
	RETURN, spikes		
END


	

