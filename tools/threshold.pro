FUNCTION threshold, picks
On_error, 2
COMPILE_OPT idl2, HIDDEN
    ndays = fix(n_elements(picks)/4);int(len(picks)/4)

    ;picks = np.array(picks)  

    ;picks = picks[~np.isnan(picks)]
    
    ; Filter out NaN values
    ;valid_indices = WHERE(FINITE(picks), count)
    ;IF count GT 0 THEN picks = picks[valid_indices]
    
    ;hist, bins = np.histogram(picks, bins=ndays*2, density=True)  
 
    hist = HISTOGRAM(picks, NBINS=ndays*2, MIN=minval, MAX=maxval, /NAN)

    ; Compute bin size
    binsize = (maxval - minval) / (ndays * 2)

    ; Generate bin edges
    bin_edges = minval + binsize * FINDGEN(ndays*2 + 1)    


    ;GPD_paramet = distr.gpa.lmom_fit(picks)

    ;shape = GPD_paramet['c']
    
    ;threshold = GPD_paramet['loc']
    
    ;scale = GPD_paramet['scale']
    
    ;x = np.linspace(min(picks), max(picks), len(picks))    
    
    ;GPD =  genpareto.pdf(x, shape, loc=threshold, scale=scale)
    
    ;GPD = np.array(GPD)
    
    ;if any(v == 0.0 for v in GPD):
    ;    GPD =  genpareto.pdf(x, shape, loc=min(bins), scale=scale)
   
    ;params = genpareto.fit(picks)
    ;D, p_value = kstest(picks, 'genpareto', args=params)
    ;print(f"K-S test result:\nD statistic: {D}\np-value: {p_value}")
    
; Interpretation of the p-value & TEST KS for evaluating IQR picks
    ;alpha = 0.05

    ;if p_value > alpha:
    ;    print("Fail to reject the null hypothesis: data follows the GPD")
    ;else:
    ;    print("Reject the null hypothesis: data does not follow the GPD")   
        
    ;kneedle = kn.KneeLocator(x, GPD, curve='convex', direction='decreasing', S=5, online=True, interp_method='interp1d')

    ;knee_point = kneedle.knee #elbow_point = kneedle.elbow

    ;print(f'knee point: {knee_point}')

;The Knee point is then considered as threshold.

    return, x, GPD, knee_point

end