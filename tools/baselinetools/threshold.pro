FUNCTION threshold, picks
On_error, 2
COMPILE_OPT idl2, HIDDEN
    ndays = fix(n_elements(picks)/4)
    
    ;hist, bins = np.histogram(picks, bins=ndays*2, density=True)  
 
    hist = HISTOGRAM(picks, nbins=ndays*2, /NAN, locations = bin_center)

    ; Normalize histogram to PDF
    total_area = total(hist * (bin_center[1] - bin_center[0]))
    hist = hist / total_area

    bin_width = bin_center[1] - bin_center[0]  ; Assumes uniform bin widths
    ;bin_edges = [bin_center - bin_width/2, bin_center[-1] + bin_width/2]

   ; k_fit = 0.0001
    sigma_fit = stddev(bin_center, /nan)
    mu_fit = min(picks, /nan)

    gpd_params = gpd_fit(bin_center, sigma_fit*2, result)

    gpd_pdf = gpd_pdf(bin_center, gpd_params[0], gpd_params[1], gpd_params[2])
    PLOT, bin_center, hist, TITLE='Normalized Histogram', XTITLE='Value', YTITLE='Density'
    OPLOT, bin_center, gpd_pdf

    ; Compute bin size

    ; Generate bin edges
    ;bin_edges = minval + binsize * FINDGEN(ndays*2 + 1)    




    return, hist;x, GPD, knee_point

end