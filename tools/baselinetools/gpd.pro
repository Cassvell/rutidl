    ; Define the GPD log-likelihood function
function gpd_log_likelihood, params, data 
    compile_opt idl2, hidden

    k = params[0]   ; Shape parameter
    sigma = params[1] ; Scale parameter
    mu = params[2]    ; Location parameter
    
    ; Filter out data points that are below the threshold (assuming threshold is 0)
    excess = data[data gt mu] - mu
    
    ; Calculate log-likelihood for the Generalized Pareto Distribution
    ll = -n_elements(excess) * alog(sigma) - (1 + 1/k) * total(alog(1 + excess/sigma)) 
    return, ll
end

function gpd_pdf, x, k, sigma, mu
On_error, 2
COMPILE_OPT idl2, HIDDEN
    ; Ensure the shape parameter is not equal to -1 (undefined in that case)
    if k EQ -1 then return, 0.0

    ; Calculate the GPD PDF for each value of x
    result = (1 / sigma) * (1 + k * (x - mu) / sigma)^(-1 / (1 + k))

    ; If k is positive, the function is only defined for x >= mu, so set values outside this range to 0
    result = (x GE mu) * result
    return, result
end




function gpd_fit, picks, threshold, result
On_error, 2
COMPILE_OPT idl2, HIDDEN
    ; Select excesses above threshold

    idx_picks = WHERE(FINITE(picks), count)
    IF count EQ 0 THEN BEGIN
       MESSAGE, 'No valid data in picks array.'
       
    ENDIF

 
    ; Initial guess for parameters [k, sigma, mu]
    k_init = 0.01
    sigma_init = STDDEV(picks)
    mu_init = MIN(picks)
    params_init = [k_init, sigma_init, mu_init]
 
    ; Optimize the parameters
    opt_params = MINIMIZE(params_init, picks)
 
    ; Extract fitted parameters
    k_fit = opt_params[0]
    sigma_fit = opt_params[1]
    mu_fit = opt_params[2]
 
    ; Print results
    PRINT, 'Fitted Parameters:'
    PRINT, '  Shape (k): ', k_fit
    PRINT, '  Scale (sigma): ', sigma_fit
    PRINT, '  Location (mu): ', mu_fit
    return, opt_params
END



function minimize, params_init, data
    On_error, 2
    compile_opt idl2, hidden
    

    n = n_elements(params_init)
    simplex = fltarr(n+1, n)  ; (n+1) points, each with n dimensions
    
    ; Initialize the simplex
    delta = 0.1  ; Perturbation
    simplex[0, *] = params_init  ; First point is the initial guess
    
    for i = 0, n-1 do begin
        simplex[i+1, *] = params_init
        simplex[i+1, i] = simplex[i+1, i] + delta
    endfor
    
    func_vals = fltarr(n + 1)
    for i = 0, n do begin
        func_vals[i] = gpd_log_likelihood(simplex[i, *], data)
    endfor
    
    max_iter = 1000
    tolerance = 1e-6
    iter = 0
    converged = 0

    while (iter lt max_iter) and (converged eq 0) do begin
        sorted = sort(func_vals)
        sorted_simplex = simplex[sorted, *]
        sorted_func_vals = func_vals[sorted]

        centroid = total(sorted_simplex[0:n, *], 0) / n
        reflected = centroid + 1 * (centroid - sorted_simplex[n, *])
        reflected_val = gpd_log_likelihood(reflected, data)
        
        if reflected_val lt sorted_func_vals[0] then begin
            expanded = centroid + 2 * (reflected - centroid)
            expanded_val = gpd_log_likelihood(expanded, data)
            if expanded_val lt reflected_val then begin
                sorted_simplex[n, *] = expanded
                func_vals[n] = expanded_val
            endif else begin
                sorted_simplex[n, *] = reflected
                func_vals[n] = reflected_val
            endelse
        endif else if reflected_val lt sorted_func_vals[n-1] then begin
            sorted_simplex[n, *] = reflected
            func_vals[n] = reflected_val
        endif else begin
            contracted = centroid + 0.5 * (sorted_simplex[n, *] - centroid)
            contracted_val = gpd_log_likelihood(contracted, data)
            if contracted_val lt sorted_func_vals[n] then begin
                sorted_simplex[n, *] = contracted
                func_vals[n] = contracted_val
            endif else begin
                for i = 1, n do begin
                    sorted_simplex[i, *] = sorted_simplex[0, *] + 0.5 * (sorted_simplex[i, *] - sorted_simplex[0, *])
                    func_vals[i] = gpd_log_likelihood(sorted_simplex[i, *], data)
                endfor
            endelse
        endelse
        
        if max(abs(func_vals - sorted_func_vals)) lt tolerance then begin
            converged = 1
        endif
        
        iter = iter + 1
    endwhile

    return, sorted_simplex[0, *]  ; Return the best simplex point (optimized parameters)
end
