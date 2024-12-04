FUNCTION constrained_log_likelihood, params, data, lower_bounds, upper_bounds
On_error, 2
COMPILE_OPT idl2, HIDDEN
    ; Ensure parameters are within bounds
    IF MIN(params - lower_bounds) LT 0 OR MAX(params - upper_bounds) GT 0 THEN $
        RETURN, !VALUES.F_NAN

    RETURN, gpd_log_likelihood(params, data)
END

PRO gpd_fit, data, threshold, result
On_error, 2
COMPILE_OPT idl2, HIDDEN
    ; Select excesses above threshold
    excesses = data[WHERE(data > threshold)] - threshold
    IF N_ELEMENTS(excesses) EQ 0 THEN BEGIN
        PRINT, 'No excesses above threshold.'
        RETURN
    ENDIF

    ; Initial guesses: shape=0.1, scale=stddev, location=0
    init_params = [0.1, STDDEV(excesses), 0]

    ; Define parameter bounds
    lower_bounds = [-2, 1e-5, 0]
    upper_bounds = [2, MAX(excesses), threshold]

    ; Initialize simplex
    simplex = [[0.1, STDDEV(excesses), 0], $
               [0.2, STDDEV(excesses)*1.1, 0.1], $
               [0, STDDEV(excesses)*0.9, -0.1]]

    ; Run AMOEBA optimization
    result_params = simplex[*, 0]  ; Start with the initial guess
    log_likelihood = 1e30          ; Initialize to a large value
    tolerance = 1e-5               ; Convergence tolerance
    max_iter = 500                 ; Maximum number of iterations
    iteration = 0

    WHILE iteration LT max_iter DO BEGIN
        ; Call AMOEBA for one step
        new_simplex = AMOEBA(simplex, 'constrained_log_likelihood', excesses, $
                             lower_bounds, upper_bounds)
        
        ; Compute the new log-likelihood
        new_log_likelihood = constrained_log_likelihood(new_simplex[*, 0], excesses, $
                                                         lower_bounds, upper_bounds)
        
        ; Check for convergence
        IF ABS(new_log_likelihood - log_likelihood) LT tolerance THEN BEGIN
            result_params = new_simplex[*, 0]
            BREAK
        ENDIF
        
        ; Update variables for next iteration
        simplex = new_simplex
        log_likelihood = new_log_likelihood
        iteration += 1
    ENDWHILE

    ; Check if convergence was reached
    IF iteration EQ max_iter THEN BEGIN
        PRINT, 'AMOEBA did not converge.'
        RETURN
    ENDIF

    ; Output fitted parameters
    result.shape = result_params[0]
    result.scale = result_params[1]
    result.location = result_params[2]
    PRINT, 'Fitted Parameters:'
    PRINT, 'Shape (xi): ', result.shape
    PRINT, 'Scale (sigma): ', result.scale
    PRINT, 'Location (mu): ', result.location
END