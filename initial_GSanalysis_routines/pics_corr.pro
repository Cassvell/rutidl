pro pics_corr
On_error, 2
compile_opt idl2, HIDDEN

    @set_up_commons
    set_up	
    path = set_var.Mega_dir
    readcol, path+'picos_tgm.csv', Fecha, Estructura, Bz, Kp, Kmex, Dst, dH,  /NAN,  DELIMITER=',', SKIPLINE = 1,$
    FORMAT='A,A,F,F,F,F,F', NLINES = ndata
    
    Bz = add_nan(Bz, 99.9, 'equal')
    Kmex = add_nan(Kmex, 99.9, 'equal')
    Dst = add_nan(Dst, 9999.9, 'equal')
    dH  = add_nan(dH, 9999.9, 'equal')
    
    ; Create non-NaN arrays by filtering out rows with NaN values
    valid_k_indices = WHERE(finite(Kmex) AND finite(Kp), count_k)
    valid_h_indices = WHERE(finite(dH) AND finite(Dst), count_h)

    IF count_k GT 0 THEN BEGIN
        kmex_nonan = Kmex[valid_k_indices]
        kp_nonan = Kp[valid_k_indices]
        Fecha_nonan = Fecha[valid_k_indices]
    ENDIF ELSE BEGIN
        print, 'No valid data found for K arrays.'
        kmex_nonan = 0.0
        kp_nonan = 0.0
        Fecha_nonan = 0.0
    ENDELSE

    IF count_h GT 0 THEN BEGIN
        dH_nonan = dH[valid_h_indices]
        Dst_nonan = Dst[valid_h_indices]
       ; Fecha_nonan = Fecha[valid_h_indices]
    ENDIF ELSE BEGIN
        print, 'No valid data found for H arrays.'
        dH_nonan = 0.0
        Dst_nonan = 0.0
       ; Fecha_nonan = 0.0
    ENDELSE

    print, 'Fecha   |   Kmex    |   Kp'
    for i = 0, n_elements(Fecha_nonan)-1 do begin
        if kmex_nonan[i] - kp_nonan[i] GE 2 then begin
            print, string(Fecha_nonan[i], kmex_nonan[i], kp_nonan[i], format = '(A8, 2X, F9.2, X, F9.2)')
        endif    
    endfor   
    ; Calculate correlations and covariances if sufficient data is available
    
    corr_k = correlate(kp_nonan, kmex_nonan)
    var_k = correlate(kp_nonan, kmex_nonan, /COVARIANCE)
    print, 'k correlation: ', corr_k
    print, 'k variance: ', var_k

    ; Perform a linear least-squares fit
    coeffs = POLY_FIT(kp_nonan, kmex_nonan, 1, YFIT=yfit)  ; Linear fit (order = 1)
    PRINT, 'Fit coefficients: Intercept = ', coeffs[0], ', Slope = ', coeffs[1]

    corr_h = correlate(Dst_nonan, dH_nonan)
    var_h = correlate(Dst_nonan, dH_nonan, /COVARIANCE)
    print, 'H correlation: ', corr_h
    print, 'H variance: ', var_h

    ; Perform a linear least-squares fit
    coeffs2 = POLY_FIT(Dst_nonan, dH_nonan, 1, YFIT=yfit2)  ; Linear fit (order = 1)
    PRINT, 'Fit coefficients: Intercept = ', coeffs2[0], ', Slope = ', coeffs2[1]


    WINDOW, 0,  XSIZE=800, YSIZE=800, TITLE='kmex vs kp'
    DEVICE, true=24, retain=2, decomposed=0, WINDOW_STATE=win_state
    TVLCT, R_bak, G_bak, B_bak, /GET        
    LOADCT, 39, /SILENT    

    PLOT, kp_nonan, kmex_nonan, YRANGE=[0.0,9.0], CHARSIZE = 1.8, background=255, color=0, CHARTHICK=2.0,$
    XTITLE = 'KP', YTITLE='Kmex', THICK=3, psym=4
    OPLOT, kp_nonan, yfit, LINESTYLE=0, THICK=2, COLOR=70  ; Dashed line

    xyouts, 0.8, 0.8, string('R: ', corr_k, FORMAT='(A,F9.3)'), charsize=2.0, /NORMAL, color=0,  CHARTHICK=2


    WINDOW, 1,  XSIZE=800, YSIZE=800, TITLE='Dst vs dH' 

    PLOT, Dst_nonan, dH_nonan, Ystyle=1, CHARSIZE = 1.8, background=255, color=0, CHARTHICK=2.0,$
    XTITLE = 'Dst', YTITLE='dH', THICK=3, psym=4
    OPLOT, Dst_nonan, yfit2, LINESTYLE=0, THICK=2, COLOR=70  ; Dashed line

    xyouts, 0.8, 0.8, string('R: ', corr_h, FORMAT='(A,F9.3)'), charsize=2.0, /NORMAL, color=0,  CHARTHICK=2   
    
    print, min(dH_nonan), min(Dst_nonan)
end