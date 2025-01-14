function picks, data, tw, tw_pick
On_error, 2
COMPILE_OPT idl2, HIDDEN
;###############################################################################
    @set_up_commons
    set_up
    
    ndata = n_elements(data)
    ndays = fix(ndata / 1440)

    n = 0

    IF 24.0 MOD tw_pick EQ 0 THEN BEGIN
        n = 24.0 / tw_pick
    ENDIF ELSE BEGIN
        STOP, 'Please, enter a time window in hours that is a divisor of 24 h'
    ENDELSE
    
    
    hourly_data = hourly_iqr(data, tw)
    
    ;hourly_data = tri_hourly(hourly_data)

    daily = fltarr(n*ndays)

    iqr_mov = fltarr(n_elements(hourly_data/tw_pick))

    for i = 0, n_elements(daily)-1 do begin
        iqr_mov = hourly_data[i * (tw_pick/3) : ((i + 1) * (tw_pick/3))-1 ]
        ;print, iqr_mov;TOTAL(  FINITE(data[ i * tw : ((i + 1) * tw)-1 ]))
        if TOTAL(  FINITE(hourly_data[i * (tw_pick/3) : ((i + 1) * (tw_pick/3))-1 ]))  GE (tw_pick/3)*0.7  THEN BEGIN
            iqr_picks = max(iqr_mov, /nan)            
            
        endif else begin
            iqr_picks = !values.f_nan

        endelse      

        daily[i] = iqr_picks
    endfor    

    ;print, daily
    return, daily
end

function hourly_iqr, data, tw
    On_error, 2
    COMPILE_OPT idl2, HIDDEN    

    ndata = n_elements(data)
    IF ndata MOD tw NE 0 THEN BEGIN
        STOP, 'Error: The data length must be divisible by the time window (tw).'
    ENDIF

    ; Initialize the output array with NaN values
    H_IQR = FLTARR(ndata / tw)

    ; Loop through each time window
    FOR i = 0, n_elements(H_IQR) - 1 DO BEGIN
        window_data = data[i * tw : ((i + 1) * tw) - 1]

        finite_count = TOTAL(FINITE(window_data))
        ; If all values are NaN, skip the calculation
        IF finite_count GE tw*0.5 THEN BEGIN
            ;print, finite_count
            ; Compute IQR for valid data
            QR1 = cgPercentiles(window_data, Percentiles=[0.25])
            QR3 = cgPercentiles(window_data, Percentiles=[0.75])
            H_IQR[i] = QR3 - QR1              
            
        ENDIF ELSE BEGIN
            H_IQR[i] = !values.f_nan
        ENDELSE
    ENDFOR 

    RETURN, H_IQR
END

function tri_hourly, data
    On_error, 2
    COMPILE_OPT idl2, HIDDEN   

    ; Determine the number of tri-hourly chunks
    n_chunks = n_elements(data) / 3
    if n_elements(data) MOD 3 NE 0 then $
        print, 'Warning: Data size not divisible by 3. Ignoring leftover elements.'
    ; Create output array
    tri_hour = fltarr(n_chunks)

    ; Loop through chunks and compute standard deviation
    for i = 0, n_chunks-1 do begin
        ; Extract the current chunk
        chunk = data[i * 3 : ((i + 1) * 3) - 1]

        ; Compute the standard deviation (ignoring NaN if present)
        tri_hour[i] = median(chunk)
    endfor    

    return, tri_hour
end
