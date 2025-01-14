FUNCTION mode, data
    ; Define a local variable to store unique values
    unique_vals = 0

    ; Iterate through the data array and find unique values
    FOR i=0, N_ELEMENTS(data)-1 DO BEGIN
        ; Check if the current value is already in unique_vals
        if total(where(unique_vals eq data[i])) eq 0 then begin
            ; If not, append it to unique_vals
            unique_vals = [unique_vals, data[i]]
        endif
    ENDFOR

    ; Remove the first element, which is just a placeholder
    unique_vals = unique_vals[1:*]

    ; Initialize variables
    counts = REPLICATE(0, N_ELEMENTS(unique_vals))

    ; Count occurrences of each unique value
    FOR i=0, N_ELEMENTS(unique_vals)-1 DO BEGIN
        counts[i] = N_ELEMENTS(WHERE(data EQ unique_vals[i]))
    ENDFOR

    ; Find the index of the maximum count
    max_count_index = WHERE(counts EQ MAX(counts), count)

    ; Return the mode(s)
    mode = unique_vals[max_count_index]
    RETURN, mode
END
