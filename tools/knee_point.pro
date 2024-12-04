FUNCTION knee_point, x, y
On_error, 2
COMPILE_OPT idl2, HIDDEN
    ; Ensure inputs are arrays of the same size
    IF N_ELEMENTS(x) NE N_ELEMENTS(y) THEN BEGIN
        PRINT, 'Error: X and Y must have the same number of elements.'
        RETURN, -1
    ENDIF

    ; Normalize X and Y
    x_norm = (x - MIN(x)) / (MAX(x) - MIN(x))
    y_norm = (y - MIN(y)) / (MAX(y) - MIN(y))

    ; Line between the first and last points
    x1 = x_norm[0]
    y1 = y_norm[0]
    x2 = x_norm[N_ELEMENTS(x) - 1]
    y2 = y_norm[N_ELEMENTS(y) - 1]

    ; Compute distances from all points to the line
    distances = FLTARR(N_ELEMENTS(x))
    FOR i = 0, N_ELEMENTS(x) - 1 DO BEGIN
        ; Perpendicular distance formula
        distances[i] = ABS((y2 - y1) * x_norm[i] - (x2 - x1) * y_norm[i] + x2 * y1 - y2 * x1) / $
                       SQRT((y2 - y1)^2 + (x2 - x1)^2)
    ENDFOR

    ; Find the index of the maximum distance
    max_dist = MAX(distances, max_index)  ; MAX returns the value and the index
    RETURN, max_index
END