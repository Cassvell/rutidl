;
;Name:
;	dst_0.pro
;purpose:
;	calcular el dst corregido, removiendo la contribución de la magnetopausa, permaneciendo un aproximado de la contribución a 
;   dst "únicamente" por parte de la corriente del anillo
;   
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data generator
;
;calling sequence:
;   .r dst_0
;   dst_0, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;   KEYWORD = 
;
;dependencies:
;
;
;input files
;   planetary geomagnetic indices 
;   geomagnetic indices SYM-H, ASY-H
;   Induced Electric field Ey
;   Dynamic Preassure P
;
;output files:
;   dst_0 array for an specific time win806256.dow
;
;   imported to: 
;version
;   Aug, 2024
;   sept, 2024
;
;note
;   para futuras referencias, es necesario mejorar el cálculo del error para cada regresión
;

FUNCTION Qfunc, a, E
    ON_ERROR, 2
    COMPILE_OPT idl2, HIDDEN

    ndata = N_ELEMENTS(E)
    Q = FLTARR(ndata)

    FOR i = 0, ndata-1 DO BEGIN
        IF E[i] GE 0.5 THEN BEGIN
            Q[i] = a * (E[i] - 0.5)
        ENDIF ELSE BEGIN
            Q[i] = 0
        ENDELSE
    ENDFOR

    RETURN, Q
END

FUNCTION param_Q, Ey, symH_avr

    ON_ERROR, 2
    COMPILE_OPT idl2, HIDDEN
    dif_symH = ts_diff(symH_avr, 1);fltarr(n_elements(symH_avr))
    FOR i = 0, N_ELEMENTS(symH_avr) - 1 DO BEGIN
    ; print, symH_avr[i], dif_symH[i], format = '(F12.4, F12.4)'
    ENDFOR
 
     threshold = 0.5
     tmp_top = round(max(Ey, /NAN))+threshold
     tmp_bottom = threshold
     range = tmp_top-tmp_bottom
     bins = fltarr(round((range)/2)+1)
     nbin = n_elements(bins)
 
     for i = 0, nbin-1 do begin
 
         bins[i] = threshold
         threshold = threshold + 2
     endfor    
 
     bin_counts = intarr(nbin)
 
     n_samples = n_elements(Ey)
 
     dist = fltarr(nbin, n_samples) - 1 
     bin_width = 2
 
        ; Fill the histogram and store indices
    ; Fill the histogram and store indices
    FOR k = 0, n_samples - 1 DO BEGIN
        ; Calculate the bin index for Ey[k], adjusted by the bin width
        bin_index = FLOOR((Ey[k] - 0.5) / bin_width)
        
        ; Ensure the bin_index is within the valid range
        IF bin_index GE 0 AND bin_index LT nbin THEN BEGIN
            dist[bin_index, bin_counts[bin_index]] = k  ; Store the index k in the appropriate bin
            bin_counts[bin_index] += 1  ; Increment the count for that bin
        ENDIF
    ENDFOR
 
    dist2 = fltarr(nbin, n_samples) - 1 
    dist3 = fltarr(nbin, n_samples) - 1
 
    params = FLTARR(nbin, 2)
   ; imax = where(Ey GE round(max(Ey)))
    FOR j = 0, nbin-1 DO BEGIN
         FOR m = 0, n_samples-1 DO BEGIN
             IF dist[j,m] GE 0 THEN BEGIN  ; Ensure dist[j,m] is a valid index
                 dist2[j, m] = symH_avr[dist[j,m]]
                 dist3[j, m] = dif_symH[dist[j,m]]
                         
                 ; Replace -1 and -99999.9 by NaN
                 IF dist2[j, m] EQ -1.0 OR dist2[j, m] EQ -99999.9 THEN dist2[j, m] = !VALUES.F_NAN
                 
                 IF dist3[j, m] EQ -1.0 OR dist3[j, m] EQ -99999.9 THEN dist3[j, m] = !VALUES.F_NAN
 
             ENDIF ELSE BEGIN
                 dist2[j, m]= !VALUES.F_NAN
                 dist3[j, m]= !VALUES.F_NAN
             ENDELSE    
             
         ENDFOR
 
         mask = FINITE(dist2[j, *])    
         Y =  dist2[j, [where(mask eq 1)]]
         X =  dist3[j, [where(mask eq 1)]]  
         
         
         if FINITE(Y[0]) NE 0 then begin

            ytmp_top = round(max(Y/10, /NAN))*10
            ytmp_bottom = round(min(Y/10, /nan))*10
            yrange = ytmp_top-ytmp_bottom
            ybins = fltarr(round((yrange)/10)+1)
            ynbin = n_elements(ybins)
            ybottom = ytmp_bottom
            
            for i = 0, ynbin-1 do begin   
                ybins[i] = ytmp_bottom
                ytmp_bottom = ytmp_bottom + 10
            endfor  
            
            ybin_counts = intarr(ynbin)
 
            yn_samples = n_elements(Y)
         
            Ydist = fltarr(ynbin, yn_samples) - 1 
            ybin_width = 10

            ; Fill the histogram and store indices
            FOR k = 0, yn_samples - 1 DO BEGIN
            ; Calculate the bin index for Ey[k], adjusted by the bin width
                ybin_index = FLOOR((Y[k] - ybottom) / ybin_width)
            ; Ensure the bin_index is within the valid range
                IF ybin_index GE 0 AND ybin_index LT ynbin THEN BEGIN
                    Ydist[ybin_index, ybin_counts[ybin_index]] = k  ; Store the index k in the appropriate bin
                    ybin_counts[ybin_index] += 1  ; Increment the count for that bin

                ENDIF
            ENDFOR
 
            ;distY = fltarr(nbin, n_samples) - 1 

            mean_Y = FLTARR(ynbin)
            mean_X = fltarr(ynbin)
            FOR l = 0, ynbin-1 DO BEGIN
                valid = WHERE(Ydist[l, *] NE -1, count)
                IF count GT 0 THEN BEGIN
                    ; Calculate and store the mean value for the current bin
                   ; print, l, X[Ydist[l, valid]]
                    mean_X[l] = MEAN(X[Ydist[l, valid]])
                    mean_Y[l] = MEAN(Y[Ydist[l, valid]])
                ENDIF ELSE BEGIN
                    ; If no valid data, store NaN (optional)
                    mean_Y[l] = !VALUES.F_NAN
                    mean_X[l] = !VALUES.F_NAN
                ENDELSE
                
            ENDFOR
            
            ; Filter out NaN values after the loop
            valid_mean_Y_indices = WHERE(FINITE(mean_Y), valid_mean_Y_count)
            valid_mean_X_indices = WHERE(FINITE(mean_X), valid_mean_X_count)

            IF valid_mean_Y_count GT 0 THEN mean_Y = mean_Y[valid_mean_Y_indices]
            IF valid_mean_X_count GT 0 THEN mean_X = mean_X[valid_mean_X_indices]

            IF FINITE(mean_Y[0]) NE 0 then begin
                IF n_elements(mean_Y) EQ n_elements(mean_X) and n_elements(mean_Y) GE 3 THEN BEGIN
                    result = LINFIT(mean_X, mean_Y, YFIT=yfit)
                   ; WINDOW, j
                   ; print, bins[j]
                    ;plot, mean_X, mean_Y, psym=4, background=255, color=0, THICK=2.0, xtitle='diff sym-H', ytitle='sym-H'        
                    ;oplot, [MIN(mean_X), MAX(mean_X)], [result[0] + result[1]*MIN(mean_X), result[0] + (result[1])*MAX(mean_X)], color=0
        
                    params[j,0] = result[0] ; INTERSECCIÓN
                    params[j,1] = result[1] ; PENDIENTE

                ENDIF 
            ENDIF
        ENDIF
 
    ENDFOR

     for i = 0, n_elements(params[*,0])-1 do begin
         if params[i,0] eq 0 then params[i,0] = !VALUES.F_NAN
         ;print, params[i,0]
     endfor    
    mask = FINITE(params[*,0])
    dtQ = params[[where(mask eq 1)],0]

    dtQ = dtQ[1:n_elements(dtQ)-1]
    nQ = n_elements(dtQ)
    X = bins[1:nQ]
    
   ; stop, 'end of the test'
     err = SQRT(abs(dtQ))
     res = LINFIT(X, dtQ,  YFIT=yfit)
 
     a = res[1]
     print, 'coef a eq= ', a

     RETURN, res
END 

FUNCTION param_b, Q, symH_avr, Ey, P

    ON_ERROR, 2
    COMPILE_OPT idl2, HIDDEN
    dif_symH = ts_diff(symH_avr, 1);fltarr(n_elements(symH_avr))
    FOR i = 0, N_ELEMENTS(symH_avr) - 1 DO BEGIN
    ; print, symH_avr[i], dif_symH[i], format = '(F12.4, F12.4)'
    ENDFOR
 
    tmp_top = round(max(P, /NAN)*10) + 4
    tmp_bottom = round(min(P, /NAN)*10)-4

    range = tmp_top-tmp_bottom
    bins = fltarr(round((range)/2)+1)
    nbin = n_elements(bins)
    bottom = float(tmp_bottom) /10
    for i = 0, nbin-1 do begin
 
         bins[i] = tmp_bottom 
         tmp_bottom = tmp_bottom + 4
    endfor  
    bins = bins/10

    bin_counts = intarr(nbin)
 
    n_samples = n_elements(P)
 
    dist = fltarr(nbin, n_samples) - 1 
    bin_width = 0.4

    

    ; Fill the histogram and store indices
    FOR k = 0, n_samples - 1 DO BEGIN
        ; Calculate the bin index for Ey[k], adjusted by the bin width
        bin_index = FLOOR((P[k] - bottom) / bin_width)
        ; Ensure the bin_index is within the valid range
        IF bin_index GE 0 AND bin_index LT nbin THEN BEGIN
            dist[bin_index, bin_counts[bin_index]] = k  ; Store the index k in the appropriate bin
            bin_counts[bin_index] += 1  ; Increment the count for that bin

        ENDIF
    ENDFOR
 
    dist2 = fltarr(nbin, n_samples) - 1 
    dist3 = fltarr(nbin, n_samples) - 1
    ;distQ = fltarr(nbin, n_samples) - 1

    concatenated_p_array = REPLICATE(!VALUES.F_NAN, n_samples)
    concatenated_dif = REPLICATE(!VALUES.F_NAN, n_samples)
    concatenated_s   = REPLICATE(!VALUES.F_NAN, n_samples)
    FOR j = 0, nbin-1 DO BEGIN
        FOR m = 0, n_samples-1 DO BEGIN
            
            IF dist[j,m] GE 0 THEN BEGIN  ; Ensure dist[j,m] is a valid index
            
            dist2[j, m] = Ey[dist[j,m]]
            dist3[j, m] = Ey[dist[j,m]]
            ;distQ[j, m] = Q[dist[j,m]]
                ; Replace -1 and -99999.9 by NaN
            IF dist2[j, m] EQ -1.0 OR dist2[j, m] EQ -99999.9 THEN dist2[j, m] = !VALUES.F_NAN
                
            IF dist3[j, m] EQ -1.0 OR dist3[j, m] EQ -99999.9 THEN dist3[j, m] = !VALUES.F_NAN

            ;IF distQ[j, m] EQ -1.0 OR distQ[j, m] EQ 0.0 THEN distQ[j, m] = !VALUES.F_NAN
            
            ENDIF ELSE BEGIN
                dist2[j, m]= !VALUES.F_NAN
                dist3[j, m]= !VALUES.F_NAN
                ;distQ[j, m]= !VALUES.F_NAN
            ENDELSE    
            
        ENDFOR
        
        mask = FINITE(dist2[j, *])    
        ;mask2 = FINITE(distQ[j, *])
        Y =  dist2[j, [where(mask eq 1)]]                
        
        ;Z =  distQ[j, [where(mask2 eq 1)]]  ; Z = Q en espacio fase de Pdyn

        if FINITE(Y[0]) NE 0 then begin
            ;s = stddev(Y)
            
            IF n_elements(Y) GT 3 THEN BEGIN
            ;#########################################################################################    
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################

            threshold = 0.5
            tmp_top2 = round(max(Y, /NAN)) + threshold
            tmp_bottom2 = threshold
            range2 = tmp_top2 - tmp_bottom2
            n_bins_in_range = round((range2)/2) + 1
            if n_bins_in_range LE 0 then n_bins_in_range = 1 ; Ensure there's at least one bin
        
            bins2 = fltarr(n_bins_in_range)
            nbin2 = n_elements(bins2)
            
            for l = 0, nbin2-1 do begin
                bins2[l] = tmp_bottom2
                tmp_bottom2 = tmp_bottom2 + 2
            endfor
            
            bin_counts2 = intarr(nbin2)
            n_samples2 = n_elements(Y)
            
            ndist = fltarr(nbin2, n_samples2) - 1
            bin_width2 = 2
            
            
            ; Adjust the loop to avoid out-of-range issues
            FOR n = 0, n_samples2 - 1 DO BEGIN
                bin_index2 = FLOOR((Y[n] - bins2[0]) / bin_width2) ; Adjust bin index calculation
                
                IF bin_index2 GE 0 AND bin_index2 LT nbin2 THEN BEGIN
                    ndist[bin_index2, bin_counts2[bin_index2]] = n  ; Store the index n in the appropriate bin
                    bin_counts2[bin_index2] += 1  ; Increment the count for that bin
                ENDIF
            ENDFOR
            ndist2 = fltarr(nbin2, n_samples2) - 1 
            ndist3 = fltarr(nbin2, n_samples2) - 1
            ndistQ = fltarr(nbin2, n_samples2) - 1
            params2 = FLTARR(nbin2, 2)
           ; imax = where(Ey GE round(max(Ey)))
            FOR o = 0, nbin2-1 DO BEGIN
                FOR r = 0, n_samples2-1 DO BEGIN
                    IF ndist[o,r] GE 0 THEN BEGIN  ; Ensure dist[j,m] is a valid index
                        ndist2[o,r] = symH_avr[ndist[o,r]]
                        ndist3[o,r] = dif_symH[ndist[o,r]]
                        ndistQ[o,r] = Q[ndist[o,r]]        
                        ; Replace -1 and -99999.9 by NaN
                        IF ndist2[o,r] EQ -1.0 OR ndist2[o,r] EQ -99999.9 THEN ndist2[o,r] = !VALUES.F_NAN
                        
                        IF ndist3[o,r] EQ -1.0 OR ndist3[o,r] EQ -99999.9 THEN ndist3[o,r] = !VALUES.F_NAN

                        IF ndistQ[o,r] EQ -1.0 OR ndistQ[o,r] EQ -99999.9 THEN ndistQ[o,r] = !VALUES.F_NAN
                    ENDIF ELSE BEGIN
                        ndist2[o,r]= !VALUES.F_NAN
                        ndist3[o,r]= !VALUES.F_NAN
                        ndistQ[o,r]= !VALUES.F_NAN
                    ENDELSE    
                    
                ENDFOR
        
                mask2 = FINITE(ndist2[o, *])    
                Y2 =  ndist2[o, [where(mask2 eq 1)]]
                X2 =  ndist3[o, [where(mask2 eq 1)]]  
                Z2 =  ndistQ[o, [where(mask2 eq 1)]]  
                ;print, 'dynamic preassure bin: ' + string(bins[j]), 'Electric field bin: ' + string(bins2[o])
                ;print, Z2, format = '(F24.6)'  ; 
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################
            ytmp_top = round(max(Y2/10, /NAN))*10
            ytmp_bottom = round(min(Y2/10, /nan))*10
            yrange = ytmp_top-ytmp_bottom
            ybins = fltarr(round((yrange)/10)+1)
            ynbin = n_elements(ybins)
            ybottom = ytmp_bottom
            
            for t = 0, ynbin-1 do begin   
                ybins[t] = ytmp_bottom
                ytmp_bottom = ytmp_bottom + 10
            endfor  
            
            ybin_counts = intarr(ynbin)
 
            yn_samples = n_elements(Y2)
         
            Ydist = fltarr(ynbin, yn_samples) - 1 
            ybin_width = 10

            ; Fill the histogram and store indices
            FOR u = 0, yn_samples - 1 DO BEGIN
            ; Calculate the bin index for Ey[k], adjusted by the bin width
                ybin_index = FLOOR((Y2[u] - ybottom) / ybin_width)
            ; Ensure the bin_index is within the valid range
                IF ybin_index GE 0 AND ybin_index LT ynbin THEN BEGIN
                    Ydist[ybin_index, ybin_counts[ybin_index]] = u  ; Store the index k in the appropriate bin
                    ybin_counts[ybin_index] += 1  ; Increment the count for that bin

                ENDIF
            ENDFOR

                ;distY = fltarr(nbin, n_samples) - 1 
    
                mean_Y = FLTARR(ynbin)
                mean_X = FLTARR(ynbin)
                FOR v = 0, ynbin-1 DO BEGIN
                    valid = WHERE(Ydist[v, *] NE -1, count)
                    IF count GT 0 THEN BEGIN
                        ; Calculate and store the mean value for the current bin
                       ; print, l, X[Ydist[l, valid]]
                        mean_X[v] = MEAN(X2[Ydist[v, valid]])
                        mean_Y[v] = MEAN(Y2[Ydist[v, valid]])
                    ENDIF ELSE BEGIN
                        ; If no valid data, store NaN (optional)
                        mean_Y[v] = !VALUES.F_NAN
                        mean_X[v] = !VALUES.F_NAN
                    ENDELSE
                    
                ENDFOR

                ; Filter out NaN values after the loop
                valid_mean_Y_indices = WHERE(FINITE(mean_Y), valid_mean_Y_count)
                valid_mean_X_indices = WHERE(FINITE(mean_X), valid_mean_X_count)
    
                IF valid_mean_Y_count GT 0 THEN mean_Y = mean_Y[valid_mean_Y_indices]
                IF valid_mean_X_count GT 0 THEN mean_X = mean_X[valid_mean_X_indices]
    
                IF FINITE(mean_Y[0]) NE 0 then begin
                    IF n_elements(mean_Y) EQ n_elements(mean_X) and n_elements(mean_Y) GE 3 THEN BEGIN

                        result = LINFIT(mean_X, mean_Y, YFIT=yfit)
                        ;WINDOW, o
                        ;print, bins[j]
                        ;plot, mean_X, mean_Y, psym=4, background=255, color=0, THICK=2.0, xtitle='diff sym-H', ytitle='sym-H'        
                        ;oplot, [MIN(mean_X), MAX(mean_X)], [result[0] + result[1]*MIN(mean_X), result[0] + (result[1])*MAX(mean_X)], color=0
            
                        params2[o,0] = result[0] ; INTERSECCIÓN
                        params2[o,1] = result[1] ; PENDIENTE
                        ;print, params2[o,0]
                    ENDIF 
                ENDIF
                
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################
            ;#########################################################################################

            dif = params2[o,0]-Z2
            ;print,params2[o,0]
            dif = REFORM(dif, n_elements(dif))

            if count NE 0 then begin
                p_array = REPLICATE(bins[j], n_elements(dif))
                ;print, dif
                concatenated_p_array = [concatenated_p_array, p_array]          

                ;s = 1/sqrt(stddev(dif))
                ;s_array = REPLICATE(s, n_elements(dif))
                ;concatenated_s = [concatenated_s, s_array]

                concatenated_dif = [concatenated_dif, dif] 
            endif
            ENDFOR ; fin de o
            ENDIF    
        ENDIF
    
    ;print, bins[j]
    ENDFOR    ;fin de j

p_array = concatenated_p_array[WHERE(FINITE(concatenated_p_array) EQ 1)]
dif_array = concatenated_dif[WHERE(FINITE(concatenated_dif) EQ 1)]
; Step 1: Create a mask to exclude bins with zeroes

; Now use the filtered arrays with limfit

;s_array = concatenated_s[WHERE(FINITE(concatenated_s) EQ 1)] ; error = 1/sqrt(stddev(dif_array))



; Initialize an empty array to store the concatenated values

    WINDOW, 6, XSIZE=600, YSIZE=600, TITLE='offset-Q vs P^(1/2)'
    PLOT, p_array, dif_array, PSYM=4, xstyle=2, thick=3
    ;print, p_array
    res = LINFIT(p_array, dif_array, YFIT=yfit)

    OPLOT, [MIN(p_array), MAX(p_array)], [res[0] + res[1]*MIN(p_array), res[0] + (res[1])*MAX(p_array)]
    RETURN, res[1]

END 


;function sym_v_p, Ey, symH, Pdyn, mw2
    
 ;   return, 
;end


function dst_0, date_i, date_f
On_error, 2
COMPILE_OPT idl2, HIDDEN

;   RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
    @set_up_commons
    set_up 

    yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]

    idx = sym_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    symH = idx.symH
    asyH = idx.asyH

    ip   = e_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
    E = ip.Ey
    P = ip.Pdyn
    Pdyn = sqrt(P)

    timeax = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 23,59), UNITS='Minutes', STEP_SIZE=1)

    timeip = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0,0), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 23,55), UNITS='Minutes', STEP_SIZE=5)

    timeh = TIMEGEN(START=JULDAY(mh_i, dy_i, yr_i, 0), $
					FINAL=JULDAY(mh_f, dy_f, yr_f, 23), UNITS='Hours', STEP_SIZE=1)
    date_label = LABEL_DATE(DATE_FORMAT = ['%D', '%M %Y'])
    CALDAT, timeip, mh, dy, yr, hr, min

   ;####################################################################################################### 
    mw = 30   
    rate = mw/5
    symH_avr = fltarr(n_elements(symH)/mw)
    Ey = fltarr(n_elements(E)/rate)
    asyH_sigma = fltarr(n_elements(symH)/mw)
    P_sqrt = fltarr(n_elements(E)/rate)

    FOR i = 0, N_ELEMENTS(asyH_sigma) - 1 DO BEGIN
        symH_avr[i] = STDDEV(asyH[i * mw:(i + 1) * mw - 1], /NAN)      
    ENDFOR

    FOR i = 0, N_ELEMENTS(symH_avr) - 1 DO BEGIN
        symH_avr[i] = MEAN(symH[i * mw:(i + 1) * mw - 1], /NAN)      
    ENDFOR

    FOR i = 0, N_ELEMENTS(Ey) - 1 DO BEGIN
       Ey[i] = MEAN(E[i * rate:(i + 1) * rate - 1], /NAN)      
    ENDFOR

    FOR i = 0, N_ELEMENTS(P_sqrt) - 1 DO BEGIN
        P_sqrt[i] = SQRT(MEAN(P[i * rate:(i + 1) * rate - 1], /NAN))      
    ENDFOR


    Q_coeff = param_Q(Ey, symH_avr)
    a = Q_coeff[1]

    Q = Qfunc(a, E)
    Q_1min = extrapol(Q, n_elements(symH))
    ;print, Q    
    ;P_dif = TS_DIFF(P_sqrt, 1)
    
    fac_mcp = -0.18
    tau_mcpherron = EXP(fac_mcp * E + 2.41)

    fac_bgon = -0.09
    tau_ballgon = EXP(fac_bgon * E + 2.2)

    mc_pherron = {b : 12.58, c : 33.24, tau : tau_mcpherron}
    
    ballarta_gon = {b : 10.01, c : 32.18, tau : tau_ballgon}
    ;b = param_b(Q, symH_avr, Ey, P_dif)

    ;c = b*(total(sqrt(P_sqrt), /NAN)/n_elements(P_dif))

    ;print, 'parametro b = ', b
    ;print, 'parametro c = ', c
    P_sqrt_1min = extrapol(P_sqrt, n_elements(symH))

    symH_0 = symH - (mc_pherron.b * P_sqrt_1min) + mc_pherron.c


    
    values = {symH_0 : symH_0, Q : Q_1min}
	
	dir = set_var.Mega_dir+'sym_0/'
	test = FILE_TEST(dir, /DIRECTORY) 
	IF test EQ 0 THEN BEGIN
		FILE_MKDIR, dir
		PRINT, 'PATH directory '+dir
		PRINT, 'created'
	ENDIF ELSE BEGIN
		PRINT, ''
	ENDELSE

    ndays = (JULDAY(mh_f,dy_f,yr_f)-JULDAY(mh_i,dy_i,yr_i))+1
	outfile = STRARR(ndays) 
	string_date = STRARR(ndays)      
    ;values.symH_0 = add_nan(values.symH_0, !VALUES.F_NAN, 'equal') 
    i_nan = where(~finite(values.symH_0), count)
    ;nan_indices = where(values.symH_0 eq !VALUES.F_NAN, count)
    if count gt 0 then values.symH_0[i_nan] = 9999
    ;print, i_nan

    FOR i=0, ndays-1 DO BEGIN
        tmp_year    = 0
        tmp_month   = 0
        tmp_day     = 0
        tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
        CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
        string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')        
    
        outfile[i] = dir+'sym0_'+string_date[i]+'.dat'    
        OPENW, LUN, outfile[i], /GET_LUN        
    
        ; Get the corresponding data for the day
        symH_0_day = values.symH_0[i*1440:(i+1)*1440-1]
        Q_day = values.Q[i*1440:(i+1)*1440-1]
    
        ; Loop through each value of the day and print as columns
        FOR j=0, 1439 DO BEGIN               
            PRINTF, LUN, symH_0_day[j], Q_day[j], FORMAT='(F10.4,1X,F10.4)'

        ENDFOR
    
        CLOSE, LUN
        FREE_LUN, LUN    
    ENDFOR 

    return, values
END    