;
; Name:
; dst_0.pro
; purpose:
; calcular el dst corregido, removiendo la contribución de la magnetopausa, permaneciendo un aproximado de la contribución a
; dst "únicamente" por parte de la corriente del anillo
;
; author:
; Carlos Isaac Castellanos Velazco
; Estudiante de Maestría en Ciencias de la Tierra
; Instituto de Geofísica, Unidad Michoacan
; UNAM
; ccastellanos@igeofisica.unam.mx
;
; category:
; data generator
;
; calling sequence:
; .r dst_0
; dst_0, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
; parameters:
; KEYWORD =
;
; dependencies:
;
;
; input files
; planetary geomagnetic indices
; geomagnetic indices SYM-H, ASY-H
; Induced Electric field Ey
; Dynamic Preassure P
;
; output files:
; dst_0 array for an specific time win806256.dow
;
; imported to:
; version
; Aug, 2024
; sept, 2024
;
; note
; para futuras referencias, es necesario mejorar el cálculo del error para cada regresión
;

function Qfunc, a, E
  on_error, 2
  compile_opt idl2, hidden

  ndata = n_elements(E)
  Q = fltarr(ndata)

  for i = 0, ndata - 1 do begin
    if E[i] ge 0.5 then begin
      Q[i] = a * (E[i] - 0.5)
    endif else begin
      Q[i] = 0
    endelse
  endfor

  RETURN, Q
end

function param_Q, Ey, symH_avr
  on_error, 2
  compile_opt idl2, hidden
  dif_symH = ts_diff(symH_avr, 1) ; fltarr(n_elements(symH_avr))
  for i = 0, n_elements(symH_avr) - 1 do begin
    ; print, symH_avr[i], dif_symH[i], format = '(F12.4, F12.4)'
  endfor

  threshold = 0.5
  tmp_top = round(max(Ey, /nan)) + threshold
  tmp_bottom = threshold
  range = tmp_top - tmp_bottom
  bins = fltarr(round((range) / 2) + 1)
  nbin = n_elements(bins)

  for i = 0, nbin - 1 do begin
    bins[i] = threshold
    threshold = threshold + 2
  endfor

  bin_counts = intarr(nbin)

  n_samples = n_elements(Ey)

  dist = fltarr(nbin, n_samples) - 1
  bin_width = 2

  ; Fill the histogram and store indices
  ; Fill the histogram and store indices
  for k = 0, n_samples - 1 do begin
    ; Calculate the bin index for Ey[k], adjusted by the bin width
    bin_index = floor((Ey[k] - 0.5) / bin_width)

    ; Ensure the bin_index is within the valid range
    if bin_index ge 0 and bin_index lt nbin then begin
      dist[bin_index, bin_counts[bin_index]] = k ; Store the index k in the appropriate bin
      bin_counts[bin_index] += 1 ; Increment the count for that bin
    endif
  endfor

  dist2 = fltarr(nbin, n_samples) - 1
  dist3 = fltarr(nbin, n_samples) - 1

  params = fltarr(nbin, 2)
  ; imax = where(Ey GE round(max(Ey)))
  for j = 0, nbin - 1 do begin
    for m = 0, n_samples - 1 do begin
      if dist[j, m] ge 0 then begin ; Ensure dist[j,m] is a valid index
        dist2[j, m] = symH_avr[dist[j, m]]
        dist3[j, m] = dif_symH[dist[j, m]]

        ; Replace -1 and -99999.9 by NaN
        if dist2[j, m] eq -1.0 or dist2[j, m] eq -99999.9 then dist2[j, m] = !values.f_nan

        if dist3[j, m] eq -1.0 or dist3[j, m] eq -99999.9 then dist3[j, m] = !values.f_nan
      endif else begin
        dist2[j, m] = !values.f_nan
        dist3[j, m] = !values.f_nan
      endelse
    endfor

    mask = finite(dist2[j, *])
    Y = dist2[j, [where(mask eq 1)]]
    X = dist3[j, [where(mask eq 1)]]

    if finite(Y[0]) ne 0 then begin
      ytmp_top = round(max(Y / 10, /nan)) * 10
      ytmp_bottom = round(min(Y / 10, /nan)) * 10
      yrange = ytmp_top - ytmp_bottom
      ybins = fltarr(round((yrange) / 10) + 1)
      ynbin = n_elements(ybins)
      ybottom = ytmp_bottom

      for i = 0, ynbin - 1 do begin
        ybins[i] = ytmp_bottom
        ytmp_bottom = ytmp_bottom + 10
      endfor

      ybin_counts = intarr(ynbin)

      yn_samples = n_elements(Y)

      Ydist = fltarr(ynbin, yn_samples) - 1
      ybin_width = 10

      ; Fill the histogram and store indices
      for k = 0, yn_samples - 1 do begin
        ; Calculate the bin index for Ey[k], adjusted by the bin width
        ybin_index = floor((Y[k] - ybottom) / ybin_width)
        ; Ensure the bin_index is within the valid range
        if ybin_index ge 0 and ybin_index lt ynbin then begin
          Ydist[ybin_index, ybin_counts[ybin_index]] = k ; Store the index k in the appropriate bin
          ybin_counts[ybin_index] += 1 ; Increment the count for that bin
        endif
      endfor

      ; distY = fltarr(nbin, n_samples) - 1

      mean_Y = fltarr(ynbin)
      mean_X = fltarr(ynbin)
      for l = 0, ynbin - 1 do begin
        valid = where(Ydist[l, *] ne -1, count)
        if count gt 0 then begin
          ; Calculate and store the mean value for the current bin
          ; print, l, X[Ydist[l, valid]]
          mean_X[l] = mean(X[Ydist[l, valid]])
          mean_Y[l] = mean(Y[Ydist[l, valid]])
        endif else begin
          ; If no valid data, store NaN (optional)
          mean_Y[l] = !values.f_nan
          mean_X[l] = !values.f_nan
        endelse
      endfor

      ; Filter out NaN values after the loop
      valid_mean_Y_indices = where(finite(mean_Y), valid_mean_Y_count)
      valid_mean_X_indices = where(finite(mean_X), valid_mean_X_count)

      if valid_mean_Y_count gt 0 then mean_Y = mean_Y[valid_mean_Y_indices]
      if valid_mean_X_count gt 0 then mean_X = mean_X[valid_mean_X_indices]

      if finite(mean_Y[0]) ne 0 then begin
        if n_elements(mean_Y) eq n_elements(mean_X) and n_elements(mean_Y) ge 3 then begin
          result = linfit(mean_X, mean_Y, yfit = yfit)
          ; WINDOW, j
          ; print, bins[j]
          ; plot, mean_X, mean_Y, psym=4, background=255, color=0, THICK=2.0, xtitle='diff sym-H', ytitle='sym-H'
          ; oplot, [MIN(mean_X), MAX(mean_X)], [result[0] + result[1]*MIN(mean_X), result[0] + (result[1])*MAX(mean_X)], color=0

          params[j, 0] = result[0] ; INTERSECCIÓN
          params[j, 1] = result[1] ; PENDIENTE
        endif
      endif
    endif
  endfor

  for i = 0, n_elements(params[*, 0]) - 1 do begin
    if params[i, 0] eq 0 then params[i, 0] = !values.f_nan
    ; print, params[i,0]
  endfor
  mask = finite(params[*, 0])
  dtQ = params[[where(mask eq 1)], 0]

  dtQ = dtQ[1 : n_elements(dtQ) - 1]
  nQ = n_elements(dtQ)
  X = bins[1 : nQ]

  ; stop, 'end of the test'
  err = sqrt(abs(dtQ))
  res = linfit(X, dtQ, yfit = yfit)

  a = res[1]
  print, 'coef a eq= ', a

  RETURN, res
end

function param_b, Q, symH_avr, Ey, P
  on_error, 2
  compile_opt idl2, hidden
  dif_symH = ts_diff(symH_avr, 1) ; fltarr(n_elements(symH_avr))
  for i = 0, n_elements(symH_avr) - 1 do begin
    ; print, symH_avr[i], dif_symH[i], format = '(F12.4, F12.4)'
  endfor

  tmp_top = round(max(P, /nan) * 10) + 4
  tmp_bottom = round(min(P, /nan) * 10) - 4

  range = tmp_top - tmp_bottom
  bins = fltarr(round((range) / 2) + 1)
  nbin = n_elements(bins)
  bottom = float(tmp_bottom) / 10
  for i = 0, nbin - 1 do begin
    bins[i] = tmp_bottom
    tmp_bottom = tmp_bottom + 4
  endfor
  bins = bins / 10

  bin_counts = intarr(nbin)

  n_samples = n_elements(P)

  dist = fltarr(nbin, n_samples) - 1
  bin_width = 0.4

  ; Fill the histogram and store indices
  for k = 0, n_samples - 1 do begin
    ; Calculate the bin index for Ey[k], adjusted by the bin width
    bin_index = floor((P[k] - bottom) / bin_width)
    ; Ensure the bin_index is within the valid range
    if bin_index ge 0 and bin_index lt nbin then begin
      dist[bin_index, bin_counts[bin_index]] = k ; Store the index k in the appropriate bin
      bin_counts[bin_index] += 1 ; Increment the count for that bin
    endif
  endfor

  dist2 = fltarr(nbin, n_samples) - 1
  dist3 = fltarr(nbin, n_samples) - 1
  ; distQ = fltarr(nbin, n_samples) - 1

  concatenated_p_array = replicate(!values.f_nan, n_samples)
  concatenated_dif = replicate(!values.f_nan, n_samples)
  concatenated_s = replicate(!values.f_nan, n_samples)
  for j = 0, nbin - 1 do begin
    for m = 0, n_samples - 1 do begin
      if dist[j, m] ge 0 then begin ; Ensure dist[j,m] is a valid index

        dist2[j, m] = Ey[dist[j, m]]
        dist3[j, m] = Ey[dist[j, m]]
        ; distQ[j, m] = Q[dist[j,m]]
        ; Replace -1 and -99999.9 by NaN
        if dist2[j, m] eq -1.0 or dist2[j, m] eq -99999.9 then dist2[j, m] = !values.f_nan

        if dist3[j, m] eq -1.0 or dist3[j, m] eq -99999.9 then dist3[j, m] = !values.f_nan

        ; IF distQ[j, m] EQ -1.0 OR distQ[j, m] EQ 0.0 THEN distQ[j, m] = !VALUES.F_NAN
      endif else begin
        dist2[j, m] = !values.f_nan
        dist3[j, m] = !values.f_nan
        ; distQ[j, m]= !VALUES.F_NAN
      endelse
    endfor

    mask = finite(dist2[j, *])
    ; mask2 = FINITE(distQ[j, *])
    Y = dist2[j, [where(mask eq 1)]]

    ; Z =  distQ[j, [where(mask2 eq 1)]]  ; Z = Q en espacio fase de Pdyn

    if finite(Y[0]) ne 0 then begin
      ; s = stddev(Y)

      if n_elements(Y) gt 3 then begin
        ; #########################################################################################
        ; #########################################################################################
        ; #########################################################################################
        ; #########################################################################################

        threshold = 0.5
        tmp_top2 = round(max(Y, /nan)) + threshold
        tmp_bottom2 = threshold
        range2 = tmp_top2 - tmp_bottom2
        n_bins_in_range = round((range2) / 2) + 1
        if n_bins_in_range le 0 then n_bins_in_range = 1 ; Ensure there's at least one bin

        bins2 = fltarr(n_bins_in_range)
        nbin2 = n_elements(bins2)

        for l = 0, nbin2 - 1 do begin
          bins2[l] = tmp_bottom2
          tmp_bottom2 = tmp_bottom2 + 2
        endfor

        bin_counts2 = intarr(nbin2)
        n_samples2 = n_elements(Y)

        ndist = fltarr(nbin2, n_samples2) - 1
        bin_width2 = 2

        ; Adjust the loop to avoid out-of-range issues
        for n = 0, n_samples2 - 1 do begin
          bin_index2 = floor((Y[n] - bins2[0]) / bin_width2) ; Adjust bin index calculation

          if bin_index2 ge 0 and bin_index2 lt nbin2 then begin
            ndist[bin_index2, bin_counts2[bin_index2]] = n ; Store the index n in the appropriate bin
            bin_counts2[bin_index2] += 1 ; Increment the count for that bin
          endif
        endfor
        ndist2 = fltarr(nbin2, n_samples2) - 1
        ndist3 = fltarr(nbin2, n_samples2) - 1
        ndistQ = fltarr(nbin2, n_samples2) - 1
        params2 = fltarr(nbin2, 2)
        ; imax = where(Ey GE round(max(Ey)))
        for o = 0, nbin2 - 1 do begin
          for r = 0, n_samples2 - 1 do begin
            if ndist[o, r] ge 0 then begin ; Ensure dist[j,m] is a valid index
              ndist2[o, r] = symH_avr[ndist[o, r]]
              ndist3[o, r] = dif_symH[ndist[o, r]]
              ndistQ[o, r] = Q[ndist[o, r]]
              ; Replace -1 and -99999.9 by NaN
              if ndist2[o, r] eq -1.0 or ndist2[o, r] eq -99999.9 then ndist2[o, r] = !values.f_nan

              if ndist3[o, r] eq -1.0 or ndist3[o, r] eq -99999.9 then ndist3[o, r] = !values.f_nan

              if ndistQ[o, r] eq -1.0 or ndistQ[o, r] eq -99999.9 then ndistQ[o, r] = !values.f_nan
            endif else begin
              ndist2[o, r] = !values.f_nan
              ndist3[o, r] = !values.f_nan
              ndistQ[o, r] = !values.f_nan
            endelse
          endfor

          mask2 = finite(ndist2[o, *])
          Y2 = ndist2[o, [where(mask2 eq 1)]]
          X2 = ndist3[o, [where(mask2 eq 1)]]
          Z2 = ndistQ[o, [where(mask2 eq 1)]]
          ; print, 'dynamic preassure bin: ' + string(bins[j]), 'Electric field bin: ' + string(bins2[o])
          ; print, Z2, format = '(F24.6)'  ;
          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################
          ytmp_top = round(max(Y2 / 10, /nan)) * 10
          ytmp_bottom = round(min(Y2 / 10, /nan)) * 10
          yrange = ytmp_top - ytmp_bottom
          ybins = fltarr(round((yrange) / 10) + 1)
          ynbin = n_elements(ybins)
          ybottom = ytmp_bottom

          for t = 0, ynbin - 1 do begin
            ybins[t] = ytmp_bottom
            ytmp_bottom = ytmp_bottom + 10
          endfor

          ybin_counts = intarr(ynbin)

          yn_samples = n_elements(Y2)

          Ydist = fltarr(ynbin, yn_samples) - 1
          ybin_width = 10

          ; Fill the histogram and store indices
          for u = 0, yn_samples - 1 do begin
            ; Calculate the bin index for Ey[k], adjusted by the bin width
            ybin_index = floor((Y2[u] - ybottom) / ybin_width)
            ; Ensure the bin_index is within the valid range
            if ybin_index ge 0 and ybin_index lt ynbin then begin
              Ydist[ybin_index, ybin_counts[ybin_index]] = u ; Store the index k in the appropriate bin
              ybin_counts[ybin_index] += 1 ; Increment the count for that bin
            endif
          endfor

          ; distY = fltarr(nbin, n_samples) - 1

          mean_Y = fltarr(ynbin)
          mean_X = fltarr(ynbin)
          for v = 0, ynbin - 1 do begin
            valid = where(Ydist[v, *] ne -1, count)
            if count gt 0 then begin
              ; Calculate and store the mean value for the current bin
              ; print, l, X[Ydist[l, valid]]
              mean_X[v] = mean(X2[Ydist[v, valid]])
              mean_Y[v] = mean(Y2[Ydist[v, valid]])
            endif else begin
              ; If no valid data, store NaN (optional)
              mean_Y[v] = !values.f_nan
              mean_X[v] = !values.f_nan
            endelse
          endfor

          ; Filter out NaN values after the loop
          valid_mean_Y_indices = where(finite(mean_Y), valid_mean_Y_count)
          valid_mean_X_indices = where(finite(mean_X), valid_mean_X_count)

          if valid_mean_Y_count gt 0 then mean_Y = mean_Y[valid_mean_Y_indices]
          if valid_mean_X_count gt 0 then mean_X = mean_X[valid_mean_X_indices]

          if finite(mean_Y[0]) ne 0 then begin
            if n_elements(mean_Y) eq n_elements(mean_X) and n_elements(mean_Y) ge 3 then begin
              result = linfit(mean_X, mean_Y, yfit = yfit)
              ; WINDOW, o
              ; print, bins[j]
              ; plot, mean_X, mean_Y, psym=4, background=255, color=0, THICK=2.0, xtitle='diff sym-H', ytitle='sym-H'
              ; oplot, [MIN(mean_X), MAX(mean_X)], [result[0] + result[1]*MIN(mean_X), result[0] + (result[1])*MAX(mean_X)], color=0

              params2[o, 0] = result[0] ; INTERSECCIÓN
              params2[o, 1] = result[1] ; PENDIENTE
              ; print, params2[o,0]
            endif
          endif

          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################
          ; #########################################################################################

          dif = params2[o, 0] - Z2
          ; print,params2[o,0]
          dif = reform(dif, n_elements(dif))

          if count ne 0 then begin
            p_array = replicate(bins[j], n_elements(dif))
            ; print, dif
            concatenated_p_array = [concatenated_p_array, p_array]

            ; s = 1/sqrt(stddev(dif))
            ; s_array = REPLICATE(s, n_elements(dif))
            ; concatenated_s = [concatenated_s, s_array]

            concatenated_dif = [concatenated_dif, dif]
          endif
        endfor ; fin de o
      endif
    endif

    ; print, bins[j]
  endfor ; fin de j

  p_array = concatenated_p_array[where(finite(concatenated_p_array) eq 1)]
  dif_array = concatenated_dif[where(finite(concatenated_dif) eq 1)]
  ; Step 1: Create a mask to exclude bins with zeroes

  ; Now use the filtered arrays with limfit

  ; s_array = concatenated_s[WHERE(FINITE(concatenated_s) EQ 1)] ; error = 1/sqrt(stddev(dif_array))

  ; Initialize an empty array to store the concatenated values

  window, 6, xsize = 600, ysize = 600, title = 'offset-Q vs P^(1/2)'
  PLOT, p_array, dif_array, psym = 4, xstyle = 2, thick = 3
  ; print, p_array
  res = linfit(p_array, dif_array, yfit = yfit)

  oplot, [min(p_array), max(p_array)], [res[0] + res[1] * min(p_array), res[0] + (res[1]) * max(p_array)]
  RETURN, res[1]
end

; function sym_v_p, Ey, symH, Pdyn, mw2

; return,
; end

function dst_0, date_i, date_f
  on_error, 2
  compile_opt idl2, hidden

  ; RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
  @set_up_commons
  set_up

  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]

  idx = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'm')
  symH = idx.symH
  asyH = idx.asyH

  ip = e_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
  E = ip.ey
  P = ip.pdyn
  Pdyn = sqrt(P)

  timeax = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), $
    final = julday(mh_f, dy_f, yr_f, 23, 59), units = 'Minutes', step_size = 1)

  timeip = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), $
    final = julday(mh_f, dy_f, yr_f, 23, 55), units = 'Minutes', step_size = 5)

  timeh = timegen(start = julday(mh_i, dy_i, yr_i, 0), $
    final = julday(mh_f, dy_f, yr_f, 23), units = 'Hours', step_size = 1)
  date_label = label_date(date_format = ['%D', '%M %Y'])
  caldat, timeip, mh, dy, yr, hr, min

  ; #######################################################################################################
  mw = 30
  rate = mw / 5
  symH_avr = fltarr(n_elements(symH) / mw)
  Ey = fltarr(n_elements(E) / rate)
  asyH_sigma = fltarr(n_elements(symH) / mw)
  P_sqrt = fltarr(n_elements(E) / rate)

  for i = 0, n_elements(asyH_sigma) - 1 do begin
    symH_avr[i] = stddev(asyH[i * mw : (i + 1) * mw - 1], /nan)
  endfor

  for i = 0, n_elements(symH_avr) - 1 do begin
    symH_avr[i] = mean(symH[i * mw : (i + 1) * mw - 1], /nan)
  endfor

  for i = 0, n_elements(Ey) - 1 do begin
    Ey[i] = mean(E[i * rate : (i + 1) * rate - 1], /nan)
  endfor

  for i = 0, n_elements(P_sqrt) - 1 do begin
    P_sqrt[i] = sqrt(mean(P[i * rate : (i + 1) * rate - 1], /nan))
  endfor

  Q_coeff = param_Q(Ey, symH_avr)
  a = Q_coeff[1]

  Q = Qfunc(a, E)
  Q_1min = extrapol(Q, n_elements(symH))
  ; print, Q
  ; P_dif = TS_DIFF(P_sqrt, 1)

  fac_mcp = -0.18
  tau_mcpherron = exp(fac_mcp * E + 2.41)

  fac_bgon = -0.09
  tau_ballgon = exp(fac_bgon * E + 2.2)

  mc_pherron = {b: 12.58, c: 33.24, tau: tau_mcpherron}

  ballarta_gon = {b: 10.01, c: 32.18, tau: tau_ballgon}
  ; b = param_b(Q, symH_avr, Ey, P_dif)

  ; c = b*(total(sqrt(P_sqrt), /NAN)/n_elements(P_dif))

  ; print, 'parametro b = ', b
  ; print, 'parametro c = ', c
  P_sqrt_1min = extrapol(P_sqrt, n_elements(symH))

  symH_0 = symH - (mc_pherron.b * P_sqrt_1min) + mc_pherron.c

  values = {symH_0: symH_0, q: Q_1min}

  dir = set_var.mega_dir + 'sym_0/'
  test = file_test(dir, /directory)
  if test eq 0 then begin
    file_mkdir, dir
    print, 'PATH directory ' + dir
    print, 'created'
  endif else begin
    print, ''
  endelse

  ndays = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  outfile = strarr(ndays)
  string_date = strarr(ndays)
  ; values.symH_0 = add_nan(values.symH_0, !VALUES.F_NAN, 'equal')
  i_nan = where(~finite(values.symH_0), count)
  ; nan_indices = where(values.symH_0 eq !VALUES.F_NAN, count)
  if count gt 0 then values.symH_0[i_nan] = 9999
  ; print, i_nan

  for i = 0, ndays - 1 do begin
    tmp_year = 0
    tmp_month = 0
    tmp_day = 0
    tmp_julday = julday(mh_i, dy_i, yr_i)
    caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
    string_date[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,I02,I02)')

    outfile[i] = dir + 'sym0_' + string_date[i] + '.dat'
    openw, LUN, outfile[i], /get_lun

    ; Get the corresponding data for the day
    symH_0_day = values.symH_0[i * 1440 : (i + 1) * 1440 - 1]
    Q_day = values.q[i * 1440 : (i + 1) * 1440 - 1]

    ; Loop through each value of the day and print as columns
    for j = 0, 1439 do begin
      printf, LUN, symH_0_day[j], Q_day[j], format = '(F10.4,1X,F10.4)'
    endfor

    close, LUN
    free_lun, LUN
  endfor

  return, values
end
