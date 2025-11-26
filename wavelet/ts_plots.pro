pro ts_plots, asymH, symH, H, SQ, Bdiono, res, date_i, date_f, path, station_code
  on_error, 2
  compile_opt idl2, hidden
  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]
  ; ###############################################################################
  @set_up_commons
  set_up

  resolution = 0
  sample = 0
  if res eq 'h' then begin
    resolution = 'Hours'
    sample = 24
  endif else begin
    resolution = 'Minutes'
    sample = 1440
  endelse

  date_time = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), $
    final = julday(mh_f, dy_f, yr_f, 23, 59), units = resolution)
  date_label = label_date(date_format = ['%D', '%M %Y'])
  Date = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  psfile = path + station_code + '_' + Date + '.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 10, ysize = 8

  class = gms_class(station_code)
  info = stationlist(class, station_code)

  print, 'UTC: ', info.utc
  ; DEVICE, true=24, retain=2, decomposed=0
  ; TVLCT, R_bak, G_bak, B_bak, /GET
  ; LOADCT, 39
  ; WINDOW, 1, XSIZE=800, YSIZE=500, TITLE='GS'

  if max(H) gt max(symH) then up = max(H) else up = max(symH)
  if min(H) lt min(symH) then down = min(H) else down = min(symH)

  ; panel a
  cgPlot, date_time, symH, background = 'white', color = 'black', position = [.1, .65, .92, .9], xtickformat = ['LABEL_DATE'], $
    xminor = 8, xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, $
    yrange = [down, up], /nodata

  ndata = n_elements(symH) - 1

  info_lt = mlt(station_code, date_time)
  ; local_time = info_lt.glt
  utc = info_lt.utc_lt
  print, 'UTC offset (hours): ', utc
  jul_conv = abs((0.1 / 2.4) * utc)

  if utc lt 0 then begin
    local_ini = date_time[0] + (jul_conv)
    local_fin = date_time[n_elements(date_time) - 1] + jul_conv
  endif else begin
    local_ini = date_time[0] - jul_conv
    local_fin = date_time[n_elements(date_time) - 1] - jul_conv
  endelse

  local_time = timegen(start = local_ini, final = local_fin, units = resolution)

  midsample = sample / 2

  midday = fltarr(n_elements(local_time) / midsample)

  midddays = n_elements(symH) / midsample

  if utc lt 0 then begin
    if local_time[(0 * midsample)] - 0.25 ge date_time[0] then begin
      cgPolygon, [local_time[(0 * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[(0 * midsample)] - 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [date_time[0], local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, date_time[0]], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse

    print, ndata
    if date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) le 0.5 and date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) ge 0 then begin
      cgPolygon, [local_time[(midddays * (midsample - 1))] - 0.25, date_time[ndata], date_time[ndata], local_time[(midddays * (midsample - 1))] - 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif
  endif else begin
    if local_time[(0 * midsample)] - 0.25 ge date_time[0] then begin
      cgPolygon, [local_time[(0 * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, local_time[(0 * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [date_time[0], local_time[((1) * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, date_time[0]], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse

    if date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) le 0 then begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, date_time[ndata], date_time[ndata], local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse
  endelse

  for i = 0, n_elements(midday) - 1 do begin
    ; Define color based on even/odd index
    if utc lt 0 then begin
      if (i mod 2) eq 0 then color_shade = 'white' else color_shade = 'light gray'

      if i lt n_elements(midday) - 1 and i gt 0 then begin
        cgPolygon, [local_time[(i * midsample)] + 0.25, local_time[((i + 1) * midsample)] + 0.25, local_time[((i + 1) * midsample)] + 0.25, local_time[(i * midsample)] + 0.25], $
          [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = color_shade, /fill
      endif
    endif else begin
      if (i mod 2) eq 0 then color_shade = 'light gray' else color_shade = 'white'
      if i lt n_elements(midday) - 1 and i gt 0 then begin
        cgPolygon, [local_time[(i * midsample)] - 0.25, local_time[((i + 1) * midsample)] - 0.25, local_time[((i + 1) * midsample)] - 0.25, local_time[(i * midsample)] - 0.25], $
          [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = color_shade, /fill
      endif
    endelse
  endfor

  cgOPlot, date_time, symH, color = 'GRN5', thick = 2, linestyle = 0
  cgOPlot, date_time, H, color = 'black', thick = 2, linestyle = 0

  cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 1, thick = 2, color = 'black'
  ; first panel legend
  ; cgPolygon, [0.78,0.81,0.81,0.78], [0.661,0.661,0.664,0.664], color = 'orange', /NORMAL, /FILL
  cgPolygon, [0.78, 0.81, 0.81, 0.78], [0.72, 0.72, 0.723, 0.723], color = 'GRN5', /normal, /fill
  cgPolygon, [0.78, 0.81, 0.81, 0.78], [0.69, 0.69, 0.692, 0.692], color = 'black', /normal, /fill

  d_h = Textoidl('\Delta H')
  ; XYOUTS, 0.822, 0.657 , /NORMAL, 'ASYM-H', CHARSIZE = 1.2, CHARTHICK=chr_thick1

  xyouts, 0.822, 0.715, /normal, 'SYM-H', charsize = 1.2, charthick = chr_thick1

  xyouts, 0.822, 0.685, /normal, d_h, charsize = 1.2, charthick = chr_thick1

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xtitle = '', $
    xstyle = 1, $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.04

  cgAxis, yaxis = 0, yrange = [down, up], $
    ytitle = 'G. Indices [nT]', $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [down, up], $
    ; COLOR=negro, $
    ystyle = 1, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.6

  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; panel b
  down = min(asymH)
  up = max(asymH)

  cgPlot, date_time, symH, background = 'white', color = 'black', position = [.1, .38, .92, .63], xtickformat = ['LABEL_DATE'], $
    xminor = 8, xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, $
    yrange = [down, up], /nodata, /noerase

  ndata = n_elements(symH) - 1

  if utc lt 0 then begin
    if local_time[(0 * midsample)] - 0.25 ge date_time[0] then begin
      cgPolygon, [local_time[(0 * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[(0 * midsample)] - 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [date_time[0], local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, date_time[0]], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse

    print, ndata
    if date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) le 0.5 and date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) ge 0 then begin
      cgPolygon, [local_time[(midddays * (midsample - 1))] - 0.25, date_time[ndata], date_time[ndata], local_time[(midddays * (midsample - 1))] - 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif
  endif else begin
    if local_time[(0 * midsample)] - 0.25 ge date_time[0] then begin
      cgPolygon, [local_time[(0 * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, local_time[(0 * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [date_time[0], local_time[((1) * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, date_time[0]], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse

    if date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) le 0 then begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, date_time[ndata], date_time[ndata], local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse
  endelse

  for i = 0, n_elements(midday) - 1 do begin
    ; Define color based on even/odd index
    if utc lt 0 then begin
      if (i mod 2) eq 0 then color_shade = 'white' else color_shade = 'light gray'

      if i lt n_elements(midday) - 1 and i gt 0 then begin
        cgPolygon, [local_time[(i * midsample)] + 0.25, local_time[((i + 1) * midsample)] + 0.25, local_time[((i + 1) * midsample)] + 0.25, local_time[(i * midsample)] + 0.25], $
          [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = color_shade, /fill
      endif
    endif else begin
      if (i mod 2) eq 0 then color_shade = 'light gray' else color_shade = 'white'
      if i lt n_elements(midday) - 1 and i gt 0 then begin
        cgPolygon, [local_time[(i * midsample)] - 0.25, local_time[((i + 1) * midsample)] - 0.25, local_time[((i + 1) * midsample)] - 0.25, local_time[(i * midsample)] - 0.25], $
          [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = color_shade, /fill
      endif
    endelse
  endfor

  cgOPlot, date_time, asymH, color = 'orange', thick = 2

  cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 1, thick = 2, color = 'black'
  ; first panel legend

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xtitle = '', $
    xstyle = 1, $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.04

  cgAxis, yaxis = 0, yrange = [down, up], $
    ytitle = 'G. Indices [nT]', $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [down, up], $
    ; COLOR=negro, $
    ystyle = 1, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.6

  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; panel c
  updiono = max(Bdiono)
  downdiono = min(Bdiono)

  cgPlot, date_time, Bdiono, background = 'white', color = 'black', position = [.1, .11, .92, .36], xtickformat = ['LABEL_DATE'], $
    xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, yrange = [downdiono, updiono], /noerase, /nodata

  if utc lt 0 then begin
    if local_time[(0 * midsample)] - 0.25 ge date_time[0] then begin
      cgPolygon, [local_time[(0 * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[(0 * midsample)] - 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [date_time[0], local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, date_time[0]], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse

    print, ndata
    if date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) le 0.5 and date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) ge 0 then begin
      cgPolygon, [local_time[(midddays * (midsample - 1))] - 0.25, date_time[ndata], date_time[ndata], local_time[(midddays * (midsample - 1))] - 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif
  endif else begin
    if local_time[(0 * midsample)] - 0.25 ge date_time[0] then begin
      cgPolygon, [local_time[(0 * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, local_time[(0 * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [date_time[0], local_time[((1) * midsample)] + 0.25, local_time[((1) * midsample)] + 0.25, date_time[0]], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse

    if date_time[ndata] - (local_time[(midddays * (midsample - 1))] - 0.25) le 0 then begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, date_time[ndata], date_time[ndata], local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse
  endelse

  for i = 0, n_elements(midday) - 1 do begin
    ; Define color based on even/odd index
    if utc lt 0 then begin
      if (i mod 2) eq 0 then color_shade = 'white' else color_shade = 'light gray'

      if i lt n_elements(midday) - 1 and i gt 0 then begin
        cgPolygon, [local_time[(i * midsample)] + 0.25, local_time[((i + 1) * midsample)] + 0.25, local_time[((i + 1) * midsample)] + 0.25, local_time[(i * midsample)] + 0.25], $
          [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = color_shade, /fill
      endif
    endif else begin
      if (i mod 2) eq 0 then color_shade = 'light gray' else color_shade = 'white'
      if i lt n_elements(midday) - 1 and i gt 0 then begin
        cgPolygon, [local_time[(i * midsample)] - 0.25, local_time[((i + 1) * midsample)] - 0.25, local_time[((i + 1) * midsample)] - 0.25, local_time[(i * midsample)] - 0.25], $
          [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = color_shade, /fill
      endif
    endelse
  endfor

  cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 1, thick = 2, color = 'black'
  cgOPlot, date_time, Bdiono, color = 'red', thick = 3
  ; cgoplot,date_time, SQ, color='blue', thick =3

  cgPolygon, [0.78, 0.81, 0.81, 0.78], [0.18, 0.18, 0.182, 0.182], color = 'red', /normal, /fill
  cgPolygon, [0.78, 0.81, 0.81, 0.78], [0.148, 0.148, 0.145, 0.145], color = 'blue', /normal, /fill

  P_I = Textoidl('D_{I,' + string(station_code) + '}')
  h_sq = Textoidl('H_{SQ}')
  xyouts, 0.822, 0.175, /normal, P_I, charsize = 1.2, charthick = chr_thick1

  xyouts, 0.822, 0.135, /normal, h_sq, charsize = 1.2, charthick = chr_thick1

  month = month_name(mh_i, 'english')
  xtitle = Textoidl('Universal Time [h], ' + month + ' ' + string(yr_i, format = '(I04)'))
  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtitle = xtitle, $
    xstyle = 1, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    ; XTICKFORMAT='(A1)',$
    ; COLOR=negro, $
    charsize = 1.2, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.04

  cgAxis, yaxis = 0, yrange = [downdiono, updiono], $
    ytitle = '[nT]', $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [downdiono, updiono], $
    ytitle = '', $
    ytickformat = '(A1)', $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################

  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  title = string(strupcase(station_code), info.mlat, info.mhem, info.mlon, info.mhem2, $
    format = '("Obs: ", A, ", mlat: ", F7.2, " ", A, ",    ", "mlon: ", F7.2," ", A)')

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  y = 0.93
  xyouts, x, y, title, /normal, $
    alignment = 0.5, charsize = 1.65

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  y = 0.02

  cgPS_Close, density = 300, width = 1600 ; , /PNG
end

pro ip_plots, symH, P, V, T, E, Bz, Bt, AE, PCN, date_i, date_f, path
  on_error, 2
  compile_opt idl2, hidden
  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]
  ; ###############################################################################
  @set_up_commons
  set_up

  date_time = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), $
    final = julday(mh_f, dy_f, yr_f, 23, 59), units = 'Minutes')
  date_label = label_date(date_format = ['%D', '%M %Y'])
  Date = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  psfile = path + 'ip_' + Date + '.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 10, ysize = 10
  ; DEVICE, true=24, retain=2, decomposed=0
  ; TVLCT, R_bak, G_bak, B_bak, /GET
  ; LOADCT, 39
  ; WINDOW, 1, XSIZE=800, YSIZE=500, TITLE='GS'

  if min(Bz) lt min(Bt) then down = min(Bz) else down = min(Bt)
  if max(Bz) gt max(Bt) then up = max(Bz) else up = max(Bt)
  cgPlot, date_time, Bz, background = 'white', color = 'black', position = [.1, .76, .92, .98], xtickformat = ['LABEL_DATE'], $
    xminor = 8, xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, yrange = [down, up] ; , /nodata

  cgText, 0.12, 0.95, '(a)', charthick = 3, charsize = 2, /normal, font = 1, tt_font = 'Helvetica Bold'

  cgOPlot, date_time, Bz, color = 'red', thick = 3, linestyle = 0
  cgOPlot, date_time, Bt, color = 'black', thick = 2, linestyle = 0

  cgOPlot, [date_time[3150], date_time[3150]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgOPlot, [date_time[3600], date_time[3600]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgOPlot, [date_time[4320], date_time[4320]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 1, thick = 2, color = 'black'
  ; first panel legend
  cgPolygon, [0.72, 0.75, 0.75, 0.72], [0.793, 0.793, 0.796, 0.796], color = 'black', /normal, /fill
  cgPolygon, [0.81, 0.84, 0.84, 0.81], [0.793, 0.793, 0.796, 0.796], color = 'red', /normal, /fill

  B_z = Textoidl('B_Z')
  B_T = Textoidl('B_T, ')
  xyouts, 0.76, 0.79, /normal, B_T, charsize = 1.2, charthick = chr_thick1

  xyouts, 0.852, 0.79, /normal, B_z, charsize = 1.2, charthick = chr_thick1

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xtitle = '', $
    xstyle = 1, $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.04

  cgAxis, yaxis = 0, yrange = [down, up], $
    ytitle = 'IMF [nT]', $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [down, up], $
    ; COLOR=negro, $
    ystyle = 1, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.6

  ; ##################################################################################################################
  ; ##################################################################################################################

  cgPlot, date_time, E, background = 'white', color = 'black', position = [.1, .53, .92, .75], xtickformat = ['LABEL_DATE'], $
    xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, yrange = [min(E), max(E)], $
    /noerase, /nodata

  cgText, 0.12, 0.72, '(b)', charthick = 3, charsize = 2, /normal, font = 1, tt_font = 'Helvetica Bold'

  cgOPlot, [date_time[3150], date_time[3150]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgOPlot, [date_time[3600], date_time[3600]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgOPlot, [date_time[4320], date_time[4320]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgPolygon, [date_time[3660], date_time[3960], date_time[3960], date_time[3660]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'violet', /fill

  cgOPlot, date_time, E, color = 'black', thick = 4
  cgOPlot, date_time, PCN, color = 'GRN6', thick = 3
  cgOPlot, [!x.crange[0], !x.crange[1]], [5, 5], $
    linestyle = 2, thick = 2, color = 'blue' ; IP shock

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xtitle = '', $
    xstyle = 1, $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.04
  E_Y = Textoidl('E & PCN [mV m^{-1}] ')

  cgAxis, yaxis = 0, yrange = [min(E), max(E)], $
    ytitle = E_Y, $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [min(E), max(E)], $
    ; ytitle = 'PCN [mV/m]', $
    ytickformat = '(A1)', $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.8

  ; ##################################################################################################################
  ; ##################################################################################################################
  ; cgPlot, date_time, PCN, background = 'white', color = 'black', position = [.1, .53, .92, .75], xtickformat = ['LABEL_DATE'], $
  ; xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, yrange = [min(PCN), max(PCN)], $
  ; /noerase, /nodata

  ; cgAxis, yaxis = 1, yrange = [min(PCN), max(PCN)], $
  ; ytitle = 'PCN [mV/m]', $
  ; ytickformat = '(A1)', $
  ; ystyle = 1, $
  ; charsize = 1.2, $
  ; charthick = 1.8

  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################

  cgPlot, date_time, P, background = 'white', color = 'black', position = [.1, .30, .92, .52], xtickformat = ['LABEL_DATE'], $
    xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, /noerase, /nodata

  t_p = acc_threshold(P, 0.95)
  cgText, 0.12, 0.49, '(c)', charthick = 3, charsize = 2, /normal, font = 1, tt_font = 'Helvetica Bold'

  cgOPlot, [date_time[3150], date_time[3150]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgOPlot, [date_time[3600], date_time[3600]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgOPlot, [date_time[4320], date_time[4320]], [!y.crange[0], !y.crange[1]], $
    linestyle = 2, thick = 2, color = 'black' ; IP shock

  cgPolygon, [date_time[3660], date_time[3960], date_time[3960], date_time[3660]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'violet', /fill

  cgOPlot, date_time, P, color = 'black', thick = 4

  cgOPlot, [!x.crange[0], !x.crange[1]], [t_p, t_p], $
    linestyle = 2, thick = 2, color = 'blue' ; IP shock

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xtitle = '', $
    xstyle = 1, $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.04

  cgAxis, yaxis = 0, yrange = [min(P), max(P)], $
    ; /ylog, $
    ytitle = 'P [nPa]', $
    color = 'black', $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [min(P), max(P)], $
    ; /ylog, $
    ; ytitle = T_p + ' [nT]', $
    ytickformat = '(A1)', $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################

  ; cgPlot, date_time, P, background = 'white', color = 'black', position = [.1, .3, .92, .52], xtickformat = ['LABEL_DATE'], $
  ; xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, /noerase, /nodata

  ; cgOPlot, date_time, P, color = 'blue', thick = 1
  ; proton = Textoidl('P_{din} [nPa]')
  ; cgAxis, yaxis = 1, yrange = [min(P), max(P)], $
  ; /ylog, $
  ; ytitle = proton, $
  ; color = 'blue', $
  ; ystyle = 1, $
  ; YTICKFORMAT='(A1)',$
  ; charsize = 1.2, $
  ; charthick = 1.8
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  ; ##################################################################################################################
  up = max(AE)
  down = min(symH)
  cgPlot, date_time, symH, background = 'white', color = 'black', position = [.1, .07, .92, .29], xtickformat = ['LABEL_DATE'], $
    xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, yrange = [down, up], /noerase, /nodata

  cgText, 0.12, 0.1, '(e)', charthick = 3, charsize = 2, /normal, font = 1, tt_font = 'Helvetica Bold'

  for i = 0, n_elements(Q) - 1 do begin
    if Q[i] eq 0 then Q[i] = !values.f_nan
  endfor

  cgPolygon, [date_time[3150], date_time[3300], date_time[3300], date_time[3150]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill

  cgPolygon, [date_time[3300], date_time[3600], date_time[3600], date_time[3300]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'gray', /fill

  cgPolygon, [date_time[3600], date_time[4260], date_time[4260], date_time[3600]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'gray', /fill

  cgPolygon, [date_time[3660], date_time[3960], date_time[3960], date_time[3660]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'violet', /fill

  cgOPlot, date_time, symH, color = 'GRN5', thick = 3
  cgOPlot, date_time, AE, color = 'orange', thick = 3
  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtitle = 'Universal Time [days], March 2015', $
    xstyle = 1, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = ['LABEL_DATE'], $
    ; COLOR=negro, $
    charsize = 1.2, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 8, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.04

  cgAxis, yaxis = 0, yrange = [down, up], $
    ytitle = 'SYMH / ASYH [nT]', $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [down, up], $
    ; COLOR=negro, $
    ystyle = 1, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.6

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  y = 0.93
  xyouts, x, y, '', /normal, $
    alignment = 0.5, charsize = 1.65

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  y = 0.02

  cgPS_Close, density = 300, width = 1600 ; , /PNG
end
