pro diono_figures, date_time, local_time, asymH, diono, H, date_i, date_f, utc, station_code, window, window2
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

  up = 280
  down = -300

  sample = 1440

  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  ndata = n_elements(diono) - 1
  ; date_label = label_date(date_format = ['%D', '%M', '%Y'])
  date_label = label_date(date_format = ['%Y/%M/%D'])
  Date = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')

  path = '/home/isaac/longitudinal_studio/fig/magdata/'
  psfile = path + station_code + '_' + Date + 'm' + '.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 10, ysize = 4
  X_label = xlabel([yr_i, mh_i, dy_i], file_number)
  old_month = month_name(mh_i, 'english')

  class = gms_class(station_code)
  info = stationlist(class, station_code)

  time_title = ' UT [days]'
  title = string(strupcase(station_code), info.mlat, info.mhem, info.mlon, info.mhem2, $
    format = '(A, ", magnetic lat: ", F7.2, " ", A, ", ", "magnetic lon: ", F7.2," ", A)')

  cgPlot, date_time, diono, xticks = file_number, xminor = 8, background = 'white', $
    color = 'black', charsize = 1, charthick = 1, $
    position = [0.1, 0.1, 0.99, 0.9], xstyle = 5, ystyle = 5, xtickformat = ['LABEL_DATE'], xtickunits = ['day'], $
    xticklayout = 1, xtickinterval = 1, yrange = [down, up], /noerase, /nodata

  midsample = sample / 2

  midday = fltarr(n_elements(local_time) / midsample)

  midddays = n_elements(diono) / midsample

  if utc lt 0 then begin
    if local_time[(0 * midsample)] - 0.25 ge date_time[0] then begin
      cgPolygon, [local_time[(0 * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, local_time[(0 * midsample)] - 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [date_time[0], local_time[((1) * midsample)] - 0.25, local_time[((1) * midsample)] - 0.25, date_time[0]], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endelse

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

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  cgOPlot, date_time, asymH, color = 'orange', thick = 3
  cgOPlot, date_time, diono, color = 'red', linestyle = 0, thick = 3
  cgOPlot, date_time, H, color = 'black', thick = 3

  ; cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 1, thick = 4, color = 'black'
  cgOPlot, [date_time[window[0]], date_time[window[0]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  cgOPlot, [date_time[window[1]], date_time[window[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  cgOPlot, [date_time[window2[1]], date_time[window2[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  month = month_name(mh_i, 'english')
  xtitle = Textoidl('Universal Time[m], ' + month + ' ' + string(yr_i, format = '(I04)'))

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 24, $
    ; xtitle = xtitle, $
    xstyle = 1, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = ['LABEL_DATE'], $
    ; COLOR=negro, $
    charsize = 1.4, $
    ticklen = 0.08, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 24, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.0, $
    charthick = 1.5, $
    ticklen = 0.08
  obs = Textoidl('D_I')
  ytittle = string(obs, strupcase(station_code), format = '(A, " (",A,")", " [nT]")')
  cgAxis, yaxis = 0, $
    ytitle = ytittle, $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.4, $
    charthick = 1.6

  cgAxis, yaxis = 1, $
    ; COLOR=negro, $
    ystyle = 1, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.6
  ; ###############################################################################
  ; ###############################################################################

  x = (date_time[window[1]] - date_time[window[0]]) / 2. + date_time[window[0]]
  y = 298
  xyouts, x, y, 'TW 1', $
    alignment = 0.5, charsize = 1.2

  x = (date_time[window2[1]] - date_time[window2[0]]) / 2. + date_time[window2[0]]
  y = 298
  xyouts, x, y, 'TW 2', $
    alignment = 0.5, charsize = 1.2

  cgPolygon, [date_time[window[0]], date_time[window[1]], date_time[window[1]], date_time[window[0]]], $
    [287, 287, 292, 292], color = 'blue', /fill
  cgPolygon, [date_time[window2[0]], date_time[window2[1]], date_time[window2[1]], date_time[window2[0]]], $
    [287, 287, 292, 292], color = 'green', /fill

  ; ###############################################################################
  !p.font = 1
  ; XYOuts, 0.53, 0.735, '(a)', /Normal, $
  ; Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font= 3
  ; ###############################################################################

  cgPS_Close, density = 300, width = 1600 ; , /PNG
end
