pro diono_recons, date_i, date_f, station_code
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

  res = 'm'
  ; If no station_code is provided or it's an empty string, ask for user input
  if n_params() lt 3 then begin
    station_class = ''
    print, 'Enter GMS class code: 0: regmex or 1: intermagnet'
    read, station_class, prompt = '> '

    case station_class of
      '0': station_class = 'regmex'
      '1': station_class = 'intermagnet'
      else: begin
        print, 'Non-available GMS class. Exiting.'
        RETURN
      end
    endcase

    print, 'Enter GMS idx: If you do not know the GMS idx, please run PRO gms_code_table'
    read, station_idx, prompt = '> '

    ; ###############################################################################
    ; Assign station code based on selected class
    if station_class eq 'regmex' then begin
      station = set_var.gms[fix(station_idx)]
      station_code = set_var.gms_code[fix(station_idx)]
    endif

    if station_class eq 'intermagnet' then begin
      station = set_var.gmsi[fix(station_idx)]
      station_code = set_var.gmsi_code[fix(station_idx)]
    endif

    print, 'GMS selected: ' + station + ' IAGA code: ' + station_code
  endif
  resolution = 0
  sample = 0
  if res eq 'h' then begin
    resolution = 'Hours'
    sample = 24
  endif else begin
    resolution = 'Minutes'
    sample = 1440
  endelse

  ; ###############################################################################
  ; ###############################################################################

  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  idate0 = string(yr_i, mh_i, format = '(I4,I02)')
  TGM_n = event_case([yr_i, mh_i, dy_i])
  ; ###############################################################################
  date_time = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), $
    final = julday(mh_f, dy_f, yr_f, 23, 59), units = resolution)
  date_label = label_date(date_format = ['%Y/%M/%D'])
  Date = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
  ; ###############################################################################
  ; ###############################################################################
  ; generate local time data

  class = gms_class(station_code)
  info = stationlist(class, station_code)
  mlat = info.mlat

  jul_conv = abs((0.1 / 2.4) * info.utc)
  info_lt = mlt(station_code, date_time)
  ; local_time = info_lt.glt
  utc = info_lt.utc_lt

  jul_conv = abs((0.1 / 2.4) * utc)

  if utc lt 0 then begin
    local_ini = date_time[0] - (jul_conv)
    local_fin = date_time[n_elements(date_time) - 1] + jul_conv
  endif else begin
    local_ini = date_time[0] + jul_conv
    local_fin = date_time[n_elements(date_time) - 1] - jul_conv
  endelse

  local_time = timegen(start = local_ini, final = local_fin, units = resolution)
  caldat, date_time, mh_lt, dy_lt, yr_lt, hr_lt, min_lt
  ; ###############################################################################

  data = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
  H = data.h
  SQ = data.sq
  idx = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], res)
  symH = idx.symH
  asymH = idx.asyH
  H = add_nan(H, 99999.0, 'equal')
  H = add_nan(H, 200.0, 'greater')
  a = ae_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])

  ; AE = a.AE
  ; AL = a.AL
  ; symH0 = fillnan(symH0)
  H = fillnan(H)

  df = tec_2015_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code)
  med_tec = med_tec([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code)

  tec = df.tec
  tec = add_nan(tec, 9999.0, 'equal')

  mlat_rad = mlat * !pi / 180.

  H2 = H - (asymH * cos(mlat_rad))
  if res eq 'm' then begin
    dionstr = gen_diono(symH, H, mlat, 'm', '30', station_code, dig_filter = 'dig_filter')
    dionstr2 = gen_diono(symH, H2, mlat, 'm', '30', station_code, dig_filter = 'dig_filter')
  endif else begin
    H_hr = fltarr(n_elements(H) / 60)
    for i = 0, n_elements(H_hr) - 1 do begin
      H_hr[i] = median(H[i * 60 : (i + 1) * 60 - 1])
    endfor
    dionstr = gen_diono(symH, H_hr, mlat, 'h', '30', station_code, dig_filter = 'dig_filter')
  endelse
  ; compute frequencies
  f_k = dionstr.f_k
  fn = dionstr.fn

  ; compute and define Power Spectrum
  pws = dionstr.pws
  ; pws = pws/SQRT(TOTAL(pws^2))
  ; compute diono variables
  diono = dionstr.diono
  ; PRINT, dst
  dp2 = dionstr.dp2
  ddyn = dionstr.ddyn

  dp2_2 = dionstr2.dp2
  ; prc = dionstr.prc
  ; prc2 = dionstr.prc2

  ; ppef_mod = ppfm_array(station_code)
  ; print, ppef_mod
  ; date_time_5min = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), $
  ; final = julday(mh_f, dy_f, yr_f, 23, 59), units = 'min', step_size = 5)

  ; ndata = n_elements(symH[1440:n_elements(symH)-2880])-2
  ndata = n_elements(symH) - 1
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; rc = dst_0([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
  ; Q = rc.Q
  ;
  str_date = string(yr_i, mh_i, dy_i, format = '(I4,I02,I02)')
  window = [781, 1140]
  ; window2 = [1140, 1500]
  ; window = [1380, 1520]
  window2 = [1140, 1500]
  window3 = [781, 1500]
  ; case str_date of
  ; '20150317': window = [780, 960] ; primer pico
  ; '20150317': window = [960, 1080] ; segundo pico
  ; '20150317': window = [1080, 1200] ; tercer pico
  ; '20150317': window = [1200, 1320] ; cuarto pico
  ; '20150317': window = [1380, 1520] ; quinto pico
  ; '20150317': window = [781, 1180], window2 = [1180, 1500] ; ventana 1
  ; '20150317': window2 = [1180, 1500] ; ventana 2
  ; '20150317': window = [781, 1080] ; pulsos 1 y 2
  ; '20150317': window = [1080, 1520] ; pulsos 3-5

  ; '20151007': window = [1180, 1500] ; ventana 2
  ; '20151007': window = [960, 1070] ; segundo pico

  ; '20151007': window = [1740, 1900] ; tercer pico, no relacionado con la tormenta, casi todos los obs tienen mauyor corr
  ; '20151219': window = [2340, 2655] ; primer pico
  ; '20151219': window = [1800, 2100] ; segundo pico
  ; '20160306': window = [1020, 1200] ; primer pico
  ; '20160306': window = [1320, 1440] ; segundo pico
  ; '20160306': window = [1580, 1790] ; tercer pico
  ; '20170527': window = [1660, 1890] ; primer pico
  ; '20170527': window = [1440, 1560] ; segundo pico mayor correlacion, teo y jai en atardecer y amanecer
  ; '20180825': window = [1570, 1800] ; primer pico
  ; '20180825': window = [1920, 2070] ; segundo pico
  ; '20180825': window = [2360, 2450] ; tercer pico
  ; else: print, 'agregar evento'
  ; endcase
  WindowI = window[0]
  windowF = window[1]

  WindowI2 = window2[0]
  windowF2 = window2[1]

  d_asym = ts_diff(asymH, 1)
  if max(H) gt max(diono) then up = max(H) else up = max(diono)

  if min(H) lt min(symH) then down = min(H) else down = min(symH)

  ; PLOT, date_time, d_asym, xticks = file_number, xminor = 8, background = 0, $
  ; color = 255, charsize = 1, charthick = 1, $
  ; position = [0.1, 0.2, 0.9, 0.8], xstyle = 1, ystyle = 1, xtickformat = ['LABEL_DATE'], xtickunits = ['day'], $
  ; xticklayout = 2, xtickinterval = 1, /nodata

  ; oplot, date_time, abs(d_asym), color = 150, linestyle = 0, thick = 3
  ; oplot, date_time, symH, color = 250, thick = 2
  ; oplot, date_time, H, color = 250, thick = 2, linestyle = 2

  time = 3900
  dp2_max = min(dp2, time)
  ; oplot, [date_time[WindowI], date_time[WindowI]], [!y.crange[0], !y.crange[1]], linestyle = 2
  ; oplot, [date_time[windowF], date_time[windowF]], [!y.crange[0], !y.crange[1]], linestyle = 2
  ; oplot, [date_time[WindowI2], date_time[WindowI2]], [!y.crange[0], !y.crange[1]], linestyle = 2
  ; oplot, [date_time[windowF2], date_time[windowF2]], [!y.crange[0], !y.crange[1]], linestyle = 2

  caldat, date_time[WindowI], month, iday, year, ihour, iminute
  caldat, date_time[windowF], month, fday, year, fhour, fminute
  print, 'initial time: ', string(iday, ihour, iminute, format = '(I3,2X, I02,":",I02)')
  print, 'final time: ', string(fday, fhour, fminute, format = '(I3,2X, I02,":",I02)')

  maxsym = max(asymH[WindowI : windowF], i)
  print, 'time UT of max ASYH: ', string(dy_lt[i + WindowI], hr_lt[i + WindowI], min_lt[i + WindowI], format = '(I02,2X, I02,":",I02)')
  print, 'max ASYM: ', maxsym
  print, 'min SYMH: ', min(symH)
  print, 'Correlation ASYH vs Diono', correlate(asymH[WindowI : windowF], diono[WindowI : windowF])

  ppef_max = max(dp2, i)
  ppef_min = min(dp2, j)

  print, 'max PPEF: ', max(dp2), 'nT   at: ', string(hr_lt[i], min_lt[i], format = '(I02,":",I02)')
  print, 'min PPEF: ', min(dp2), 'nT   at: ', string(hr_lt[j], min_lt[j], format = '(I02,":",I02)')
  ; stop, 'end of test'

  tw = local_time[WindowI : windowF]

  ; dp2vsQ, prc[WindowI : windowF], diono[WindowI : windowF], asymH[WindowI : windowF], dp2[WindowI : windowF], $
  ; H[WindowI : windowF], station_code, date_time[WindowI : windowF], local_time[WindowI : windowF], '1'

  ; dp2vsQ, prc[WindowI2 : windowF2], diono[WindowI2 : windowF2], asymH[WindowI2 : windowF2], dp2[WindowI2 : windowF2], $
  ; H[WindowI2 : windowF2], station_code, date_time[WindowI2 : windowF2], local_time[WindowI2 : windowF2], '2'

  export_dp2files, diono, asymH, dp2, dp2_2, station_code, date_time

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  path = '/home/isaac/longitudinal_studio/fig/diono_recons/'
  psfile = path + station_code + '_' + Date + '.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 10, ysize = 8
  X_label = xlabel([yr_i, mh_i, dy_i], file_number)
  old_month = month_name(mh_i, 'english')

  class = gms_class(station_code)
  info = stationlist(class, station_code)

  time_title = ' UT [days]'

  periodo = 'Period [h]'
  ; ###############################################################################
  chr_size1 = 0.9
  chr_thick1 = 1.5
  cgPlot, f_k, pws, /xlog, /ylog, position = [0.1, 0.11, 0.95, 0.89], $
    background = 'white', color = 'black', $
    charsize = chr_size1, xstyle = 5, ystyle = 5, subtitle = '', thick = 4, /nodata
  ; print, 'UTC offset (hours): ', utc

  ppef_max = max(dp2, i)
  ppef_min = min(dp2, j)

  print, 'max PPEF: ', max(dp2), 'nT   at: ', string(hr_lt[i], min_lt[i], format = '(I02,":",I02)')
  print, 'min PPEF: ', min(dp2), 'nT   at: ', string(hr_lt[j], min_lt[j], format = '(I02,":",I02)')
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; panel a
  if utc lt 0 then begin
    local_ini = date_time[0] + (jul_conv)
    local_fin = date_time[n_elements(date_time) - 1] + jul_conv
  endif else begin
    local_ini = date_time[0] - jul_conv
    local_fin = date_time[n_elements(date_time) - 1] - jul_conv
  endelse

  local_time = timegen(start = local_ini, final = local_fin, units = resolution)
  caldat, date_time, mh_lt, dy_lt, yr_lt, hr_lt, min_lt

  ; diono_figures, date_time, local_time, asymH, diono, H, tec, med_tec, date_i, date_f, utc, station_code, window, window2

  midsample = sample / 2

  midday = fltarr(n_elements(local_time) / midsample)

  midddays = n_elements(symH) / midsample
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  cgPlot, date_time, abs(d_asym), xticks = file_number, xminor = 8, background = 'white', $
    color = 'black', charsize = 1, charthick = 1, $
    position = [0.1, 0.65, 0.92, 0.92], xstyle = 5, ystyle = 5, xtickformat = ['LABEL_DATE'], xtickunits = ['day'], $
    xticklayout = 1, xtickinterval = 1, yrange = [min(abs(d_asym)), max(abs(d_asym))], /noerase, /nodata

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
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, date_time[ndata], date_time[ndata], local_time[((midddays - 1) * midsample)] + 0.25], $
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
  ; Set up color environment for POLYFILL
  loadct, 39
  polyfill, [date_time[window[0]], date_time[window[1]], date_time[window[1]], date_time[window[0]]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], $
    color = 100, /line_fill, orientation = 45, spacing = 0.2

  polyfill, [date_time[window[0]], date_time[window[1]], date_time[window[1]], date_time[window[0]]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], $
    color = 100, /line_fill, orientation = -45, spacing = 0.2

  cgOPlot, date_time, abs(d_asym), color = 'orange', thick = 3

  ytittle = Textoidl('\Delta_t |ASYH|')
  cgAxis, yaxis = 1, yrange = [min(abs(d_asym)), max(abs(d_asym))], $
    ; COLOR=negro, $
    ystyle = 1, $

    ytitle = ytittle + ' [nT/min]', $
    charsize = 1.2, $
    charthick = 1.6

  cgPlot, date_time, diono, xticks = file_number, xminor = 8, background = 'white', $
    color = 'black', charsize = 1, charthick = 1, $
    position = [0.1, 0.65, 0.92, 0.92], xstyle = 5, ystyle = 5, xtickformat = ['LABEL_DATE'], xtickunits = ['day'], $
    xticklayout = 1, xtickinterval = 1, yrange = [down, up], /noerase, /nodata

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################

  cgOPlot, date_time, diono, color = 'red', linestyle = 0, thick = 3
  cgOPlot, date_time, H, color = 'black', thick = 3

  ; cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 1, thick = 4, color = 'black'
  ; cgOPlot, [date_time[window[0]], date_time[window[0]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  ; cgOPlot, [date_time[window[1]], date_time[window[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black

  ; cgOPlot, [date_time[window2[1]], date_time[window2[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  month = month_name(mh_i, 'english')
  xtitle = Textoidl('Universal Time[m], ' + month + ' ' + string(yr_i, format = '(I04)'))

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 24, $
    ; xtitle = xtitle, $
    xstyle = 1, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
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

  ytittle = string(strupcase(station_code), format = '(A, " local response [nT]")')
  cgAxis, yaxis = 0, $
    ytitle = ytittle, $
    ; COLOR=negro, $
    ystyle = 1, $
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
  ; cgPolygon, [date_time[window2[0]], date_time[window2[1]], date_time[window2[1]], date_time[window2[0]]], $
  ; [287, 287, 292, 292], color = 'green', /fill

  ; ###############################################################################

  ; ###############################################################################
  up = 60
  down = -60
  ; IF downddyn LT downdp2 THEN down = downddyn ELSE down=downdp2
  ; panel b
  cgPlot, date_time, dp2, xticks = file_number, xminor = 8, background = 'white', $
    color = 'black', charsize = chr_size1, charthick = chr_thick1, $
    position = [0.1, 0.37, 0.92, 0.64], xstyle = 5, ystyle = 5, xtickformat = ['LABEL_DATE'], xtickunits = ['day'], $
    xticklayout = 1, xtickinterval = 1, yrange = [down, up], /noerase, /nodata

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
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, date_time[ndata], date_time[ndata], local_time[((midddays - 1) * midsample)] + 0.25], $
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

  polyfill, [date_time[window[0]], date_time[window[1]], date_time[window[1]], date_time[window[0]]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], $
    color = 100, /line_fill, orientation = 45, spacing = 0.2

  polyfill, [date_time[window[0]], date_time[window[1]], date_time[window[1]], date_time[window[0]]], $
    [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], $
    color = 100, /line_fill, orientation = -45, spacing = 0.2
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; cgOPlot, date_time, ddyn, color = 'black', linestyle = 0, thick = 3
  cgOPlot, date_time, dp2, color = 'red', thick = 4
  ; cgOPlot, date_time, dp2_2, color = 'GRN6', thick = 4
  ; cgOPLOT, date_time, SQ, color='blue', THICK=3
  ; cgOPlot, [date_time[window[0]], date_time[window[0]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  ; cgOPlot, [date_time[window[1]], date_time[window[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  ; cgOPlot, [date_time[window2[1]], date_time[window2[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'

  cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 2, thick = 4, color = 'black'

  month = month_name(mh_i, 'english')
  ; xtitle = Textoidl('Universal Time[m], ' + month + ' ' + string(yr_i, format = '(I04)'))

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 24, $
    ; xtitle = xtitle, $
    xstyle = 1, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickformat = '(A1)', $
    xtickinterval = 1, $
    ; xtickformat = ['LABEL_DATE'], $
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
  ppef = Textoidl('H_{PPEF}')
  cgAxis, yaxis = 0, $
    ytitle = string(ppef, strupcase(station_code), format = '(A, " (",A,")", " [nT]")'), $
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

  ; ###############################################################################
  !p.font = 1
  ; XYOuts, 0.53, 0.735, '(a)', /Normal, $
  ; Alignment=0.5, Charsize=3.2, CHARTHICK= 10;, font= 3
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  new_tec = fltarr(n_elements(dp2))
  tmp_tec = interpol(tec, n_elements(dp2))
  new_tec = tmp_tec

  new_med = fltarr(n_elements(dp2))
  tmp_med = interpol(med_tec, n_elements(dp2))
  new_med = tmp_med
  tec_index = ((new_tec - new_med) / new_med) * 100
  ; if max(tec) gt max(med_tec) then up = max(tec) else up = max(med_tec)
  ; if min(tec) lt min(med_tec) then down = min(tec) else down = min(med_tec)

  up = max(tec_index)
  down = min(tec_index)
  ; panel c
  cgPlot, date_time, tec_index, xticks = file_number, xminor = 8, background = 'white', $
    color = 'black', charsize = chr_size1, charthick = chr_thick1, $
    position = [0.1, 0.09, 0.92, 0.36], xstyle = 5, ystyle = 5, xtickformat = ['LABEL_DATE'], xtickunits = ['day'], $
    xticklayout = 1, xtickinterval = 1, yrange = [down, up], /noerase, /nodata

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
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[(midddays * (midsample - 1))] + 0.25, local_time[((midddays - 1) * midsample)] + 0.25], $
        [!y.crange[0], !y.crange[0], !y.crange[1], !y.crange[1]], color = 'light gray', /fill
    endif else begin
      cgPolygon, [local_time[((midddays - 1) * midsample)] + 0.25, date_time[ndata], date_time[ndata], local_time[((midddays - 1) * midsample)] + 0.25], $
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

  ; cgOPlot, date_time, new_tec, color = 'ORG4', thick = 4
  cgOPlot, date_time, tec_index, color = 'blue', thick = 4

  ; cgOPlot, [date_time[window[0]], date_time[window[0]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  ; cgOPlot, [date_time[window[1]], date_time[window[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'
  ; cgOPlot, [date_time[window2[1]], date_time[window2[1]]], [!y.crange[0], !y.crange[1]], linestyle = 1, thick = 4, color = 'black'

  ; cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 2, thick = 4, color = 'black'

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 24, $
    ; xtitle = xtitle, $
    xstyle = 1, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = ['LABEL_DATE'], $
    ; COLOR=negro, $
    charsize = 1.6, $
    ticklen = 0.08, $
    charthick = 3.5

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    xminor = 24, $
    xtickunits = ['day'], $
    xticklayout = 0, $
    xtickinterval = 1, $
    xtickformat = '(A1)', $
    ; COLOR=negro, $
    charsize = 1.2, $
    charthick = 1.5, $
    ticklen = 0.08
  ppef = Textoidl('\Delta TEC')
  cgAxis, yaxis = 0, $
    ytitle = string(ppef, strupcase(station_code), format = '(A, " (",A,")", " [TECu %]")'), $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.6, $
    charthick = 1.6

  cgAxis, yaxis = 1, $
    ; COLOR=negro, $
    ystyle = 1, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.6

  ; ppef_mod_max = max(ppef_mod, k)
  ; ppef_mod_min = min(ppef_mod, l)
  ; print, 'max PPEF sim: ', ppef_mod_max, ' at time UT: ', string(hr_lt[k], min_lt[k], format = '(I02,":",I02)')
  ; print, 'min PPEF sim: ', ppef_mod_min, ' at time UT: ', string(hr_lt[l], min_lt[l], format = '(I02,":",I02)')
  cgPS_Close, density = 300, width = 1600 ; , /PNG

  ; dp2vsQ, prc, diono, asymH, dp2, $
  ; H, new_ppef, station_code, date_time, local_time, 'tot'

  ; dp2vsQ, prc[WindowI : windowF], diono[WindowI : windowF], asymH[WindowI : windowF], dp2[WindowI : windowF], $
  ; H[WindowI : windowF], new_ppef[WindowI2 : windowF2], station_code, date_time[WindowI : windowF], local_time[WindowI : windowF], '1'

  ; dp2vsQ, prc[WindowI2 : windowF2], diono[WindowI2 : windowF2], asymH[WindowI2 : windowF2], dp2[WindowI2 : windowF2], $
  ; H[WindowI2 : windowF2], new_ppef[WindowI2 : windowF2], station_code, date_time[WindowI2 : windowF2], local_time[WindowI2 : windowF2], '2'
end
