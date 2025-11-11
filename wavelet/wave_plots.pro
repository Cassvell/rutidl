pro make_psfig_composed, asymH, diono, H, H_sq, SQ, power, xwt, ddyn, period, coi, date_i, date_f, station_code
  @set_up_commons
  set_up
  on_error, 2
  compile_opt idl2, hidden

  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]
  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  TGM_n = event_case([yr_i, mh_i, dy_i])

  ; ###############################################################################
  Date = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
  X_label = xlabel([yr_i, mh_i, dy_i], file_number)

  psfile = '/home/isaac/rutidl/output/wavelet/' + station_code + '/' + station_code + '_' + Date + '.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 10, ysize = 7

  ; ###############################################################################
  ; ###############################################################################

  cgLoadCT, 40

  date_time = timegen(start = julday(mh_i, dy_i, yr_i, 0, 1), $
    final = julday(mh_f, dy_f, yr_f, 24, 0), units = 'Hours')
  date_label = label_date(date_format = ['%D', '%M %Y'])
  ; ################################################################################
  ; ################################################################################
  ; ################################################################################
  ; ################################################################################
  ; panel a

  nLevels = 36
  minPower = min(real_part(SQ))
  maxPower = max(real_part(SQ))
  levels = findgen(nLevels) * ((maxPower - minPower) / (nLevels - 1)) + minPower

  cgContour, SQ, date_time, period, xstyle = 5, ytitle = '', title = '', position = [.07, .66, .77, .92], $
    ystyle = 5, c_colors = colors, yrange = [480, 2880], xminor = 8, ytickformat = 'exponent', $
    /ytype, levels = levels, nlevels = nLevels, /fill, $
    xtickformat = '(A1)', xtickunits = ['day', 'month'], xticklayout = 0, $
    xtickinterval = 1, /noerase ; ,  xTITLE = 'Time [days]'

  nColors = !d.table_size

  title = Textoidl('Amplitude [nT h]')
  tickNames = string(levels, format = '(F8.1)')

  cgColorbar, ncolors = nColors, position = [0.66, 0.87, 0.92, 0.89], ticknames = tickNames, range = [minPower, maxPower], $
    charsize = 1.0, title = title, vertical = 1, right = 1 ; Moves title and labels to the right

  ; Create a half rectangle (right half)
  x_coords = [0, 2, 2, 0, 0] ; X coordinates: left, right, right, left, left
  y_coords = [0, 0, 0.2, 0.2, 0] ; Y coordinates: bottom, bottom, top, top, bottom

  ; Register the custom symbol
  usersym, x_coords, y_coords, /fill

  cgPlotS, min(date_time), 1440, psym = 8, color = 'black', thick = 4
  cgPlotS, min(date_time), 720, psym = 8, color = 'black', thick = 4

  ; Create a half rectangle (right half)
  x_coords = [0, 1, 1, 0, 0] ; X coordinates: left, right, right, left, left
  y_coords = [0, 0, 0.2, 0.2, 0] ; Y coordinates: bottom, bottom, top, top, bottom

  ; Register the custom symbol
  usersym, x_coords, y_coords, /fill

  cgPlotS, min(date_time), 2520, psym = 8, color = 'black', thick = 1
  cgPlotS, min(date_time), 2160, psym = 8, color = 'black', thick = 1
  cgPlotS, min(date_time), 1800, psym = 8, color = 'black', thick = 1
  cgPlotS, min(date_time), 1080, psym = 8, color = 'black', thick = 1

  cgText, min(date_time), 1380, '24 ', $
    color = 'black', alignment = 1.0, charsize = 1.2

  cgText, min(date_time), 680, '12 ', $
    color = 'black', alignment = 1.0, charsize = 1.2
  ; ##################################################

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    color = 'black', $
    xstyle = 1, $
    xminor = 8, $
    xticks = file_number, $
    ; xTITLE = 'Time [days]',$
    charsize = 1.2, $
    ticklen = 0.04, $
    charthick = 1.5, $
    xtickformat = '(A1)'

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $ ; .0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)
    color = 'black', $
    xstyle = 1, $
    xticks = file_number, $
    xminor = 8, $
    xtickformat = '(A1)', $
    xtickunits = ['day']

  cgAxis, yaxis = 0, yrange = [480, 2880], $
    ; YTITLE = 'Period [Hr]', $

    ytickformat = '(A1)', $
    ystyle = 5, $
    color = 'black', $
    charsize = 1.2, $
    charthick = 1.5

  y = (!y.window[1] - !y.window[0]) / 2. + !y.window[0]
  x = 0.03
  cgText, x, y, 'Period [h]', /normal, $
    color = 'black', alignment = 0.5, charsize = 1.2, orientation = 90

  y = (!y.window[1] - !y.window[0]) / 2. + !y.window[0]
  x = 0.845

  sqlabel = Textoidl('H_{SQ} & D_I [nT]')
  cgText, x, y, sqlabel, /normal, $
    color = 'black', alignment = 0.5, charsize = 1.2, orientation = 90
  ; ###############################################################################
  ; ###############################################################################
  ; ################################################################################
  ; ################################################################################
  ; OVERPLOT LAYER

  cgPlot, date_time, findgen(n_elements(date_time)), position = [0.66, 0.87, 0.92, 0.89], $
    color = 'black', xstyle = 1, ystyle = 1, /overplot, axiscolor = 'white', /nodata
  ; print, n_elements(period2), n_elements(date_time)

  x = [date_time[0], date_time, max(date_time)]
  y = [max(period), coi, max(period)]

  cgPolygon, x, y, orien = + 45, spacing = 0.5, noclip = 0, linestyle = 0, fcolor = 'black', /fill
  cgPolygon, x, y, orien = -45, spacing = 0.5, noclip = 0, linestyle = 0, fcolor = 'black', /fill
  ; ###############################################################################
  ; ###############################################################################
  cgPlot, date_time, H_sq, position = [.07, .66, .77, .92], $
    color = 'black', xstyle = 5, ystyle = 5, /nodata, /noerase, yrange = [-90, 150]

  cgOPlot, date_time, H_sq, color = 'black', thick = 3, linestyle = 0
  cgOPlot, date_time, diono, color = 'hot pink', thick = 4, linestyle = 0

  cgAxis, yaxis = 1, yrange = [-90, 150], $
    color = 'black', $
    ; YTICKFORMAT='(A1)',$
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.5

  ; cgtext, 0.68, 0.69, '(a)', color='black', /normal, TT_FONT='Helvetica Bold', charsize = 2
  ; ################################################################################
  ; ################################################################################

  ; Define the levels and colors used in CGCONTOUR
  ; Data range for the colorbar

  ; Define the range of the power series and compute levels
  minPower = min((power))
  maxPower = max((power))
  period2 = fix(alog(period) / alog(2))
  print, maxPower, minPower

  nLevels = 36

  levels_arr = findgen(nLevels)
  levels = levels_arr * ((maxPower - minPower) / (nLevels - 1.0)) + minPower

  ; Generate tick names based on levels

  ; ###############################################################################
  ; ###############################################################################
  ; panel b
  cgContour, power, date_time, period, xstyle = 1, ytitle = '', title = '', position = [.07, .38, .77, 0.64], $
    ystyle = 5, c_colors = colors, yrange = [480, 2880], xminor = 8, ytickformat = 'exponent', $
    /ytype, levels = levels, nlevels = nLevels, /fill, $
    xtickformat = '(A1)', xtickunits = ['day', 'month'], xticklayout = 0, $
    xtickinterval = 1, /noerase ; ,  xTITLE = 'Time [days]'

  nColors = !d.table_size

  title = Textoidl('Energy Density [nT^2 h^{2}]')

  tick_indices = [0, nLevels / 4, nLevels / 2, 3 * nLevels / 4, nLevels - 1]
  tickValues = levels[tick_indices]

  tickNames = string(tickValues, format = '(F8.1)')

  cgColorbar, ncolors = nColors, position = [0.38, 0.87, 0.64, 0.89], ticknames = tickNames, range = [0, maxPower], $
    divisions = n_elements(tickValues) - 1, charsize = 1.0, title = title, vertical = 1, right = 1

  ; cgtext, 0.6775, 0.418, '(b)', color='black', /normal, TT_FONT='Helvetica Bold', charsize = 2.3
  ; cgtext, 0.68, 0.42, '(b)', color='white', /normal, TT_FONT='Helvetica Bold', charsize = 2

  ; Create a half rectangle (right half)
  x_coords = [0, 2, 2, 0, 0] ; X coordinates: left, right, right, left, left
  y_coords = [0, 0, 0.2, 0.2, 0] ; Y coordinates: bottom, bottom, top, top, bottom

  ; Register the custom symbol
  usersym, x_coords, y_coords, /fill

  cgPlotS, min(date_time), 1440, psym = 8, color = 'white', thick = 4
  cgPlotS, min(date_time), 720, psym = 8, color = 'white', thick = 4

  ; Create a half rectangle (right half)
  x_coords = [0, 1, 1, 0, 0] ; X coordinates: left, right, right, left, left
  y_coords = [0, 0, 0.2, 0.2, 0] ; Y coordinates: bottom, bottom, top, top, bottom

  ; Register the custom symbol
  usersym, x_coords, y_coords, /fill

  cgPlotS, min(date_time), 2520, psym = 8, color = 'white', thick = 1
  cgPlotS, min(date_time), 2160, psym = 8, color = 'white', thick = 1
  cgPlotS, min(date_time), 1800, psym = 8, color = 'white', thick = 1
  cgPlotS, min(date_time), 1080, psym = 8, color = 'white', thick = 1

  cgText, min(date_time), 1380, '24 ', $
    color = 'black', alignment = 1.0, charsize = 1.2

  cgText, min(date_time), 680, '12 ', $
    color = 'black', alignment = 1.0, charsize = 1.2
  ; ##################################################

  x = [date_time[0], date_time, max(date_time)]
  y = [max(period), coi, max(period)]

  cgPolygon, x, y, orien = + 45, spacing = 0.5, noclip = 0, linestyle = 0, fcolor = 'white', /fill
  cgPolygon, x, y, orien = -45, spacing = 0.5, noclip = 0, linestyle = 0, fcolor = 'white', /fill

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    color = 'white', $
    xstyle = 1, $
    xminor = 8, $
    xticks = file_number, $
    ; xTITLE = 'Time [days]',$
    charsize = 1.2, $
    ticklen = 0.04, $
    charthick = 1.5, $
    xtickformat = '(A1)'

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $ ; .0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)
    color = 'white', $
    xstyle = 1, $
    xticks = file_number, $
    xminor = 8, $
    xtickformat = '(A1)', $
    xtickunits = ['day']

  cgAxis, yaxis = 0, yrange = [480, 2880], $
    ytitle = 'Freq [Hz]', $
    ystyle = 5, $
    color = 'black', $
    /ylog, $
    charsize = 1.2, $
    charthick = 1.5

  cgAxis, yaxis = 1, yrange = [480, 2880], $
    /ylog, $
    color = 'black', $
    ytickformat = '(A1)', $
    ystyle = 5, $
    charsize = 1.4, $
    charthick = 1.5

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; OVERPLOT LAYER

  cgPlot, date_time, findgen(n_elements(date_time)), position = [.07, .38, .77, 0.64], $
    color = 'white', xstyle = 5, ystyle = 5, /overplot, axiscolor = 'white', /nodata
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; OVERPLOT TICK LAYERS

  cgAxis, yaxis = 0, yrange = [480, 2880], $
    ytitle = '', $
    ystyle = 5, $
    color = 'white', $
    /ylog, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.5

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    color = 'white', $
    xstyle = 1, $
    xminor = 8, $
    xticks = file_number, $
    ; xTITLE = 'Time [days]',$
    charsize = 1.4, $
    ticklen = 0.04, $
    charthick = 1.5, $
    xtickformat = '(A1)'

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $ ; .0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)
    color = 'white', $
    xstyle = 1, $
    xticks = file_number, $
    xminor = 8, $
    xtickformat = '(A1)', $
    xtickunits = ['day']

  ; ###############################################################################
  ; ###############################################################################
  y = (!y.window[1] - !y.window[0]) / 2. + !y.window[0]
  x = 0.03
  cgText, x, y, 'Period [h]', /normal, $
    color = 'black', alignment = 0.5, charsize = 1.2, orientation = 90

  y = (!y.window[1] - !y.window[0]) / 2. + !y.window[0]
  x = 0.845

  d_h = Textoidl('\Delta H [nT]')

  cgText, x, y, d_h, /normal, $
    color = 'black', alignment = 0.5, charsize = 1.2, orientation = 90
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################

  cgPlot, indgen(n_elements(H)), H, position = [.07, .38, .77, 0.64], $
    color = 'black', xstyle = 5, ystyle = 5, /nodata, /noerase, yrange = [min(H), max(H)]

  cgOPlot, findgen(n_elements(H)), H, color = 'white', thick = 5, linestyle = 0

  cgAxis, yaxis = 1, yrange = [min(H), max(H)], $
    color = 'black', $
    ; YTICKFORMAT='(A1)',$
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 0.05, $
    ticklen = 0.0

  cgPlot, findgen(n_elements(H)), H, position = [.07, .38, .77, 0.64], $
    color = 'black', xstyle = 5, ystyle = 5, /nodata, /noerase, yrange = [min(H), max(H)]

  cgAxis, yaxis = 1, yrange = [min(H), max(H)], $
    color = 'white', $
    ytickformat = '(A1)', $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 2.5

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; panel c

  ddyn = ddyn * (-1) ; invert the scale of Ddyn
  minPower = min(ddyn)
  maxPower = max(ddyn)

  ddyn = (ddyn - minPower) / (maxPower - minPower) ; Normalize to [0, 1]

  nLevels = 36
  ; Levels from -1 to 1

  ; for i = 0, n_elements(ddyn)-1 do begin
  ; if ddyn[i] LT 0 then ddyn[i] = 0
  ; endfor

  minPower = min(ddyn)
  maxPower = max(ddyn)

  step = (maxPower - minPower) / (nLevels - 1.0) ; Floating-point division
  levels = minPower + findgen(nLevels) * step ; High precision

  cgContour, ddyn, date_time, period, $
    xstyle = 1, ystyle = 5, $
    ytitle = 'Period [min]', $
    position = [0.07, 0.1, 0.77, 0.36], c_colors = colors, levels = levels, yrange = [480, 2880], /fill, /ytype, $
    xtickformat = ['LABEL_DATE'], xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, $
    c_labels = levels, c_charsize = 1.0, $ ; Show labels for all levels
    charsize = 1.2, /noerase

  nColors = !d.table_size

  title = Textoidl('DDEF correlation')
  tick_indices = [0, nLevels / 4, nLevels / 2, 3 * nLevels / 4, nLevels - 1]
  tickValues = levels[tick_indices]
  tickNames = string(tickValues, format = '(F4.1)')

  cgColorbar, ncolors = nColors, position = [0.1, 0.87, 0.36, 0.89], $
    range = [minPower, maxPower], charsize = 1.0, title = title, vertical = 1, right = 1 ;

  ; ###############################################################################
  ; ###############################################################################
  ; Create a half rectangle (right half)
  x_coords = [0, 2, 2, 0, 0] ; X coordinates: left, right, right, left, left
  y_coords = [0, 0, 0.2, 0.2, 0] ; Y coordinates: bottom, bottom, top, top, bottom

  ; Register the custom symbol
  usersym, x_coords, y_coords, /fill

  cgPlotS, min(date_time), 1440, psym = 8, color = 'white', thick = 4
  cgPlotS, min(date_time), 720, psym = 8, color = 'white', thick = 4

  ; Create a half rectangle (right half)
  x_coords = [0, 1, 1, 0, 0] ; X coordinates: left, right, right, left, left
  y_coords = [0, 0, 0.2, 0.2, 0] ; Y coordinates: bottom, bottom, top, top, bottom

  ; Register the custom symbol
  usersym, x_coords, y_coords, /fill

  cgPlotS, min(date_time), 2520, psym = 8, color = 'white', thick = 1
  cgPlotS, min(date_time), 2160, psym = 8, color = 'white', thick = 1
  cgPlotS, min(date_time), 1800, psym = 8, color = 'white', thick = 1
  cgPlotS, min(date_time), 1080, psym = 8, color = 'white', thick = 1

  cgText, min(date_time), 1380, '24 ', $
    color = 'black', alignment = 1.0, charsize = 1.2

  cgText, min(date_time), 680, '12 ', $
    color = 'black', alignment = 1.0, charsize = 1.2

  ; ##################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; OVERPLOT LAYER

  cgPlot, date_time, H, position = [.07, .1, .77, .36], $
    color = 'black', xstyle = 5, ystyle = 5, /overplot, axiscolor = 'white', /nodata, /noerase

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  x = [date_time[0], date_time, max(date_time)]
  y = [max(period), coi, max(period)]

  cgPolygon, x, y, orien = + 45, spacing = 0.5, noclip = 0, linestyle = 0, fcolor = 'white', /fill
  cgPolygon, x, y, orien = -45, spacing = 0.5, noclip = 0, linestyle = 0, fcolor = 'white', /fill

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; OVERPLOT TICK LAYERS

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    color = 'white', $
    xstyle = 1, $
    xminor = 8, $
    xticks = file_number, $
    ; xTITLE = 'Time [days]',$
    charsize = 1.4, $
    ticklen = 0.04, $
    charthick = 1.5, $
    xtickformat = '(A1)'

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $ ; .0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)
    color = 'white', $
    xstyle = 1, $
    xticks = file_number, $
    xminor = 8, $
    xtickformat = '(A1)', $
    xtickunits = ['day']

  cgAxis, yaxis = 0, yrange = [480, 2880], $
    ytitle = 'Freq [Hz]', $
    ystyle = 5, $
    color = 'black', $
    /ylog, $
    charsize = 1.2, $
    charthick = 1.5

  cgAxis, yaxis = 1, yrange = [480, 2880], $
    /ylog, $
    color = 'black', $
    ytickformat = '(A1)', $
    ystyle = 5, $
    charsize = 1.4, $
    charthick = 1.5

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; OVERPLOT LAYER

  cgPlot, date_time, findgen(n_elements(date_time)), position = [.07, .36, .77, .92], $
    color = 'white', xstyle = 1, ystyle = 5, /overplot, axiscolor = 'white', /nodata
  ; print, n_elements(period2), n_elements(date_time)
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; OVERPLOT TICK LAYERS

  cgAxis, yaxis = 0, yrange = [480, 2880], $
    ytitle = '', $
    ystyle = 5, $
    color = 'white', $
    /ylog, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.5
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  asymHmin = min(asymH)
  asymHmax = max(asymH)
  cgPlot, findgen(n_elements(asymH)), asymH, position = [0.07, 0.1, 0.77, 0.36], yrange = [asymHmin, asymHmax], $
    color = 'black', xstyle = 5, ystyle = 6, /nodata, /noerase

  cgOPlot, findgen(n_elements(asymH)), asymH, color = 'yellow', thick = 5, linestyle = 3

  cgAxis, yaxis = 1, yrange = [asymHmin, asymHmax], $
    color = 'white', $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.5

  cgAxis, yaxis = 1, yrange = [asymHmin, asymHmax], $
    color = 'black', $
    ystyle = 1, $
    charsize = 1.2, $
    ticklen = 0.0, $
    charthick = 0.01

  cgPlot, date_time, H, position = [0.07, 0.1, 0.77, 0.36], yrange = [asymHmin, asymHmax], $
    color = 'black', xstyle = 5, ystyle = 6, /nodata, /noerase

  cgAxis, yaxis = 1, yrange = [asymHmin, asymHmax], $
    color = 'white', $
    ystyle = 1, $
    ytickformat = '(A1)', $
    charsize = 1.2, $
    charthick = 1.5

  ; cgtext, 0.68, 0.13, '(c)', color='yellow', /normal, TT_FONT='Helvetica Bold', charsize = 2
  ; ###############################################################################
  ; ###############################################################################

  class = gms_class(station_code)
  info = stationlist(class, station_code)
  title = string(strupcase(station_code), info.mlat, info.mhem, info.mlon, info.mhem2, $
    format = '(A, ", mlat: ", F7.2, " ", A, ", ", "mlon: ", F7.2," ", A)')

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  y = 0.93
  xyouts, x, y, title, /normal, $
    alignment = 0.5, charsize = 1.65

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  y = 0.03

  month = month_name(mh_i, 'english')
  xtitle = Textoidl('Universal Time [days], ' + month + ' ' + string(yr_i, format = '(I04)'))

  xyouts, x, y, xtitle, /normal, $
    color = 'black', alignment = 0.5, charsize = 1.2

  ; ###############################################################################
  ; ###############################################################################
  y = (!y.window[1] - !y.window[0]) / 2. + !y.window[0]
  x = 0.03
  cgText, x, y, 'Period [h]', /normal, $
    color = 'black', alignment = 0.5, charsize = 1.2, orientation = 90

  y = (!y.window[1] - !y.window[0]) / 2. + !y.window[0]
  x = 0.845
  cgText, x, y, 'AE [nT]', /normal, $
    color = 'black', alignment = 0.5, charsize = 1.2, orientation = 90
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; spawn, 'evice psfile'
  cgPS_Close, density = 300, width = 1600, /png

  RETURN
end
