pro iono_ts, tec, med, date_i, date_f, path, station_code
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
  resolution = 0
  sample = 0
  if res eq 'h' then begin
    resolution = 'Hours'
    sample = 24
  endif else begin
    resolution = 'Minutes'
    sample = 1440
  endelse

  date_time = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), final = julday(mh_f, dy_f, yr_f, 23, 59), units = 'Minutes')

  date_label = label_date(date_format = ['%D', '%M %Y'])
  Date = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  psfile = path + station_code + '_TEC_' + Date + '.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 10, ysize = 4

  class = gms_class(station_code)
  info = stationlist(class, station_code)

  print, 'UTC: ', info.utc
  ; DEVICE, true=24, retain=2, decomposed=0
  ; TVLCT, R_bak, G_bak, B_bak, /GET
  ; LOADCT, 39
  ; WINDOW, 1, XSIZE=800, YSIZE=500, TITLE='GS'

  if max(tec) gt max(med) then up = max(tec) else up = max(med)
  if min(tec) lt min(med) then down = min(tec) else down = min(med) ; panel a
  cgPlot, date_time, tec, background = 'white', color = 'black', position = [.07, .15, .97, .91], xtickformat = ['LABEL_DATE'], $
    xtickunits = ['day'], xticklayout = 1, xtickinterval = 1, charsize = 1.1, xstyle = 5, ystyle = 5, yrange = [down, up], /nodata

  ; cgOPlot, [!x.crange[0], !x.crange[1]], [0., 0.], linestyle = 1, thick = 2, color = 'black'
  cgOPlot, date_time, tec, color = 'red', thick = 3
  cgOPlot, date_time, med, color = 'blue', thick = 3

  ; cgPolygon, [0.78, 0.81, 0.81, 0.78], [0.18, 0.18, 0.182, 0.182], color = 'red', /normal, /fill
  ; cgPolygon, [0.78, 0.81, 0.81, 0.78], [0.148, 0.148, 0.145, 0.145], color = 'blue', /normal, /fill

  month = month_name(mh_i, 'english')
  xtitle = Textoidl('Universal Time, ' + month + ' ' + string(yr_i, format = '(I04)'))
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

  cgAxis, yaxis = 0, yrange = [down, up], $
    ytitle = 'TEC [TECu]', $
    ; COLOR=negro, $
    ystyle = 1, $
    charsize = 1.2, $
    charthick = 1.6

  cgAxis, yaxis = 1, yrange = [down, up], $
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
