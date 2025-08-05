;
; Name:
; index_plot.pro
; purpose:
; plot kmex and kp data in line format
;
; author:
; Carlos Isaac Castellanos Velazco
; Estudiante de Maestría en Ciencias de la Tierra
; Instituto de Geofísica, Unidad Michoacan
; UNAM
; ccastellanos@igeofisica.unam.mx
;
; category:
; data analysis
;
; calling sequence:
; .r kmx_plot
; kmx_plot, date_i, date_f
; parameters:
; date(_i,_f): format = [yyyy,mm,dd]
;
; dependencies:
;
;
; input files
; kmex data files
;
; output files:
; kp and kmex in an IDL GUI window
;
; version
; Dec, 2022
;

pro index_plot, date_i, date_f
  on_error, 2
  compile_opt idl2, hidden

  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]
  ; ##############################################################################
  ; RESOLVE_ROUTINE, 'set_up',/COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
  @set_up_commons
  set_up
  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  ; ###############################################################################
  idate0 = string(yr_i, mh_i, format = '(I4,I02)')
  TGM_n = event_case([yr_i, mh_i, dy_i])
  ; ###############################################################################
  time = findgen(file_number * 1440) / 1440.0
  ; time_h = findgen(file_number*24)/24.0
  Date = string(yr_i, mh_i, dy_i, format = '(I4, "-", I02, "-", I02)')
  ; ###############################################################################
  station_class = ''
  print, 'Enter GMS class code: 		0:regmex or 1:intermagnet'
  read, station_class, prompt = '> '

  case station_class of
    '0': station_class = 'regmex'
    '1': station_class = 'intermagnet'
    else: print, 'non avaiable gms class'
  end
  print, 'Enter GMS idx: If do not know the GMS idx, please run PRO gms code table'
  read, station_idx, prompt = '> '

  if station_class eq 'regmex' then begin
    station = set_var.gms[fix(station_idx)]
    station_code = set_var.gms_code[fix(station_idx)]
  endif

  if station_class eq 'intermagnet' then begin
    station = set_var.gmsi[fix(station_idx)]
    station_code = set_var.gmsi_code[fix(station_idx)]
  endif
  print, 'GMS selected: ' + station + ' IAGA code: ' + station_code
  ; ###############################################################################
  ; define K variables
  ; ###############################################################################
  ; Generate the time series DH and Dst
  data = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
  H = data.h
  pidx = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
  dst = pidx.dst
  ; ###############################################################################
  ; identifying NAN percentage values in the Time Series

  ndata_hourly = n_elements(H) / 60
  H_hr = fltarr(ndata_hourly)

  for i = 0, ndata_hourly - 1 do begin
    tmp = mean(H[i * 60 : (i * 60 + 1) - 1])
    H_hr[i] = tmp[*]
  endfor

  plot, findgen(ndata_hourly), H_hr

  ; if test eq 0 then begin
  ; file_mkdir, path
  ; print, 'PATH directory ' + path
  ; print, 'created'
  ; endif else begin
  ; print, ''
  ; endelse
  ; psfile = path + 'idx_' + Date + '.eps'
  ; makepsfigure, kp, kmex, dst, dH, psfile, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f]
end

pro makepsfigure, kp, kmex, dst, dH, psfile, date_i, date_f
  on_error, 2
  compile_opt idl2, hidden

  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]
  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1

  Date = string(yr_i, mh_i, dy_i, yr_f, mh_f, dy_f, format = '(I4, "-", I02, "-", I02, "_", I4, "-", I02, "-", I02)')
  ; X_label = xlabel([yr_i, mh_i, dy_i], file_number)

  tot_days = findgen(file_number * 8) / 8.
  tot_days2 = findgen(file_number * 24) / 24.

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 16, ysize = 10

  date_time = timegen(start = julday(mh_i, dy_i, yr_i, 0, 0), $
    final = julday(mh_f, dy_f, yr_f, 24, 0), units = 'Hours')

  xtick_values = [date_time[0], date_time[n_elements(date_time) - 1]] ; First and last tick
  date_label = label_date(xtick_values, date_format = ['%D %M', '%Y']) ; Format them

  CHARSIZE = 1.8
  CHARTHICK = 2.0
  print, 'kmex pick', max(kmex, /nan)

  chr_size1 = 0.9
  chr_thick1 = 1.0
  space = 0.015
  rojo = 248
  amarillo = 190
  verde = 150
  negro = 0
  azul = 90
  blanco = 255
  gris = 110
  morado = 16
  naranja = 220

  cgPolygon, [0.70, 0.74, 0.74, 0.70], [0.80, 0.80, 0.806, 0.806], color = 'green', /normal, /fill
  cgPolygon, [0.79, 0.83, 0.83, 0.79], [0.80, 0.80, 0.806, 0.806], color = 'blue', /normal, /fill

  cgText, 0.745, 0.802, /normal, $
    'Kp,            Kmex'

  cgPlot, tot_days, kp, psym = 6, /nodata, max_value = 9., xticks = file_number, xminor = 8, $
    background = 'white', color = 'black', yrange = [0, 9], yticks = 9, $
    yminor = 0, charsize = 1.8, charthick = 1.2, $
    position = [0.15, 0.51, 0.9, 0.9], xstyle = 1, ystyle = 1, $
    xtickformat = '(A1)', xrange = [0, file_number], $
    /noerase
  j = n_elements(kp)

  for i = 0, j - 1 do begin
    ; Default color for bars
    ; color_final = 'blue'   ; Default color (for when both values are not even)

    ; Check if both values are even
    if kp[i] eq kmex[i] then begin
      color_final = 'yellow' ; Color the bar yellow if both are even
    endif

    ; Determine which value is higher and which is lower
    if kp[i] gt kmex[i] then begin
      bottom_value = kmex[i] ; kmex is the bottom bar
      top_value = kp[i] ; Kp is the top bar
      top_color = 'green' ; Color for the top bar (Kp)
    endif else begin
      bottom_value = kp[i] ; Kp is the bottom bar
      top_value = kmex[i] ; kmex is the top bar
      top_color = 'blue' ; Color for the top bar (kmex)
    endelse

    if kp[i] lt kmex[i] then begin
      bottom_value = kp[i] ; kmex is the bottom bar
      top_value = kmex[i] ; Kp is the top bar
      bottom_color = 'green' ; Color for the top bar (Kp)
    endif else begin
      bottom_value = kmex[i] ; Kp is the bottom bar
      top_value = kp[i] ; kmex is the top bar
      bottom_color = 'blue' ; Color for the top bar (kmex)
    endelse

    ; Small offset for zero values
    step = (bottom_value eq 0) ? 0.1 : 0.

    ; Draw the bottom part (the smaller value)
    cgPolygon, [0. + space, 0.125 - space, 0.125 - space, 0. + space] + tot_days[i], $
      [0, 0, bottom_value + step, bottom_value + step], color = bottom_color, /fill

    ; Draw the top part (the larger value)
    cgPolygon, [0. + space, 0.125 - space, 0.125 - space, 0. + space] + tot_days[i], $
      [bottom_value, bottom_value, top_value + step, top_value + step], color = top_color, /fill
    print, kp[i], kmex[i]
  endfor

  for i = 0, file_number - 1 do begin
    ; cgOPLOT, [i,i], [0.,9.], LINESTYLE=1, COLOR=negro
  endfor

  if max(dst, /nan) gt max(dH, /nan) then up = max(dst, /nan) else up = max(dH, /nan)
  if min(dst, /nan) lt min(dH, /nan) then down = min(dst, /nan) else down = min(dH, /nan)

  cgPlot, date_time, dst, yrange = [down, up], charsize = 1.8, background = 'white', color = 'black', charthick = 2.0, thick = 3, $
    xticks = file_number, xtickformat = ['LABEL_DATE', 'LABEL_DATE'], xtickunits = ['day', 'year'], xticklayout = 2, $
    position = [0.15, 0.15, 0.9, 0.49], /noerase, xrange = [0, file_number], xstyle = 5, /nodata

  cgOPlot, tot_days2, dst, linestyle = 0, color = 'black', thick = 3
  cgOPlot, tot_days2, dH, linestyle = 0, color = 'red', thick = 3

  threshold = fltarr(n_elements(dH))
  threshold[*] = -50
  ; print, 	threshold
  cgOPlot, tot_days2, threshold, linestyle = 2, color = 'blue', thick = 2

  cgPolygon, [0.70, 0.74, 0.74, 0.70], [0.4, 0.4, 0.406, 0.406], color = 'black', /normal, /fill
  cgPolygon, [0.79, 0.83, 0.83, 0.79], [0.4, 0.4, 0.406, 0.406], color = 'red', /normal, /fill

  cgText, 0.745, 0.402, /normal, $
    'Dst,            dH', color = 0, $
    charsize = 1.8, $
    charthick = 2.0

  cgAxis, xaxis = 0, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $
    color = 'black', $
    xstyle = 1, $
    xminor = 8, $
    xticks = file_number, $
    ; xTITLE = 'Time [days]',$
    charsize = 1.4, $
    ticklen = 0.04, $
    charthick = 1.5, $
    xtickformat = ['LABEL_DATE', 'LABEL_DATE'], $
    xtickunits = ['day', 'year'], $
    xticklayout = 2

  cgAxis, xaxis = 1, xrange = [date_time[0], date_time[n_elements(date_time) - 1]], $ ; .0/(!X.CRANGE), $                    (!X.CRANGE+date_time[1440]-0.25)
    color = 'black', $
    xstyle = 1, $
    xticks = file_number, $
    xminor = 8, $
    xtickformat = '(A1)'

  cgPS_Close, density = 300, width = 1600 ; , /PNG
end
