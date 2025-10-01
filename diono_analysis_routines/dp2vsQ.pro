pro dp2vsQ, prc, diono, asymH, dp2, H, station_code, tw, tw_lt, sector
  on_error, 2
  compile_opt idl2, hidden
  ; ###############################################################################

  @set_up_commons
  set_up

  ndata = (n_elements(asymH))

  ; ndays = (JULDAY(mh_f,dy_f,yr_f)-JULDAY(mh_i,dy_i,yr_i))+1
  ; asymH = asymH;[250:1089]
  ; dp2 = dp2[250:1089]
  ; Q = Q[250:1089]
  ; diono = diono[250:1089]

  caldat, tw, month, day, year, hour, minute
  caldat, tw_lt, month_lt, day_lt, year_lt, hour_lt, minute_lt

  ; print, hour_lt
  y_i = year[0]
  m_i = month[0]
  d_i = day[0]

  y_f = year[ndata - 1]
  m_f = month[ndata - 1]
  d_f = day[ndata - 1]

  date = string(y_i, m_i, d_i, y_f, m_f, d_f, format = '(I4,"-",I02,"-",I02,"_",I4,"-",I02,"-",I02)')
  ndata = (n_elements(asymH))
  dir = set_var.mega_dir
  outfile = dir + 'pca/' + station_code + '_' + date + '_TW' + sector + '.dat'
  openw, LUN, outfile, /get_lun
  for i = 0, ndata - 1 do begin
    printf, LUN, asymH[i], diono[i], dp2[i], format = '(F8.4,X,F10.4,X,F10.4,X,F10.4,X,F20.10)'
    ; Get the corresponding data for the day
  endfor
  close, LUN
  free_lun, LUN
  ; stop, 'end of process'
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################

  time = findgen(n_elements(asymH))
  ;
  ;
  path = '/home/isaac/longitudinal_studio/fig/corr/'
  ; path = '/home/isaac/rutidl/output/corr/'
  psfile = path + station_code + string(y_i, m_i, d_i, y_f, m_f, d_f, format = '("_",I4,I02, I02, "_", I4,I02, I02)') + '_corr' + sector + '.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 6, ysize = 6

  class = gms_class(station_code)
  info = stationlist(class, station_code)

  cgPlot, asymH, diono, position = [0.16, .15, 0.9, 0.9], xstyle = 5, ystyle = 5, xrange = [min(asymH), 350], /nodata

  x = [0, 0.5, 0, -0.5]
  y = [0.5, 0, -0.5, 0]

  ; Load the symbol into IDL
  usersym, x, y, /fill

  cgOPlot, asymH, prc, psym = 8, color = 'gray', symsize = 2
  cgOPlot, asymH, diono, psym = 8, color = 'red', symsize = 2
  cgAxis, xaxis = 0, $
    xminor = 8, $
    xtitle = 'ASYMH [nT]', $
    xstyle = 1, $
    ; COLOR=negro, $
    charsize = 1.4, $
    ticklen = 0.04, $
    charthick = 3.5

  cgAxis, xaxis = 1, $
    xminor = 8, $
    xtickformat = '(A1)', $
    xstyle = 1, $
    ; COLOR=negro, $
    charsize = 1.4, $
    ticklen = 0.04, $
    charthick = 3.5

  H_I = Textoidl('H_I')
  cgAxis, yaxis = 0, $
    ytitle = H_I + ' [nT]', $
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

  corr5 = correlate(asymH, diono)
  corr = correlate(asymH, prc)
  P = Textoidl('\rho_1')
  P2 = Textoidl('\rho_2')

  cgText, 0.7, 0.8, string(P, corr5, format = '(A, ": ", F6.2)'), /normal, $
    alignment = 0.5, charsize = 1.65, color = 'black'

  cgText, 0.7, 0.7, string(P2, corr, format = '(A, ": ", F6.2)'), /normal, $
    alignment = 0.5, charsize = 1.65, color = 'black'

  caldat, tw_lt, month_lt, day_lt, year_lt, hour_lt, minute_lt
  yr_i = year_lt[0]
  mh_i = month_lt[0]
  dy_i = day_lt[0]
  hr_i = hour_lt[0]
  min_i = minute_lt[0]

  yr_f = year_lt[ndata - 1]
  mh_f = month_lt[ndata - 1]
  dy_f = day_lt[ndata - 1]
  hr_f = hour_lt[ndata - 1]
  min_f = minute_lt[ndata - 1]

  title = string(strupcase(station_code), yr_f, mh_i, dy_i, dy_f, hr_i, min_i, hr_f, min_f, $
    format = '(A,", ",I4,"/",I02,"/",I02, "-",I02 , 2X, "(",I02,":",I02, " to ", I02,":",I02, " LT", ")")')
  if dy_i eq dy_f then begin
    title = string(strupcase(station_code), yr_f, mh_i, dy_i, hr_i, min_i, hr_f, min_f, $
      format = '(A," ",I4,"/",I02,"/",I02, " (", I02, ":", I02, " to ", I02,":", I02, " LT)")')
  endif

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  y = 0.93
  xyouts, x, y, title, /normal, $
    alignment = 0.5, charsize = 1.65

  ;
  cgPS_Close, density = 300, width = 1600 ; , /PNG
end
