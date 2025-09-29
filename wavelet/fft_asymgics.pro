; Name:
; fft_asymgics.pro
; purpose:
; plot geomagnetic and ionospheric response due to Dst and Ddyn magnetic trace
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
; .r iono_resp
; iono_resp, idate, fdate, PNG='png', PS='ps'
; parameters:
; idate/fdate: format ([yyyy,mm,dd])
;
; dependencies:
;
;
; input files
; Dst files, H obs, Bsq baseline, TEC data files
;
; output files:
; .PNG figure
; imported to /output/eventos_tgm/iono_resp_V9_yyyy-mm-dd.png
;
; version
; apr, 2023
;
; note
; in order to run this routine, it is necessary, first to:
; 1. having Bsq data files (run the Bsq routines)
; 2. having the H clean data files (H_filmaker.pro)
pro fft_asymgics, date_i, date_f
  on_error, 2
  compile_opt idl2, hidden

  @set_up_commons
  set_up

  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]

  res = 'm'

  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  ; ###############################################################################
  idate0 = string(yr_i, mh_i, format = '(I4,I02)')
  TGM_n = event_case([yr_i, mh_i, dy_i])
  ; ###############################################################################
  time = findgen(file_number * 1440) / 1440.0
  time_h = findgen(file_number * 24) / 24.0
  Date = string(yr_i, mh_i, dy_i, format = '(I4, "-", I02, "-", I02)')
  ; ###############################################################################

  ; data   = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
  ; H = data.H

  idx = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], res)
  symH = idx.symH
  asymH = idx.asyH

  window, 0, colors = 1, retain = 0, xsize = 1000, ysize = 600, title = 'ASYM vs SYMH'
  plot, time, asymH, yrange = [min(symH), max(asymH)], /nodata
  oplot, time, asymH, linestyle = 0
  oplot, time, symH, linestyle = 1

  n = n_elements(asymH)
  dt = 0.

  case res of
    'h': dt = 3600.0
    'm': dt = 60.0
  endcase

  fny = float(1.0 / (2.0 * dt)) ; frecuencia de Nyquist

  hann_w = hanning(n)
  w_ss = (total((hann_w) ^ 2)) / n

  y = fft(asymH * hann_w) ; Compute Fast Fourie Transform from diono time series

  power_s = (abs(y[0 : n / 2]) ^ 2) / w_ss

  fk = (1 + findgen(n)) / (n * dt)
  print, 'Nyquist freq: ', fny, 'Hz'

  window, 1, retain = 0, xsize = 600, ysize = 600, title = 'PWS ASYM'

  plot, fk, power_s, /xlog, /ylog, xrange = [fk[0], fny], xstyle = 1

  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; load gic data

  lav = gics_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'LAV')
  qro = gics_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'QRO')
  rmy = gics_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'RMY')
  mzt = gics_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'MZT')

  lav = add_nan(lav, 999, 'greater')
  rmy = add_nan(rmy, 999, 'greater')
  qro = add_nan(qro, 999, 'greater')
  mzt = add_nan(mzt, 999, 'greater')

  window, 2, retain = 0, xsize = 1000, ysize = 600, title = 'gics'

  plot, time, lav, position = [0.1, 0.75, 0.9, 0.9], xstyle = 1, ytitle = 'LAV [nT]', title = 'Geomagnetic Induction Currents', /noerase

  plot, time, qro, position = [0.1, 0.55, 0.9, 0.7], xstyle = 1, ytitle = 'ITU [nT]', /noerase

  plot, time, rmy, /noerase, position = [0.1, 0.35, 0.9, 0.5], xstyle = 1, ytitle = 'RMY [nT]'

  plot, time, mzt, /noerase, position = [0.1, 0.15, 0.9, 0.3], xstyle = 1, ytitle = 'MZT [nT]'

  xyouts, 0.5, 0.95, 'GICS Monitoring Stations', /normal, alignment = 0.5, $
    charsize = 1.5
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  ; ###############################################################################
  lav = fillnan(lav)
  rmy = fillnan(rmy)
  qro = fillnan(qro)
  mzt = fillnan(mzt)

  hann_w = hanning(n)
  w_ss = (total((hann_w) ^ 2)) / n

  y_lav = fft(lav * hann_w) ; Compute Fast Fourie Transform from diono time series
  y_rmy = fft(rmy * hann_w)
  y_mzt = fft(mzt * hann_w)

  pws_lav = (abs(y_lav[0 : n / 2]) ^ 2) / w_ss
  pws_rmy = (abs(y_rmy[0 : n / 2]) ^ 2) / w_ss
  pws_mzt = (abs(y_mzt[0 : n / 2]) ^ 2) / w_ss

  window, 3, retain = 0, xsize = 1000, ysize = 1000, title = 'PWS gic'

  plot, fk, pws_lav, /xlog, /ylog, xrange = [fk[0], fny], xstyle = 1, /noerase, position = [0.1, 0.55, 0.45, 0.9], $
    xtitle = 'Frequency [Hz]', ytitle = 'Power [nT²/Hz]', title = 'LAV Station'

  plot, fk, pws_rmy, /xlog, /ylog, xrange = [fk[0], fny], xstyle = 1, /noerase, position = [0.55, 0.55, 0.9, 0.9], $
    xtitle = 'Frequency [Hz]', ytitle = 'Power [nT²/Hz]', title = 'ITU Station'

  plot, fk, pws_mzt, /xlog, /ylog, xrange = [fk[0], fny], xstyle = 1, /noerase, position = [0.1, 0.1, 0.45, 0.45], $
    xtitle = 'Frequency [Hz]', ytitle = 'Power [nT²/Hz]', title = 'RMY Station'
  print, max(lav), max(mzt), max(rmy)

  path = '/home/isaac/rutidl/output/gics_'
  psfile = path + string(date_i, date_f, format = '("_",I4,I02, I02, "_", I4,I02, I02)') + '.eps'

  ; inicia la figura PS
  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 1., font = 0, /encapsulated, $
    /nomatch, xsize = 6, ysize = 6

  cgPlot, time, lav, position = [0.1, 0.75, 0.9, 0.9], xstyle = 5, ytitle = 'LAV [nT]', title = '', /noerase

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

  cgPlot, time, qro, position = [0.1, 0.55, 0.9, 0.7], xstyle = 1, ytitle = 'ITU [nT]', /noerase

  cgPlot, time, rmy, /noerase, position = [0.1, 0.35, 0.9, 0.5], xstyle = 1, ytitle = 'RMY [nT]'

  cgPlot, time, mzt, /noerase, position = [0.1, 0.15, 0.9, 0.3], xstyle = 1, ytitle = 'MZT [nT]'

  xyouts, 0.5, 0.95, 'GICS Monitoring Stations', /normal, alignment = 0.5, $
    charsize = 1.5

  cgPS_Close, density = 300, width = 1600 ; , /PNG
end
