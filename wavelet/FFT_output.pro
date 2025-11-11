; Name:
; FFT_output.pro
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
; iono_resp, idate, fdate
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
; Dec, 2022
;
; note
; in order to run this routine, it is necessary, first to:
; 1. having Bsq data files (run the Bsq routines)
; 2. having the H clean data files (H_filmaker.pro)
;
pro FFT_output, date_i, date_f, station_code, ps = ps, bsq = Bsq
  on_error, 2
  compile_opt idl2, hidden

  res = 'min'
  yr_i = date_i[0]
  mh_i = date_i[1]
  dy_i = date_i[2]

  yr_f = date_f[0]
  mh_f = date_f[1]
  dy_f = date_f[2]
  ; ###############################################################################
  @set_up_commons
  set_up

  if station_code eq '' then begin
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
    ; ###############################################################################
    ; ###############################################################################
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
  ; ###############################################################################
  ; ###############################################################################

  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  ; ###############################################################################
  idate0 = string(yr_i, mh_i, format = '(I4,I02)')
  TGM_n = event_case([yr_i, mh_i, dy_i])
  ; ###############################################################################
  time = findgen(file_number * 1440) / 1440.0
  Date = string(yr_i, mh_i, dy_i, format = '(I4, "-", I02, "-", I02)')
  ; ###############################################################################

  ; ###############################################################################
  ; Generate the time series variables
  ; define H variables
  ; dH  = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
  ; dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
  data = lmag_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'min')
  H = data.h
  SQ = data.sq

  idx = sym_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'm')
  symH = idx.symH
  asymH = idx.asyH

  ; idx2 = sym0_array([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])

  ; rc = dst_0([yr_i,mh_i,dy_i], [yr_f,mh_f,dy_f])
  ; Q = rc.Q
  ; symH0 = idx2.symH0
  ip = ip_arraym([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
  Bt = ip.bt
  Bz = ip.bz
  ; Bx = ip.Bx
  ; By = ip.By
  P = ip.n_p
  V = ip.vx
  ; Vy = ip.Vy
  ; Vz = ip.Vz
  T = ip.t_p
  E = ip.ey
  a = ae_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
  AE = a.ae

  ; ###############################################################################
  ; identifying NAN percentage values in the Time Series
  ; implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo

  H = add_nan(H, 99999.0, 'equal')
  H = add_nan(H, 200.0, 'greater')

  ; symH0 = fillnan(symH0)
  ; H = fillnan(H)
  ; plot, time, H
  ; oplot, time, symH, color=150
  ; DEVICE, true=24, retain=2, decomposed=0
  ; TVLCT, R_bak, G_bak, B_bak, /GET
  ; LOADCT, 39, /SILENT

  ; X_label = xlabel([yr_i, mh_i, dy_i], file_number)
  old_month = month_name(mh_i, 'english')
  new_month = month_name(mh_f, 'english')
  if mh_i ne mh_f then begin
    time_name = 'days of ' + old_month + ' and ' + new_month
  endif else begin
    time_name = 'days of ' + old_month
  endelse
  set_plot, 'x'
  time = findgen(n_elements(H)) / 1440.0

  class = gms_class(station_code)
  info = stationlist(class, station_code)
  mlat = info.mlat

  H_hr = fltarr(n_elements(H) / 60)

  for i = 0, n_elements(H_hr) - 1 do begin
    H_hr[i] = median(H[i * 60 : (i + 1) * 60 - 1])
  endfor

  l = mlat
  mlat = l * !pi
  ld = cos(mlat / 180)
  p_a = symH * ld
  baseline = p_a
  Bdiono = H - baseline

  Bdiono_hr = fltarr(n_elements(H) / 60)

  for i = 0, n_elements(Bdiono_hr) - 1 do begin
    Bdiono_hr[i] = median(Bdiono[i * 60 : (i + 1) * 60 - 1])
  endfor
  H = fillnan(H)
  ; ###############################################################################
  ; ###############################################################################
  ; print, Q
  ; path='/home/isaac/longitudinal_studio/fig/magdata/'
  path2 = '/home/isaac/longitudinal_studio/fig/'
  ; path = set_var.local_dir + 'output/TS_plots/'
  ; antes de aplicar XWT, se modifica la resolución temporal de H de 1 minuto a una hora

  SQ_hr = fltarr(n_elements(SQ) / 60)

  for i = 0, n_elements(SQ_hr) - 1 do begin
    SQ_hr[i] = median(SQ[i * 60 : (i + 1) * 60 - 1])
  endfor

  n = 1
  tmp = findgen(n_elements(H_hr))
  ; plot, tmp[n:*], H_hr[n:*], title='H hourly median'
  ; print, min(H_hr[n:*])
  Bdiono_hr = fillnan(Bdiono_hr)
  wave_test, AE, H, Bdiono_hr, SQ_hr, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, ps = 'ps'
  ; ts_plots, asymH,symH, H_hr, SQ, Bdiono,res, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code
  ; ip_plots, symH, P, V, T, E, Bz, Bt, asymH, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path2
end
