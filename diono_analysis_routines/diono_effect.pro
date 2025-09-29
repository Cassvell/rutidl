; Name:
; diono_effect
; purpose:
; print approximation of the Diono effect in ppc
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
; .r diono_valid
; diono_valid, [yyyy, mm, dd], [yyyy, mm, dd]
; parameters:
;
;
; dependencies:
;
;
; input files
; Dst, dH, Kp, Kmex, Ap, Amex, Newkmex, Bsq data
;
; output files:
; geomagnetic index plot with local effects
; prompt list with comparative analysis
; .PNG figure imported to /output/eventos_tgm/diono_final_V6_yyyy-mm-dd.png
;
; VERSION
; Dec, 2022

pro diono_effect, date_i, date_f, jpeg = jpeg
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
  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1

  tot_days = findgen(file_number * 24) / 24.0
  Date = string(yr_i, mh_i, dy_i, format = '(I4, "-", I02, "-", I02)')
  ; ###############################################################################
  idate0 = string(yr_i, mh_i, format = '(I4,I02)')
  ; TGM_n  = event_case([yr_i,mh_i,dy_i])
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
  TGM_n = ''
  print, 'Enter GMS event: 	0:20'
  read, TGM_n, prompt = '> '
  print, 'event: ', TGM_n
  ; ###############################################################################
  ; Generate the time series

  ; define Dst and dH variables
  dH = dh_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station, fix(station_idx))
  dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
  dat = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code, 'H')
  H = dat.h
  ; ###############################################################################
  ; identifying NAN percentage values in the Time Series
  dH = nanpc(dH, 999999.0, 'equal')
  dH = nanpc(dH, 100.0, 'greater')
  ; setting certain values as NaN
  ; dH      = add_nan(dH, 999999.0, 'equal')
  dH = add_nan(dH, 100.0, 'greater')
  ; H       = add_nan(H, 99999.0, 'greater')

  ; implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo
  dH = fillnan(dH)
  H = fillnan(H)
  ; ###############################################################################
  ; Import the structure of diono generated variables
  dionstr = gen_diono(dst, H, 28.06, 'h', TGM_n, dig_filter = 'dig_filter')

  ; compute frequencies
  fn = dionstr.fn

  ; compute diono variables
  diono = dionstr.diono
  dp2 = dionstr.dp2
  ddyn = dionstr.ddyn
  dst_l = dionstr.p_a + dp2 + ddyn

  ; ###############################################################################
  time = timegen(n_elements(dst), start = julday(mh_i, dy_i, yr_i, 0), units = 'Hours')
  caldat, time, M, D, Y, hour

  doy = intarr(n_elements(hour))
  yy = intarr(n_elements(hour))
  ; print, N_ELEMENTS(Y)
  for i = 0ll, n_elements(hour) - 1 do begin
    yy[i] = strmid(string(Y[i], format = '(I4)'), 2, 2)
    ; print, yy[i]
    doy[i] = Date2DOY(string(yy[i], M[i], D[i], format = '(I02,I02,I02)'))
  endfor

  ; PRINT, doy
  ; ###############################################################################
  data_path = set_var.mega_dir + '/article_events/dst_lambda/'

  idate = string(yr_i, mh_i, dy_i, format = '(I4,I02,I02)')
  fdate = string(yr_f, mh_f, dy_f, format = '(I4,I02,I02)')

  outfile = data_path + 'tgmdata' + idate + '_' + fdate + '.dat'
  openw, lun, outfile, /get_lun

  ; PRINTF, lun, 'DOY', 'hora', 'Dst(l)', 'DH', FORMAT = '(3A,6A,6A,6A)'
  for i = 0, n_elements(dst_l) - 1 do begin
    ; print, doy[i], hour[i], dst[i], H[i], tec[i], med[i], FORMAT = '(I03, F5.1, I5, I5, F6.1, F5.2)'
    printf, lun, doy[i], hour[i], dst_l[i], dH[i], format = '(I03, F5.1, I5, F6.1)'
  endfor

  close, lun
  free_lun, lun
end
