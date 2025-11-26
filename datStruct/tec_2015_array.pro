function tec_2015data, date, station_code
  on_error, 2
  compile_opt idl2, hidden
  @set_up_commons
  set_up

  year = date[0]
  month = date[1]
  day = date[2]

  path = set_var.mega_dir + 'tec/' + station_code + '2015/'

  station = 0

  case station_code of
    'teo': station = 'ucoe'
    'gui': station = 'lpal'
    'jai': station = 'npgj'
    'kak': station = 'tsk2'
    else: print, 'use valid station'
  endcase

  date_string = strmid(string(year, format = '(i4)'), 2, 2) + string(month, format = '(i2)') + string(day, format = '(i2)')

  doy = Date2DOY(date_string)

  file_name = path + station + '_' + string(doy, format = '(I03)') + '_' + string(year, format = '(I4)') + '.dat'
  print, file_name

  file = file_search(file_name, count = opened_files)

  header = 1
  number_of_lines = file_lines(file)
  data = strarr(number_of_lines)

  if station_code ne 'jai' then begin
    if opened_files ne n_elements(file) then message, file_name + ' not found'

    openr, lun, file, /get_lun, error = err
    readf, lun, data, format = '(A)'
    close, lun
    free_lun, lun

    DStruct = {ut: 0.0, tec: 0.0, lon_grad: 0.0, lat_grad: 0.0, lon_grad_q: 0.0, lat_grad_q: 0.0, gradt: 0.0, gradt_q: 0.0}
    r_tec = replicate(DStruct, number_of_lines - header)
    reads, data[header : number_of_lines - 1], r_tec, $
      format = '(F7, F11, F11, F11, F11, F10, F11, F11)'
  endif else begin
    readcol, file_name, ut, tec, lon_grad, lat_grad, lon_grad_q, lat_grad_q, gradt, gradt_q, format = ('F,F,F,F,F,F,F,F'), /nan

    r_tec = {ut: ut, tec: tec, lon_grad: lon_grad, lat_grad: lat_grad, lon_grad_q: lon_grad_q, lat_grad_q: lat_grad_q, $
      gradt: gradt, gradt_q: gradt_q}
  endelse
  RETURN, r_tec
end

function tec_2015_array, idate, fdate, station_code
  on_error, 2
  compile_opt idl2, hidden

  yr_i = idate[0]
  mh_i = idate[1]
  dy_i = idate[2]

  yr_f = fdate[0]
  mh_f = fdate[1]
  dy_f = fdate[2]
  ; ###############################################################################
  @set_up_commons
  set_up

  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  ; define DH variables

  station = 0

  case station_code of
    'teo': station = 'ucoe'
    'gui': station = 'lpal'
    'jai': station = 'npgj'
    'kak': station = 'tsk2'
    else: print, 'use valid station'
  endcase

  path = set_var.mega_dir + 'tec/' + station_code + '2015/'

  ; date_string = strmid(string(year, format = '(i4)'), 2, 2) + string(month, format = '(i2)') + string(day, format = '(i2)')

  ; doy = Date2DOY(date_string)

  string_date = strarr(file_number)
  string_date2 = strarr(file_number)
  doy = strarr(file_number)
  data_file_name = strarr(file_number)

  for i = 0ll, file_number - 1 do begin
    tmp_year = 0
    tmp_month = 0
    tmp_day = 0
    tmp_julday = julday(mh_i, dy_i, yr_i)

    caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
    string_date[i] = strmid(string(tmp_year, format = '(i4)'), 2, 2) + $
      string(tmp_month, format = '(i2)') + string(tmp_day, format = '(i2)')
    doy[i] = Date2DOY(string_date[i])

    data_file_name[i] = path + station + '_' + string(doy[i], format = '(I03)') + '_' + $
      string(tmp_year, format = '(I4)') + '.dat'

    ; print, data_file_name[i]

    file = file_search(data_file_name[i], count = opened_files)
  endfor
  ; file_name = path + 'u' + station + '_' + string(doy, format = '(I03)') + '_' + string(year, format = '(I4)') + '.dat'
  exist_data_file = file_test(data_file_name)
  capable_to_plot = n_elements(where(exist_data_file eq 1))

  sample = 0
  if station_code eq 'teo' or station_code eq 'kak' then begin
    sample = 48
  endif else begin
    sample = 24
  endelse

  ; if res eq 'm' then sample = 1440 else sample = 24
  tmp_UT = fltarr(file_number * sample)
  tmp_tec = fltarr(file_number * sample)
  tmp_gradlon = fltarr(file_number * sample)
  tmp_gradlat = fltarr(file_number * sample)
  tmp_grad_qlat = fltarr(file_number * sample)
  tmp_grad_qlon = fltarr(file_number * sample)
  tmp_grad_t = fltarr(file_number * sample)
  tmp_grad_qt = fltarr(file_number * sample)

  for i = 0, n_elements(exist_data_file) - 1 do begin
    if exist_data_file[i] eq 1 then begin
      tmp_year = 0
      tmp_month = 0
      tmp_day = 0
      tmp_julday = julday(mh_i, dy_i, yr_i)

      caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
      string_date2[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,I02,I02)')
      dat = tec_2015data([tmp_year, tmp_month, tmp_day], station_code)

      tmp_UT[i * sample : (i + 1) * sample - 1] = dat.ut[*]
      tmp_tec[i * sample : (i + 1) * sample - 1] = dat.tec[*]
      tmp_gradlon[i * sample : (i + 1) * sample - 1] = dat.lon_grad[*]
      tmp_gradlat[i * sample : (i + 1) * sample - 1] = dat.lat_grad_q[*]
      tmp_grad_qlat[i * sample : (i + 1) * sample - 1] = dat.lon_grad_q[*]
      tmp_grad_qlon[i * sample : (i + 1) * sample - 1] = dat.lat_grad_q[*]
      tmp_grad_t[i * sample : (i + 1) * sample - 1] = dat.gradt[*]
      tmp_grad_qt[i * sample : (i + 1) * sample - 1] = dat.gradt_q[*]
    endif else begin
      tmp_UT[i * sample : (i + 1) * sample - 1] = 999.999
      tmp_tec[i * sample : (i + 1) * sample - 1] = 999.999
      tmp_gradlon[i * sample : (i + 1) * sample - 1] = 999.999
      tmp_gradlat[i * sample : (i + 1) * sample - 1] = 999.999
      tmp_grad_qlat[i * sample : (i + 1) * sample - 1] = 999.999
      tmp_grad_qlon[i * sample : (i + 1) * sample - 1] = 999.999
      tmp_grad_t[i * sample : (i + 1) * sample - 1] = 999.999
      tmp_grad_qt[i * sample : (i + 1) * sample - 1] = 999.999
    endelse
  endfor
  tmp_tec = add_nan(tmp_tec, 999.999, 'equal')
  ; print, tmp_tec

  variable = {ut: tmp_UT, tec: tmp_tec, lon_grad: tmp_gradlon, lat_grad: tmp_gradlat, $
    lon_grad_q: tmp_grad_qlon, lat_grad_q: tmp_grad_qlat, gradt: tmp_grad_t, gradt_q: tmp_grad_qt}

  return, variable
end

function med_tec, idate, fdate, station_code
  on_error, 2
  compile_opt idl2, hidden

  yr_i = idate[0]
  mh_i = idate[1]
  dy_i = idate[2]

  yr_f = fdate[0]
  mh_f = fdate[1]
  dy_f = fdate[2]
  ; ###############################################################################
  @set_up_commons
  set_up

  ndays = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  file_number = (27)
  ; define DH variables
  path = set_var.mega_dir + 'tec/' + station_code + '2015/'
  station = 0

  case station_code of
    'teo': station = 'ucoe'
    'gui': station = 'lpal'
    'jai': station = 'npgj'
    'kak': station = 'tsk2'
    else: print, 'use valid station'
  endcase
  ; date_string = strmid(string(year, format = '(i4)'), 2, 2) + string(month, format = '(i2)') + string(day, format = '(i2)')

  ; doy = Date2DOY(date_string)

  string_date = strarr(file_number)
  string_date2 = strarr(file_number)
  doy = strarr(file_number)
  data_file_name = strarr(file_number)
  prev_doy = strarr(file_number)

  for i = 0ll, file_number - 1 do begin
    tmp_year = 0
    tmp_month = 0
    tmp_day = 0
    tmp_julday = julday(mh_i, dy_i, yr_i)

    caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
    string_date[i] = strmid(string(tmp_year, format = '(i4)'), 2, 2) + $
      string(tmp_month, format = '(i2)') + string(tmp_day, format = '(i2)')
    doy[i] = Date2DOY(string_date[i])
    prev_doy[i] = fix(doy[i]) - 27
    data_file_name[i] = path + station + '_' + string(prev_doy[i], format = '(I03)') + '_' + $
      string(tmp_year, format = '(I4)') + '.dat'

    ; print, data_file_name[i]
    file = file_search(data_file_name[i], count = opened_files)
  endfor
  ; file_name = path + 'u' + station + '_' + string(doy, format = '(I03)') + '_' + string(year, format = '(I4)') + '.dat'
  exist_data_file = file_test(data_file_name)
  capable_to_plot = n_elements(where(exist_data_file eq 1))

  sample = 0 ; 048
  if station_code eq 'teo' or station_code eq 'kak' then begin
    sample = 48
  endif else begin
    sample = 24
  endelse

  tmp_tec = fltarr(file_number, sample)

  date_27dprev = daytodoy(prev_doy[0], yr_i)

  mh_ii = strmid(string(date_27dprev), 4, 2)

  dy_ii = strmid(string(date_27dprev), 6, 2)

  for i = 0, n_elements(exist_data_file) - 1 do begin
    if exist_data_file[i] eq 1 then begin
      tmp_year = 0
      tmp_month = 0
      tmp_day = 0
      tmp_julday = julday(fix(mh_ii), fix(dy_ii), yr_i)

      caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
      string_date2[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,I02,I02)')
      dat = tec_2015data([tmp_year, tmp_month, tmp_day], station_code)
      tmp_tec[i, *] = dat.tec[*] ; Store first 48 values
    endif ; else begin
    ; tmp_tec[i * sample : (i + 1) * sample - 1] = 9999
    ; endelse
  endfor

  tmp_tec = add_nan(tmp_tec, 999.999, 'equal')
  tec_med = fltarr(sample)
  for j = 0, sample - 1 do begin
    tec_med[j] = median(tmp_tec[*, j])
  endfor

  tec_med_array = fltarr(sample * ndays)

  for k = 0, ndays - 1 do begin
    start_idx = k * sample
    end_idx = start_idx + sample - 1

    ; Assuming you have daily data for each day
    ; Replace 'daily_data[k]' with your actual daily data source
    daily_data = tec_med ; You need to implement this function

    tec_med_array[start_idx : end_idx] = daily_data
  endfor

  return, tec_med_array
end
