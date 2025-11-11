function ae_struct, date
  on_error, 2
  compile_opt idl2, hidden

  year = date[0]
  month = date[1]
  day = date[2]
  ; ###############################################################################
  ; reading data files
  @set_up_commons
  set_up

  path = set_var.mega_dir + 'ae/daily/'
  date = string(year, month, day, format = '(I4,I02,I02)')
  file_name = path + 'ae_' + date + '.dat'

  header = 0 ; Defining number of lines of the header
  ; ###############################################################################
  ; reading data files

  file = file_search(file_name, count = opened_files)
  if opened_files ne n_elements(file) then message, file_name + ' not found'

  number_of_lines = file_lines(file)
  ;
  data = strarr(number_of_lines)

  openr, lun, file, /get_lun, error = err
  readf, lun, data, format = '(A)'
  close, lun
  free_lun, lun

  DataStruct = {ae: 0, au: 0, al: 0, ao: 0}
  r_ip = replicate(DataStruct, number_of_lines - header)
  ; PRINT, data[header:number_of_lines-1]
  reads, data[header : number_of_lines - 1], r_ip, format = '(I6, I7, I7, I7)'
  RETURN, r_ip
end

function ae_array, date_i, date_f
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
  ; define DH variables

  data_path = set_var.mega_dir + 'ae/daily/'
  string_date = strarr(file_number)
  fname = strarr(file_number)

  for i = 0ll, file_number - 1 do begin
    tmp_year = 0
    tmp_month = 0
    tmp_day = 0
    tmp_julday = julday(mh_i, dy_i, yr_i)

    caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
    string_date[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,I02,I02)')

    fname[i] = data_path + 'ae_' + string_date[i] + '.dat'

    ; PRINT, data_file_name_sym
  endfor
  exist_data_file = file_test(fname)
  capable_to_plot = n_elements(where(exist_data_file eq 1))

  if capable_to_plot ne n_elements(fname) then begin
    print, format = '(''CRITICAL ERROR: impossible to read data file(s).'')'
    print, format = '(''                missing GMS_YYYYMMDD.AE electric field data .'',A,'' impossible to plot all data.'')'
  endif
  ; ###############################################################################
  ; IP Data
  tmp_AE = fltarr(file_number * 1440)
  tmp_AU = fltarr(file_number * 1440)
  tmp_AL = fltarr(file_number * 1440)
  tmp_AO = fltarr(file_number * 1440)

  for i = 0, n_elements(exist_data_file) - 1 do begin
    if exist_data_file[i] eq 1 then begin
      tmp_year = 0
      tmp_month = 0
      tmp_day = 0
      tmp_julday = julday(mh_i, dy_i, yr_i)

      caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
      string_date[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,I02,I02)')
      dat = ae_struct([tmp_year, tmp_month, tmp_day])
      tmp_AE[i * 1440 : ((i + 1) * 1440) - 1] = dat.ae[*]
      tmp_AU[i * 1440 : ((i + 1) * 1440) - 1] = dat.au[*]
      tmp_AL[i * 1440 : ((i + 1) * 1440) - 1] = dat.al[*]
      tmp_AO[i * 1440 : ((i + 1) * 1440) - 1] = dat.ao[*]
      ; asym[i*1440:(i+1)*1440-1] = dat.ASY_H[*]
    endif else begin
      tmp_AE[i * 1440 : ((i + 1) * 1440) - 1] = 999.99
      tmp_AU[i * 1440 : ((i + 1) * 1440) - 1] = 9999.99
      tmp_AL[i * 1440 : ((i + 1) * 1440) - 1] = 9999.99
      tmp_AO[i * 1440 : ((i + 1) * 1440) - 1] = 99999.9

      ; asym[i*1440:(i+1)*1440-1] =999999.0
    endelse
  endfor
  tmp_AE = add_nan(tmp_AE, 999.99, 'equal')
  tmp_AU = add_nan(tmp_AU, 9999.99, 'equal')
  tmp_AL = add_nan(tmp_AL, 9999.99, 'equal')
  tmp_AO = add_nan(tmp_AO, 99999.9, 'equal')

  ae_struct = {ae: tmp_AE, au: tmp_AU, al: tmp_AL, ao: tmp_AO}

  RETURN, ae_struct
end
