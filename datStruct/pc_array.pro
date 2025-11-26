function pc_struct, date
  on_error, 2
  compile_opt idl2, hidden

  year = date[0]
  month = date[1]
  day = date[2]
  ; ###############################################################################
  ; reading data files
  @set_up_commons
  set_up

  path = set_var.mega_dir + 'PC/daily/'
  date = string(year, month, day, format = '(I4,I02,I02)')
  file_name = path + 'pcn_' + date + '.dat'

  ; header = 0 ; Defining number of lines of the header
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

  DataStruct = {pcn: 0.}
  r_ip = replicate(DataStruct, number_of_lines)
  ; PRINT, data[header:number_of_lines-1]
  reads, data[0 : number_of_lines - 1], r_ip, format = '(F6)'
  RETURN, r_ip
end

function pc_array, date_i, date_f
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

  data_path = set_var.mega_dir + 'PC/daily/'
  string_date = strarr(file_number)
  fname = strarr(file_number)

  for i = 0ll, file_number - 1 do begin
    tmp_year = 0
    tmp_month = 0
    tmp_day = 0
    tmp_julday = julday(mh_i, dy_i, yr_i)

    caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
    string_date[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,I02,I02)')

    fname[i] = data_path + 'pcn_' + string_date[i] + '.dat'

    ; PRINT, data_file_name_sym
  endfor
  exist_data_file = file_test(fname)
  capable_to_plot = n_elements(where(exist_data_file eq 1))

  if capable_to_plot ne n_elements(fname) then begin
    print, format = '(''CRITICAL ERROR: impossible to read data file(s).'')'
    print, format = '(''                missing GMS_YYYYMMDD.PC electric field data .'',A,'' impossible to plot all data.'')'
  endif
  ; ###############################################################################
  ; IP Data
  tmp_pc = fltarr(file_number * 1440)

  for i = 0, n_elements(exist_data_file) - 1 do begin
    if exist_data_file[i] eq 1 then begin
      tmp_year = 0
      tmp_month = 0
      tmp_day = 0
      tmp_julday = julday(mh_i, dy_i, yr_i)

      caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
      string_date[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,I02,I02)')
      dat = pc_struct([tmp_year, tmp_month, tmp_day])
      tmp_pc[i * 1440 : ((i + 1) * 1440) - 1] = dat.pcn[*]
      ; asym[i*1440:(i+1)*1440-1] = dat.ASY_H[*]
    endif else begin
      tmp_pc[i * 1440 : ((i + 1) * 1440) - 1] = 999.00

      ; asym[i*1440:(i+1)*1440-1] =999999.0
    endelse
  endfor
  tmp_pc = add_nan(tmp_pc, 999.00, 'equal')

  RETURN, tmp_pc
end
