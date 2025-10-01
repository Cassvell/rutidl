;
; Name:
; sym_array.pro
; purpose:
; sym_data:   Generate an structured arrar of data from planetary sym index database for day
; sym_array:  from daily data structure, generate time series of the structure data base
;
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
; .r sym_array
; sym_array([yyyy,mm,dd])
;
; parameters:
; Sym index data files
;
; dependencies:
; ISGI
;
; input files
; sym_yyyymmddh.dat
;
; output files:
; sym time series for data analysis
;
; Version
; March, 2023

function sym_data, date, res
  on_error, 2
  compile_opt idl2, hidden

  year = date[0]
  month = date[1]
  day = date[2]
  ; ###############################################################################
  ; reading data files
  @set_up_commons
  set_up
  path = set_var.mega_dir
  date = string(year, month, day, format = '(I4,"-",I02,"-",I02)')
  file_name = path + 'sym/daily/sym_' + date + res + '_D.dat'

  header = 0 ; Defining number of lines of the header
  ; ###############################################################################
  ; reading data files
  file = file_search(file_name, count = opened_files)
  if opened_files ne n_elements(file) then begin
    file_name = path + 'sym/daily/sym_' + date + res + '_P.dat'
    file = file_search(file_name, count = opened_files)
    file = file_search(file_name, count = opened_files)
  endif

  file = file_search(file_name, count = opened_files)
  if opened_files ne n_elements(file) then begin
    file_name = path + 'sym/daily/sym_' + date + res + '_Q.dat'
    file = file_search(file_name, count = opened_files)
    file = file_search(file_name, count = opened_files)
  endif

  file = file_search(file_name, count = opened_files)
  if opened_files ne n_elements(file) then message, file_name + ' not found'

  number_of_lines = file_lines(file)
  ;
  data = strarr(number_of_lines)

  openr, lun, file, /get_lun, error = err
  readf, lun, data, format = '(A)'
  close, lun
  free_lun, lun

  DataStruct = {asy_d: 0, asy_h: 0, sym_d: 0, sym_h: 0}
  r_sym = replicate(DataStruct, number_of_lines - header)
  ; PRINT, data[header:number_of_lines-1]
  reads, data[header : number_of_lines - 1], r_sym, format = '(I4, X, I4, X, I5, X, I5)'
  RETURN, r_sym
end

function sym_array, date_i, date_f, res, help = help
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

  file_number = (julday(mh_f, dy_f, yr_f) - julday(mh_i, dy_i, yr_i)) + 1
  ; define DH variables
  data_path = set_var.mega_dir

  string_date_2 = strarr(file_number)
  data_file_name_sym = strarr(file_number)

  for i = 0ll, file_number - 1 do begin
    tmp_year = 0
    tmp_month = 0
    tmp_day = 0
    tmp_julday = julday(mh_i, dy_i, yr_i)

    caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
    string_date_2[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,"-",I02,"-",I02)')

    data_file_name_sym[i] = data_path + 'sym/daily/sym_' + string_date_2[i] + res + '_D.dat'

    file_sym = file_search(data_file_name_sym[i], count = opened_files)
    if opened_files ne n_elements(file_sym) then begin
      data_file_name_sym[i] = data_path + 'sym/daily/sym_' + string_date_2[i] + res + '_P.dat'
    endif

    file_sym = file_search(data_file_name_sym[i], count = opened_files)
    if opened_files ne n_elements(file_sym) then begin
      data_file_name_sym[i] = data_path + 'sym/daily/sym_' + string_date_2[i] + res + '_Q.dat'
    endif
    print, data_file_name_sym
  endfor
  exist_data_file_sym = file_test(data_file_name_sym)
  capable_to_plot_sym = n_elements(where(exist_data_file_sym eq 1))

  if capable_to_plot_sym ne n_elements(data_file_name_sym) then begin
    print, format = '(''CRITICAL ERROR: impossible to read data file(s).'')'
    print, format = '(''                missing GMS_YYYYMMDD.sym_index.'',A,'' impossible to plot all data.'')'
  endif
  ; ###############################################################################
  ; sym Data
  sample = 0
  if res eq 'm' then sample = 1440 else sample = 24
  tmp_symH = fltarr(file_number * sample)
  tmp_symD = fltarr(file_number * sample)
  tmp_AsyH = fltarr(file_number * sample)
  tmp_AsyD = fltarr(file_number * sample)
  ; asym   = FLTARR(file_number*sample)
  for i = 0, n_elements(exist_data_file_sym) - 1 do begin
    if exist_data_file_sym[i] eq 1 then begin
      tmp_year = 0
      tmp_month = 0
      tmp_day = 0
      tmp_julday = julday(mh_i, dy_i, yr_i)

      caldat, tmp_julday + i, tmp_month, tmp_day, tmp_year
      string_date_2[i] = string(tmp_year, tmp_month, tmp_day, format = '(I4,"-",I02,"-",I02)')
      dat = sym_data([tmp_year, tmp_month, tmp_day], res)

      tmp_symH[i * sample : (i + 1) * sample - 1] = dat.sym_h[*]
      tmp_AsyH[i * sample : (i + 1) * sample - 1] = dat.asy_h[*]
      tmp_symD[i * sample : (i + 1) * sample - 1] = dat.sym_d[*]
      tmp_AsyD[i * sample : (i + 1) * sample - 1] = dat.asy_d[*]
    endif else begin
      tmp_symH[i * sample : (i + 1) * sample - 1] = 9999
      tmp_AsyH[i * sample : (i + 1) * sample - 1] = 9999
      tmp_symD[i * sample : (i + 1) * sample - 1] = 9999
      tmp_AsyD[i * sample : (i + 1) * sample - 1] = 9999
    endelse
  endfor

  variable = {symH: tmp_symH, symD: tmp_AsyH, asyH: tmp_AsyH, asyD: tmp_AsyD}

  RETURN, variable
end
