pro export_dp2files, asymH, diono, dp2, dp2_2, station_code, tw
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
  outfile = dir + 'pca/' + station_code + '_' + date + '.dat'
  openw, LUN, outfile, /get_lun
  for i = 0, ndata - 1 do begin
    printf, LUN, asymH[i], diono[i], dp2[i], dp2_2[i], format = '(F9.4,X,F10.4,X,F10.4,X,F10.4,X,F20.10,X,F20.10)'
    ; Get the corresponding data for the day
  endfor
  close, LUN
  free_lun, LUN
end
