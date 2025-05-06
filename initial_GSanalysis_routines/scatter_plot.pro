;+
; NAME:
;       scatter_plot.pro
;
;
; PURPOSE:
;
;       statistic data analysis
;
; AUTHOR:
;
;       Pedro Corona Romero
;       C. Isaac Castellanos Velazco
;       Instituto de Geofisica, UNAM
;       UNAM Campus Morelia, Antigua Carretera a Patzcuaron,
;       Exhacienda de San Jose del Cerrito, Morelia, Michoacan, Mexico
;       piter.cr@gmail.com
;       ccastellanos@igeofisica.unam.mx
;       06.iv.mmxxii
;
; CATEGORY:
;
;       Numerical Data Analize
;
; CALLING SEQUENCE:
;       .r scatter_plot
;       scatter_plot
;
;       Description:
;       ???????????????????????
;
;
; PARAMETERS:
;       Dst index    : dH index
;
; KEYWORD PARAMETERS:
;
;       /????????? : ?????????????
;
; DEPENDENCIAS:
;       ?????????? : ????????????
;
; ARCHIVOS ANALIZADOS:
;       ??????????
;
; ARCHIVOS DE SALIDA: grafica.png de dispersión
;
; HISTORIA:
;   versión 1.1, agosto 2022

pro scatter_plot, png = png, ps = ps
  compile_opt idl2
  @set_up_commons
  set_up
  input_dir = set_var.mega_dir + '/article_events/dst_lambda/'
  path = set_var.local_dir + 'output/'

  ; iyr	= idate[0]
  ; imh	= idate[1]
  ; idy = idate[2]

  ; fyr	= fdate[0]
  ; fmh	= fdate[1]
  ; fdy = fdate[2]

  ; header = 1      ; Defining number of lines of the header

  ; idate = string(iyr, imh, idy, format = '(I4, I02, I02)')
  ; fdate = string(fyr, fmh, fdy, format = '(I4, I02, I02)')

  data_files = file_search(input_dir + 'tgmdata' + '?????????????????' + '.dat')

  files_lines_number = file_lines(data_files)
  MX_latitude = 28.06 * !pi / 180.
  ; corellation_index  = FLTARR(N_ELEMENTS(data_files));<<<<<<<<<<<<<<<<preguntar

  limits = [0, -40, -50, -60, -80, -100, -120, -200] ; [-50, -200]

  ; print, data_files

  dat_str0 = {doy: intarr(max(files_lines_number)), $
    hour: fltarr(max(files_lines_number)), $
    dst: intarr(max(files_lines_number)), $
    dh: fltarr(max(files_lines_number)), $
    number_of_lines: 0, $
    correlation: fltarr(n_elements(limits)), $
    correlation_points: intarr(n_elements(limits))}

  data = replicate(dat_str0, n_elements(data_files))
  data[*].number_of_lines = files_lines_number
  ; print, data[*].number_of_lines
  data[*].number_of_lines = files_lines_number - 1
  data[*].correlation[*] = 999.
  ; print, data[*].number_of_lines
  ; Return
  dh = fltarr(1)

  dst = fltarr(1)
  for event = 0, n_elements(data_files) - 1 do begin
    data_strings = strarr(files_lines_number[event])

    openr, file_lun, data_files[event], /get_lun
    readf, file_lun, data_strings ; , FORMAT = '(A)'
    free_lun, file_lun
    ; print, data_strings
    ; return
    dat_str1 = {doy: 0, hour: 0., dst: 0, dh: 0}
    tmp_data = replicate(dat_str1, data[event].number_of_lines)

    reads, data_strings[1 : *], tmp_data, $
      format = '(I03,X,F4.1,X,I04,F6.1)'

    data[event].doy[*] = 999
    ; print, data[event].number_of_lines, N_ELEMENTS(tmp_data[*].doy)
    ; RETURN
    data[event].doy[0 : data[event].number_of_lines - 1] = tmp_data[*].doy

    data[event].hour[*] = 999.
    data[event].hour[0 : data[event].number_of_lines - 1] = tmp_data[*].hour

    data[event].dst[*] = 999.
    data[event].dst[0 : data[event].number_of_lines - 1] = tmp_data[*].dst * cos(MX_latitude)

    data[event].dh[*] = 999.
    data[event].dh[0 : data[event].number_of_lines - 1] = tmp_data[*].dh

    dh = [dh, tmp_data[*].dh]
    dst = [dst, tmp_data[*].dst * cos(MX_latitude)]
    ; PRINT, correlate(dh, dst)^2
    tmp_index = where(data[event].dh[*] ge 999.)
    data[event].dst[tmp_index] = 999.
  endfor

  print, 'Correlación global'
  print, correlate(dh, dst) ^ 2
  n = where(dh GE -1000 )
  dh = dh[n]
  dst = dst[n]
  ; Define thresholds and calculate partial correlations
  thresholds = [0, -50, -100]
  for t=0, n_elements(thresholds)-1 do begin
      thresh = thresholds[t]
      
      ; Positive range indices
      idx = where(dst ge thresh and dst lt 20, count)
      ; Negative range indices
      j = where(dst lt thresh and dst ge -250, count)
      
      ; Only calculate if we have enough points
      if count gt 1 then begin
          corr1 = correlate(dh[idx], dst[idx]) ^ 2
          corr2 = correlate(dh[j], dst[j]) ^ 2
          print, 'límite, ' + string(thresh) + ' nT'
          print, corr1
          print, corr2
      endif
  endfor

lim = 711


  plot, dst[0:lim], dh[0:lim], psym=4, xrange=[-250,100],yrange=[-250,100], ystyle=1, xstyle=1

  correlation = fltarr(n_elements(data_files))
  for event = 0, n_elements(data_files) - 1 do begin
    correlation[event] = correlate(data[event].dst[0 : data[event].number_of_lines - 1], $
      data[event].dh[0 : data[event].number_of_lines - 1])

    ; PRINT, N_ELEMENTS(data[*].dst[0:data[event].number_of_lines-1])
    for i = 0, n_elements(limits) - 1 do begin
      if i eq 0 then index_tmp = where(data[event].dst[*] ge limits[i] and data[event].dst[*] lt 200) $
      else index_tmp = where(data[event].dst[*] ge limits[i] and data[event].dst[*] lt limits[i - 1])

      if n_elements(index_tmp) gt 2 then data[event].correlation[i] = correlate(data[event].dst[index_tmp], data[event].dh[index_tmp])
      data[event].correlation_points[i] = n_elements(index_tmp)
    endfor
  endfor

  ; PRINT,  '    corr_med', '    corr_std'
  means = fltarr(n_elements(limits))
  devs = fltarr(n_elements(limits))
  maxs = fltarr(n_elements(limits))
  mins = fltarr(n_elements(limits))
  ; PRINT, means
  for i = 0, n_elements(limits) - 1 do begin
    ; PRINT, n_elements(limits), limits
    ; PRINT, data[*].correlation[i]
    index_tmp = where(data[*].correlation[i] lt 100)
    ; PRINT, index_tmp
    ; PRINT, data[index_tmp].correlation[i]
    ; means[i] = Mean( data[index_tmp].correlation[i]^2 )
    ; devs[i]  = STDDEV( data[index_tmp].correlation[i]^2, /NAN)
    ; maxs[i]  = MAX( data[index_tmp].correlation[i] )
    ; mins[i]  = MIN( data[index_tmp].correlation[i] )
    ; PRINT, means[i];, devs[i];, maxs[i], mins[i]
    print, ''
    ; PRINT, data[index_tmp].correlation[i]^2;, data[index_tmp].doy
  endfor
  if keyword_set(png) then begin
    makefig_png, dst, dh, med, desv, path
  endif

  if keyword_set(ps) then begin
    makefig_ps, dst, dh, med, corr1, corr2, desv, path
  endif

  RETURN
end

pro makefig_ps, dst, dh, med, R1, R2, desv, path
  compile_opt idl2

  ; LOADCT, 0, /SILENT

  ; path = '../rutidl/output/'
  ; H = TeXtoIDL('\Delta H')

  psfile = path + 'dispersion_general_dst_lm.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 3., font = 0, /encapsulated, $
    xsize = 6, ysize = 6, /landscape
  event = 0
  up0 = 20
  down0 = -100
  slope = findgen(601) - 500
  x1 = slope
  y1 = slope
  cgPlot, x1, y1, xstyle = 5, ystyle = 5, color = 'black', background = 'white', yrange = [down0, up0], $
    xrange = [down0, up0], charsize = 1.7, charthick = 1.2, xtitle = '', ytitle = 'H', $
    position = [0.16, 0.12, 0.96, 0.96], title = '', /nodata

  ; POLYFILL, [!X.CRANGE[0], 0, 0, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], 0, 0], COLOR=verde

  ; POLYFILL, [!X.CRANGE[0], -20, -75, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -75, -75], $
  ; COLOR=amarillo, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=0.03, SPACING=0.3

  ; POLYFILL, [!X.CRANGE[0], -75, -75, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -75, -75], $
  ; COLOR=amarillo, /LINE_FILL, ORIENTATION=45, LINESTYLE=0, THICK=0.03, SPACING=0.3

  ; cgPolygon, [!X.CRANGE[0], -50, -50, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -50, -50], $
  ; Color = cgColor("GRN2"),  /FILL

  cgPolygon, [!x.crange[0], -100, -100, !x.crange[0]], [!y.crange[0], !y.crange[0], -100, -100], $
    color = cgColor('GRN3'), /fill

  AXIS, xaxis = 0, xtitle = '', charsize = 1.6, xrange = [down0, up0], xstyle = 1, $
    charthick = 2
  AXIS, xaxis = 1, xtickname = replicate(' ', 8), xrange = [down0, up0], $
    charthick = 2

  AXIS, yaxis = 0, ytitle = '', charsize = 1.6, color = negro, ystyle = 1, $
    charthick = 2
  AXIS, yaxis = 1, ytickname = replicate(' ', 7), $
    charthick = 2

  ; symbols =[1,2,4,5,6,7]
  ; colors  =[azul,rojo,amarillo,negro,naranja,verde]

  ; FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
  ; oplot, data[event].dst[*], data[event].dh[*], $
  ; psym=symbols[event], color=colors[event], thick=1, symsize=1

  ; oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
  ; data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
  ; color=colors[event]
  ; ENDFOR

  cgOPlot, dst, dh, psym = 4, color = cgColor('Charcoal'), symsize = 0.5

  cgOPlot, x1, y1, thick = 1.0, color = 'black'
  ; ###############################################################################
  ; ###############################################################################
  med = Textoidl('R^2 = ')
  xyouts, 0.2, 0.85, /normal, $
    ; med, COLOR=negro, $
    string(med, R1, format = '(A, F4.2)'), $
    charsize = 2, $
    charthick = chr_thick1

  ;desv = Textoidl('(20 \leq dst \leq -100)')
  ;xyouts, 0.2, 0.8, /normal, $
  ;  desv, $
    ; desv+' '+corr_std, COLOR=negro, $
 ;   charsize = 1.2, $
 ;   charthick = chr_thick1
  ; ###############################################################################
 ; med = Textoidl('R^2 = ')
 ; xyouts, 0.63, 0.25, /normal, $
    ; med, COLOR=negro, $
  ;  string(med, R2, format = '(A, F4.2)'), $
  ;  charsize = 1.3, $
  ;  charthick = chr_thick1

  ;desv = Textoidl('(-100 \leq dst \leq -250)')
  ;xyouts, 0.63, 0.20, /normal, $
  ;  desv, $
    ; desv+' '+corr_std, COLOR=negro, $
  ;  charsize = 1.3, $
  ;  charthick = chr_thick1

  d_H = Textoidl('\DeltaH [nT]')
  dt = Textoidl('\DeltaT')
  dstlbd = Textoidl('Dst_\lambda [nT]')

  y = (.75 - 0.25) / 2. + 0.25
  xyouts, 0.04, y, d_H, /normal, $
    alignment = 0.5, charsize = 1.6, orientation = 90, charthick = 1.8

  x = (!x.window[1] - !x.window[0]) / 2. + !x.window[0]
  xyouts, x, 0.02, dstlbd, /normal, $
    color = negro, alignment = 0.5, charsize = 1.4, charthick = 1.5

  ; pic = cgSnapshot(/PNG, FILENAME='test_1')
  cgPS_Close, density = 300, height = 600 ; , /PNG, IM_options='-trim -bordercolor white -border 50', /DELETE_PS

  RETURN
end

pro makefig_png, dst, dh, med, desv, path
  compile_opt idl2

  ; ###############################################################################
  ; define device and color parameters
  ; ###############################################################################
  Device_bak = !d.name
  set_plot, 'Z'

  Xsize = fix(600)
  Ysize = 600
  device, set_resolution = [Xsize, Ysize]
  device, z_buffering = O
  device, set_character_size = [10, 12]

  chr_size1 = 0.9
  chr_thick1 = 1.1
  space = 0.015
  rojo = 248
  amarillo = 200
  naranja = 220
  verde = 150
  negro = 0
  azul = 100
  blanco = 255
  gris = 90
  morado = 16

  tvlct, R_bak, G_bak, B_bak, /get

  loadct, 0, /silent

  ; path = '../rutidl/output/eventos_tgm/'
  H = Textoidl('\DeltaH')
  dt = Textoidl('\DeltaT')
  dstlbd = Textoidl('Dst(\lambda)')
  event = 0
  up0 = 100
  down0 = -250
  slope = findgen(601) - 500
  x1 = slope
  y1 = slope
  plot, x1, y1, xstyle = 5, ystyle = 5, color = negro, background = blanco, yrange = [down0, up0], $
    xrange = [down0, up0], charsize = 1.7, charthick = 1.2, xtitle = 'Dst', ytitle = 'H', $
    position = [0.17, 0.1, 0.9, 0.9], title = 'GS Events: dispersion study', /nodata

  ; POLYFILL, [!X.CRANGE[0], 0, 0, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], 0, 0], COLOR=verde

  ; POLYFILL, [!X.CRANGE[0], -20, -75, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -75, -75], $
  ; COLOR=amarillo, /LINE_FILL, ORIENTATION=-45, LINESTYLE=0, THICK=0.03, SPACING=0.3

  polyfill, [!x.crange[0], -75, -75, !x.crange[0]], [!y.crange[0], !y.crange[0], -75, -75], $
    color = 'yellow', /line_fill, orientation = 45, linestyle = 0, thick = 0.03, spacing = 0.3

  ; POLYFILL, [!X.CRANGE[0], -50, -50, !X.CRANGE[0]], [!Y.CRANGE[0], !Y.CRANGE[0], -50, -50], $
  ; COLOR=amarillo

  polyfill, [!x.crange[0], -100, -100, !x.crange[0]], [!y.crange[0], !y.crange[0], -100, -100], $
    color = naranja

  AXIS, xaxis = 0, xtitle = 'Dst [nT]', charsize = 1.1, color = negro, xrange = [down0, up0], xstyle = 1, $
    charthick = 2
  AXIS, xaxis = 1, color = negro, xtickname = replicate(' ', 8), xrange = [down0, up0], $
    charthick = 2

  AXIS, yaxis = 0, ytitle = H + ' [nT]', charsize = 1.1, color = negro, ystyle = 1, $
    charthick = 2
  AXIS, yaxis = 1, color = negro, ytickname = replicate(' ', 7), $
    charthick = 2

  ; symbols =[1,2,4,5,6,7]
  ; colors  =[azul,rojo,amarillo,negro,naranja,verde]

  ; FOR event=0, N_ELEMENTS(data_files)-1 DO BEGIN
  ; oplot, data[event].dst[*], data[event].dh[*], $
  ; psym=symbols[event], color=colors[event], thick=1, symsize=1

  ; oplot, limits[WHERE(data[event].correlation_points[*] GT 3)], $
  ; data[event].correlation[WHERE(data[event].correlation_points[*] GT 3)], $
  ; color=colors[event]
  ; ENDFOR

  oplot, dst, dh, psym = 4, color = gris, symsize = 0.5

  oplot, x1, y1, thick = 1.0, color = negro
  ; ###############################################################################
  ; ###############################################################################
  med = Textoidl('R^2 = ')
  xyouts, 0.2, 0.79, /normal, $
    ; med, COLOR=negro, $
    string(med, 0.77, format = '(A, F4.2)'), color = negro, $
    charsize = 1.2, $
    charthick = chr_thick1

  desv = Textoidl('20 $\leq$ dst $\leq$ -100')
  xyouts, 0.2, 0.84, /normal, $
    desv, color = negro, $
    ; desv+' '+corr_std, COLOR=negro, $
    charsize = 1.2, $
    charthick = chr_thick1
  ; ###############################################################################
  med = Textoidl('R^2 = ')
  xyouts, 0.2, 0.65, /normal, $
    ; med, COLOR=negro, $
    string(med, 0.42, format = '(A, F4.2)'), color = negro, $
    charsize = 1.2, $
    charthick = chr_thick1

  desv = Textoidl('(-100 \leq dst \leq -250)')
  xyouts, 0.2, 0.70, /normal, $
    desv, color = negro, $
    ; desv+' '+corr_std, COLOR=negro, $
    charsize = 1.2, $
    charthick = chr_thick1

  Image = tvrd()
  tvlct, reds, greens, blues, /get ; reads Z buffer !!
  tvlct, R_bak, G_bak, B_bak

  ; DEVICE, /CLOSE
  set_plot, Device_bak

  ; -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
  ; open the post stript device
  ; -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
  print, '        Setting PNG as default file type.'
  write_png, path + 'dispersion_general_dst.png', Image, reds, greens, blues
  print, '        Saving: ' + path + 'dispersion_general_dst_ld.png'
  print, ''
  RETURN
end
