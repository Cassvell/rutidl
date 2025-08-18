
pro wave_test, H_loc, H, SQ, date_i, date_f, station_code, ps = ps
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

  bfield = H

  arr = n_elements(bfield)
  times = findgen(arr) ; arreglo de tiempo arbitrario

  dt = 60. ; resolusión temporal en minutos

  pad = 1
  s0 = dt
  dj = 0.0625
  j1 = 14. / dj
  mother = 'Morlet'

  aa = bfield
  time1 = findgen(n_elements(H))
  time2 = findgen(n_elements(SQ))
  ; Note: for accurate reconstruction and variance computation, set:
  ; s0 = dt    for Morlet
  ; s0 = dt/4  for Paul
  ; (Most commonly, s0=2*dt
  wave = wavelet(aa, dt, period = period, scale = scale, s0 = s0, $
    coi = coi, dj = dj, j = j1, mother = mother, /recon, /pad, signif = signif)

  ; wave = WAVELET(bfield,dt,PERIOD=period,SCALE=scale,S0=s0, $
  ; COI=coi,DJ=dj,J=j1,MOTHER=mother,/RECON,/PAD,signif=signif)

  power = (abs(wave)) ^ 2 ; compute wavelet power spectrum
  nscale = n_elements(period)

  ; ************************************************************************************
  n = n_elements(bfield)
  sst = aa
  recon_sst = sst ; save an extra copy, so we don't erase original sst

  global_ws = total(power, 1) / n ; global wavelet spectrum (GWS)
  J = n_elements(scale) - 1

  SIGLVL = 0.05 ; (siginficance level=.05 (95% confidence level))  ;1sigma=.683; 2sigma=.954 ; 3sigma=0.9973

  ; Significance levels, assuming the GWS as background spectrum:
  signif = wave_signif(sst, dt, scale, 0, $
    gws = global_ws, siglvl = SIGLVL, mother = mother)
  signif = rebin(transpose(signif), n, J + 1) ; expand signif --> (J+1)x(N) array
  signif = power / signif ; where ratio > 1, power is significant
  ; PRINT, GWS
  ; GWS significance levels:
  dof = n - scale ; the -scale corrects for padding at edges
  global_signif = wave_signif(sst, dt, scale, 1, $
    lag1 = 0.90, dof = dof, mother = mother, cdelta = Cdelta, psi0 = psi0)

  ; check total variance (Parseval's theorem) [Eqn(14)]
  scale_avg = rebin(transpose(scale), n, J + 1) ; expand scale-->(J+1)x(N) array
  power_norm = power / scale_avg
  variance = (moment(sst))[1]
  recon_variance = dj * dt / (Cdelta * n) * total(power_norm) ; [Eqn(14)]

;  if (n_elements(recon_sst) gt 1) then begin
;    recon_variance = (moment(recon_sst))[1]
;    ; RMS of Reconstruction [Eqn(11)]
;    rms_error = sqrt(total((sst - recon_sst) ^ 2) / n)

    ; Scale-average
;    avg = where((scale ge 0.5) and (scale lt 3.3))
;    print, 'avg: ',avg
;    scale_avg = dj * dt / Cdelta * total(power_norm[*, avg], 2) ; [Eqn(24)]
;    scaleavg_signif = wave_signif(sst, dt, scale, 2, $
      ; GWS=global_ws,SIGLVL=SIGLVL,DOF=[0.3,50.0],MOTHER=mother)
;      gws = global_ws, siglvl = SIGLVL, dof = [.5, 3.3], mother = mother)
;  endif

  ; ==============================================================================
  ; ==============================================================================
  ; SMOOTHING
  ; time average WS

  wave2 = wavelet(SQ, dt, period = period, scale = scale2, s0 = s0, $
    coi = coi1, dj = dj, j = j1, mother = mother, /recon, /pad, signif = signif)

  ; print, 'size of power: ', size(power)
  ; print, scale1 - scale2
  wave_coherency, wave2, time2, scale2, wave, time1, scale, coi1 = coi1, dt = dt, dj = dj, wave_coher = wave_coher, wave_phase = wave_phase, $
    time_out = time_out, scale_out = scale_out, coi_out = coi_out, global_coher = global_coher, global_phase = global_phase, $
    cross_wavelet = cross_wavelet, power1 = power1, power2 = power2, nosmooth = nosmooth, verbose = verbose

  n = 5
  semblance = cos(wave_phase) ^ n

  ddyn = wave_coher * semblance

  ; ==============================================================================
  ; ==============================================================================
  if keyword_set(ps) then begin
    path = set_var.local_dir + 'output/wavelet/' + station_code + '/evento13/'
    test = file_test(path, /directory)
    if test eq 0 then begin
      file_mkdir, path
      print, 'PATH directory ' + path
      print, 'created'
    endif else begin
      print, ''
    endelse
     make_psfig_composed, H, H_loc,SQ, wave2, power, cross_wavelet, ddyn, period, coi, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], station_code
    ; make_psfig1, power, period, coi, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path,  station_code
    ; make_psfig2, real_part(cross_wavelet), period, coi_out, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code
    ; make_psfig3, ddyn, period, coi_out, [yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], path, station_code
  endif
end