;
; Name:
; H_array.pro
; purpose:
; generación de vector H sin variación día a día
; author:
; Carlos Isaac Castellanos Velazco
; Estudiante de Maestría en Ciencias de la Tierra
; Instituto de Geofísica, Unidad Michoacan
; UNAM
; ccastellanos@igeofisica.unam.mx
;
; category:
; data generator
;
; calling sequence:
; .r H_array
; H = H_array(idate[yyyy,mm,dd], fdate[yyyy,mm,dd], 'station', n, resolution)
;
; parameters:
; 'station': set by set_var.gms[FIX(station_idx)],  where ;0:coeneo, 1:teoloyuca, 2:tucson,
; 3:bsl, 4:iturbide
;
; 'n': set by set_var.gms_code[FIX(station_idx)], where ;0:coeneo, 1:teoloyuca, 2:tucson,
; 3:bsl, 4:iturbide
;
; resolution : 'H' for hourly data, 'min' for min resolution data
;
; dependencies:
; INTERMAGNET
; REGMEX
;
; input files
; geomagnetic field measurements from a certain observatory or geomagnetic station.
;
; output files:
; H vector from n obs/station
;
;
; version
; Dec, 2022
; Feb, 2023
; Jun, 2023
; feb, 2024
; note
; For following analysis, this routine has to be run to create clean H obs data
;

function ppfm_array, station_code
  on_error, 2
  compile_opt idl2, hidden
  ; -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
  ; reading data files
  ; -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
  @set_up_commons
  set_up

  dir = set_var.mega_dir + '/ppef_model/'

  file_name = dir + 'PPFM_' + station_code + '.csv'

  readcol, file_name, date, ppef, quiet, both, delimiter = ',', format = 'A,F,F,F,F'

  return, ppef
end
