pro test
  compile_opt idl2
  @set_up_commons
  set_up
  ; dir = set_var.mega_dir + 'ampere/'
  dir = '/home/isaac/Descargas/'
  filename = dir + '20150307.0500.86400.600.north.grd.ncdf'

  read_ampere_ncdf, filename, ampere
end
