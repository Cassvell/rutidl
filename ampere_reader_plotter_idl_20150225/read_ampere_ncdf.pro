pro read_ampere_ncdf, ncdfname, data
  compile_opt idl2
  ;
  ; Haje Korth
  ; JHU/APL
  ; November 2012
  ;
  fileid = ncdf_open(ncdfname, /nowrite)
  varid = ncdf_varid(fileid, 'nlon')
  ncdf_varget, fileid, varid, nlon
  varid = ncdf_varid(fileid, 'nlat')
  ncdf_varget, fileid, varid, nlat
  varid = ncdf_varid(fileid, 'start_yr')
  ncdf_varget, fileid, varid, start_yr
  varid = ncdf_varid(fileid, 'start_mo')
  ncdf_varget, fileid, varid, start_mo
  varid = ncdf_varid(fileid, 'start_dy')
  ncdf_varget, fileid, varid, start_dy
  varid = ncdf_varid(fileid, 'start_hr')
  ncdf_varget, fileid, varid, start_hr
  varid = ncdf_varid(fileid, 'start_mt')
  ncdf_varget, fileid, varid, start_mt
  varid = ncdf_varid(fileid, 'start_sc')
  ncdf_varget, fileid, varid, start_sc
  varid = ncdf_varid(fileid, 'end_yr')
  ncdf_varget, fileid, varid, end_yr
  varid = ncdf_varid(fileid, 'end_mo')
  ncdf_varget, fileid, varid, end_mo
  varid = ncdf_varid(fileid, 'end_dy')
  ncdf_varget, fileid, varid, end_dy
  varid = ncdf_varid(fileid, 'end_hr')
  ncdf_varget, fileid, varid, end_hr
  varid = ncdf_varid(fileid, 'end_mt')
  ncdf_varget, fileid, varid, end_mt
  varid = ncdf_varid(fileid, 'end_sc')
  ncdf_varget, fileid, varid, end_sc
  varid = ncdf_varid(fileid, 'colat')
  ncdf_varget, fileid, varid, colat
  varid = ncdf_varid(fileid, 'mlt')
  ncdf_varget, fileid, varid, mlt
  varid = ncdf_varid(fileid, 'dBnorth1')
  ncdf_varget, fileid, varid, dbnorth1
  varid = ncdf_varid(fileid, 'dBeast1')
  ncdf_varget, fileid, varid, dbeast1
  varid = ncdf_varid(fileid, 'dBnorth2')
  ncdf_varget, fileid, varid, dbnorth2
  varid = ncdf_varid(fileid, 'dBeast2')
  ncdf_varget, fileid, varid, dbeast2
  varid = ncdf_varid(fileid, 'dBr')
  ncdf_varget, fileid, varid, dbr
  varid = ncdf_varid(fileid, 'Jr')
  ncdf_varget, fileid, varid, jr
  varid = ncdf_varid(fileid, 'ddBnorth1')
  ncdf_varget, fileid, varid, ddbnorth1
  varid = ncdf_varid(fileid, 'ddBeast1')
  ncdf_varget, fileid, varid, ddbeast1
  varid = ncdf_varid(fileid, 'ddBnorth2')
  ncdf_varget, fileid, varid, ddbnorth2
  varid = ncdf_varid(fileid, 'ddBeast2')
  ncdf_varget, fileid, varid, ddbeast2
  varid = ncdf_varid(fileid, 'ddBr')
  ncdf_varget, fileid, varid, ddbr
  varid = ncdf_varid(fileid, 'dJr')
  ncdf_varget, fileid, varid, djr
  ncdf_close, fileid

  dims = size(colat, /dimensions)
  data_struct = {nlon: 0, nlat: 0, start_yr: 0, start_mo: 0, start_dy: 0, start_hr: 0, start_mt: 0, start_sc: 0, $
    end_yr: 0, end_mo: 0, end_dy: 0, end_hr: 0, end_mt: 0, end_sc: 0, colat: fltarr(dims[0]), mlt: fltarr(dims[0]), $
    dbnorth1: fltarr(dims[0]), dbeast1: fltarr(dims[0]), dbnorth2: fltarr(dims[0]), dbeast2: fltarr(dims[0]), dbr: fltarr(dims[0]), jr: fltarr(dims[0]), $
    ddbnorth1: fltarr(dims[0]), ddbeast1: fltarr(dims[0]), ddbnorth2: fltarr(dims[0]), ddbeast2: fltarr(dims[0]), ddbr: fltarr(dims[0]), djr: fltarr(dims[0])}
  data = replicate(data_struct, dims[1])
  data.nlon = nlon
  data.nlat = nlat
  data.start_yr = start_yr
  data.start_mo = start_mo
  data.start_dy = start_dy
  data.start_hr = start_hr
  data.start_mt = start_mt
  data.start_sc = start_sc
  data.end_yr = end_yr
  data.end_mo = end_mo
  data.end_dy = end_dy
  data.end_hr = end_hr
  data.end_mt = end_mt
  data.end_sc = end_sc
  data.colat = colat
  data.mlt = mlt
  data.dbnorth1 = dbnorth1
  data.dbeast1 = dbeast1
  data.dbnorth2 = dbnorth2
  data.dbeast2 = dbeast2
  data.dbr = dbr
  data.jr = jr
  data.ddbnorth1 = ddbnorth1
  data.ddbeast1 = ddbeast1
  data.ddbnorth2 = ddbnorth2
  data.ddbeast2 = ddbeast2
  data.ddbr = ddbr
  data.djr = djr

  return
end
