pro draw_map
  on_error, 2
  compile_opt idl2, hidden

  ; Create a plotting window
  ; WINDOW, 0, TITLE='Mollweide Contour'

  ; DEVICE, DECOMPOSED = 0
  ; TVLCT, 255, 255, 255, 254 ; White color
  ; TVLCT, 0, 0, 0, 253       ; Black color
  ; !P.Color = 253
  ; !P.Background = 254

  ; MAP_SET, /MOLLWEIDE, 20, -20, /ISOTROPIC, $
  ; /HORIZON, /GRID, /CONTINENTS, $
  ; TITLE='Mollweide Contour'
  ; CONTOUR, F, lon, lat, NLEVELS=7, $
  ; /OVERPLOT, /DOWNHILL, /FOLLOW

  ; Next, create the Stereographic plot:

  ; Create another plotting window

  ; WINDOW, 1, TITLE='Conic projection'
  ; MAP_SET, /AZIM, 32, -100, LIMIT=[10,-130,35,-70],$
  ; /ISOTROPIC, /CONTINENTS, /GRID, GLINESTYLE = 0, /ADVANCE, $
  ; TITLE='Mexico Contour'
  ; Display points in the northern hemisphere only:
  ; CONTOUR, F(*,10:*), lon(*,10:*), lat(*,10:*), $
  ; /OVERPLOT, NLEVELS=5
  ; MAP_GRID, /LABEL, LATLAB=-120, LONLAB=12, COLOR=255
  ; MAP_CONTINENTS, COLOR=255

  ; CURSOR, LON, LAT &PRINT, LAT,LON

  ; make_psfig
  map
  ; make_pngfig
end

pro map
  on_error, 2
  compile_opt idl2, hidden
  path = '/home/isaac/longitudinal_studio/fig/'
  psfile = path + 'map.eps'

  cgPS_Open, psfile, xoffset = 0., yoffset = 0., default_thickness = 3., font = 0, /encapsulated, $
    /nomatch, xsize = 16, ysize = 10

  cgMap_Set, 10, 30, /continents, /fill_continents, /horizon, $
    isotropic = ISOTROPIC, /mollweide, /grid, con_color = cgColor('gray'), background = 'cyan'

  x = [0, 0.5, 0, -0.5]
  y = [0.5, 0, -0.5, 0]

  ; Load the symbol into IDL
  usersym, x, y, /fill

  teo = obscoord('teo')
  cgPlotS, -teo.longeo, teo.latgeo, psym = 8, symsize = 4, color = 'Red', thick = 12
  cgText, -teo.longeo, teo.latgeo * 1.1, 'TEO', alignment = 0.5 ; , charsize = 0.1

  ; sjg = obscoord('sjg')
  ; CGPLOTS, -sjg.longeo, sjg.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'red', THICK=12
  ; cgText, -sjg.longeo, sjg.latgeo*1.1, 'SJG', alignment = 0.5

  gui = obscoord('gui')
  cgPlotS, -gui.longeo, gui.latgeo, psym = 8, symsize = 4, color = 'Blue', thick = 12
  cgText, -gui.longeo, gui.latgeo * 1.1, 'GUI', alignment = 0.5

  ; tam = obscoord('tam')
  ; CGPLOTS, tam.longeo, tam.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Blue', THICK=12
  ; cgText, tam.longeo, tam.latgeo*1.1, 'TAM', alignment = 0.5

  jai = obscoord('jai')
  cgPlotS, jai.longeo, jai.latgeo, psym = 8, symsize = 4, color = 'Green', thick = 12
  cgText, jai.longeo, jai.latgeo * 1.1, 'JAI', alignment = 0.5

  ; lzh = obscoord('lzh')
  ; CGPLOTS, lzh.longeo, lzh.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12
  ; cgText, lzh.longeo, lzh.latgeo*1.08, 'LZH', alignment = 0.5

  ; bmt = obscoord('bmt')
  ; CGPLOTS, bmt.longeo, bmt.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12
  ; cgText, bmt.longeo, bmt.latgeo*0.85, 'BMT', alignment = 0.5

  ; cyg = obscoord('cyg')
  ; CGPLOTS, cyg.longeo, cyg.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'Gold', THICK=12
  ; cgText, cyg.longeo, cyg.latgeo*1.12, 'CYG', alignment = 0.5

  kak = obscoord('kak')
  cgPlotS, kak.longeo, kak.latgeo, psym = 8, symsize = 4, color = 'Gold', thick = 12
  cgText, kak.longeo, kak.latgeo * 1.05, 'KAK', alignment = 0.5

  ; hon = obscoord('hon')
  ; CGPLOTS, hon.longeo, hon.latgeo, PSYM = 8, SYMSIZE=4, COLOR = 'orange', THICK=12
  ; cgText, hon.longeo, hon.latgeo*1.1, 'HON', alignment = 0.5

  cgPS_Close, density = 300, width = 1000

  RETURN
end
