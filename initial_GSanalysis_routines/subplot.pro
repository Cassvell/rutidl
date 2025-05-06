; IDL script to combine two EPS images into a two-panel figure
pro subplot
compile_opt idl2
  ;-----------------------------------------------------------
  ; USER CONFIGURATION (change these paths)
  ;-----------------------------------------------------------
  path = '../rutidl/output/'
  eps1 = path+'dispersion_general_dst.eps'       ; Path to first EPS file
  eps2 = path+'idispersion_general_dst_lm.eps'       ; Path to second EPS file
  output_file = 'combined_result.png'  ; Output file
  dpi = 300                 ; Output resolution
  ;-----------------------------------------------------------

  ; Set up PostScript device for reading EPS
  set_plot, 'PS'
  device, /encapsulated, filename='temp.eps', /close

  ; Read first image
  read_eps, eps1, image1
  dims1 = size(image1, /dimensions)

  ; Read second image
  read_eps, eps2, image2
  dims2 = size(image2, /dimensions)

  ; Check if images have same dimensions
  if not array_equal(dims1, dims2) then begin
    print, 'Warning: Images have different dimensions!'
    print, 'Image1:', dims1, 'Image2:', dims2
    ; Resize image2 to match image1 (optional)
    image2 = congrid(image2, dims1[0], dims1[1])
  endif

  ; Create combined image
  combined = fltarr(2*dims1[0], dims1[1], 3)  ; For color images
  combined[0:dims1[0]-1, *, *] = image1
  combined[dims1[0]:*, *, *] = image2

  ; Write output
  write_png, output_file, combined, dpi=dpi

  ; Display result
  window, /free, xsize=dims1[0]*2, ysize=dims1[1]
  tv, combined

  print, 'Successfully created: ', output_file

end

; Helper function to read EPS files
pro read_eps, filename, image
compile_opt idl2

  ; Set up temporary PS device
  set_plot, 'PS'
  device, /encapsulated, filename='temp_read.eps', /close
  
  ; Read EPS file
  image = read_image(filename)
  
  ; Convert to [0,1] range if needed
  if max(image) gt 1 then image /= 255.0
  
  ; Handle grayscale images
  if size(image, /n_dimensions) eq 2 then begin
    new_image = fltarr(size(image,1), size(image,2), 3)
    new_image[*,*,0] = image
    new_image[*,*,1] = image
    new_image[*,*,2] = image
    image = new_image
  endif
end