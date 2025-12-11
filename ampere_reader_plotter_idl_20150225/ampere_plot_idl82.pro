;
; AMPERE Data Plotter
; 
; Purpose: This software is contains reference routine for plotting AMPERE fitted magnetic perturbations
;          and radial current densities from netcdf data files downloadable from the AMPERE data
;          browser located at http://ampere.jhuapl.edu. This software does not constitute a complete
;          analysis package for AMPERE data. It is instead aimed at helping the end user to 
;          correctly visualize the AMPERE data.
;          
; License: This software is freeware and comes with ABSOLUTE NO WARRANTIES. Use of these routines
;          or portions thereof in third-party software is permitted. The author is thankful for 
;          acknowledgment.
;          
; Use:     The software is built on IDL's new graphics system and therefore requires IDL 8.0.1 or above.
;          The calling convention is: AMPERE_PLOT, fname, start_hour, start_minute, show_globe=show_globe
;          where fname is the netcdf filename obtained through the AMPERE data browser,
;                start_hour is the hour for the begin of the interval,
;                start_minute is the minute of the start of the interval and must be even, and
;                the show_globe keyword specifies whether the world map should be displayed in the plots.
;                
; Additional requirements: This software requires IDL 8.2 or above. The use of the show_globe keyword 
;          requires the IDL AACGM DLM v5.1 or above, available at http://dysprosium.jhuapl.edu/idl_aacgm.
; 
; Author:
; Haje Korth
; JHU/APL
; November 2012
;


function ymd2dn,yr,m,d

  idays=[0,31,59,90,120,151,181,212,243,273,304,334,366]

  lpyr=(((yr mod 4) eq 0) and ((yr mod 100) ne 0)) $
    or ((yr mod 400) eq 0) and (m ge 3)

  dy=d+idays(m-1)+lpyr
return, dy
end


pro ampere_plot_idl82,ncdfname,start_hr,start_mt,show_globe=show_globe

  thisexcept=!except
  !except=0
  
; set variables
  dimensions=[1000,500]

  latmin=40.0
  latmax=90.0
  lonmin=0.0
  lonmax=360.0

  dlat=10
  dlon=90    
  dlatmin=40
  
  jrmin=-1.0
  jrmax=1.0

  arrow_scl=2000.


; read netcdf data
  read_ampere_ncdf,ncdfname,data
  
  idx=where((data.start_hr eq start_hr) and (data.start_mt eq start_mt),cnt)
  if (cnt eq 0) then begin
    print,'Interval not found!'
    return
  endif

  start_dy=data[idx].start_dy
  start_mo=data[idx].start_mo
  start_yr=data[idx].start_yr
  start_hr=data[idx].start_hr
  start_mt=data[idx].start_mt
  end_hr=data[idx].end_hr
  end_mt=data[idx].end_mt
  
  nlat=data[idx].nlat
  nlon=data[idx].nlon
  colat=data[idx].colat
  mlt=data[idx].mlt
  dbnorth1=data[idx].dbnorth1
  dbeast1=data[idx].dbeast1
  dbnorth2=data[idx].dbnorth2
  dbeast2=data[idx].dbeast2
  jr=data[idx].jr

  lat=90.0-colat
  lon=mlt*15.0
  if (mean(colat) lt 90) then begin
    hem_str='north'
    hem=1 
  endif else begin
    hem_str='south'
    hem=-1
  endelse


; add fitted dB plot
  if (hem_str eq 'north') then begin
    dbmap=map('Stereographic',dimensions=dimensions,center_latitude=90,center_longitude=0,limit=[latmin,lonmin,latmax,lonmax],$
      position=[0.05,0.05,0.45,0.9],grid_longitude=45,grid_latitude=10,label_show=0,transparency=50,name='dbmap')

    print,'Plotting dB vectors...'

    idx=where(lat ge latmin)
    tlat=lat[idx]
    tlon=lon[idx]
    tdbnorth1=dbnorth1[idx]
    tdbeast2=dbeast2[idx]

    tlon1=fltarr(n_elements(tlon))
    tlat1=fltarr(n_elements(tlat))
    for i=0,n_elements(tlon)-1 do begin
      xy0=dbmap.mapforward(tlon[i],tlat[i])    
      uvn=[-xy0[0],-xy0[1]]
      uve=[-xy0[1],+xy0[0]]
      mg=sqrt(total(uvn^2))
      uvn=uvn/mg
      uve=uve/mg       
      x1=xy0[0]+(tdbnorth1[i]*uvn[0]+tdbeast2[i]*uve[0])*arrow_scl
      y1=xy0[1]+(tdbnorth1[i]*uvn[1]+tdbeast2[i]*uve[1])*arrow_scl
      tp=dbmap.mapinverse(x1,y1)
      tlon1[i]=tp[0]       
      tlat1[i]=tp[1]       
    endfor
          
    !null=arrow([transpose(tlon),transpose(tlon1)],[transpose(tlat),transpose(tlat1)],/data,head_size=0.1,head_indent=1,target='dbmap')

    for i=lonmin,lonmax-dlon,dlon do t=text(i,latmin,string(i/15.,format='(i2.2)'),/data,alignment=0.5,target='dbmap')
    for i=latmin,latmax-dlat,dlat do t=text(22.5,i,string(i,format='(i2)'),/data,alignment=0.5,target='dbmap')
  endif else begin
    dbmap=map('Stereographic',dimensions=dimensions,center_latitude=-90,center_longitude=180,limit=[-latmin,lonmin,-latmax,lonmax],$
      position=[0.05,0.05,0.45,0.9],grid_longitude=45,grid_latitude=10,label_show=0,transparency=50,name='dbmap')
      
    print,'Plotting dB vectors...'
      
    idx=where(lat le -latmin)
    tlat=lat[idx]
    tlon=lon[idx]
    tdbnorth1=dbnorth1[idx]
    tdbeast2=dbeast2[idx]

    tlon1=fltarr(n_elements(tlon))
    tlat1=fltarr(n_elements(tlat))
    for i=0,n_elements(tlon)-1 do begin
      xy0=dbmap.mapforward(360-tlon[i],tlat[i])    
      uvn=[+xy0[0],+xy0[1]]
      uve=[-xy0[1],+xy0[0]]
      mg=sqrt(total(uvn^2))
      uvn=uvn/mg
      uve=uve/mg       
      x1=xy0[0]+(tdbnorth1[i]*uvn[0]+tdbeast2[i]*uve[0])*arrow_scl
      y1=xy0[1]+(tdbnorth1[i]*uvn[1]+tdbeast2[i]*uve[1])*arrow_scl
      tp=dbmap.mapinverse(x1,y1)       
      tlon1[i]=tp[0]       
      tlat1[i]=tp[1]       
    endfor

    !null=arrow([transpose(360-tlon),transpose(tlon1)],[transpose(tlat),transpose(tlat1)],/data,head_size=0.1,head_indent=1,target='dbmap')
    
    for i=lonmin,lonmax-dlon,dlon do t=text(i,-latmin,string(i/15.,format='(i2.2)'),/data,alignment=0.5,target='dbmap')
    for i=-latmax+dlat,-latmin,dlat do t=text(-22.5,i,string(i,format='(i3)'),/data,alignment=0.5,target='dbmap')
  endelse


; add jr plot
  jr2d=reform(jr,nlat,nlon)
  jr2d=transpose(jr2d)
  jr2d=reverse(jr2d,2)
  jr2d=[[jr2d],jr2d[0,*]]
  jr2d=congrid(jr2d,nlon*10,nlat*10,/interp,/minus_one)
  jr2d=bytscl(jr2d,min=jrmin,max=jrmax)

  if (hem_str eq 'north') then begin
    facmap=map('Stereographic',/current,center_latitude=90,center_longitude=0,limit=[latmin,lonmin,latmax,lonmax],$
      position=[0.55,0.05,0.95,0.9],grid_longitude=45,grid_latitude=10,label_show=0,transparency=50,name='facmap')
  
    if keyword_set(show_globe) then begin
      read_jpeg,'globe_aacgm_800km.jpg',globe  
      cdf_epoch,epoch,start_yr,start_mo,start_dy,start_hr,start_mt,/compute
      mlong=aacgm_mlong(epoch,0)
      globe_sz=size(globe,/dimensions)
      globe=shift(globe,0,-mlong*globe_sz[1]/360.,0)
      globeimg=image(globe,/overplot,limit=[latmin,lonmin,latmax,lonmax],grid_units=2,image_location=[-180,-90],$
        image_dimensions=[360,180],/interpolate)
    endif
  
    restore,'blue_white_red_15.ctl'
    facimg=image([[[rr[jr2d]]],[[gg[jr2d]]],[[bb[jr2d]]],[[aa[jr2d]]]],/overplot,limit=[latmin,lonmin,latmax,lonmax],$
      grid_units=2,image_location=[0,dlatmin],image_dimensions=[360,90-dlatmin],/interpolate,transparency=20)
  
    for i=lonmin,lonmax-dlon,dlon do t=text(i,latmin,string(i/15.,format='(i2.2)'),/data,alignment=0.5,$
      font_color='black',fill_color='white',fill_background=1,transparency=20,target='facmap')
    for i=latmin,latmax-dlat,dlat do t=text(22.5,i,string(i,format='(i2)'),/data,alignment=0.5,$
      font_color='black',fill_color='white',fill_background=1,transparency=20,target='facmap')

    colbarimg=image([[jrmin,jrmin],[jrmax,jrmax]],/overplot,/current,rgb_table=[[rr],[gg],[bb]],max_value=1,image_dimensions=[1.0d-5,1.0d-5])      
    colbar=colorbar(target=colbarimg,position=[0.55,0.70,0.57,0.85],title='$\mu A/m^2$',orientation=1,border_on=1)
  endif else begin
    jr2d=reverse(jr2d,1)
    jr2d=reverse(jr2d,2)

    facmap=map('Stereographic',/current,center_latitude=-90,center_longitude=180,limit=[-latmin,lonmin,-latmax,lonmax],$
      position=[0.55,0.05,0.95,0.9],grid_longitude=45,grid_latitude=10,label_show=0,transparency=50,name='facmap')
  
    if keyword_set(show_globe) then begin
      read_jpeg,'globe_aacgm_800km.jpg',globe  
      globe=reverse(globe,2)      
      cdf_epoch,epoch,start_yr,start_mo,start_dy,start_hr,start_mt,/compute
      mlong=aacgm_mlong(epoch,0)
      globe_sz=size(globe,/dimensions)
      globe=shift(globe,0,mlong*globe_sz[1]/360.,0)
      globeimg=image(globe,/overplot,limit=[latmin,lonmin,latmax,lonmax],grid_units=2,image_location=[-180,-90],$
        image_dimensions=[360,180],/interpolate)
    endif
  
    restore,'blue_white_red_15.ctl'
    facimg=image([[[rr[jr2d]]],[[gg[jr2d]]],[[bb[jr2d]]],[[aa[jr2d]]]],/overplot,limit=[-latmin,lonmin,-latmax,lonmax],$
      grid_units=2,image_location=[0,-90],image_dimensions=[360,90-dlatmin],/interpolate,transparency=20)
  
    for i=lonmin,lonmax-dlon,dlon do t=text(i,-latmin,string(i/15.,format='(i2.2)'),/data,alignment=0.5,$
      font_color='black',fill_color='white',fill_background=1,transparency=20,target='facmap')
    for i=-latmax+dlat,-latmin,dlat do t=text(-22.5,i,string(i,format='(i3)'),/data,alignment=0.5,$
      font_color='black',fill_color='white',fill_background=1,transparency=20,target='facmap')

    colbarimg=image([[jrmin,jrmin],[jrmax,jrmax]],/overplot,/current,rgb_table=[[rr],[gg],[bb]],max_value=1,image_dimensions=[1.0d-5,1.0d-5])      
    colbar=colorbar(target=colbarimg,position=[0.55,0.70,0.57,0.85],title='$\mu A/m^2$',orientation=1,border_on=1)
  endelse


; add title
  month_a=['January','February','March','April','May','June','July','August','September','October','November','December']
  title=string(start_dy,format='(i2.2)')+' '+month_a[start_mo-1]+' '+string(start_yr,format='(i4.4)')+'   '+$
    string(start_hr,format='(i2.2)')+':'+string(start_mt,format='(i2.2)')+' - '+$
    string(end_hr,format='(i2.2)')+':'+string(end_mt,format='(i2.2)')+' ('+hem_str+')'
  t=text(0.5,0.9,title,/normal,/current,alignment=0.5,color='black',font_size=20)
  
; save image
  dbmap.save,'AMPERE.png'

  !except=thisexcept

return
end
