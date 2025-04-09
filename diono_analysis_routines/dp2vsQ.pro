pro dp2vsQ, diono, asymH, dp2, Q, prc,H,station_code
On_error, 2
COMPILE_OPT idl2, HIDDEN
;###############################################################################
@set_up_commons
set_up	
ndata = (n_elements(asymH))

;ndays = (JULDAY(mh_f,dy_f,yr_f)-JULDAY(mh_i,dy_i,yr_i))+1
;asymH = asymH;[250:1089]
;dp2 = dp2[250:1089]  
;Q = Q[250:1089]
;diono = diono[250:1089]  
ndata = (n_elements(asymH))
    dir=set_var.Mega_dir
    outfile = dir+'pca/'+station_code+'_2015-03-17_2015-03-18.dat'    
    OPENW, LUN, outfile, /GET_LUN
    for i=0, ndata-1 do begin
        PRINTF, LUN, asymH[i], Q[i], diono[i], dp2[i], FORMAT='(F8.4,X,F10.4,X,F10.4,X,F10.4,X)'
    ; Get the corresponding data for the day
    endfor
    CLOSE, LUN
    FREE_LUN, LUN    

;###############################################################################
;###############################################################################
;###############################################################################
ini = 0
fin = 400

ini2 = 700
fin2 = 800

ini3 = 1000
fin3 = 1140

ini4 = 400
fin4 = 690


time = findgen(n_elements(asymH))
; 
; 
path='/home/isaac/longitudinal_studio/fig/corr/'
psfile = path+station_code+'_corr.eps'


plot, time[0:1140], H[0:1140], color=255
cgPS_open, psfile, XOffset=0., YOffset=0., default_thickness=1., font=0, /encapsulated, $
/nomatch, XSize=6, YSize=6

class = gms_class(station_code)
info = stationlist(class, station_code)



cgplot, asymH[ini4:fin4], diono[ini4:fin4], position = [0.16, .15, 0.9, 0.9], xstyle=5, ystyle=5, xrange=[min(asymH), 350],/nodata
;cgplot, asymH[ini4:fin4], diono[ini4:fin4], position = [0.16, .15, 0.9, 0.9], xstyle=5, ystyle=5, xrange=[min(asymH), 350],/nodata

x = [0, 0.5, 0, -0.5]
y = [0.5, 0, -0.5, 0]

; Load the symbol into IDL
USERSYM, x, y, /FILL

;cgoplot, asymH, diono, PSYM=8, color='gray', SYMSIZE=2
;cgOPlot, asymH[ini:fin], diono[ini:fin], PSYM=8, color='blue', SYMSIZE=2
;cgOPlot, asymH[ini2:fin2], diono[ini2:fin2], PSYM=8, color='orange', SYMSIZE=2
;cgOPlot, asymH[ini3:fin3], diono[ini3:fin3], PSYM=8, color='purple', SYMSIZE=2
cgOPlot, asymH[ini4:fin4], prc[ini4:fin4], PSYM=8, color='gray', SYMSIZE=2
cgOPlot, asymH[ini4:fin4], diono[ini4:fin4], PSYM=8, color='red', SYMSIZE=2
cgAXIS, XAXIS = 0,$   
xminor=8,$
xtitle='ASYMH [nT]',$
xstyle=1,$
; COLOR=negro, $
CHARSIZE = 1.4 , $
TICKLEN=0.04,$
CHARTHICK=3.5 

cgAXIS, XAXIS = 1,$   
xminor=8,$
XTICKFORMAT='(A1)',$
xstyle=1,$
; COLOR=negro, $
CHARSIZE = 1.4 , $
TICKLEN=0.04,$
CHARTHICK=3.5 


H_I = Textoidl('H_I')
cgAXIS, YAXIS = 0, $
YTITLE = H_I+' [nT]', $                          
;COLOR=negro, $
YSTYLE=1, $
CHARSIZE = 1.4,$
CHARTHICK=1.6 

cgAXIS, YAXIS = 1, $
; COLOR=negro, $                                                                      
YSTYLE=1, $       
YTICKFORMAT='(A1)',$
CHARSIZE = 1.2 ,$
CHARTHICK=1.6   

corr5 = correlate(asymH[ini4:fin4],diono[ini4:fin4])
corr=correlate(asymH[ini4:fin4], prc[ini4:fin4])


P = Textoidl('\rho_1')
P2 = Textoidl('\rho_2')

cgtext, 0.7, 0.8, STRING(P, corr5, FORMAT='(A, ": ", F6.2)'), /NORMAL, $
ALIGNMENT=0.5, CHARSIZE=1.65, color='black'

cgtext, 0.7, 0.7, STRING(P2, corr, FORMAT='(A, ": ", F6.2)'), /NORMAL, $
ALIGNMENT=0.5, CHARSIZE=1.65, color='black'


ini_time = 13 + info.utc 
fin_time = 17 + info.utc
title = STRING(STRUPCASE(station_code), ini_time, fin_time,FORMAT='(A, " LT:  ", I02,"25","-", I02,"50","h")')

x = (!X.Window[1] - !X.Window[0]) /  2. + !X.Window[0]
y = 0.93   
XYOUTS, X, y, title, /NORMAL, $
ALIGNMENT=0.5, CHARSIZE=1.65     

diono_zoom = diono[0:ndata-1]
asymH_zoom= asymH[0:ndata-1]
res = linfit(asymH_zoom, diono_zoom, SIGMA=sigma, YFIT=yfit, COVAR=covar)

a = res[0]
b = res[1]

ndata_zoom = n_elements(asymH_zoom)
sum_x = fltarr(ndata_zoom)
sum_y = fltarr(ndata_zoom)

for i = 0, ndata_zoom-1 do begin
    tmp_y = (diono_zoom[i]-yfit[i])^2
    sum_y[i] = tmp_y
    tmp_x = (asymH_zoom[i] - mean(asymH_zoom))^2
    sum_x[i] = tmp_x
endfor

free_degrees = ndata_zoom-2
sb1 = sqrt((1.0/float(free_degrees))*(total(sum_y)/total(sum_x)))

print, 'standard error: ', string(sb1, format = '(F10.8)')

print, 'slope ', b
a_std = sigma[0]
b_std = sigma[1]


t_critical=t_cvf(0.025, free_degrees)

;ci_a = t_critical * a_std
ci_b = 1.963 * sb1


print, 'b_std', b_std
print, 'slope ci; ', ci_b
print, 't_critical: ',t_critical
print, 'number of data: ', ndata
;print, 'confidence interval slope: ', ci_b


;cgoplot, asymH_zoom, yfit, linestyle=0, color= 'black', thick=2

yfit2 = (a) + asymH_zoom*(b-ci_b)
yfit3 = (a) + asymH_zoom*(b+ci_b)

;cgoplot, asymH_zoom, yfit2, linestyle=1, color= 'black', thick=1
;cgoplot, asymH_zoom, yfit3, linestyle=1, color= 'black', thick=1
i = where(diono lE 0)


cgPS_Close, density = 300, width = 1600;, /PNG 




end
