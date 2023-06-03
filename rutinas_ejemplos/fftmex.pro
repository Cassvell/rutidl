file = DIALOG_PICKFILE(/READ, GET_PATH=dir)
file_name = FILE_BASENAME(file)


readcol, file, dt, x ; leer las columnas del archivo .txt
;print, dt	;asegurarse de que los datos leídos son los correctos
;print, x



n_int=12  ;number of subintervals ;*******************************
secs=180.0; seconds in each subinterval (2^9 points) *********************
n=LONG(secs/dt) ; n is the number of points for each subinterval 
interval=n_int*n  ;number of points in all the interval;

plot, dt, x, title = file_name

;By using the cursor over the plot choose the initial point of an interval, 
PRINT, "Click cursor at ON-SOURCE start time"    
      CURSOR, x, y, /DOWN, /DATA
i0=0L
WHILE (dt[i0] LT x) DO i0=i0+1
t1=dt[i0]
t2=dt[i0+interval-1]                              
OPLOT,[t1,t2],[y, y], LINESTYLE = 5, THICK = 3 ;Plot an horizontal line on the interval
 
time1 = dt[i0:i0+interval-1] ; 
flux1 = x[i0:i0+interval-1] ;the chosen interval 

plot, time1, flux1


y = fft(flux1)	; aplicamos la transformada rápida de fourier a los 			;datos de la segunda columna
;print, y
;plot, y, /xlog, /ylog	;revisamos los datos 
;plot, y[10463:*], yrange = [-6e-2, 0.5e-2]	; ajustamos la escala del rango
;help, y
f_k = findgen(n_elements(flux1))/(n_elements(flux1)*time1)	;obtenemos las 							;frecuencias
;plot, f_k			; se grafica para revisar los resultados




;plot, y, abs(y)^2; se obtiene el espectro de 	


					;potencia
psw = abs(y)^2
psw_s = SMOOTH(psw, 25); se aplica un suavizado al espectro de potencia
plot, f_k, psw_s, xrange = [0.001, 1], xtitle = 'freq', ytitle = 'psw', /xlog, /ylog	; se evalua la frecuencia contra el espectro de potencia y se ajusta el rango de valores de los ejes



