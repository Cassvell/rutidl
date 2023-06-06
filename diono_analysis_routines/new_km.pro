;Name: new_km.pro
;	
;purpose:
;	this routine will call read a certain number of files containing 
;   magnetic data index to derive a new kmex index considering diono effects on Kp
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data analysis
;
;calling sequence:
;   .r new_km
;   new_km, r_dst, Bsq, DOY, idate[yyyy,mm,dd], fdate[yyyy,mm,dd]
;parameters:
;
;
;dependencies:
;   Instituto de geofísica, UNAM
;
;input files
;   DST index, DH index, SQ_Baseline
;
;output files:
;   n_kmex index en archivos de un día con 3 horas de muestreo.
;
;version
;   Dec, 2022
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having Bsq data files (run the Bsq routines)
;       2. having the H clean data files (H_filmaker.pro)
;
;

PRO new_km, date_i, date_f
	ON_ERROR, 2
	COMPILE_OPT idl2, HIDDEN

	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2]
;###############################################################################
    idate0 = string(yr_i, mh_i, format='(I4,I02)')
    TGM_n = event_case([yr_i,mh_i,dy_i])
;###############################################################################
; define DH variables
    file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1
    tot_days= FINDGEN(file_number*24)/24.0
    k_days  = FINDGEN(file_number*8)/8.0    
  
    Date    = STRING(yr_i[0], mh_i, dy_i, FORMAT='(I4, "-", I02, "-", I02)') 
        
; define H variables                  
    dst = dst_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'dst')
    H   = H_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])
    
; define K variables   
    kp      = kp_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'kp')
    k_mex   = kmex_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f], 'k_mex')    
    k_mex   = add_nan(k_mex, 9.0, 'greater') 
    k_days  = FINDGEN(file_number*8)/8. 
; define Bsq 
    Bsq     = SQbaseline_array([yr_i, mh_i, dy_i], [yr_f, mh_f, dy_f])    
;###############################################################################
;identifying NAN percentage values in the Time Series
    H = nanpc(H, 999999.0, 'equal')
    H = nanpc(H, 100.0, 'greater')   
;implementar una función de interpolación en caso de que el porcentaje de nan sea muy bajo       
    H = fillnan(H)      

; define Diono  
; Import the structure of diono generated variables   
    dionstr = gen_diono(dst, H, Bsq, 28.06, 'h', TGM_n, DIG_FILTER = 'dig_filter')

; compute diono variables    
    diono = dionstr.diono
    dp2   = dionstr.dp2
    ddyn  = dionstr.ddyn
;###############################################################################
;Computing new DH
new_H       = dionstr.baseline+ddyn+dp2
;Pasar de horario a tri-horario
PRINT, '########################################################################'
;###############################################################################
;a partir de Kp, obtenemos dos valores DH superior e inferior
dH_tab      = [4.8, 5.5, 6.5, 7.3, 8.3, 9.8, 11.1, 12.6, 14.9, 16.9, 19.2,  $
               22.7, 25.7, 29.2, 34.5, 39.1, 44.4, 52.4, 59.5, 67.4, 79.7, 90.4, $
               102.5, 121.2, 137.4, 155.8, 184.2, 208.9, 500.0]

dh1         = FLTARR(N_ELEMENTS(kp))   
dh2         = FLTARR(N_ELEMENTS(kp))              
FOR i=0, N_ELEMENTS(kp)-1 DO BEGIN
    IF kp[i] EQ 0.0 THEN dh1[i] = 0.0     
    IF kp[i] EQ 0.3 THEN dh1[i] = dH_tab[1] 
    IF kp[i] EQ 0.7 THEN dh1[i] = dH_tab[2]
    IF kp[i] EQ 1.0 THEN dh1[i] = dH_tab[3]   
    IF kp[i] EQ 1.3 THEN dh1[i] = dH_tab[4] 
    IF kp[i] EQ 1.7 THEN dh1[i] = dH_tab[5]  
    IF kp[i] EQ 2.0 THEN dh1[i] = dH_tab[6]  
    IF kp[i] EQ 2.3 THEN dh1[i] = dH_tab[7]   
    IF kp[i] EQ 2.7 THEN dh1[i] = dH_tab[8]  
    IF kp[i] EQ 3.0 THEN dh1[i] = dH_tab[9]  
    IF kp[i] EQ 3.3 THEN dh1[i] = dH_tab[10] 
    IF kp[i] EQ 3.7 THEN dh1[i] = dH_tab[11] 
    IF kp[i] EQ 4.0 THEN dh1[i] = dH_tab[12]  
    IF kp[i] EQ 4.3 THEN dh1[i] = dH_tab[13] 
    IF kp[i] EQ 4.7 THEN dh1[i] = dH_tab[14] 
    IF kp[i] EQ 5.0 THEN dh1[i] = dH_tab[15] 
    IF kp[i] EQ 5.3 THEN dh1[i] = dH_tab[16] 
    IF kp[i] EQ 5.7 THEN dh1[i] = dH_tab[17] 
    IF kp[i] EQ 6.0 THEN dh1[i] = dH_tab[18] 
    IF kp[i] EQ 6.3 THEN dh1[i] = dH_tab[19]
    IF kp[i] EQ 6.7 THEN dh1[i] = dH_tab[20] 
    IF kp[i] EQ 7.0 THEN dh1[i] = dH_tab[21] 
    IF kp[i] EQ 7.3 THEN dh1[i] = dH_tab[22] 
    IF kp[i] EQ 7.7 THEN dh1[i] = dH_tab[23] 
    IF kp[i] EQ 8.0 THEN dh1[i] = dH_tab[24] 
    IF kp[i] EQ 8.3 THEN dh1[i] = dH_tab[25] 
    IF kp[i] EQ 8.7 THEN dh1[i] = dH_tab[26] 
    IF kp[i] EQ 9.0 THEN dh1[i] = dH_tab[27]                                                                  
;###############################################################################
    IF kp[i] EQ 0.0 THEN dh2[i] = dH_tab[1]    
    IF kp[i] EQ 0.3 THEN dh2[i] = dH_tab[2] 
    IF kp[i] EQ 0.7 THEN dh2[i] = dH_tab[3]
    IF kp[i] EQ 1.0 THEN dh2[i] = dH_tab[4]    
    IF kp[i] EQ 1.3 THEN dh2[i] = dH_tab[5] 
    IF kp[i] EQ 1.7 THEN dh2[i] = dH_tab[6]   
    IF kp[i] EQ 2.0 THEN dh2[i] = dH_tab[7] 
    IF kp[i] EQ 2.3 THEN dh2[i] = dH_tab[8]    
    IF kp[i] EQ 2.7 THEN dh2[i] = dH_tab[9] 
    IF kp[i] EQ 3.0 THEN dh2[i] = dH_tab[10]  
    IF kp[i] EQ 3.3 THEN dh2[i] = dH_tab[11]
    IF kp[i] EQ 3.7 THEN dh2[i] = dH_tab[12]   
    IF kp[i] EQ 4.0 THEN dh2[i] = dH_tab[13]
    IF kp[i] EQ 4.3 THEN dh2[i] = dH_tab[14] 
    IF kp[i] EQ 4.7 THEN dh2[i] = dH_tab[15] 
    IF kp[i] EQ 5.0 THEN dh2[i] = dH_tab[16] 
    IF kp[i] EQ 5.3 THEN dh2[i] = dH_tab[17] 
    IF kp[i] EQ 5.7 THEN dh2[i] = dH_tab[18]  
    IF kp[i] EQ 6.0 THEN dh2[i] = dH_tab[19]
    IF kp[i] EQ 6.3 THEN dh2[i] = dH_tab[20] 
    IF kp[i] EQ 6.7 THEN dh2[i] = dH_tab[21] 
    IF kp[i] EQ 7.0 THEN dh2[i] = dH_tab[22] 
    IF kp[i] EQ 7.3 THEN dh2[i] = dH_tab[23] 
    IF kp[i] EQ 7.7 THEN dh2[i] = dH_tab[24] 
    IF kp[i] EQ 8.0 THEN dh2[i] = dH_tab[25] 
    IF kp[i] EQ 8.3 THEN dh2[i] = dH_tab[26] 
    IF kp[i] EQ 8.7 THEN dh2[i] = dH_tab[27]  
    IF kp[i] EQ 9.0 THEN dh2[i] = dH_tab[28]                                                                            
ENDFOR
;###############################################################################
;Computing new kmex
;###############################################################################
H_3h    = FLTARR(N_ELEMENTS(kp))
n_km1   = FLTARR(N_ELEMENTS(kp))
n_km2   = FLTARR(N_ELEMENTS(kp))
err_sup = FLTARR(N_ELEMENTS(kp))
err_inf = FLTARR(N_ELEMENTS(kp))
FOR i=0, N_ELEMENTS(kp)-1 DO BEGIN
    H_3h[i]    = ABS(MAX(new_H[(i*3):(i+1)*3-1])-MIN(new_H[(i*3):(i+1)*3-1]))
    err_inf[i] = dh1[i]-H_3h[i]
    err_sup[i] = dh2[i]+H_3h[i] 
    
    IF err_sup[i] LE dH_tab[1]  THEN n_km1[i] = 0 
    IF err_sup[i] GT dH_tab[1]  AND err_sup[i] LE dh_tab[2]  THEN n_km2[i] = 3 
    IF err_sup[i] GT dH_tab[2]  AND err_sup[i] LE dh_tab[3]  THEN n_km2[i] = 7
    IF err_sup[i] GT dH_tab[3]  AND err_sup[i] LE dH_tab[4]  THEN n_km2[i] = 10 
    IF err_sup[i] GT dH_tab[4]  AND err_sup[i] LE dH_tab[5]  THEN n_km2[i] = 13
    IF err_sup[i] GT dH_tab[5]  AND err_sup[i] LE dH_tab[6]  THEN n_km2[i] = 17
    IF err_sup[i] GT dH_tab[6]  AND err_sup[i] LE dH_tab[7]  THEN n_km2[i] = 20
    IF err_sup[i] GT dH_tab[7]  AND err_sup[i] LE dH_tab[8]  THEN n_km2[i] = 23
    IF err_sup[i] GT dH_tab[8]  AND err_sup[i] LE dH_tab[9]  THEN n_km2[i] = 27
    IF err_sup[i] GT dH_tab[9]  AND err_sup[i] LE dH_tab[10] THEN n_km2[i] = 30
    IF err_sup[i] GT dH_tab[10] AND err_sup[i] LE dH_tab[11] THEN n_km2[i] = 33
    IF err_sup[i] GT dH_tab[11] AND err_sup[i] LE dH_tab[12] THEN n_km2[i] = 37
    IF err_sup[i] GT dH_tab[12] AND err_sup[i] LE dH_tab[13] THEN n_km2[i] = 40
    IF err_sup[i] GT dH_tab[13] AND err_sup[i] LE dH_tab[14] THEN n_km2[i] = 43
    IF err_sup[i] GT dH_tab[14] AND err_sup[i] LE dH_tab[15] THEN n_km2[i] = 47
    IF err_sup[i] GT dH_tab[15] AND err_sup[i] LE dH_tab[16] THEN n_km2[i] = 50 
    IF err_sup[i] GT dH_tab[16] AND err_sup[i] LE dH_tab[17] THEN n_km2[i] = 53
    IF err_sup[i] GT dH_tab[17] AND err_sup[i] LE dH_tab[18] THEN n_km2[i] = 57
    IF err_sup[i] GT dH_tab[18] AND err_sup[i] LE dH_tab[19] THEN n_km2[i] = 60
    IF err_sup[i] GT dH_tab[19] AND err_sup[i] LE dH_tab[20] THEN n_km2[i] = 63
    IF err_sup[i] GT dH_tab[20] AND err_sup[i] LE dH_tab[21] THEN n_km2[i] = 67
    IF err_sup[i] GT dH_tab[21] AND err_sup[i] LE dH_tab[22] THEN n_km2[i] = 70
    IF err_sup[i] GT dH_tab[22] AND err_sup[i] LE dH_tab[23] THEN n_km2[i] = 73
    IF err_sup[i] GT dH_tab[23] AND err_sup[i] LE dH_tab[24] THEN n_km2[i] = 77
    IF err_sup[i] GT dH_tab[24] AND err_sup[i] LE dH_tab[25] THEN n_km2[i] = 80
    IF err_sup[i] GT dH_tab[25] AND err_sup[i] LE dH_tab[26] THEN n_km2[i] = 83
    IF err_sup[i] GT dH_tab[26] AND err_sup[i] LE dH_tab[27] THEN n_km2[i] = 87
    IF err_sup[i] GT dH_tab[27] THEN n_km2[i] = 90 
;###############################################################################
    IF err_inf[i] LE dH_tab[1]  THEN n_km1[i] = 0 
    IF err_inf[i] GT dH_tab[1]  AND err_inf[i] LE dh_tab[2]  THEN n_km1[i] = 3 
    IF err_inf[i] GT dH_tab[2]  AND err_inf[i] LE dh_tab[3]  THEN n_km1[i] = 7
    IF err_inf[i] GT dH_tab[3]  AND err_inf[i] LE dH_tab[4]  THEN n_km1[i] = 10 
    IF err_inf[i] GT dH_tab[4]  AND err_inf[i] LE dH_tab[5]  THEN n_km1[i] = 13
    IF err_inf[i] GT dH_tab[5]  AND err_inf[i] LE dH_tab[6]  THEN n_km1[i] = 17
    IF err_inf[i] GT dH_tab[6]  AND err_inf[i] LE dH_tab[7]  THEN n_km1[i] = 20
    IF err_inf[i] GT dH_tab[7]  AND err_inf[i] LE dH_tab[8]  THEN n_km1[i] = 23
    IF err_inf[i] GT dH_tab[8]  AND err_inf[i] LE dH_tab[9]  THEN n_km1[i] = 27
    IF err_inf[i] GT dH_tab[9]  AND err_inf[i] LE dH_tab[10] THEN n_km1[i] = 30
    IF err_inf[i] GT dH_tab[10] AND err_inf[i] LE dH_tab[11] THEN n_km1[i] = 33
    IF err_inf[i] GT dH_tab[11] AND err_inf[i] LE dH_tab[12] THEN n_km1[i] = 37
    IF err_inf[i] GT dH_tab[12] AND err_inf[i] LE dH_tab[13] THEN n_km1[i] = 40
    IF err_inf[i] GT dH_tab[13] AND err_inf[i] LE dH_tab[14] THEN n_km1[i] = 43
    IF err_inf[i] GT dH_tab[14] AND err_inf[i] LE dH_tab[15] THEN n_km1[i] = 47
    IF err_inf[i] GT dH_tab[15] AND err_inf[i] LE dH_tab[16] THEN n_km1[i] = 50 
    IF err_inf[i] GT dH_tab[16] AND err_inf[i] LE dH_tab[17] THEN n_km1[i] = 53
    IF err_inf[i] GT dH_tab[17] AND err_inf[i] LE dH_tab[18] THEN n_km1[i] = 57
    IF err_inf[i] GT dH_tab[18] AND err_inf[i] LE dH_tab[19] THEN n_km1[i] = 60
    IF err_inf[i] GT dH_tab[19] AND err_inf[i] LE dH_tab[20] THEN n_km1[i] = 63
    IF err_inf[i] GT dH_tab[20] AND err_inf[i] LE dH_tab[21] THEN n_km1[i] = 67
    IF err_inf[i] GT dH_tab[21] AND err_inf[i] LE dH_tab[22] THEN n_km1[i] = 70
    IF err_inf[i] GT dH_tab[22] AND err_inf[i] LE dH_tab[23] THEN n_km1[i] = 73
    IF err_inf[i] GT dH_tab[23] AND err_inf[i] LE dH_tab[24] THEN n_km1[i] = 77
    IF err_inf[i] GT dH_tab[24] AND err_inf[i] LE dH_tab[25] THEN n_km1[i] = 80
    IF err_inf[i] GT dH_tab[25] AND err_inf[i] LE dH_tab[26] THEN n_km1[i] = 83 
    IF err_inf[i] GT dH_tab[26] AND err_inf[i] LE dH_tab[27] THEN n_km1[i] = 87
    IF err_inf[i] GT dH_tab[27] THEN n_km1[i] = 90    
ENDFOR
outfile = STRARR(file_number)
        Device_bak2 = !D.Name         
        SET_PLOT, 'X'
       ; print, n_km1    
        ;print, '################################################################'
       ; print, n_km2    
       ; print, '################################################################'        
    ;plot, tot_days, dst
    ;plot, tot_days, new_H, linestyle=0 
    ;oplot, tot_days, H, linestyle=2 
    ;oplot, k_days, n_km2/10.0, psym=2;linestyle=2 
   ; plot, k_days, kp, yrange=[0,9], ystyle=1
   ; oplot, k_days, kp, psym=6  
    ;oplot, k_days, n_km1/10.,psym=1 
    ;oplot, k_days, n_km2/10.,psym=2 
    string_date        = STRARR(file_number)    
FOR i=0, file_number-1 DO BEGIN
    tmp_year    = 0
    tmp_month   = 0
    tmp_day     = 0
    tmp_julday  = JULDAY(mh_i, dy_i, yr_i)
    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
    string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')
                
    outfile[i] = '/home/isaac/MEGAsync/datos/Kmex/new_kmex/'+string_date[i]+'.dat'
    OPENW, LUN, outfile[i], /GET_LUN
    PRINTF, LUN, n_km1[i*8:(i+1)*8-1], FORMAT='(8(I02,X))'
    PRINTF, LUN, n_km2[i*8:(i+1)*8-1], FORMAT='(8(I02,X))' 
    CLOSE, LUN
    FREE_LUN, LUN 
ENDFOR
END		
