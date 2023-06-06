;
;Name:
;	ip_fragm.pro
;purpose:
;	split datafiles from omniweb nasa into daily files
;   
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
;   .r ip_dat
;   ip_dat([yyyy,mm,dd])
;
;parameters:
;   ip: daily data from Omniweb data base
;
;dependencies:
;   Ommniweb NASA
;
;input files
;   ip_yyyymmddh.dat
;
;output files:
;   data structure for data analysis
;
;Version
; Dec, 2022


FUNCTION ip_data, date
	On_error, 2
	compile_opt idl2, HIDDEN

	year	= date[0]
	month	= date[1]	
    day     = date[2]
        header = 0      ; Defining number of lines of the header 
;###############################################################################
;reading data files
        date = string(year, month, day, format = '(I4, "-", I02, "-", I02)')
        path='/home/isaac/MEGAsync/datos'				
		file_name = path+'/ip/'+date+'.dat'
		
		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

        DStruct = {YEAR : 0, DOY : 0, hour : 0, bartels : 0, id_ifm : 0, $
                      id_sw : 0, n_points_ifm: 0, n_points_plasma : 0, B_esc : 0., $
                      B_vec : 0., B_lat : 0., B_long : 0., Bx : 0., By : 0., Bz : 0., $
                      By0 : 0., Bz0 : 0., B_rms_esc : 0., B_rms_vec : 0., Bx_rms : 0., $
                      By_rms : 0., Bz_rms : 0., Temp : 0., density_p : 0., v_p : 0., $
                      flow_long : 0., flow_lat : 0., alfa_prot : 0., sigma_T : 0., $
                      sigma_n : 0., sigma_v : 0., sigma_phi : 0., sigma_theta : 0., $
                      sigma_ratio : 0., flow_P : 0., E : 0., beta_p : 0., alfven_M : 0., $
                      mag_M : 0., q_invariant : 0., kp : 0, R : 0, dst : 0, ap : 0, $
                      f10_idx : 0., AE : 0, AL : 0, AU : 0, pc_idx : 0., lyman_alfa : 0.,$
                      p_flux_1MeV : 0., p_flux_2MeV : 0., p_flux_4MeV : 0., p_flux_10MeV : 0.,$
                      p_flux_30MeV : 0., p_flux_60MeV : 0., flux_FLAG : 0}
                                      

		r_ip = REPLICATE(DStruct, number_of_lines-header)	
		READS, data[header:number_of_lines-1], r_ip, $
FORMAT='(I4,I4,I3,I5,I3,I3,I4,I4,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,F6,'+$
'F9,F6,F6,F6,F6,F6,F9,F6,F6,F6,F6,F6,F6,F7,F7,F6,F5,F7,I3,I4,I6,I4,F6,I5,'+$
'I6,I6,F6,F9,F10,F9,F9,F9,F9,F9,I3)'		
		return, r_ip
end


PRO ip_fragm, date_i
	On_error, 2
	compile_opt idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
    dy_i    = date_i[2]
    
    ip_d = ip_data([yr_i, mh_i, dy_i]) 
;###################################################################################################
;    Definiendo parámetros
YEAR            = ip_d.YEAR
hour            = ip_d.hour
DOY             = ip_d.DOY
bartels         = ip_d.bartels
id_ifm          = ip_d.id_ifm
id_sw           = ip_d.id_sw
n_points_ifm    = ip_d.n_points_ifm
n_points_plasma = ip_d.n_points_plasma
B_esc           = ip_d.B_esc
B_vec           = ip_d.B_vec
B_lat           = ip_d.B_lat
B_lon           = ip_d.B_long
Bx              = ip_d.Bx
By              = ip_d.By
Bz              = ip_d.Bz
By0             = ip_d.By0
Bz0             = ip_d.Bz0
B_rms_esc       = ip_d.B_rms_esc
B_rms_vec       = ip_d.B_rms_vec
Bx_rms          = ip_d.Bx_rms
By_rms          = ip_d.By_rms
Bz_rms          = ip_d.Bz_rms
Temp            = ip_d.Temp
density_p       = ip_d.density_p
v_p             = ip_d.v_p
flow_long       = ip_d.flow_long
flow_lat        = ip_d.flow_lat
alfa_prot       = ip_d.alfa_prot
sigma_T         = ip_d.sigma_T
sigma_v         = ip_d.sigma_v
sigma_n         = ip_d.sigma_n
sigma_phi       = ip_d.sigma_phi
sigma_theta     = ip_d.sigma_theta
sigma_ratio     = ip_d.sigma_ratio
flow_P          = ip_d.flow_P
Ey              = ip_d.E
beta_p          = ip_d.beta_p
alfven_M        = ip_d.alfven_M
mag_M           = ip_d.mag_M
q_invariant     = ip_d.q_invariant
kp              = ip_d.kp
R               = ip_d.R
dst             = ip_d.dst
ap              = ip_d.ap
f10_idx         = ip_d.f10_idx
AE              = ip_d.AE
AL              = ip_d.AL
AU              = ip_d.AU
pc_idx          = ip_d.pc_idx
lyman_alfa      = ip_d.lyman_alfa
p_flux_1MeV     = ip_d.p_flux_1MeV
p_flux_2MeV     = ip_d.p_flux_2MeV
p_flux_4MeV     = ip_d.p_flux_4MeV  
p_flux_10MeV    = ip_d.p_flux_10MeV
p_flux_30MeV    = ip_d.p_flux_30MeV
p_flux_60MeV    = ip_d.p_flux_60MeV  
flux_FLAG       = ip_d.flux_FLAG


n_days          = FINDGEN(N_ELEMENTS(ip_d.DOY)/24)  
string_date     = STRARR(N_ELEMENTS(ip_d.DOY)/24)
outfile         = STRARR(N_ELEMENTS(n_days))


path='/home/isaac/MEGAsync/datos'

FOR i=0, N_ELEMENTS(n_days)-1 DO BEGIN
    tmp_year    = 0
    tmp_month   = 0
    tmp_day     = 0
    tmp_julday  = JULDAY(1, DOY[0], YEAR[0])
    CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
    string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)') 
    
    outfile[i] = path+'/ip/daily/ip_'+string_date[i]+'h.dat'    
    OPENW, LUN, outfile[i], /GET_LUN        
    PRINTF, LUN, YEAR[i*24:(i+1)*24-1], FORMAT='(24(I4,X))'
    PRINTF, LUN, DOY[i*24:(i+1)*24-1], FORMAT='(24(I4))'        
    PRINTF, LUN, hour[i*24:(i+1)*24-1], FORMAT='(24(I3,X))'
    PRINTF, LUN, bartels[i*24:(i+1)*24-1], FORMAT='(24(I5,x))'
    PRINTF, LUN, id_ifm[i*24:(i+1)*24-1], FORMAT='(24(I3,X))'
    PRINTF, LUN, id_sw[i*24:(i+1)*24-1], FORMAT='(24(I3,X))'                     
    PRINTF, LUN, n_points_ifm[i*24:(i+1)*24-1], FORMAT='(24(I4,X))'
    PRINTF, LUN, n_points_plasma[i*24:(i+1)*24-1], FORMAT='(24(I4,X))'
    PRINTF, LUN, B_esc[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, B_vec[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, B_lat[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, B_lon[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, Bx[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, By[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, Bz[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, By0[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, Bz0[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, B_rms_esc[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, B_rms_vec[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, Bx_rms[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, By_rms[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'    
    PRINTF, LUN, Bz_rms[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, Temp[i*24:(i+1)*24-1], FORMAT='(24(F9.0,X))'
    PRINTF, LUN, density_p[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, v_p[i*24:(i+1)*24-1], FORMAT='(24(F6.0,X))'                     
    PRINTF, LUN, flow_long[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, flow_lat[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, alfa_prot[i*24:(i+1)*24-1], FORMAT='(24(F6.3,X))'
    PRINTF, LUN, sigma_T[i*24:(i+1)*24-1], FORMAT='(24(F9.0,X))'
    PRINTF, LUN, sigma_n[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, sigma_v[i*24:(i+1)*24-1], FORMAT='(24(F6.0,X))'
    PRINTF, LUN, sigma_phi[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, sigma_theta[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, sigma_ratio[i*24:(i+1)*24-1], FORMAT='(24(F6.3,X))'
    PRINTF, LUN, flow_P[i*24:(i+1)*24-1], FORMAT='(24(F6.2,X))'
    PRINTF, LUN, Ey[i*24:(i+1)*24-1], FORMAT='(24(F7.2,X))'
    PRINTF, LUN, beta_p[i*24:(i+1)*24-1], FORMAT='(24(F7.2,X))'
    PRINTF, LUN, alfven_M[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, mag_M[i*24:(i+1)*24-1], FORMAT='(24(F5.1,X))'
    PRINTF, LUN, q_invariant[i*24:(i+1)*24-1], FORMAT='(24(F7.4,X))' 
    PRINTF, LUN, kp[i*24:(i+1)*24-1], FORMAT='(24(I3,X))'
    PRINTF, LUN, R[i*24:(i+1)*24-1], FORMAT='(24(I4,X))'
    PRINTF, LUN, dst[i*24:(i+1)*24-1], FORMAT='(24(I6,X))'
    PRINTF, LUN, ap[i*24:(i+1)*24-1], FORMAT='(24(I4,X))'                     
    PRINTF, LUN, f10_idx[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, AE[i*24:(i+1)*24-1], FORMAT='(24(I5,X))'
    PRINTF, LUN, AL[i*24:(i+1)*24-1], FORMAT='(24(I6,X))'
    PRINTF, LUN, AU[i*24:(i+1)*24-1], FORMAT='(24(I6,X))'
    PRINTF, LUN, pc_idx[i*24:(i+1)*24-1], FORMAT='(24(F6.1,X))'
    PRINTF, LUN, lyman_alfa[i*24:(i+1)*24-1], FORMAT='(24(F9.6,X))'
    PRINTF, LUN, p_flux_1MeV[i*24:(i+1)*24-1], FORMAT='(24(F10.2,X))'
    PRINTF, LUN, p_flux_2MeV[i*24:(i+1)*24-1], FORMAT='(24(F9.2,X))'
    PRINTF, LUN, p_flux_4MeV[i*24:(i+1)*24-1], FORMAT='(24(F9.2,X))'
    PRINTF, LUN, p_flux_10MeV[i*24:(i+1)*24-1], FORMAT='(24(F9.2,X))'
    PRINTF, LUN, p_flux_30MeV[i*24:(i+1)*24-1], FORMAT='(24(F9.2,X))'
    PRINTF, LUN, p_flux_60MeV[i*24:(i+1)*24-1], FORMAT='(24(F9.2,X))'
    PRINTF, LUN, flux_FLAG[i*24:(i+1)*24-1], FORMAT='(24(I3,X))'

    CLOSE, LUN
    FREE_LUN, LUN          
ENDFOR

END


