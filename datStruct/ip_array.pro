;
;Name:
;	ip_array.pro
;purpose:
;	Import an structured array of data from interplatetary database to Export a time series
;   of a selected data of the structure
;
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data export
;
;calling sequence:
;   .r ip_array
;   ip_array(idate, fdate, variable, HELP=help)
;
;parameters:
;   ip_dat      : function which generates a structured data array for day
;   idate/fdate : ([yyyy,mm,dd])
;   HELP=help   : Keyword to obtain info about the types of variables avaiable in the data structure
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

FUNCTION ip_dat, date

	year	= date[0]
	month	= date[1]
	day 	= date[2]

;###############################################################################
;reading data files
        date = string(year, month, day, format = '(I4, I02, I02)')

        path='/home/isaac/MEGAsync/datos'		
		file_name = path+'/ip/daily/'+'ip_'+date+'h.dat'

		file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'

		number_of_lines = FILE_LINES(file)
		data = STRARR(number_of_lines)

		openr, lun, file, /GET_LUN, ERROR=err
		readf, lun, data, FORMAT = '(A)'
		CLOSE, lun
		FREE_LUN, lun

;extracting data and denfining an structure data
        idx_ip        = {year      : INTARR(24), $
                         DOY       : INTARR(24), $
                         hour      : INTARR(24), $
                         Bartel    : INTARR(24), $
                         id_ifm    : INTARR(24), $
                         id_sw     : INTARR(24), $
                         n_pt_ifm  : INTARR(24), $
                         n_pt_plasm: INTARR(24), $
                         B_esc     : FLTARR(24), $
                         B_vec     : FLTARR(24), $
                         B_lat     : FLTARR(24), $
                         B_long    : FLTARR(24), $
                         Bx        : FLTARR(24), $
                         By        : FLTARR(24), $
                         Bz        : FLTARR(24), $
                         By0       : FLTARR(24), $
                         Bz0       : FLTARR(24), $
                         B_rms_esc : FLTARR(24), $
                         B_rms_vec : FLTARR(24), $
                         Bx_rms    : FLTARR(24), $
                         By_rms    : FLTARR(24), $
                         Bz_rms    : FLTARR(24), $
                         Temp      : FLTARR(24), $
                         density_p : FLTARR(24), $
                         v_p       : FLTARR(24), $
                         flow_long : FLTARR(24), $
                         flow_lat  : FLTARR(24), $
                         alfa_prot : FLTARR(24), $
                         sigma_T   : FLTARR(24), $
                         sigma_n   : FLTARR(24), $
                         sigma_v   : FLTARR(24), $
                         sigma_phi : FLTARR(24), $
                         sigma_thet: FLTARR(24), $
                         sigma_rat : FLTARR(24), $
                         flow_P    : FLTARR(24), $
                         E         : FLTARR(24), $
                         beta_p    : FLTARR(24), $
                         alfven_M  : FLTARR(24), $
                         mag_M     : FLTARR(24), $
                         q_invar   : FLTARR(24), $
                         kp        : INTARR(24), $
                         R         : INTARR(24), $
                         dst       : INTARR(24), $
                         ap        : INTARR(24), $
                         f10_idx   : FLTARR(24), $
                         AE        : INTARR(24), $
                         AL        : INTARR(24), $
                         AU        : INTARR(24), $
                         pc_idx    : FLTARR(24), $
                         lyman_alfa: FLTARR(24), $
                         pflux_1MeV: FLTARR(24), $
                         pflux_2MeV: FLTARR(24), $
                         pflux_4MeV: FLTARR(24), $ 
                         pflux10MeV: FLTARR(24), $
                         pflux30MeV: FLTARR(24), $
                         pflux60MeV: FLTARR(24), $
                         flux_FLAG : INTARR(24)}
        
        struct = {x : [0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.]}
        tmp_var = REPLICATE(struct, 39)    

    READS, data, tmp_var, FORMAT='(24(I4,X),/,24(I4),/,24(I3,X),/,24(I5,X),/,24(I3,X),/,'+$
                                 '24(I3,X),/,24(I4,X),/,24(I4,X),/,24(F6.1,X),/,24(F6.1,X),/,'+$
                                 '24(F6.1,X),/,24(F6.1,X),/,24(F6.1,X),/,24(F6.1,X),/,'+$
                                 '24(F6.1,X),/,24(F6.1,X),/,24(F6.1,X),/,24(F6.1,X),/,'+$'
                                 '24(F6.1,X),/,24(F6.1,X),/,24(F6.1,X),/,24(F6.1,X),/,'+$
                                 '24(F9.0,X),/,24(F6.1,X),/,24(F6.0,X),/,24(F6.1,X),/,'+$
                                 '24(F6.1,X),/,24(F6.3,X),/,24(F9.0,X),/,24(F6.1,X),/,'+$
                                 '24(F6.0,X),/,24(F6.1,X),/,24(F6.1,X),/,24(F6.3,X),/,'+$
                                 '24(F6.2,X),/,24(F7.2,X),/,24(F7.2,X),/,24(F6.1,X),/,'+$
                                 '24(F5.1,X))

    idx_ip.year[*]              = tmp_var[0].x    
    idx_ip.DOY[*]               = tmp_var[1].x  
    idx_ip.hour[*]              = tmp_var[2].x
    idx_ip.Bartel[*]            = tmp_var[3].x
    idx_ip.id_ifm[*]            = tmp_var[4].x            
    idx_ip.id_sw[*]             = tmp_var[5].x    
    idx_ip.n_pt_ifm[*]          = tmp_var[6].x    
    idx_ip.n_pt_plasm[*]        = tmp_var[7].x    
    idx_ip.B_esc[*]             = tmp_var[8].x    
    idx_ip.B_vec[*]             = tmp_var[9].x
    idx_ip.B_lat[*]             = tmp_var[10].x 
    idx_ip.B_long[*]            = tmp_var[11].x    
    idx_ip.Bx[*]                = tmp_var[12].x     
    idx_ip.By[*]                = tmp_var[13].x    
    idx_ip.Bz[*]                = tmp_var[14].x    
    idx_ip.By0[*]               = tmp_var[15].x    
    idx_ip.Bz0[*]               = tmp_var[16].x 
    idx_ip.B_rms_esc[*]         = tmp_var[17].x    
    idx_ip.B_rms_vec[*]         = tmp_var[18].x    
    idx_ip.Bx_rms[*]            = tmp_var[19].x    
    idx_ip.By_rms[*]            = tmp_var[20].x 
    idx_ip.Bz_rms[*]            = tmp_var[21].x    
    idx_ip.Temp[*]              = tmp_var[22].x    
    idx_ip.density_p[*]         = tmp_var[23].x    
    idx_ip.v_p[*]               = tmp_var[24].x 
    idx_ip.flow_long[*]         = tmp_var[25].x    
    idx_ip.flow_lat[*]          = tmp_var[26].x    
    idx_ip.alfa_prot[*]         = tmp_var[27].x    
    idx_ip.sigma_T[*]           = tmp_var[28].x             
    idx_ip.sigma_n[*]           = tmp_var[29].x    
    idx_ip.sigma_v[*]           = tmp_var[30].x    
    idx_ip.sigma_phi[*]         = tmp_var[31].x    
    idx_ip.sigma_thet[*]        = tmp_var[32].x     
    idx_ip.sigma_rat[*]         = tmp_var[33].x    
    idx_ip.flow_P[*]            = tmp_var[34].x    
    idx_ip.E[*]                 = tmp_var[35].x    
    idx_ip.beta_p[*]            = tmp_var[36].x     
    idx_ip.alfven_M[*]          = tmp_var[37].x    
    idx_ip.mag_M[*]             = tmp_var[38].x             
                        
    RETURN, idx_ip
END

FUNCTION ip_array, date_i, date_f, variable, HELP=help
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	yr_i	= date_i[0]
	mh_i	= date_i[1]
	dy_i 	= date_i[2]	

	yr_f	= date_f[0]
	mh_f	= date_f[1]
	dy_f 	= date_f[2] 

;###############################################################################
; define array structure
        data_path='/home/isaac/MEGAsync/datos'
        file_number    = (JULDAY(mh_f, dy_f, yr_f) - JULDAY(mh_i, dy_i, yr_i))+1 
        string_date        = STRARR(file_number)               
        data_file_name  = STRARR(file_number) 
             
        FOR i=0ll, file_number-1 DO BEGIN
                tmp_year    = 0
                tmp_month   = 0
                tmp_day     = 0
                tmp_julday  = JULDAY(mh_i, dy_i, yr_i)

                CALDAT, tmp_julday+i, tmp_month, tmp_day, tmp_year
                string_date[i]    = STRING(tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)')	        	                                        
                data_file_name[i] = data_path+'/ip/daily/ip_'+string_date[i]+'h.dat'                		       		            
		        file = FILE_SEARCH(data_file_name[i], COUNT=opened_files)        	                            
        ENDFOR


        exist_data_file   = FILE_TEST(data_file_name)
        capable_to_plot   = N_ELEMENTS(WHERE(exist_data_file EQ 1))
                
        IF capable_to_plot NE N_ELEMENTS(data_file_name) THEN BEGIN 
                PRINT, FORMAT="('CRITICAL ERROR: impossible to read data file(s).')"
                PRINT, FORMAT="('                missing GMS_YYYYMMDD.ip_index.',A,' impossible to plot all data.')"              
        ENDIF
  	
;###############################################################################
; Generate variables and time series             
    CASE variable of
                         'year'      : variable = 0
                         'DOY'       : variable = 1 
                         'hour'      : variable = 2  
                         'Bartel'    : variable = 3  
                         'id_ifm'    : variable = 4
                         'id_sw'     : variable = 5
                         'n_pt_ifm'  : variable = 6
                         'n_pt_plasm': variable = 7
                         'B_esc'     : variable = 8
                         'B_vec'     : variable = 9
                         'B_lat'     : variable = 10
                         'B_long'    : variable = 11
                         'Bx'        : variable = 12
                         'By'        : variable = 13
                         'Bz'        : variable = 14
                         'By0'       : variable = 15
                         'Bz0'       : variable = 16
                         'B_rms_esc' : variable = 17
                         'B_rms_vec' : variable = 18
                         'Bx_rms'    : variable = 19
                         'By_rms'    : variable = 20
                         'Bz_rms'    : variable = 21
                         'Temp'      : variable = 22
                         'density_p' : variable = 23
                         'v_p'       : variable = 24
                         'flow_long' : variable = 25
                         'flow_lat'  : variable = 26
                         'alfa_prot' : variable = 27
                         'sigma_T'   : variable = 28
                         'sigma_n'   : variable = 29
                         'sigma_v'   : variable = 30
                         'sigma_phi' : variable = 31
                         'sigma_thet': variable = 32
                         'sigma_rat' : variable = 33
                         'p_dyn'    : variable = 34
                         'E'         : variable = 35
                         'beta_p'    : variable = 36
                         'alfven_M'  : variable = 37
                         'mag_M'     : variable = 38
                         'q_invar'   : variable = 39
                         'kp'        : variable = 40
                         'R'         : variable = 41
                         'dst'       : variable = 42
                         'ap'        : variable = 43
                         'f10_idx'   : variable = 44
                         'AE'        : variable = 45
                         'AL'        : variable = 46
                         'AU'        : variable = 47
                         'pc_idx'    : variable = 48
                         'lyman_alfa': variable = 49
                         'pflux_1MeV': variable = 50
                         'pflux_2MeV': variable = 51
                         'pflux_4MeV': variable = 52
                         'pflux10MeV': variable = 53
                         'pflux30MeV': variable = 54
                         'pflux60MeV': variable = 55
                         'flux_FLAG' : variable = 56
        ELSE: PRINT, 'Parámetro no disponible. Seleccione otro o teclée ayuda para tener referencia.'
    ENDCASE


    IF KEYWORD_SET(help) THEN BEGIN
        PRINT, 'Variable (options) and short description'
        PRINT,  'year       : year of the time series'
        PRINT,  'DOY        : Day of the year' 
        PRINT,  'hour       : Hour of the day. Format 24h, from 0 to 23 (UTC)'
        PRINT,  'Bartel     : Bartels rotation number'
        PRINT,  'id_ifm     : ID for IMF spacecraft'
        PRINT,  'id_sw      : ID for SW Plasma spacecraft'
        PRINT,  'n_pt_ifm   : # of points in IMF averages'
        PRINT,  'n_pt_plasm : # of points in Plasma averag.'
        PRINT,  'B_esc      : Scalar B, nT '
        PRINT,  'B_vec      : Vector B Magnitude,nT '
        PRINT,  'B_lat      : Lat. Angle of B (GSE)'
        PRINT,  'B_long     : Long. Angle of B (GSE)'
        PRINT,  'Bx         : BX, nT (GSE, GSM)'
        PRINT,  'By         : BY, nT (GSE)'
        PRINT,  'Bz         : BZ, nT (GSE)'
        PRINT,  'By0        : BY, nT (GSM)'
        PRINT,  'Bz0        : BZ, nT (GSM)'
        PRINT,  'B_rms_esc  : RMS_magnitude, nT '
        PRINT,  'B_rms_vec  : RMS_field_vector, nT '
        PRINT,  'Bx_rms     : RMS_BX_GSE, nT '
        PRINT,  'By_rms     : RMS_BY_GSE, nT '
        PRINT,  'Bz_rms     : RMS_BZ_GSE, nT '
        PRINT,  'Temp       : SW Plasma Temperature, K '
        PRINT,  'density_p  : SW Proton Density, N/cm^3'
        PRINT,  'v_p        : SW Plasma Speed, km/s'
        PRINT,  'flow_long  : SW Plasma flow long. angle '
        PRINT,  'flow_lat   : SW Plasma flow lat. angle'
        PRINT,  'alfa_prot  : Alpha/Prot. ratio'
        PRINT,  'sigma_T    : sigma-T,K'
        PRINT,  'sigma_n    : sigma-n, (N/cm^3)'
        PRINT,  'sigma_v    : sigma-V, km/s'
        PRINT,  'sigma_phi  : sigma-phi V, degrees'
        PRINT,  'sigma_thet : sigma-theta V, degrees'  
        PRINT,  'sigma_rat  : sigma-ratio'
        PRINT,  'p_dyn      : Flow pressure (dynamic Preassure)' 
        PRINT,  'E          : E elecrtric field'
        PRINT,  'beta_P     : Plasma Beta '
        PRINT,  'alfven_M   : Alfen mach number'
        PRINT,  'mag_M      : Magnetosonic Much num.'
        PRINT,  'q_invar    : Quasy-Invariant '
        PRINT,  'kp         : Kp index '  
        PRINT,  'R          : R (Sunspot No.) '
        PRINT,  'dst        : Dst-index, nT'
        PRINT,  'ap         : ap_index, nT '
        PRINT,  'f10_idx    : f10.7 index '
        PRINT,  'AE         : AE-index, nT'
        PRINT,  'AL         : AL-index, nT'
        PRINT,  'AU         : AU-index, nT'
        PRINT,  'pc_idx     : pc-index '
        PRINT,  'lyman_alfa : Lyman_alpha'
        PRINT,  'pflux_1MeV : Proton flux (>1 Mev)'
        PRINT,  'pflux_2MeV : Proton flux (>2 Mev)'
        PRINT,  'pflux_4MeV : Proton flux (>4 Mev)'
        PRINT,  'pflux_10MeV: Proton flux (>10 Mev)'
        PRINT,  'pflux_30MeV: Proton flux (>30 Mev)'
        PRINT,  'pflux_60MeV: Proton flux (>60 Mev)'
        PRINT,  'flux_FLAG  : Flux FLAG '
        PRINT,  '###############################################################################'
        PRINT,  'For better references, consult https://omniweb.gsfc.nasa.gov/form/dx1.html'
        PRINT,  '###############################################################################'                                                    
    ENDIF

;Generate time series of the selected data structured
        parameter    = FLTARR(file_number*24)                       
        FOR i = 0, N_ELEMENTS(exist_data_file)-1 DO BEGIN
                IF exist_data_file[i] EQ 1 THEN BEGIN
                        tmp_year    = 0
                        tmp_month   = 0
                        tmp_day     = 0
                        READS, string_date[i], tmp_year, tmp_month, tmp_day, FORMAT='(I4,I02,I02)'
                        d_ip = ip_dat([tmp_year, tmp_month, tmp_day])
                        
                        parameter[i*24:(i+1)*24-1] = d_ip.(variable)[*]
                                                                                                                       
                ENDIF ELSE BEGIN
                        parameter[i*24:(i+1)*24-1] = 999999.0
                ENDELSE                
        ENDFOR
    RETURN, parameter

END         
