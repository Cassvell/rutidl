PRO geomagic_test, initial_date, final_date
On_error, 2
COMPILE_OPT idl2, HIDDEN
	yr_i	= initial_date[0]
	mh_i	= initial_date[1]
	dy_i 	= initial_date[2]	

	yr_f	= final_date[0]
	mh_f	= final_date[1]
	dy_f 	= final_date[2] 


    gms_stn     = 'coe'
    real_time   = keyword_set(1)
    geomagixs_setup_commons
    @geomagixs_commons
    geomagixs_check_system
    geomagixs_check_gms, STATION=gms_stn, /force_all
    geomagixs_setup_dates, STATION=gms[system.gms].name, /force_all
    geomagixs_quietdays_download, initial_date, final_date, /force_all
    geomagixs_magneticdata_download, initial_date, final_date, STATION=gms[system.gms].name, /force_all
    geomagixs_magneticdata_prepare, initial_date, final_date, STATION=gms[system.gms].name, /force_all
    geomagixs_magneticdata_process, initial_date, final_date, STATION=gms[system.gms].name, /force_all, REAL_TIME=real_time
    geomagixs_magneticindex_compute, initial_date, final_date, STATION=gms[system.gms].name, /force_all, REAL_TIME=real_time
    geomagixs_magneticindex_monthlyfile, initial_date, final_date, STATION=gms[system.gms].name, /force_all, REAL_TIME=real_time
    geomagixs_setup_dates, STATION=gms[system.gms].name, /force_all, /update_file


	
    
	geomagixs_magneticindex_plotk, initial_date, final_date, STATION=gms[system.gms].name, /force_all, REAL_TIME=real_time
	
    geomagixs_magneticindex_plotdh, initial_date, final_date, STATION=gms[system.gms].name, /force_all, REAL_TIME=real_time
        
	;geomagixs_magneticindex_plotb, initial_date, final_date, STATION=gms[system.gms].name, /force_all, REAL_TIME=real_time    

END
