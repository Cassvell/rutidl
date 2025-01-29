

function freq_cuts, gs_event, station_code
On_error, 2
COMPILE_OPT idl2, HIDDEN
    @set_up_commons
    set_up	

    path = set_var.local_dir+'tools/filtertools/frequency_cuts/'
    file_name = path+station_code+'_cuts.csv'

    file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'
    ; Open the file for reading

    READCOL, file_name, event, ddyn_lfc, ddyn_hfc,  ddyn_lfcpick, ddyn_hfcpick,  ddyn_h1, dp2_lfc, dp2_hfc, DELIMITER= ',',$
    FORMAT='I,F,F,F,F,F,F,F'
    
    idx = gs_event-1
    
    
    info = {event : 0 , ddyn_lfc : 0.0, ddyn_hfc : 0.0, ddyn_lfcpick : 0.0, ddyn_hfcpick : 0.0, $
            ddyn_h1 : 0.0, dp2_lfc : 0.0, dp2_hfc : 0.0}

    info.ddyn_lfc = ddyn_lfc[idx]
    info.ddyn_hfc  = ddyn_hfc[idx]
    info.ddyn_lfcpick  = ddyn_lfcpick[idx]
    info.ddyn_hfcpick  = ddyn_hfcpick[idx]
    info.ddyn_h1  = ddyn_h1[idx]
    info.dp2_lfc  = dp2_lfc[idx]
    info.dp2_hfc  = dp2_hfc[idx]



    return, info
end