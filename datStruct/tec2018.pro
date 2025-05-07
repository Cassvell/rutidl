

function tec2018
On_error, 2
COMPILE_OPT idl2, HIDDEN
    @set_up_commons
    set_up	

    path = set_var.Mega_dir+'tec/'
    file_name = path+'ago201824-30.dat'
    print, file_name

    
    file = FILE_SEARCH(file_name, COUNT=opened_files)
		IF opened_files NE N_ELEMENTS(file) THEN MESSAGE, file_name+' not found'
    ; Open the file for reading

    READCOL, file_name, day, hour, pcTEC,  TEC, MEDT,  idx, DELIMITER= '    ',$
    FORMAT='I,I,F,F,F,F'   
    
    df = {day : day , hour : hour, pcTEC : pcTEC, TEC : TEC, MEDT : MEDT, idx : idx}

    return, df
end