;
;Name:
;	fix_offset.pro
;purpose:
;	fix baseline in case of changing point
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data pre processing
;
;calling sequence:
;   .r fix_offset
;   fix_offset(x)
;parameters:
;
;dependencies:
;
;
;input files
;   time series with a changed point
;
;output files:
;   fixed changed point
;
;   imported to: 
;version
;   Apr, 2024
;
;note
;   For following analysis, this routine has to be run to create clean H obs data
;


FUNCTION fix_offset, x

	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	x_med = MEDIAN(x)
	x_baseline = REPLICATE(x_med, N_ELEMENTS(x))
	
	x_fixed = x-x_baseline
	RETURN, x_fixed
END
