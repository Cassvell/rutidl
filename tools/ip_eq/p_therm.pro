;Name:
;	p_therm.pro
;purpose:
;   Generate a time series of Thermal preassure
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
;   .r p_therm
;  P_T = p_term(n0, T)
;parameters:
;   n0: electronic density [n/cc]
;   T:  plasma temperature
;
;dependencies:
;
;
;input files
;   ip data: n0, T
;   Boltzmann cte: already set       
;
;output files:
;   P_T: termic preassure in MKS system
;
;version
;   apr, 2023
;
;note
;   in order to run this routine, it is necessary, first to:
;       1. having ip data for the time window desired
;       2. this routine changes system units of n0 form CGS to MKS
;

FUNCTION p_therm, n0, T
	On_error, 2
	COMPILE_OPT idl2, HIDDEN
	
	;cambiar n CGS de densidad a MKS
	;n0 [N/cm^-3]
	
	n_mks = n0 * 1e6
	
	K = !Boltzmann
	p_T = 2*n_mks*K*T ;sistema MKS
	p_T = p_T*1e9
	

    RETURN, p_T 
END
