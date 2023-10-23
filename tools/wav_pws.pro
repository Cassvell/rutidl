;
;Name:
;	wav_pws.pro
;purpose:
;	generate power law array for power spectra density graph
;author:
;	Carlos Isaac Castellanos Velazco
;	Estudiante de Maestría en Ciencias de la Tierra
;	Instituto de Geofísica, Unidad Michoacan
;	UNAM
;	ccastellanos@igeofisica.unam.mx
;
;category:
;   data generator
;
;calling sequence:
;   .r pws_powerlaw
;   pws_powerlaw, fk, pws, a, N, eps
;parameters:
;   fk:  freq data
;	pws: power spectra data
;	a:   power law 
;	N:   Abbcisae
;	eps: confidence limit
;dependencies:
;
;
;input data
;   Fourier freq data and it's resulting power spectra density. File of power law 
;   from power-aw_table
;
;output data:
;   array of N_ELEMENTS(pws) P model 
;
;   imported to: 
;version
;   oct, 2023
;
;note

FUNCTION wav_pws

END
