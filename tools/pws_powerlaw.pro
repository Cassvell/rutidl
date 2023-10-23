;
;Name:
;	pws_powerlaw.pro
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

FUNCTION pws_powerlaw, fk, pws, a, N, eps
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

;###############################################################################
;###############################################################################
logX = (-0.57721466/alog10(10));+ALOG10(2)
X = (exp(logX))*2
logP = ALOG10(N) -a*(ALOG10(fk))
P = EXP(logP)
P = P/SQRT(TOTAL(P^2))
;###############################################################################
;###############################################################################
;Chi
QR1 = cgPercentiles(pws, Percentiles=[0.25])  
QR3 = cgPercentiles(pws, Percentiles=[0.75])
IQR = (QR3-QR1)*1.5
chi2 = TOTAL((ALOG10(pws)-ALOG10(P))^2/(ALOG10(IQR))^2)

;Chi dist
e = -fk
Fchi = e/2
;PRINT, 'Chi'
;PRINT, chi2/2


;###############################################################################
;###############################################################################

;KS test
gamma_hat = 2*pws/P 
;PRINT, ''
;PRINT, 'TEST KS: MEDIAN(gamma), (Xi), chi2'
;print, MEDIAN(gamma_hat),X, chi2
;###############################################################################
;###############################################################################
;###############################################################################
;Incertidumbre de parametros
sigma = !PI^2 / (6*(ALOG10(10)^2)); Varianza de la ordenada del periodograma
a_j = ALOG10(fk)
n1 = N_ELEMENTS(fk)
delta = (n1 * TOTAL(a_j^2))-(TOTAL(a_j))^2

;error de alfa
err2_a = n1 * sigma^2/delta

;error de N
err2_Nlog = sigma^2 * TOTAL(a_j^2)/delta
err2_N    = EXP(err2_Nlog)
;print, ''
;Print, 'error de alfa, error log(N), error N'
;print, err2_a, err2_Nlog, EXP(err2_Nlog)
;###############################################################################
;###############################################################################
;###############################################################################
;Covarianza
cov= (sigma^2 * TOTAL(a_j))/delta
;print, ''
;Print, 'Cov'
;print, cov
;###############################################################################
;###############################################################################
;###############################################################################
;Icertidumbre del modelo
log_fj = ALOG(fk)
cross_prodd1 = err2_a*(log_fj)^2
cross_prodd2 = (2*cov) * log_fj

err2_mod_log = cross_prodd1 + err2_Nlog - cross_prodd2
err2_mod 	 = EXP(err2_mod_log)
print, ""
;PRINT, EXP(err2_mod_log)
y = fk; Una variable dada por los gamma ??
M_j = ALOG(P)
yy = ALOG(y)
diffsq = (yy-M_j)^2
S_j = SQRT(err2_mod_log) * ALOG(10)

PDF = 1/(S_j*y * 2*!PI)*EXP(-diffsq/(2*S_j^2))
;###############################################################################
;###############################################################################
;###############################################################################
;CONFIDENCE LEVELS
	P_hat = P/SQRT(TOTAL(P)^2)
	;eps = 0.1
	eps_n = 1-(1-eps)^n1
	gamma_e = (-2)*ALOG(eps_n/N_ELEMENTS(fk))

	;gamma_e2 = -2*ALOG(epsn)
	;Plog_95 = logP + ALOG10(gamma_e/2)
	P_lim = (gamma_e/2)* P
	;P_95_2 = gamma_e2*P
;	PRINT, 'e = ',eps, ', confidence limit: ', (1-eps)*100
;###############################################################################
;###############################################################################
err_a = a+err2_a
err_a2 = a-err2_a
logPerr1 = ALOG10(N) -(err_a)*(ALOG10(fk))
P_err1 = EXP(logPerr1)

logPerr2 = ALOG10(N) -(err_a2)*(ALOG10(fk))
P_err2 = EXP(logPerr2)
;###############################################################################

;###############################################################################
;##############################################################################
;caso lineal del libro
;a_l (minimo cuadrado)
;a_l = (1/delta) * [[sum(a_j²), -sum(a_j)], [-sum(a_j), fk]] * [[sum(pws)], [sum(a_j*pws)]]
;mat1 = [[TOTAL(a_j^2), -TOTAL(a_j)], [-TOTAL(a_j), n1]]
;mat2 = [[TOTAL(pws)],[TOTAL(a_j*pws)]]
;mat1 = FLTARR(2, N_ELEMENTS(a_j))
;mat1[0,*] = 1
;mat1[1,*] = fk

;mat1T  = TRANSPOSE(mat1)
d   = FLTARR(1, N_ELEMENTS(pws))
;mat_rev = REVERSE(mat1T##mat1)
;a_l = mat_rev##mat1T##d
;a_l = (1/delta)*mat1*mat2

a_l = (n1*TOTAL(P*ALOG(pws))-TOTAL(P)*TOTAL(ALOG(PWS)))/(n1*TOTAL(P^2)-(TOTAL(P))^2)
N_l = (TOTAL(ALOG(pws))*TOTAL(P^2)-TOTAL(P)*TOTAL(P*ALOG(pws)))/(n1*TOTAL(P^2)-(TOTAL(P))^2) 

a_lw= (TOTAL(pws^2)*TOTAL(P*pws^2*ALOG(pws))-TOTAL(P*pws^2)*TOTAL(pws^2*ALOG(pws)))/(TOTAL(pws^2)*TOTAL(P^2*pws^2)-(TOTAL(P*pws^2))^2)
N_lw= (TOTAL(P^2*pws^2)*TOTAL(pws^2*ALOG(pws))-TOTAL(P*pws^2)*TOTAL(P*pws^2*ALOG(pws)))/(TOTAL(pws^2)*TOTAL(P^2*pws^2)-(TOTAL(P*pws^2))^2)
	
	fit = {P : FLTARR(N_ELEMENTS(P)), P_lim : FLTARR(N_ELEMENTS(P)), $
	PDF : FLTARR(N_ELEMENTS(PDF)), a_2err : 0.0, N_2err : 0.0, P_err : FLTARR(N_ELEMENTS(P)),$
	a_l : 0.0, N_l : 0.0, a_lw : 0.0, N_lw : 0.0};
	fit.P[*] 		= P
	fit.P_lim[*]	= P_lim
	fit.PDF[*]		= PDF
	fit.a_2err		= err2_a
	fit.N_2err		= err2_N
	fit.P_err[*]    = err2_mod
	fit.a_l			= a_l
	fit.N_l			= N_l
	fit.a_lw		= a_lw
	fit.N_lw		= N_lw	
	RETURN, fit
END
