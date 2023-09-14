FUNCTION pws_powerlaw, fk, pws, a, N, eps
	On_error, 2
	COMPILE_OPT idl2, HIDDEN

;###############################################################################
;###############################################################################
;###############################################################################
logX = (-0.57721466/alog10(10))-ALOG10(2)

logP = ALOG10(N) -a*(ALOG10(fk))
P = EXP(logP)
P = P/SQRT(TOTAL(P^2))
;###############################################################################
;###############################################################################
;KS test
gamma_hat = pws/P 
;print, ''
;PRINT, 'TEST KS: MEDIAN(gamma), EXP(Xi), MAX(gamma)'
;print, MEDIAN(gamma_hat),EXP(logX), MAX(gamma_hat)
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
;print, ""
;PRINT, "Error^2 del modelo logarítmico, Error^2 del modelo"
;PRINT, err2_mod_log, EXP(err2_mod_log)

M_j = ALOG(P)
S_j = SQRT(err2_mod_log) * ALOG(10)
y = 2.5e-5; Una variable dada por los gamma ??
PDF = 1/(S_j*y * 2*!PI)*EXP(-(ALOG(y)-M_j)^2/(2*(S_j)^2))

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
;###############################################################################
	fit = {P : FLTARR(N_ELEMENTS(P)), P_lim : FLTARR(N_ELEMENTS(P))}
	fit.P[*] 		= P
	fit.P_lim[*]	= P_lim
	
	RETURN, fit
END
