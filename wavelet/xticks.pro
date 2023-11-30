
;****************************************************************
; Se define la función XTICKS antes, para dar formato al eje X en el
; Tiempo en el formato de hh:mm:ss
;************************************
FUNCTION XTICKS, axis, index, value
hour = LONG(value)/3600
minute = LONG(value-3600 * hour) / 60
;sec = value mod 60
day = LONG(value)/(24*60)
;month = LONG
;RETURN, STRING(hour, minute, sec, FORMAT="(i2.2, ':', i2.2, ':', i2.2)")
RETURN, STRING(day, FORMAT="(i4)")
END
