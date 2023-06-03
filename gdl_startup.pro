; TRUE COLOR AND MAINTAIN BACKING STORE
;DEVICE, true_color = 24
DEVICE, retain = 2

; INTEGERS AT MAIN LEVERL ARE LONG AND ENFORCE SQUARE BRAKETS FOR ARRAYS
COMPILE_OPT IDL2

; GDL PAHT
!PATH =               EXPAND_PATH('+/usr/local/rsi/idl70/lib') +':'+$
                      EXPAND_PATH('+/home/isaac/geomstorm/rutidl') +':'+$
                      EXPAND_PATH('+/home/isaac/geomstorm/rutpy')  +':'+$
                      !PATH 
        
;!quiet = 1
        
print, 'GDL_STARTUP.pro executed!!!'
print, ''
