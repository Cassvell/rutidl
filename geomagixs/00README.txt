el sistema se debe de dividir en modulos:
kmex_data_...
kmex_system_...
kmex_plot...
kmex_index...

para el 26 de enero, ya está incorporado el sistema IAGA para el cálculo de Kmex.
los archivos resultantes tienen en su nombre "standard".
el proceso que no es compatible con los anteriores es processstandarddata,
los demás procesos son compatibles con el modificador /no_standard para hacer
cálculos por el método estadístico.

se deben de agregar procedimientos para kp y dst. los procedimientos deben:
descargar-actualizar archivo de datos
generar archivo diario de datos
hacer grafica

PATH=/home/piter/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin
SHELL=/bin/sh
# m h  dom mon dow   command
59 */1 * * * sh /home/piter/GDL/geomagixs/geomagixs.sh > /home/piter/GDL/geomagixs/log.txt
https://www.redeszone.net/tutoriales/servidores/cron-crontab-linux-programar-tareas/
comprobar cron
https://crontab.guru/

ES IMPORTANTE REGISTRAR LA COMPUTADORA EN EL SISTE SSH EN UNA INSTALACION NUEVA!!!!
