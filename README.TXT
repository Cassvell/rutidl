Instrucciones para la ejecución de las rutinas:

(A)Empezar con las rutinas en el directorio "initial_GSanalysis_routines".

-Datos requeridos:
    >archivos DH.
    >archivos Dst.
    >archivos Kp/Ap.
    >archivos Kmex/Amex.
    >archivos ip.
    >archivos TEC
    
    * Los formatos de los archivos deben ser de acuerdo a las funciones de estructura
    de datos, que está determinada para cada tipo de archivo, en las rutinas disponibles 
    en el directorio "datStruct". 
    Si algún archivo de la base de datos no coincide, se recomienda modificar y/o generar 
    una nueva estructura de datos. Alternativamente se puede modificar la estructura de los
    archivos a través de la escritura de nuevos archivos.
    
    En este caso, usar las rutinas:
        "tec_newformat.py" y "idx_newformat", en el directorio /master_thesis/rutpy/. Estas tomarán
        los archivos descagados de Kp, symH y Dst provenientes del sitio ISGI y los archivos de 
        salida del software "IONEX READER" para generar archivos diarios.        
    
    
1-Como primera rutina se puede iniciar con "index_listing.pro" cuyo propósito es el de generar la
escritura de listas en formato .dat o .txt de los eventos considerados como relevantes, de acuerdo
con el criterio del estudio a realizar, en relación a índices geomagnéticos para las tormentas 
geomagnéticas (TGM).
En la lista, pueden aparecer dos o más días consecutivos que cumplan con los estándares de índice
geomagnético requerido. 

2-Es necesario considerar que, como las TGM pueden tener más de un día de duración, por lo que se 
recomienda, complementar con la rutina "dh_dst_plot.pro". Con esta rutina, se graficarán los índices
DH y Dst dentro de una ventana de tiempo ajustada a criterio del usuario que enmarque el (los) 
día(s) en que aparezcan valores asociados con la TGM. Esto permitirá reconocer los días en que
dure en total una TGM y reconcer también los periodos de TGM compleja, permitiendo eliminar de 
forma manual, fechas de la lista de TGM generada que correspondan a un mismo evento.
El otro propósito es el de eliminar de la lista aquellos eventos que involucren presencia de un
porcentaje excesivo de Gaps en los datos. Esto se decidirá a criterio del (de los) usuario(s).

2.5-Con el mismo propósito, se recomienda usar la rutina "k_plot.pro" y "kmex.pro", cuyo propósito
es similar, solo que usando como referencia valores Kp y Kmex. La diferencia de ambas rutinas es
el formato en que se presentan las gráficas.

3-En caso de ser necesario realizar el análisis de correlación y dispersión de datos, usar en el 
siguiente orden:
    >tgm_file.pro - que genera archivos con un formato específico de salida. Estos serán importados
    en el directorio especificado en la rutina, que debe ser determinado con base en el tipo de 
    estudio y de, si hay o no clasificación de los eventos.
    
    >scatter_plot.pro/corr_plot.pro - que generarán análisis y gráficas de correlación y dispersión 
    de datos. Las figuras son importadas en un directorio especificado en las rutinas.
    
4-Para realizar un estudio preliminar usando datos de medio interplanetario, se encuentran la 
rutinas que deben ser ejecutadas en el siguiente orden:
    > ip_fragm: para dividir los archivos descargados del sitio Omniweb de la NASA, de varios días 
    por archivo a archivos por día, para un manejo más práctico.
    
    > ip_plot: para graficar los datos de medio interplanetario de importancia para el estudio.
    
5-Para estudio preliminar de la ionosfera, utilizar en el siguiente orden las rutinas:
    > index_listing que incluye datos de índices geomagnéticos y TEC, para graficarlos contra el 
    tiempo
    
    > mag_iono: Rutina para graficar la respuesta ionosférica, en comparación con la respuesta
    geomagnética, destacando los periodos de tiempo en que se obtienen las respuestas más 
    significativas.
    

(B) Continuar con rutinas en el directorio "baseline_routines".

    - Datos requeridos: archivo de días quietos (Qdays) de los meses y años de estudio requeridos.
    Se descargan del sitio web ISGI. El archivo contiene una lista de las fechas de los días quietos 
    globales, así como un orden en cuanto cuál día fue más 'quieto' que otro.
    

1-H_filmaker: Esta rutina tiene como propósito la escritura de archivos diarios de datos de la 
componente H del campo geomagnético provenientes de estaciones y/o observatorios magnéticos. Con 
ésta, se espera limpiar los archivos para un uso más adecuado, además de escribirlos en un formato
adecuado para las rutinas posteriores.

2-Para derivar la linea base de variación diurna, ejecutar en el siguiente orden:
    >"sel_qday": Aunque en ISGI ya viene una lista, esta lista es global, por lo que un día quieto
                global no necesariamente será quieto a escala regional. También se debe considerar
                la presencia de datos locales para ese día quieto en partícular, así como la 
                presencia de NaN. 
                La rutina "sel_day.pro", a través de un análisis de desviación estándar, graficará 
                la actividad geomagnética y retornará los picos de desviación estándar por día. El 
                punto clave es el de seleccionar los dos días quieto de la lista con menor 
                desviación estándar, sin presencia de NaN y que además enmarquen al evento de 
                interés.
    
    >"bsq_plot.pro": Generará una interpolación entre los valores del día quieto 1 y 2. Esta mísma
                    la graficará, lo que permitirá observar cualquier irregularidad con los datos
                    antes de escribir los archivos Bsq diarios.
                    
    >"bsq_V2.pro": Escribirá archivos diarios con resolución en minutos y horas, de la linea base de
                   variación diurna. Los importará en el directorio /output/qdays                  
                
                
(C) Proceder con rutinas en el directorio "diono_analysis_routines", donde se realiza un análisis de
los efectos magnéticos de origen ionosférico Ddyn y DP2.

    Datos requeridos:
        > Archivos Bsq.
        > Archivos DK

1-Iniciar de preferencia con "new_km.pro". Este archivo generará los archivos diarios DK, en los 
cuales se agrega el efecto de Ddyn y DP2 a Kp, para intentar aproximar el índice Kmex a partir de
rangos de errores. 

2-Ejecutar la rutina "iono_resp.pro". En esta rutina se calcula el efecto magnético de Diono, un 
espectro de potencias, se separan los efectos de Ddyn y DP2 a partir de filtros de frecuencias y 
se compara con respuesta ionosférica significativa.
Las gráficas de salida, sirven para ajustar el ancho de banda de los filtros para cada evento con 
base en lo observado en la misma gráfica de espectro de potencia.                      

3-"diono_valid.pro": En esta rutina se intenta aproximar el comportamiento de los índices locales
DH y kmex, a partir de agregar los efectos DP2 y Ddyn a Dst y Kp. Adicionalmente, en la terminal
se imprime por evento una tabla con el promedio de la distancia entre las series de tiempo DH, Dst y
Dst(l), donde Dst(l) = Dst*cos(latg) + Ddyn + DP2.

4-"kdiono_effect": grafica los efectos de dK, km y Kp en forma de barras. También grafica la 
diferencia de Diono, DP2 y Ddyn. Esto ayuda a comparar las máximas variaciones en las firmas 
magnéticas y DK.



NOTAS:
    Rutinas adicionales, se encuentran en el directorio de funciones:
    * Referente a NaN
        > add_nsn.pro: ajusta determinados valores y los convierte en valores NaN.
        > nanpc.pro  : calcula el porcentaje de valores NaN en cada serie de tiempo.
        > fillnan.pro: En caso de que el porcentaje de NaN sea mínimo (criterio del usuario), 
                       rellena los huecos en la serie de tiempo, a través de interpolación.
                       
    * Referente a Análisis de frecuencias:
        > lowpass_filter    : Filtro pasa bajas para series de tiempo.
        > passband_filter   : Filtro pasa bandas para series de tiempo.
        > highpass_filter   : Filtro pasa altas para series de tiempo.
    
    * Referente a etiquetas (labeling)
        > event_case.pro    : retorna un número entero que hace referencia al número de evento con 
                              base en la ventana de tiempo en que transcurre el evento.
        
        > month_name.pro    : retorna una cadena de caracteres del nombre del mes a partir del
                              formato de entrada [yyyy,mm,dd]. Puede ser usado tanto el més de 
                              inicio, como el de fin (en caso de que el evento ocurra durante la 
                              transición de dos meses).

        > xlabel.pro        : retorna un vector de cadenas de caracteres y enteros para etiquetar
                              el eje de tiempo de una gráfica. La unidad utilizada es en formato de 
                              días.                               
        

                
        
        

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
