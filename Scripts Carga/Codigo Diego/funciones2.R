#Funciones v2
#las funciones depuradas

# Analiza un paciente
idusuario =  "GIUS"
dias_a = 7
hoy <- date (today()) # dar opcion de cambiar día


diariousuario <- individual (idfiltro = idusuario)  # todos los datos
diariousuario_a <-  filter (diariousuario, fecha > (hoy-7)) # datos de la ultima semana
message ("ANALISIS DE ", idusuario)
message ("_________________________")
faltas   (individual (idusuario),dias_a,idusuario)
observde (usuario = idusuario,dia2 = dias_a)


#alertas de sueño 
mediaduracion <- mean (diariousuario [,'duracionsueño'])
mediaduracion_a <- mean (diariousuario_a [,'duracionsueño'])
message  ("La media de sueño esta semana es ", mediaduracion_a, " ( media habitual = ",mediaduracion,").")

               