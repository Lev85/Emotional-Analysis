### LISTA DE FUNCIONES

## FUNCIONES GENERALES
# individual (idfiltro, datos 0 diario) Genera un archivo de datos individual a partir del diario
individual   <- function (idfiltro,datos = diario) {
  
  ## INTERPOLA INDIDIVIDUALMENTE LOS NA
  ## DA COMO SALIDA diario2
  ## USUARIO A ANALIZAR idfiltro
  ## - AÑADE LAS INTERVENCIONES APROPIADAS
  ## - AÑADE RESULTADOS DE LOS TEST  
  # filtro usuarios segun la configuración
  diario.individual <- filter (datos,id==idfiltro)
  if (nrow(diario.individual)==1) {
    message ("Usuario ", idfiltro, " sólo tiene un dato")
    return (0)
  }
  ####################################################################
  ##########################################################
  ## SEGUNDA PARTE: Convertir diario a dataable con los NA interpolados y todo numérico
  ## debo hacer un datatable con una columna que sea de dates... con una frecuencua continua, diaria...
  ## voy  a seguir estas instrucciones: https://bocoup.com/blog/padding-time-series-with-r
  
  #lo ordeno
  diario.ordenado <- arrange(diario.individual, inicio)
  #calculo longitud
  diario.longitud <- length(diario.ordenado$inicio)
  # Busco min y max. Como están ordenados, primero y último elementos.
  tiempo.min <- diario.ordenado$inicio[1]
  tiempo.max <- diario.ordenado$inicio[diario.longitud]
  # Genero una secuencia de fechas con 1 día de intervalo. Es Date y no POSIX!
  rango.fechas <- as.Date (seq(tiempo.min, tiempo.max, by="day"))
  # Creo una variable fecha que es Date y no posix a partir de inicio
  diario.ordenado <- mutate (diario.ordenado, fecha = as.Date(inicio))
  # quito duplicados pero dejo el último, ya que si haces varias entradas en un día, la última es la que vale
  #para eso primero ordeno al revés...
  diario.ordenado <- arrange(diario.ordenado, desc(inicio))
  #y ahora quito duplicados para que quite los más recientes sólo
  diario.ordenado <-distinct(diario.ordenado,fecha,.keep_all = TRUE)
  ##lo reordeno
  diario.ordenado <- arrange(diario.ordenado, inicio)
  # Convierto rango.fechas en dataframe
  # Estoy poniendo el rango de fechas en columna called "fecha" con el mismo
  # nombre para luego poder hacer un merge
  todas.fechas <- data.frame(list(fecha=rango.fechas))
  # Merge los dos datasets 
  merge.diario <- merge(todas.fechas, diario.ordenado, all=T)
  
  #AÑADO INTERVENCIONES
  #selecciona las del usuario
  intervenciones.individual <- filter (intervenciones,id==idfiltro)
  hrsd.individual <- filter (hrsd, id == idfiltro)
  young.individual <- filter (young, id == idfiltro)
  ##PREVALENCIA DEL MEDICO SI HAY DOS OBSERVACIONES EL MISMO DIA.
  #borrar los no DU que tengan el mismo día que un DU
  #ordeno
  intervenciones.individual <- arrange(intervenciones.individual, desc(terapeuta))
  ##ESTO APARENTEMENTE BORRA LAS REPETICIONES https://www.datanalytics.com/2014/09/19/primer-elemento-de-un-grupo-dentro-de-un-dataframe-de-r/
  intervenciones.individual <- intervenciones.individual[!duplicated(intervenciones.individual$terapeuta),]
  #uno los datos en table resultado
  resultado <- full_join (merge.diario,intervenciones.individual, by = 'fecha' , copy=FALSE)
  resultado <- resultado [,!colnames(resultado)=='inicio']
  resultado <- resultado [,!colnames(resultado)=='id.x']
  resultado <- resultado [,!colnames(resultado)=='id.y']
  
  resultado <- full_join (resultado,young.individual, by = 'fecha' , copy=FALSE)
  resultado <- full_join (resultado,hrsd.individual, by = 'fecha' , copy=FALSE)
  
  resultado <- resultado [,!colnames(resultado)=='inicio']
  resultado <- resultado [,!colnames(resultado)=='id.x']
  resultado <- resultado [,!colnames(resultado)=='id.y']
  
  merge.diario<- resultado
  
  
  
  
  
  # creo una variable que sea datoNA para no perder la información de que el paciente no lo rellenó
  merge.diario$datoNA <- 0
  for (x in 1:nrow (merge.diario)) {
    if (is.na(merge.diario$ansiedad [x])) merge.diario$datoNA [x] = 1 #uno es que no lo ha hecho...
  }
  
  # Borro cosas que no usaré
  diario.ordenado <- diario.ordenado [,!colnames(diario.ordenado)=="inicio"]
  diario.ordenado <- diario.ordenado [,!colnames(diario.ordenado)=="fin"]
  
  
  
  # Voy a generar valores interpolados para los NA con la libreria imputeTS
  # Segun el artículo imputeTS: Time Series Missing Value Imputation in R
  # by Steffen Moritz and Thomas Bartz-Beielstein
  diario2  <-merge.diario
  # valores redondeados
  diario2$ánimo <- round (na.interpolation(diario2$ánimo))
  diario2$ansiedad <- round (na.interpolation(diario2$ansiedad))
  diario2$irritabilidad <- round (na.interpolation(diario2$irritabilidad))
  diario2$concentración <- round (na.interpolation(diario2$concentración))
  diario2$tabaco <- round (na.interpolation(diario2$tabaco))
  diario2$cafeina <- round (na.interpolation(diario2$cafeina))
  diario2$motivación <- round (na.interpolation(diario2$motivación))
  diario2$calidad_sueño <- round (na.interpolation(diario2$calidad_sueño))
  diario2$alcohol_num <- round (na.interpolation(diario2$alcohol_num))
  diario2$otras_drogas_num <- round (na.interpolation(diario2$otras_drogas_num))
  diario2$observ_num <- round (na.interpolation(diario2$observ_num))
  # valores continuos ( y de paso cambio tiempos a numeric si hace falta)
  diario2$lentitud <- as.numeric (diario2$lentitud)
  diario2$lentitud <-  (na.interpolation(diario2$lentitud))
  diario2$dormir <-  (na.interpolation(diario2$dormir))
  diario2$despertar <-  (na.interpolation(diario2$despertar))
  # Rescato la hora con decimales de la variable... 
  diario2$duracionsueño <-  (na.interpolation(diario2$duracionsueño))
  diario2$cenitsueño <-  (na.interpolation(diario2$cenitsueño))
  diario2$horatest <-  (na.interpolation(diario2$horatest))
  
  #genero variable actividad subjetiva que es ansiedad + irritabilidad + motivación
  diario2$actividadsubjetiva <- (as.numeric (diario2$ansiedad)+ as.numeric(diario2$irritabilidad)+as.numeric(diario2$motivación))
  
  #genero lentitudsinoutliers y lentitudescalada para hacer gráficos normalizados
  diario2$lentitudsinoutliers <- rm.outlier(diario2$lentitud,fill = TRUE,)
  diario2$lentitudescalada <- rescale(diario2$lentitudsinoutliers, to = c(1, 5), from = range(diario2$lentitudsinoutliers, na.rm = TRUE, finite = TRUE))
  diario2$semana <- (week (diario2$fecha)+year (diario2$fecha)*52)
  
  diarioindiv <- diario2
  
  
  return (diarioindiv)
}
#genera archivo de datos individuales limpios e interpolados a partir del diario
faltas       <- function (datosindiv2, diasrevisar ,usuario2) {
  #dice los fallos en los últimos días en el dataframe que le demos
  numfil <- nrow (datosindiv2)
  
  if ((numfil-diasrevisar+1)<1) {
    message (usuario2,  " no tiene datos suficientes")
    return ()
  
    }
  datosindiv <- slice (datosindiv2, (numfil-diasrevisar+1):numfil)
  fallosindiv <- (nrow (datosindiv)-nrow (filter (datosindiv, datoNA == 0 )))
  message (c ('Paciente ', usuario2, ' tiene ', fallosindiv, ' faltas en los últimos ', diasrevisar, ' días' ))
  return (fallosindiv) 
}
faltas_n       <- function (datosindiv) {
  #devuelve  fallos en el dataframe que le demos
  fallosindiv <- (nrow (datosindiv)-nrow (filter (datosindiv, datoNA == 0 )))
  return (fallosindiv) 
}
observ       <- function (datosindiv2, diasrevisar,usuario2) {
  #dice las observaciones en los últimos días en el dataframe que le demos
  numfil <- nrow (datosindiv2)
  if ((numfil-diasrevisar+1)<1) {
    message (usuario2,  " no tiene datos suficientes")
    return ()
  }
  datosindiv <- slice (datosindiv2, (numfil-diasrevisar+1):numfil)
  observaciones <- (nrow (datosindiv)-nrow (filter (datosindiv, observ_num == 0 )))
  message (c ('Paciente', usuario2, '  tiene ', observaciones, ' observaciones en los últimos ', diasrevisar, ' días' ))
  return (observaciones) 
}
faltasde     <- function (usuario, dia2) {
  # dice los fallos de usuario, en los últimos dia2 días
  faltas (individual (usuario),dia2,usuario)
  
}
observde     <- function (usuario, dia2) {
  # dice observaciones de usuario en los últimos dia2 días
  observ (individual (usuario),dia2,usuario)
  
}
todasfaltas  <- function (dias){
  for (paciente in pacientes) {
    faltasde (paciente,dias)
  }
}
#saca un listado de faltas
todasobserv  <- function (dias){
  for (paciente in pacientes) {
    observde (paciente,dias)
  }
}
#saca un listado de observ
normalizar   <- function (datos) {
  datos <- mutate_at (datos, 2:18, funs(rescale(.)))
}
#normaliza una lista de datos de 2 a 18!! ,lo normaliza consigo mismo...
normalizadiario <- function () {  ## DATOS NORMALIZADOS CON TODOS LOS PACIENTES
  diarion <- mutate_at (diario, 2:4, funs(rescale(.)))
  diarion <- mutate_at (diarion, 6:19, funs(rescale(.)))
  return (diarion)
}
#normaliza el diario, entre todos los paciente
multimergecsv = function(mypath){
  filenames = list.files(path = mypath, full.names = TRUE, pattern = "csv")
  datalist = lapply(filenames, 
                    function(x){read.csv(file = x,
                                         header = TRUE,
                                         stringsAsFactors = FALSE)})
  Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
} #une los csv de un directorio en un solo datafram
autoggir <- function (directorio1){
  
  #Automatizar conversión
  # le decimos el directorio
  # hace un directorio temporal
  # copia allí el primer archivo
  # ejecuta el script...
  # copia los archivos de interes (2)
  # borra temporal
  
  
  #pongo el directorio a procesar
  #directorio1 <- "GENEACTIV UBIP/GIUS"
  
  listabin <- dir (directorio1) #lista de archivos a procesar
  message (" voy a procesar ", listabin)
  message ("__________________")
  for(archivo in listabin) {
    message("     PROCESO ",archivo)
    message ("__________________")
    # creo directorio temporal
    dir.create(paste (directorio1,"/temporal",sep = ""))
    # copio a temporal el archivo
    origen <- paste (directorio1,"/",archivo, sep = "")
    fin <- paste (directorio1,"/temporal/","trabajo.bin", sep = "")
    file.copy (origen, fin)
    
    
    #### LO PROCESO 
    {
      
      directorio <- paste (directorio1,"/temporal/", sep ="")
      
      getwd()
      #setwd("") # Directorio en el que se encuentra el .bin
      # Es importante poner la misma ruta en "datadir" y "outputdir"
      g.shell.GGIR(#=======================================
                   # INPUT NEEDED:
                   mode=c(1,2,3,4,5),
                   datadir=directorio,
                   outputdir=directorio,
                   f0=1, f1=2,
                   #-------------------------------
                   # Part 1:
                   #-------------------------------
                   # Key functions: reading file, auto-calibration, and extracting features
                   do.enmo = TRUE,             do.anglez=TRUE,
                   chunksize=1,                printsummary=TRUE,
                   #-------------------------------
                   # Part 2:
                   #-------------------------------
                   # strategy antes 2, ndays antes 7, maxdur antes 9
                   strategy = 3,               ndayswindow=300,
                   hrs.del.start = 0,          hrs.del.end = 0,
                   maxdur = 900,                 includedaycrit = 16,
                   winhr = c(5,10),
                   qlevels = c(c(1380/1440),c(1410/1440)),
                   qwindow=c(0,24),
                   ilevels = c(seq(0,400,by=50),8000),
                   mvpathreshold =c(100,120), #Umbral de ejercicio MVPT!!!
                   bout.metric = 4,
                   closedbout=FALSE,
                   #-------------------------------
                   # Part 3:
                   #-------------------------------
                   # Key functions: Sleep detection
                   timethreshold= c(5),        anglethreshold=5,
                   ignorenonwear = TRUE,
                   #-------------------------------
                   # Part 4:
                   #-------------------------------
                   # Key functions: Integrating sleep log (if available) with sleep detection
                   # storing day and person specific summaries of sleep
                   
                   ##nnights antes 9, ahora 200, outliers only antes T, ahora F
                   excludefirstlast = TRUE,
                   includenightcrit = 16,
                   def.noc.sleep = c(),
                   outliers.only = FALSE,
                   criterror = 4,
                   relyonsleeplog = FALSE,
                   sleeplogidnum = TRUE,
                   colid=1,
                   coln1=2,
                   do.visual = TRUE,
                   nnights = 200 ,
                   #-------------------------------
                   # Part 5:
                   # Key functions: Merging physical activity with sleep analyses
                   #-------------------------------
                   threshold.lig = c(30), threshold.mod = c(100),  threshold.vig = c(400),
                   boutcriter = 0.8,      boutcriter.in = 0.9,     boutcriter.lig = 0.8,
                   boutcriter.mvpa = 0.8, boutdur.in = c(1,10,30), boutdur.lig = c(1,10),
                   boutdur.mvpa = c(1),   timewindow = c("WW"),
                   #-----------------------------------
                   # Report generation
                   #-------------------------------
                   # Key functions: Generating reports based on meta-data
                   do.report=c(2,4,5),
                   visualreport=TRUE,     dofirstpage = TRUE,
                   viewingwindow=1)
      
    }
    
    archivopdf <- paste (directorio1,"/temporal/output_temporal/results/file summary reports/Report_trabajo.bin.pdf", sep = "")
    archivop5 <- paste (directorio1,"/temporal/output_temporal/results/part5_daysummary_WW_L30M100V400_T5A5.csv", sep = "")
    archivopdf2 <- paste (directorio1,"/temporal/output_temporal/results/visualisation_sleep.pdf", sep = "")
    
    pdffin <- paste (directorio1,"/",archivo,".pdf",sep="")
    csvfin <- paste (directorio1, "/",archivo,"_p5.csv",sep="")
    pdf2fin <- paste (directorio1,"/",archivo,"sleep.pdf",sep="")
    file.copy (from = archivopdf, to = pdffin,overwrite = TRUE )
    file.copy (from = archivop5, to = csvfin,overwrite = TRUE  )
    file.copy (from = archivopdf2, to = pdf2fin,overwrite = TRUE )
    unlink (paste (directorio1,"/temporal",sep = ""), recursive = TRUE)
    
    
    
  }
}
#le damos el directorio y procesa los bin
#deja los csv y los pdf en el mismo directorio

#Une un indiviudal a los archivos csv de GGIR que existan en un directorio

unirdatos <- function(directorioggir1,individuo) {
  
  individual (individuo)->diarioindividual            #genero individual
  csvunido <- multimergecsv(directorioggir1)  #uno csvs en uno
  colnames(csvunido)[4]<-"fecha"                      #cambio nombre para que coincida
  csvunido$fecha<- dmy(csvunido$fecha)                #cambio formato de fecha
  datospaciente<- full_join (y = csvunido, x = diarioindividual, by = 'fecha' , copy=F)   #lo uno 
  return (datospaciente)
}


# hace un unir dato y escribe el csv en el directorio
csvfinal <- function(directorioggir1, individuo) {
unirdatos (directorioggir1,individuo)->datofinal
  write.csv(file = paste (directorioggir1,"/CSVFINAL/dat_total_",individuo,".csv", sep = ""),x = datofinal)
}
# Funciones gráficas
grafica1     <- function (usuario) {
  datosg <- individual (usuario)
  datosg <- normalizar(datosg )
  zz <- ggplot (datosg, aes(x = fecha, y = value, color = variable)) +
    geom_line(aes(y=  cenitsueño, col = "Cénit")) +
    geom_line(aes(y=  ansiedad, col = "Ansiedad")) +
    geom_line(aes(y=  irritabilidad, col = "Irritabilidad"))+
    geom_line(aes(y=  lentitud, col = "lentitud")) +
    #+ geom_line(aes(y=  concentración, col = "Concentración"))
    #+ geom_line(aes(y=  tabaco, col = "Tabaco"))
    #+ geom_line(aes(y=  cafeina, col = "Cafeína"))
    #+ geom_line(aes(y=  calidad_sueño, col = "Calidad"))
    geom_line(aes(y = duracionsueño, col = "Duración"))
  return (zz)} #datos normalizados con todos
grafica2     <- function (usuario) {
  diarion <- normalizadiario()
  datosg <- individual (usuario, diarion)
  zz <- ggplot (datosg, aes(x = fecha, y = value, color = variable)) +
    geom_line(aes(y=  cenitsueño, col = "Cénit")) +
    geom_line(aes(y=  ansiedad, col = "Ansiedad")) +
    geom_line(aes(y=  irritabilidad, col = "Irritabilidad"))+
    geom_line(aes(y=  lentitud, col = "lentitud"))+
    geom_line(aes(y=  concentración, col = "Concentración")) +
    geom_line(aes(y=  tabaco, col = "Tabaco"))+
    geom_line(aes(y=  cafeina, col = "Cafeína"))+
    geom_line(aes(y=  calidad_sueño, col = "Calidad")) +
    geom_line(aes(y = duracionsueño, col = "Duración"))
  return (zz)
  
} #datos normalizados con uno mismo


procesapaciente <- function(usuario, directoriobin = 0) {
if (directoriobin == 0) message ("Usuario sin reloj. ") else  autoggir(directoriobin)
if (directoriobin == 0) individual (usuario) -> datofinal  else unirdatos (directoriobin, usuario)->datofinal
if (directoriobin == 0) write.csv (file = paste ("datos sin reloj/","dat_simple_",usuario,".csv", sep = ""),x = datofinal) else  write.csv(file = paste (directoriobin,"dat_total_",usuario,".csv", sep = ""),x = datofinal)
return (datofinal)
}
#Hace todo el procesamiento de bin y otros datos y escribe un csv





