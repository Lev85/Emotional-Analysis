#Carga datos iniciales y los arregla un poco
############################################


# carga el diario en el objeto diario y lo arregla
# cojo xlsx y lo paso a data frame
diario <- as.data.frame (read_excel (path = "datos/diario total.xlsx"))
#cambio nombres de etiqueras
etiquetas.diario <- c("inicio", "fin", "correo", "usuario", "ánimo",  "ansiedad", "irritabilidad", "id", "concentración", "tabaco", "cafeina","dormir", "despertar", "otras_drogas", "alcohol", "observaciones", "motivación","calidad_sueño")
names(diario)<-etiquetas.diario
#Pongo usuarios en mayusculas
diario$id<- toupper (diario$id) ##pasamos usuarios a mayusculas

##transformo hora de finalización a 'lentitud' que es el tiempo que  tarda en hacerlo con una resta
diario$lentitud <- as.numeric(diario$fin-diario$inicio)

# transformo textos de escalas Likert -3/+3 tomando solo los dos primeros caracteres o el primero
diario$irritabilidad  <- strtrim(diario$irritabilidad,2)
diario$ánimo          <- strtrim(diario$ánimo,2)
diario$motivación     <- strtrim(diario$motivación,2)
diario$ansiedad       <- strtrim(diario$ansiedad,1)
diario$calidad_sueño  <- strtrim(diario$calidad_sueño,1)
diario$concentración  <- strtrim(diario$concentración,1)

# transformo cafeína a numérico
diario$cafeina <- as.numeric(diario$cafeina)

## Transformación de los textos de horas a duraciones

#paso a las horas a duraciones en horas numeric
diario$dormir <- hm (diario$dormir)
diario$despertar <- hm (diario$despertar)

diario$dormir = as.numeric (diario$dormir)/ 3600
diario$despertar = as.numeric (diario$despertar)/ 3600

#Pasa horas de dormir a algo correcto:
## Genero duracionsueño
diario$duracionsueño <- NA

### y ahora calculo duracionsueño....

for (x in 1:nrow (diario)) {
  ##Arreglo variable dormir primero
  #D<12&D>6 ++12
  a <- diario$dormir [x]
  b <- diario$despertar [x]
  if  (a>6 & a<13)  diario$dormir [x] = (diario$dormir [x] + 12)
  #D=12 ->24
  if  (a>23.9999)  diario$dormir [x] = (diario$dormir [x] = 0 )
  ## Ahora arreglo con respecto a eso duracionsueño
  
}
diario$duracionsueño <- diario$despertar- diario$dormir
##AQUI HAY UNOS EN LOS QUE SE DUERMEN MASD DE 12 y no les suma.. porque sale del bucle con un error
for (x in 1:nrow (diario)) {
  #Si Dormir está entre 0 y 6 duracionsueño es la resta y ya está hecho
  #Si Dormir está entre  13 y 24 le sumo 24
  if  (diario$dormir [x] >12&diario$dormir [x]<25) {
  message ("UNO MAL: ", diario$duracionsueño[ x])  
  diario$duracionsueño[x] <- (diario$duracionsueño [x])+ 24
  message ("y ahoraa. ", diario$duracionsueño[x]) 
  }
  
    
   }
#genero variable del cénit del sueño que es la parte media del sueño  cenitsueño = dormir + duracionsueño/2
diario$cenitsueño <- (as.numeric (diario$despertar)-as.numeric(diario$duracionsueño)/2)

# transformar variables a numeros
diario$cafeina= as.numeric(diario$cafeina)
diario$ánimo= as.numeric(diario$ánimo)
diario$tabaco= as.numeric(diario$tabaco)
diario$irritabilidad= as.numeric(diario$irritabilidad)
diario$ansiedad= as.numeric(diario$ansiedad)
diario$concentración= as.numeric(diario$concentración)
diario$calidad_sueño= as.numeric(diario$calidad_sueño)
diario$motivación= as.numeric(diario$motivación)

#transformar cualitativas a numérico

for (x in 1:nrow (diario)) {
  if  (diario$alcohol[x] == "Sí") diario$alcohol_num [x] = 1
  else diario$alcohol_num [x] = 0
}

for (x in 1:nrow (diario)) {
  if  (diario$otras_drogas[x] == "Sí") diario$otras_drogas_num [x] = 1
  else diario$otras_drogas_num [x] = 0
}
# creo variable con la longitud de las observaciones
diario <- mutate (diario, observ_num = nchar (observaciones))



# limpio dataframe de variable no numéricas
diario <- diario [,!colnames(diario)=="correo"]
diario <- diario [,!colnames(diario)=="otras_drogas"]
diario <- diario [,!colnames(diario)=="alcohol"]
diario <- diario [,!colnames(diario)=="observaciones"]
diario <- diario [,!colnames(diario)=="usuario"]


# genero lista de paciente
pacientes <- unique (diario$id)


## Rescato la hora con decimales de la variable inicio para no perder información sobre la hora a la que rellena el test
diario$horatest <-  (hour (diario$inicio) + (minute (diario$inicio))/60)
diario[is.na(diario)] =0

## quito variable fin
diario <- diario [,!colnames(diario)=='fin']


# carga las intervenciones en intervenciones y las arregla
{


intervenciones <- as.data.frame (read_excel (path = "datos/Intervenciones v2.xlsx"))

#quito cosas inútiles y cambiando nombre de variables
intervenciones <- intervenciones [,!colnames(intervenciones)=='Hora de finalización']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Correo electrónico']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Nombre']
colnames(intervenciones)[1]<-"fecha"
colnames(intervenciones)[2]<-"id"
colnames(intervenciones)[3]<-"alivio"
intervenciones <- intervenciones [,!colnames(intervenciones)=='Id del paceinte2']
colnames(intervenciones)[4]<-"terapeuta"
intervenciones <- intervenciones [,!colnames(intervenciones)=='EEAG']
colnames(intervenciones)[5]<-"programada"
intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna1']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna2']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna3']
intervenciones <- intervenciones [,!colnames(intervenciones)=='EEAG\r\n100 – 91 Actividad satisfactoria en una amplia gama de actividades, nunca parece superado por los\r\n problemas de su vida, es valorado por los demás a causa de sus abundantes cualidades\r\n positi...']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Intervenciones']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna4']
colnames(intervenciones)[6]<-"dx"
intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención1']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención2']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención3']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención4']
intervenciones <- intervenciones [,!colnames(intervenciones)=='Observaciones']
colnames(intervenciones)[7]<-"acciones"
intervenciones$fecha<- date (intervenciones$fecha)
#Pongo usuarios en mayusculas
intervenciones$id<- toupper (intervenciones$id) ##pasamos usuarios a mayusculas


# transformo diagnóstico a 0 eutimia, 1 manía, -1 depresión, X fase mixta con los dos últimos números
# transformo textos de escalas Likert -3/+3 tomando solo los dos primeros caracteres o el primero
intervenciones$dx <- as.factor((substr (intervenciones$dx,nchar (intervenciones$dx)-1, nchar (intervenciones$dx))))

grepl ("tratamiento significativo;", intervenciones$acciones)
#duplicamos acciones
intervenciones$cambiosignificativo<-intervenciones$acciones
intervenciones<- mutate_at(intervenciones, 'cambiosignificativo', funs(grepl ("tratamiento significativo;", intervenciones$acciones)))
# de momento me deshago del resto de acciones
intervenciones <- intervenciones [,!colnames(intervenciones)=='acciones']
colnames(intervenciones)[7]<-"tipo"


## SI EL DIAGNOSTICO NO ES DU, lo PASO A NA
#  SI Cambio significativo es NA lo paso a FALSE 
for (x in 1:nrow (intervenciones)) {
  a <- intervenciones$terapeuta [x]
  if  (a != "DU")      intervenciones$dx [x] <- NA
  if  (is.na (intervenciones$cambiosignificativo [x]))      intervenciones$cambiosignificativo [x] <-  FALSE
}

#### CARGO TEST
young <- as.data.frame (read_excel (path = "datos/Young v2.xlsx"))
hrsd <-  as.data.frame (read_excel (path = "datos/HRSD v21.xlsx"))
### los limpio
young <- young [,!colnames(young)=='Hora de inicio']
young <- young [,!colnames(young)=='Hora de finalización']
young <- young [,!colnames(young)=='Correo electrónico']
young <- young [,!colnames(young)=='Nombre']
young <- young [,!colnames(young)=='Pérdida de peso']
young <- young [,!colnames(young)=='Hipocondria']
young <- young [,!colnames(young)=='Conciencia de enfermedad']
colnames(young)[7]<-"curso"
colnames(young)[8]<-"contenido"
colnames(young)[11]<-"conciencia"
colnames(young)[12]<-"id"
young <- young [,!colnames(young)=='Recoge datos']
young$Fecha <- date (young$Fecha)
colnames(young)[13]<-"fecha"


hrsd <- hrsd [,!colnames(hrsd)=='Hora de inicio']
hrsd <- hrsd [,!colnames(hrsd)=='Hora de finalización']
hrsd <- hrsd [,!colnames(hrsd)=='Correo electrónico']
hrsd <- hrsd [,!colnames(hrsd)=='Nombre']
colnames(hrsd)[1]<-"id"
hrsd <- hrsd [,!colnames(hrsd)=='Recoge datos']
colnames(hrsd)[3]<-"depresion"
colnames(hrsd)[4]<-"culpa"
colnames(hrsd)[6]<-"iprecoz"
colnames(hrsd)[7]<-"imedio"
colnames(hrsd)[8]<-"itardio"
colnames(hrsd)[9]<-"actividad"
colnames(hrsd)[10]<-"inhibicion"
colnames(hrsd)[12]<-"an_psiq"
colnames(hrsd)[13]<-"an_som"
colnames(hrsd)[14]<-"gastro"
colnames(hrsd)[15]<-"somatic"
colnames(hrsd)[16]<-"sexo"
colnames(hrsd)[17]<-"conciencia"
colnames(hrsd)[18]<-"hipocondria"
colnames(hrsd)[19]<-"peso"
hrsd$fecha = as.Date (hrsd$fecha)

}

pacientes <- unique (diario$id)




