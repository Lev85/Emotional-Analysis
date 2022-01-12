###########
# Generación del data_frame estandarizado/normalizado diario
###########

datos<-"diario total.xlsx"
diario <- as.data.frame (read_excel (paste(path_datos,datos,sep="")))



#cambio nombres de etiqueras
etiquetas.diario <- c("inicio", "fin", "correo", "usuario", "animo",  "ansiedad", "irritabilidad", "id", "concentracion", "tabaco", "cafeina","dormir", "despertar", "otras_drogas", "alcohol", "observaciones", "motivacion","calidad_suenho")

names(diario)<-etiquetas.diario

#### Limpiamos registros MAL imputados
diario<-subset(diario,id!="00:50")

#Pongo usuarios en mayusculas
diario$id<- toupper (diario$id) ##pasamos usuarios a mayusculas
diario$fecha <- as.Date(diario$inicio)

##transformo hora de finalización a 'lentitud' que es el tiempo que  tarda en hacerlo con una resta
diario$lentitud <- as.numeric(diario$fin-diario$inicio)

# transformo textos de escalas Likert -3/+3 tomando solo los dos primeros caracteres o el primero
diario$irritabilidad  <- strtrim(diario$irritabilidad,2)
diario$animo          <- strtrim(diario$animo,2)
diario$motivacion     <- strtrim(diario$motivacion,2)
diario$ansiedad       <- strtrim(diario$ansiedad,1)
diario$calidad_suenho  <- strtrim(diario$calidad_suenho,1)
diario$concentracion  <- strtrim(diario$concentracion,1)

# transformo cafeína a numérico
diario$cafeina <- as.numeric(diario$cafeina)


###################
## INI PAVEL M1
## Preguntar si esta conversión es necesaria, ya que las
## operaciones se pueden hacer sin convertir, y la conversión se puede hacer al final
##################

## Transformación de los textos de horas a duraciones

#paso a las horas a duraciones en horas numeric

##diario$dormir <- hm (diario$dormir)
##diario$despertar <- hm (diario$despertar)
##
##diario$dormir = as.numeric (diario$dormir)/ 3600
##diario$despertar = as.numeric (diario$despertar)/ 3600

##################
## FIN PAVEL M1
##################


#Pasa horas de dormir a algo correcto:
## Genero duracionsuenho
#diario$duracionsuenho <- NA

################
## INI PAVEL M2
## Calculo de la duracion del sueño
################

### y ahora calculo duracionsuenho....
##for (x in 1:nrow (diario)) {
##  ##Arreglo variable dormir primero
##  #D<12&D>6 ++12
##  a <- diario$dormir [x]
##  b <- diario$despertar [x]
##  if  (a>6 & a<13)  diario$dormir [x] = (diario$dormir [x] + 12)
##  #D=12 ->24
##  if  (a>23.9999)  diario$dormir [x] = (diario$dormir [x] = 0 )
##  ## Ahora arreglo con respecto a eso duracionsuenho
##  
##}
##diario$duracionsuenho <- diario$despertar- diario$dormir
####AQUI HAY UNOS EN LOS QUE SE DUERMEN MASD DE 12 y no les suma.. porque sale del bucle con un error
##for (x in 1:nrow (diario)) {
##  #Si Dormir está entre 0 y 6 duracionsuenho es la resta y ya está hecho
##  #Si Dormir está entre  13 y 24 le sumo 24
##  if  (diario$dormir [x] >12&diario$dormir [x]<25) {
##  message ("UNO MAL: ", diario$duracionsuenho[ x])  
##  diario$duracionsuenho[x] <- (diario$duracionsuenho [x])+ 24
##  message ("y ahoraa. ", diario$duracionsuenho[x]) 
##  }
##  
##    
##   }

#diario$num<-seq.int(nrow(diario))

#si dormir.hora > 6 y dormir.hora < 13 then dormir.hora <- dormir.hora + 12
#si dormir.hora <= 6 then dormir.hora <- dormir.hora + 24

## Reemplazmos algunos caracteres raros en las variables dormir y despertar
diario$dormir<-gsub('24:','00:',diario$dormir)
diario$dormir<-gsub('\\.',':',diario$dormir)
diario$dormir<-gsub(';',':',diario$dormir)
diario$despertar<-gsub('24:','00:',diario$despertar)
diario$despertar<-gsub('\\.',':',diario$despertar)
diario$despertar<-gsub(';',':',diario$despertar)


# Ambas variables las convertirmos a formato POSIXct que permite hacer operaciones aritmeticas con fechas
diario$dormir1<-as.POSIXct(diario$dormir,format="%H:%M",tz="UTC")
diario$despertar1<-as.POSIXct(diario$despertar,format="%H:%M",tz="UTC")

# Si la hora de dormir es 8,9,19,11,12 entonces se asume que este dato lo han puesto en formato de 12 horas
# por tanto se convierte en formato de 24 horas sumandoles 12 horas.
diario$dormir2<-ifelse(as.numeric(format(diario$dormir1, "%H")) > 7 & as.numeric(format(diario$dormir1, "%H")) < 13 , 
					   diario$dormir1 + 12*60*60 ,
					   # Si la hora de dormir es 1,2,3,4,5,6,7 (madrugada), 
					   # entonces se asume que se durmio en la madrugada del día siguiente, 
					   # por tanto se suma 24 horas.
					   ifelse(as.numeric(format(diario$dormir1, "%H")) <= 7 , 
					          diario$dormir1 + 24*60*60, 
							  # Cualquier otro caso, se considera normal, por tanto se deja tal cual
							  diario$dormir1))

# Para despertar, asumiremos siempre que se despierta al día siguiente
# Por eso sumamos 24 horas
diario$despertar2<-diario$despertar1+24*60*60

# Ambas variables van a estar formato numerico, se convierte a POSIXct
diario$despertar2<-as.POSIXct(diario$despertar2,origin = '1970-01-01',tz="UTC")			   
diario$dormir2<-as.POSIXct(diario$dormir2,origin = '1970-01-01',tz="UTC")			   

diario$duracionsuenho<-as.numeric(difftime(diario$despertar2,diario$dormir2,units="hours"))

##diario[,c("num","dormir","despertar","dormir2","despertar2","duracion")]
##diario[as.numeric(format(diario$dormir1, "%H"))==18,c("num","dormir","despertar","dormir2","despertar2","duracion")]

################
## FIN PAVEL M2
################


################
## INI PAVEL M3. Cenit Sueño
################
#genero variable del cénit del sueño que es la parte media del sueño  cenitsuenho = dormir + duracionsuenho/2
#diario$cenitsuenho <- (as.numeric (diario$despertar)-as.numeric(diario$duracionsuenho)/2)
diario$cenitsuenho <- as.POSIXct((as.numeric(diario$despertar2) + as.numeric(diario$dormir2)) / 2, origin = '1970-01-01',tz="UTC")
################
## FIN PAVEL M3
################


# transformar variables a numeros
diario$cafeina= as.numeric(diario$cafeina)
diario$animo= as.numeric(diario$animo)
diario$tabaco= as.numeric(diario$tabaco)
diario$irritabilidad= as.numeric(diario$irritabilidad)
diario$ansiedad= as.numeric(diario$ansiedad)
diario$concentracion= as.numeric(diario$concentracion)
diario$calidad_suenho= as.numeric(diario$calidad_suenho)
diario$motivacion= as.numeric(diario$motivacion)



################
## INI PAVEL M4. transformaciones
## No es necesario recorrer el data frame con un loop
################

###transformar cualitativas a numérico
##
##for (x in 1:nrow (diario)) {
##  if  (diario$alcohol[x] == "Sí") diario$alcohol_num [x] = 1
##  else diario$alcohol_num [x] = 0
##}
##
##for (x in 1:nrow (diario)) {
##  if  (diario$otras_drogas[x] == "Sí") diario$otras_drogas_num [x] = 1
##  else diario$otras_drogas_num [x] = 0
##}

diario$alcohol_num<-ifelse(diario$alcohol=="Sí",1,0)
diario$otras_drogas_num<-ifelse(diario$otras_drogas=="Sí",1,0)

################
## FIN PAVEL M4
################

################
## INI PAVEL M5. Creación de nueva variable sin usar dplyr
########
### creo variable con la longitud de las observaciones
##diario <- mutate (diario, observ_num = nchar (observaciones))
diario$observ_num <- nchar(diario$observaciones)
################
## FIN PAVEL M5
################


################
## INI PAVEL M6. Borramos variables
########
### limpio dataframe de variable no numéricas
##diario <- diario [,!colnames(diario)=="correo"]
##diario <- diario [,!colnames(diario)=="otras_drogas"]
##diario <- diario [,!colnames(diario)=="alcohol"]
##diario <- diario [,!colnames(diario)=="observaciones"]
##diario <- diario [,!colnames(diario)=="usuario"]
diario<-subset(diario,,-c(correo,otras_drogas,alcohol,observaciones,usuario))
################
## FIN PAVEL M6
################

# genero lista de paciente
pacientes <- unique (diario$id)


################
## INI PAVEL M7. calculamos horatest pero sin lubridate
########
## Rescato la hora con decimales de la variable inicio para no perder información sobre la hora a la que rellena el test
#diario$horatest <-  (hour (diario$inicio) + (minute (diario$inicio))/60)
diario$horatest<-as.numeric(format(diario$inicio, "%H")) +  as.numeric(format(diario$inicio, "%M"))/60
################
## FIN PAVEL M7
################

##################
# Convertimos a numeros algunas variables
############

diario$dormir<-as.numeric(format(diario$dormir2, "%H")) + as.numeric(format(diario$dormir2, "%M")) /60
diario$despertar<-as.numeric(format(diario$despertar2, "%H")) + as.numeric(format(diario$despertar2, "%M")) /60
diario$cenitsuenho<-as.numeric(format(diario$cenitsuenho, "%H")) + as.numeric(format(diario$cenitsuenho, "%M")) /60
diario<-subset(diario,,-c(dormir1,dormir2,despertar1,despertar2,fin))

diario[is.na(diario)]<-0

#diario$lentitudsinoutliers <- rm.outlier(diario$lentitud,fill = TRUE,)
	  
#diario$lentitudescalada <- rescale(diario$lentitudsinoutliers, to = c(1, 5), from = range(diario$lentitudsinoutliers, na.rm = TRUE, finite = TRUE))


diario$D_NAfilled<-"1"


	###########################
	# Añadimos fechas e interpolamos
	##############################
	#print("segundos unique")
	primero<-TRUE
	for (x in unique(diario$id)){
		#for (x in c("GOGA")){

		dp <- diario[diario$id==x,]
		if (nrow(dp)<=2) next #revisar


		# Ordenamos sin dplyr. Ordenamos descendentemente por variable inicio
		dp<-dp[order(dp$inicio,decreasing=TRUE),]

		# Quitamos duplicados sin dplyr
		dp<-dp[!duplicated(dp$fecha),]
		
		if (diarioFillNA!="NONE") {		
		
			tiempo.min <- min(dp$fecha)
			tiempo.max <- max(dp$fecha)

			rango.fechas <- as.Date (seq(tiempo.min, tiempo.max, by="day"))
			todas.fechas <- data.frame(list(fecha=rango.fechas))
			
			

			dp <- merge(todas.fechas, dp, all.x=T)
			dp$D_NAfilled<-ifelse(is.na(dp$D_NAfilled),"D","REAL")
			dp$id <- x

			dp$inicio<-as.numeric(dp$inicio)
			dp$lentitud <- as.numeric (dp$lentitud)
			
			
				diarioColnames<-colnames(diario)
			
				for (vr in diarioColnames[!diarioColnames %in% c("id","fecha","D_NAfilled")])	{	
					if (diarioFillNA=="INTER") {

						if (vr %in% c("inicio","animo","ansiedad","irritabilidad","concentracion","tabaco","cafeina","motivacion","calidad_suenho","alchol_num","otras_drogas_num","observ_num")){
							dp[[vr]]<-round(na_interpolation(dp[[vr]]))
						}			
						if (vr %in% c("lentitud","dormir","despertar","duracionsuenho","cenitsuenho","horatest")){
							dp[[vr]]<-na_interpolation(dp[[vr]])
						}
						
					}
									
					if (diarioFillNA=="DRAG") {
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
					}				
				}
		} else {
			dp$D_NAfilled<-"REAL"
		}		

		# dp$animo <- round (na_interpolation(dp$animo))
		# dp$ansiedad <- round (na_interpolation(dp$ansiedad))
		# dp$irritabilidad <- round (na_interpolation(dp$irritabilidad))
		# dp$concentracion <- round (na_interpolation(dp$concentracion))
		# dp$tabaco <- round (na_interpolation(dp$tabaco))
		# dp$cafeina <- round (na_interpolation(dp$cafeina))
		# dp$motivacion <- round (na_interpolation(dp$motivacion))
		# dp$calidad_suenho <- round (na_interpolation(dp$calidad_suenho))
		# dp$alcohol_num <- round (na_interpolation(dp$alcohol_num))
		# dp$otras_drogas_num <- round (na_interpolation(dp$otras_drogas_num))
		# dp$observ_num <- round (na_interpolation(dp$observ_num))
		# # valores continuos ( y de paso cambio tiempos a numeric si hace falta)
		# dp$lentitud <- as.numeric (dp$lentitud)
		# dp$lentitud <-  (na_interpolation(dp$lentitud))
		# dp$dormir <-  (na_interpolation(dp$dormir))
		# dp$despertar <-  (na_interpolation(dp$despertar))
		# # Rescato la hora con decimales de la variable... 
		# dp$duracionsuenho <-  (na_interpolation(dp$duracionsuenho))
		# dp$cenitsuenho <-  (na_interpolation(dp$cenitsuenho))
		# dp$horatest <-  (na_interpolation(dp$horatest))  
		 
		 
		dp$lentitudsinoutliers <- rm.outlier(dp$lentitud,fill = TRUE,)
		  
		dp$lentitudescalada <- rescale(dp$lentitudsinoutliers, to = c(1, 5), from = range(dp$lentitudsinoutliers, na.rm = TRUE, finite = TRUE))

		if (primero) {
		dpar<-dp
		primero<-FALSE
		} else {
		dpar<-rbind(dpar,dp)
		}
	}


	diario<-dpar



diario$inicio <- as.POSIXct(diario$inicio, origin = '1970-01-01',tz="UTC")
diario$semana <- (week (diario$fecha)+year (diario$fecha)*52)

diario$diariofill1<-1