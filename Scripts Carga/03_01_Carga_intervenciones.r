###########
# Generación del data_frame estandarizado/normalizado intervenciones
###########

datos<-"Intervenciones v2.xlsx"
intervenciones <- as.data.frame (read_excel (paste(path_datos,datos,sep="")))

if (datos == "Intervenciones v2.xlsx") { vs_int="2"
} else { vs_int="1"}

#########
## PAVEL M8
## Se puede quitar columnas referenciado el número de columnas
## Y poner nombre a las demás con una sola sentencia
######
###quito cosas inútiles y cambiando nombre de variables
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Hora de finalización']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Correo electrónico']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Nombre']
##colnames(intervenciones)[1]<-"fecha"
##colnames(intervenciones)[2]<-"id"
##colnames(intervenciones)[3]<-"alivio"
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Id del paceinte2']
##colnames(intervenciones)[4]<-"terapeuta"
##intervenciones <- intervenciones [,!colnames(intervenciones)=='EEAG']
##colnames(intervenciones)[5]<-"programada"
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna1']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna2']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna3']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='EEAG\r\n100 – 91 Actividad satisfactoria en una amplia gama de actividades, nunca parece superado por los\r\n problemas de su vida, es valorado por los demás a causa de sus abundantes cualidades\r\n positi...']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Intervenciones']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Columna4']
##colnames(intervenciones)[6]<-"dx"
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención1']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención2']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención3']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Tipo de intervención4']
##intervenciones <- intervenciones [,!colnames(intervenciones)=='Observaciones']
##colnames(intervenciones)[7]<-"acciones"

intervenciones<-subset(intervenciones,,-c(2,3,4,7,9,11,13,14,12,15,16,19,20,21,22,24))

colnames(intervenciones)<-c("fecha","id","alivio","terapeuta","prograda","I_dx_largo","acciones","Tipo de intervención5")
if (vs_int==2) {
  colnames(intervenciones)<-c("fecha","id","alivio","terapeuta","prograda","I_dx_largo","acciones","Tipo de intervención5","asiste_cita")
  intervenciones$asiste_cita<-ifelse(intervenciones$asiste_cita=="Si",1,
                ifelse(intervenciones$asiste_cita=="No",2,NA)
  )
  intervenciones<-intervenciones[!is.na(intervenciones$id) & !is.na(intervenciones$fecha),]
}

# ESTANDARIZAR VARIABLE alivio (PAVEL)
intervenciones$alivio<-factor(intervenciones$alivio,levels=c("Muchísimo mejor","Mucho mejor","Un poco mejor","Ningun cambio","Un poco peor","Mucho peor","Muchísimo peor"))
intervenciones$alivio<-as.numeric(intervenciones$alivio)

# ESTANDARIZAR VARIABLE prograda (PAVEL)
intervenciones$prograda<-ifelse(substr(intervenciones$prograda,1,1)=="S" | substr(intervenciones$prograda,1,1)=="1" ,1 , 0)
#intervenciones$prograda<-as.nuneric(intervenciones$prograda)

################
## FIN PAVEL M8
################

################
## PAVEL M9. Convertimos a fecha sin usar lubridate
################
#intervenciones$fecha<- date (intervenciones$fecha)
intervenciones$fecha<-as.Date(intervenciones$fecha)
################
## FIN PAVEL M9
################


#Pongo usuarios en mayusculas
intervenciones$id<- toupper (intervenciones$id) ##pasamos usuarios a mayusculas


# transformo diagnóstico a 0 eutimia, 1 manía, -1 depresión, X fase mixta (2) con los dos últimos números
# transformo textos de escalas Likert -3/+3 tomando solo los dos primeros caracteres o el primero
#intervenciones$dx <- as.factor((substr (intervenciones$dx,nchar (intervenciones$dx)-1, nchar (intervenciones$dx))))
# ESTANDARIZAR VARIABLE dx (PAVEL)

intervenciones<-intervenciones[!is.na(intervenciones$I_dx_largo),]

intervenciones$I_dx_char <- 
ifelse(intervenciones$I_dx_largo %in% c("F31.0 Hipomania  1","F31.0 Hipomania 1","F31.2 Mania con psicosis  1","F31.1 Mania  1",
"Sintomas psicoticos","sintomatologia ansiosa","Hipomania en remision parcial","alarmas hipomaniacas"),
"Mania",
ifelse(intervenciones$I_dx_largo %in% c("F31.3 Depresion leve o moderada  -1","F31.4 Depresion grave. -1","F31.3 Depresion leve o moderada. -1",
"Bajo estado animico","F31.4 Depresion grave  -1","F31.5 Depresion grave con psicosis  -1","Apatia intensa","Sintomatologia depresiva",
"sintomatologia depresiva","sintomatologia ansioso-depresiva","Disforia","sintomas depresivos","animo disforico"),
"Depression",
ifelse(intervenciones$I_dx_largo %in% c("Eutimia  0 ","Eutimia  0"," Eutimia  0"),
"Euthymia",
ifelse(intervenciones$I_dx_largo %in% c("F31.6 Sintomas mixtos  X","Mixto X"),
"Mixed","Not informed"
))))

intervenciones$I_dx<-
ifelse(intervenciones$I_dx_char=="Mania",1,
ifelse(intervenciones$I_dx_char=="Depression",-1,
ifelse(intervenciones$I_dx_char=="Euthymia",0,
ifelse(intervenciones$I_dx_char=="Mixed",2,3
))))

#stop()
#intervenciones$I_dx <- ifelse(intervenciones$I_dx=="Bajo estado animico","Eutimia 0",intervenciones$I_dx)
#intervenciones$I_dx <- substr (intervenciones$I_dx,nchar (intervenciones$I_dx)-1, nchar (intervenciones$I_dx))
#intervenciones$I_dx <- ifelse (intervenciones$I_dx==" X",2,ifelse(intervenciones$I_dx %in% c(" 1","-1"," 0"),trimws(intervenciones$I_dx),3))
#intervenciones$I_dx <- as.numeric(intervenciones$I_dx)

################
## PAVEL M9. Creamos variable "cambiosignificativo" sin usar dplyr
################
###grepl ("tratamiento significativo;", intervenciones$acciones)
###duplicamos acciones
##intervenciones$cambiosignificativo<-intervenciones$acciones
##intervenciones<- mutate_at(intervenciones, 'cambiosignificativo', funs(grepl ("tratamiento significativo;", intervenciones$acciones)))
intervenciones$cambiosignificativo<-grepl("tratamiento significativo;", intervenciones$acciones)
# ESTANDARIZAR VARIABLE cambiosignificativo (PAVEL)
intervenciones$cambiosignificativo<-as.numeric(intervenciones$cambiosignificativo)

################
## FIN PAVEL M9
################

# de momento me deshago del resto de acciones
intervenciones <- intervenciones [,!colnames(intervenciones)=='acciones']
colnames(intervenciones)[7]<-"tipo"
intervenciones$tipo<-ifelse(intervenciones$tipo %in% c("Presencial","1. Presencial"),"Presencial",
						ifelse(intervenciones$tipo %in% c("Online","2. Online"),"Online","Teléfono"))
intervenciones$tipo<-factor(intervenciones$tipo,levels=c("Presencial","Online","Teléfono","Correo"))
# ESTANDARIZAR VARIABLE tipo (PAVEL)
intervenciones$tipo<-as.numeric(intervenciones$tipo)

################
## PAVEL M10. Cambiamos algunas variables sin necesidad de un loop
################
#### SI EL DIAGNOSTICO NO ES DU, lo PASO A NA
###  SI Cambio significativo es NA lo paso a FALSE 
##for (x in 1:nrow (intervenciones)) {
##  a <- intervenciones$terapeuta [x]
##  if  (a != "DU")      intervenciones$dx [x] <- NA
##  if  (is.na (intervenciones$cambiosignificativo [x]))      intervenciones$cambiosignificativo [x] <-  FALSE
##}


################
# Intervenciones
################

##PREVALENCIA DEL MEDICO SI HAY DOS OBSERVACIONES EL MISMO DIA.
#borrar los no DU que tengan el mismo día que un DU
#ordeno

# Primero separamos las que son DU (int_DU) y las que no son DU (int_NODU), 
# y porsiacaso a cada grupo le quitamos las fechas repetidas
int_DU<-intervenciones[intervenciones$terapeuta=="DU",]
# creamos variable auxiliar
int_DU$nv<-paste(int_DU$fecha,int_DU$id,sep="")
int_DU<-int_DU[!duplicated(int_DU$nv),]  


int_NODU<-intervenciones[intervenciones$terapeuta!="DU",]
# creamos variable auxiliar
int_NODU$nv<-paste(int_NODU$fecha,int_NODU$id,sep="")
int_NODU<-int_NODU[!duplicated(int_NODU$nv),]


# En las que no son DU (int_NODU),
# nos quedamos solo con aquellas cuya fecha no este en las de DU (int_DU)
int_NODU<-int_NODU[ !(int_NODU$nv %in% int_DU$nv),]

# Unimos ambos grupos
intervenciones<-rbind(int_DU,int_NODU)

# Quitamos variable auxiliar
intervenciones<-subset(intervenciones,,-c(nv))

# ESTANDARIZAR VARIABLE terapeuta (PAVEL)
intervenciones$terapeuta<-ifelse(intervenciones$terapeuta=="DU",1,0)


# Creo que esto es lo anterior
#intervenciones$dx[intervenciones$terapeuta != "DU"] <- NA
#intervenciones$cambiosignificativo[is.na(intervenciones$cambiosignificativo)] <- FALSE


intervenciones$I_NAfilled<-"1"


if (intervencionesFillNA!="NONE") {

	intervencionesColNames <- colnames(intervenciones)

	primero<-TRUE
	for (x in unique(intervenciones$id)){
		#for (x in c("VELU")){

		dp <- intervenciones[intervenciones$id==x,]

		if (nrow(dp)<=2) next #revisar

		tiempo.min <- min(dp$fecha)
		tiempo.max <- max(dp$fecha)

		#print(tiempo.min)
		#print(tiempo.max)
		rango.fechas <- as.Date (seq(tiempo.min, tiempo.max, by="day"))
		todas.fechas <- data.frame(list(fecha=rango.fechas))

		# Ordenamos sin dplyr. Ordenamos descendentemente por variable fecha
		dp<-dp[order(dp$fecha,decreasing=TRUE),]

		# Quitamos duplicados sin dplyr
		dp<-dp[!duplicated(dp$fecha),]

		dp <- merge(todas.fechas, dp, all.x=T)
		dp$I_NAfilled<-ifelse(is.na(dp$I_NAfilled),"I","REAL")
		dp$id <- x
		
		for (vr in intervencionesColNames[!intervencionesColNames %in% c("id","fecha","I_NAfilled","asiste_cita","I_dx_largo")])	{	
		
			if (vr=="I_dx" | vr=="I_dx_char") { # Siempre arrastramos el diagnostico
					dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
					dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 
			} else {
				if (intervencionesFillNA=="INTER") dp[[vr]]<-round(na_interpolation(dp[[vr]]))
				if (intervencionesFillNA=="DRAG") { 
					dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
					dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 
				}
			}
		}
		if (primero) {
		dpar<-dp
		primero<-FALSE
		} else {
		dpar<-rbind(dpar,dp)
		}
	}
	
	intervenciones<-dpar
	
} else {
	intervenciones$I_NAfilled<-"REAL"
}

intervenciones$intervencionesfill1<-1