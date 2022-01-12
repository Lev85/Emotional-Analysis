###########
# Generación del data_frame estandarizado/normalizado young
###########

datos<-"Young v2.xlsx"
young <- as.data.frame (read_excel (paste(path_datos,datos,sep="")))


################
## PAVEL M11. Aplicamos algunas cosas de antes
################
##### los limpio
##young <- young [,!colnames(young)=='Hora de inicio']
##young <- young [,!colnames(young)=='Hora de finalización']
##young <- young [,!colnames(young)=='Correo electrónico']
##young <- young [,!colnames(young)=='Nombre']
##young <- young [,!colnames(young)=='Pérdida de peso']
##young <- young [,!colnames(young)=='Hipocondria']
##young <- young [,!colnames(young)=='Conciencia de enfermedad']
##colnames(young)[7]<-"curso"
##colnames(young)[8]<-"contenido"
##colnames(young)[11]<-"conciencia"
##colnames(young)[12]<-"id"
##young <- young [,!colnames(young)=='Recoge datos']
##young$Fecha <- date (young$Fecha)
##colnames(young)[13]<-"fecha"

young<-subset(young,,-c(1,2,3,4,5,6,7,20))
colnames(young)[1]<-"euforia"
colnames(young)[2]<-"hiperactividad"
colnames(young)[3]<-"impulso_sexual"

colnames(young)[4]<-"Suenho"

colnames(young)[6]<-"expresion_verbal"
colnames(young)[7]<-"curso"
colnames(young)[8]<-"contenido"
colnames(young)[11]<-"conciencia.young"
colnames(young)[12]<-"id"
colnames(young)[13]<-"fecha"
young$fecha <- as.Date (young$fecha)

# ESTANDARIZAR VARIABLES (PAVEL)
young$euforia <- as.numeric(young$euforia)
young$hiperactividad <- as.numeric(young$hiperactividad)
young$impulso_sexual <- as.numeric(young$impulso_sexual)
young$Suenho <- as.numeric(young$Suenho)
young$Irritabilidad <- as.numeric(young$Irritabilidad)
young$expresion_verbal <- as.numeric(young$expresion_verbal)
young$curso <- as.numeric(young$curso)
young$contenido <- as.numeric(young$contenido)
young$conciencia.young <- as.numeric(young$conciencia.young)
young$Agresividad <- as.numeric(young$Agresividad)
young$Apariencia <- as.numeric(young$Apariencia)


young$puntaje.young<-rowSums(young[,names(which(sapply(young,is.numeric)))])

young$Y_dx<-ifelse(young$puntaje.young %in% c(0:6),"Eutimia",
							ifelse(young$puntaje.young %in% c(7:20),"Mania Ligera",
							   ifelse(young$puntaje.young > 20,"Mania Severa","NA")))


young$Y_NAfilled<-"1"

if (youngFillNA!="NONE") {

	youngColNames <- colnames(young)

	primero<-TRUE
	for (x in unique(young$id)){
		#for (x in c("GOGA")){

		dp <- young[young$id==x,]
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
		dp$Y_NAfilled<-ifelse(is.na(dp$Y_NAfilled),"Y","REAL")
		dp$id <- x
		
		for (vr in youngColNames[!youngColNames %in% c("id","fecha","Y_NAfilled","Y_dx")])	{	
			if (youngFillNA=="INTER") dp[[vr]]<-round(na_interpolation(dp[[vr]]))
			if (youngFillNA=="DRAG") {
				dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
				dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
			}
		}

		if (primero) {
		dpar<-dp
		primero<-FALSE
		} else {
		dpar<-rbind(dpar,dp)
		}
	}
	
	young<-dpar
	
} else {
	young$Y_NAfilled<-"REAL"
}

young$youngfill1<-1