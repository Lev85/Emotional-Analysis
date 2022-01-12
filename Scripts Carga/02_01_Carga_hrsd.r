###########
# Generación del data_frame estandarizado/normalizado hrsd
###########

datos<-"HRSD v21.xlsx"
hrsd <- as.data.frame (read_excel (paste(path_datos,datos,sep="")))

hrsd<-subset(hrsd,,-c(1,2,3,4,7))

colnames(hrsd)[1]<-"id"
colnames(hrsd)[3]<-"depresion"
colnames(hrsd)[4]<-"culpa"
colnames(hrsd)[6]<-"iprecoz"
colnames(hrsd)[7]<-"imedio"
colnames(hrsd)[8]<-"itardio"
colnames(hrsd)[9]<-"actividad"
colnames(hrsd)[10]<-"inhibicion"
colnames(hrsd)[11]<-"agitacion"
colnames(hrsd)[12]<-"an_psiq"
colnames(hrsd)[13]<-"an_som"
colnames(hrsd)[14]<-"gastro"
colnames(hrsd)[15]<-"somatic"
colnames(hrsd)[16]<-"sexo"
colnames(hrsd)[17]<-"conciencia.hrsd"
colnames(hrsd)[18]<-"hipocondria"
colnames(hrsd)[19]<-"peso"
hrsd$fecha = as.Date (hrsd$fecha)



################
## FIN PAVEL M11
################


# ESTANDARIZAR VARIABLES (PAVEL)
hrsd$depresion <- as.numeric(hrsd$depresion)
hrsd$culpa <- as.numeric(hrsd$culpa)
hrsd$Suicidio <- as.numeric(hrsd$Suicidio)
hrsd$iprecoz <- as.numeric(hrsd$iprecoz)
hrsd$imedio <- as.numeric(hrsd$imedio)
hrsd$itardio <- as.numeric(hrsd$itardio)
hrsd$actividad <- as.numeric(hrsd$actividad)
hrsd$inhibicion <- as.numeric(hrsd$inhibicion)
hrsd$agitacion <- as.numeric(hrsd$agitacion)
hrsd$an_psiq <- as.numeric(hrsd$an_psiq)
hrsd$an_som <- as.numeric(hrsd$an_som)
hrsd$gastro <- as.numeric(hrsd$gastro)
hrsd$somatic <- as.numeric(hrsd$somatic)
hrsd$sexo <- as.numeric(hrsd$sexo)
hrsd$conciencia.hrsd <- as.numeric(hrsd$conciencia.hrsd)
hrsd$hipocondria <- as.numeric(hrsd$hipocondria)
hrsd$peso <- as.numeric(hrsd$peso)

hrsd$puntaje.hrsd<-rowSums(hrsd[,names(which(sapply(hrsd,is.numeric)))])

hrsd$H_dx<-ifelse(hrsd$puntaje.hrsd %in% c(0:7),"Eutimia",
							ifelse(hrsd$puntaje.hrsd %in% c(8:13),"Depresion Ligera",
							   ifelse(hrsd$puntaje.hrsd %in% c(14:18),"Depresion Moderada",
							       ifelse(hrsd$puntaje.hrsd %in% c(19:22),"Depresion Severa",
									  ifelse(hrsd$puntaje.hrsd > 23,"Muy severa","NA")))))


hrsd$H_NAfilled<-"1"

if (hrsdFillNA!="NONE") {

	hrsdColNames <- colnames(hrsd)

	primero<-TRUE
	for (x in unique(hrsd$id)){
		#for (x in c("GOGA")){

		dp <- hrsd[hrsd$id==x,]
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
		dp$H_NAfilled<-ifelse(is.na(dp$H_NAfilled),"H","REAL")
		dp$id <- x
		
		for (vr in hrsdColNames[!hrsdColNames %in% c("id","fecha","H_NAfilled","H_dx")])	{	
			if (hrsdFillNA=="INTER") dp[[vr]]<-round(na_interpolation(dp[[vr]]))
			if (hrsdFillNA=="DRAG") {
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
	
	hrsd<-dpar
	
} else {
	hrsd$H_NAfilled<-"REAL"
}



hrsd$hrsdfill1<-1