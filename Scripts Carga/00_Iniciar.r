library (readxl)
library (imputeTS)
library (outliers)
library (scales)
library (lubridate)
library (data.table)
library (zoo)
library (dplyr)

path_scripts<-"D:/Proyectos/Pavel_MoodDisorders/Scripts Carga/"

#path_datos<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/2018-Pavel LLamocca/Pavel/formularios-20180917T183440Z-001/formularios/"
path_datos<-"D:/Datos/Bip4Cast/reportes/"

binExecute=FALSE # Bin processing. It takes more than 24 hrs.

#Fill NA for continuous data
diarioFillNA<-"INTER" # 3 possible values: "NONE" ,"INTER" (Interpolation) or DRAG
pulseraFillNA<-"INTER" # 3 possible values: "NONE" ,"INTER" (Interpolation) or DRAG

#Fill NA for discrete data
youngFillNA<-"NONE" # 3 possible values: "NONE" ,"INTER" (Interpolation) or DRAG
hrsdFillNA<-"NONE" # 3 possible values: "NONE" ,"INTER" (Interpolation) or DRAG
intervencionesFillNA<-"DRAG" # 3 possible values: "NONE" ,"INTER" (Interpolation) or DRAG

foj_continuos<-TRUE
foj_discretos<-TRUE
foj_diagnostico<-FALSE


foj_cont_and_disc<-TRUE
foj_cont_and_disc_and_dx<-FALSE

scalePulsera<-FALSE

cat("Empiezo a cargar datos\n")

script_continuos<-"01_Carga_continuos.r"
source(paste0(path_scripts,script_continuos))
cat("Termino de cargar continuos\n")

script_discretos<-"02_Carga_discretos.r"
source(paste0(path_scripts,script_discretos))
cat("Termino de cargar discretos\n")

script_diagnostico<-"03_Carga_diagnostico.r"
source(paste0(path_scripts,script_diagnostico))
cat("Termino de cargar diagnostico\n")

#dftotal<-continuos

 if (foj_cont_and_disc)  {
    dftotal<-merge(continuos,discretos,by=c("fecha","id"),all=T)
 } else {
    dftotal<-merge(continuos,discretos,by=c("fecha","id"),all.x=T) 
 }

#print(nrow(unique(dftotal[dftotal$id=="GIUS",c("fecha","Nbouts_day_IN_bts_1_10")])))
#dx1<-unique(dftotal[dftotal$id=="GIUS",c("fecha","Nbouts_day_IN_bts_1_10")])
#stop()

if (foj_cont_and_disc_and_dx) {
  dftotal<-merge(dftotal,diagnosticos,by=c("fecha","id"),all=T)
} else {
  dftotal<-merge(dftotal,diagnosticos,by=c("fecha","id"),all.y=T)
}

#print(nrow(unique(dftotal[dftotal$id=="GIUS",c("fecha","Nbouts_day_IN_bts_1_10")])))
#dx2<-unique(dftotal[dftotal$id=="GIUS",c("fecha","Nbouts_day_IN_bts_1_10")])



# Cambiamos nombres al ingles
script_rename<-"renaming.r"
source(paste0(path_scripts,script_rename))


colnamesdftotal<-colnames(dftotal)

colKeys<-colnamesdftotal[grep("^A", colnamesdftotal)]
colControls<-colnamesdftotal[grep("^B", colnamesdftotal)]
pulseraCols<-colnamesdftotal[grepl('^S_',colnamesdftotal)]

#stop()

# Escalamos variables de la pulsera que sean numericas
if (scalePulsera){
dn1<-dftotal[,pulseraCols]
dn1<-dn1[sapply(dn1, is.numeric)]
dn1<-scale(dn1)
dn2<-dftotal[, ! names(dftotal) %in% colnames(dn1)]
dftotal<-cbind(dn2,dn1)
}
#print(nrow(unique(dftotal[dftotal$A_id=="GIUS",c("A_date","S_Nbouts_day_IN_bts_1_10")])))
#dx3<-unique(dftotal[dftotal$A_id=="GIUS",c("A_date","S_Nbouts_day_IN_bts_1_10")])
#stop()


for (vt in colControls[endsWith(colControls,"NAfilled")]) 
	dftotal[[vt]]<-ifelse(is.na(dftotal[[vt]]),"JOIN",dftotal[[vt]])

cat("Despues de normalizar\n")
#stop()

# Ejecutamos densidad
#script_rename<-"densidad.r"
#source(paste0(path_scripts,script_rename))




#stop()
# Esto puede ser para  generar valores una vez hecho ya todos los merge
# La diferencia de esta interpolation/drag con la que se hace al inicio
# es que la del inicio, hace el interpolation/drag dentro de los ficheros (pulsera, diario, intervenciones)
# y este, hace el interpolation/drag una vez que todo esta mergeado.

diarioFinalFillNA<-"NONE"
pulseraFinalFillNA<-"NONE"
hrsdFinalFillNA<-"NONE"
youngFinalFillNA<-"NONE"
intervencionesFinalFillNA<-"NONE"



if (diarioFinalFillNA!="NONE" | pulseraFinalFillNA!="NONE" | hrsdFinalFillNA!="NONE" | youngFinalFillNA!="NONE" | intervencionesFinalFillNA!="NONE"  ) {
  
  varsToExcludeGeneral<-c(colKeys,colControls)
  varsToExcludePulsera<-c("S_filename","S_weekday","S_sleeponset_ts","S_wakeup_ts","S_guider","S_L5TIME","S_M5TIME","S_L10TIME","S_M10TIME","S_boutdur.in","S_boutdur.lig","S_daytype")
  varsToExcludeHrsd<-c("H_hrsd_dx")
  varsToExcludeYoung<-c("Y_young_dx")
  
  
  diarioCols<-colnamesdftotal[grepl('^D_',colnamesdftotal)]
  pulseraCols<-colnamesdftotal[grepl('^S_',colnamesdftotal)]
  hrsdCols<-colnamesdftotal[grepl('^H_',colnamesdftotal)]
  youngCols<-colnamesdftotal[grepl('^Y_',colnamesdftotal)]
  intervencionesCols<-colnamesdftotal[grepl('^I_',colnamesdftotal)]
  

  cat ("\nEmpiezo filling final")

  primero<-TRUE
	for (x in unique(dftotal$A_id)){
		#for (x in c("JCAT")){
		
		boo_updtateForm<-TRUE
		boo_updatePulsera<-TRUE
		boo_updateHrsd<-TRUE
		boo_updateYoung<-TRUE
		boo_updateIntervencion<-TRUE
	  

		dp <- dftotal[dftotal$A_id==x,]
		if (nrow(dp)<=2) next #revisar

		tiempo.min <- min(dp$A_date)
		tiempo.max <- max(dp$A_date)

		#print(tiempo.min)
		#print(tiempo.max)
		rango.fechas <- as.Date (seq(tiempo.min, tiempo.max, by="day"))
		todas.fechas <- data.frame(list(A_date=rango.fechas))

		# Ordenamos sin dplyr. Ordenamos descendentemente por variable fecha
		dp<-dp[order(dp$A_date,decreasing=TRUE),]

		# Quitamos duplicados sin dplyr
		dp<-dp[!duplicated(dp$A_date),]
		dp <- merge(todas.fechas, dp, all.x=T)
		#dp$B_NAFinalfilled<-ifelse(is.na(dp$B_NAFinalfilled),"H",NA)
		dp$B_NAFinalfilled<-""
		dp$A_id <- x
		
		for (vr in colnamesdftotal[!colnamesdftotal %in% varsToExcludeGeneral])	{			
		
		  #cat("\nAntes de diario")
			if (diarioFinalFillNA != "NONE") {
			  
			  dp$D_start<-as.numeric(dp$D_start)
			  dp$D_slowness_fill_form <- as.numeric (dp$D_slowness_fill_form)
				if (vr %in% diarioCols) {
		
					if (diarioFinalFillNA=="INTER") {
					  
					  if(nrow(dp[!is.na(dp[[vr]]),])>2){
					    
					    if (vr %in% c("D_start","D_animus","D_anxiety","D_irritability","D_concentration","D_tobacco","D_caffeine","D_motivation","D_sleep_quality","D_alcohol_consumption","D_drugs_consumption","D_observation_string_length"))					      
					      dp[[vr]]<-round(na_interpolation(dp[[vr]]))					      
					    		
					    if (vr %in% c("D_slowness_fill_form","D_sleep_time","D_wakeup_time","D_sleep_duration","D_sleep_zenith","D_time_start_form"))
					      dp[[vr]]<-na_interpolation(dp[[vr]])					    												
					    
					  }				  

					}
		
					if (diarioFinalFillNA=="DRAG") {
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
					}	
		      #stop()			
					if (boo_updtateForm) {
						#dp$B_NAFinalfilled<-ifelse(dp$D_formfill1==1,paste0(dp$B_NAFinalfilled,"D"),dp$B_NAFinalfilled)
						dp$B_NAFinalfilled<-ifelse(!is.na(dp$B_formfill1),dp$B_NAFinalfilled,paste0(dp$B_NAFinalfilled,"D"))
						boo_updtateForm<-FALSE
					}

				}				
			  

				
			}
			
		  #cat("\nAntes de pulsera")
			if (pulseraFinalFillNA != "NONE") {
			
				if (vr %in% pulseraCols[!pulseraCols %in% varsToExcludePulsera]) {
			
					if (pulseraFinalFillNA=="INTER") if(nrow(dp[!is.na(dp[[vr]]),])>2) { dp[[vr]]<-na_interpolation(dp[[vr]]) }				
					if (pulseraFinalFillNA=="DRAG") {
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
					}			
				
					if (boo_updatePulsera) {
						#dp$B_NAFinalfilled<-ifelse(dp$S_smartwatchfill1==1,paste0(dp$B_NAFinalfilled,"S"),dp$B_NAFinalfilled)
						dp$B_NAFinalfilled<-ifelse(!is.na(dp$B_smartwatchfill1),dp$B_NAFinalfilled,paste0(dp$B_NAFinalfilled,"S"))
						boo_updatePulsera<-FALSE
					}				
				
				}
				

			}

		  #cat("\nAntes de hrsd")  
			if (hrsdFinalFillNA != "NONE") { 
			
				if (vr %in% hrsdCols[!hrsdCols %in% varsToExcludeHrsd]) {
			
					if (hrsdFinalFillNA=="INTER") if(nrow(dp[!is.na(dp[[vr]]),])>2) { dp[[vr]]<-na_interpolation(dp[[vr]]) }				
					if (hrsdFinalFillNA=="DRAG") {
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
					}			
					
					if (boo_updateHrsd) {
						#dp$B_NAFinalfilled<-ifelse(dp$H_hrsdfill1==1,paste0(dp$B_NAFinalfilled,"H"),dp$B_NAFinalfilled)
						dp$B_NAFinalfilled<-ifelse(!is.na(dp$B_hrsdfill1),dp$B_NAFinalfilled,paste0(dp$B_NAFinalfilled,"H"))
						boo_updateHrsd<-FALSE
					}
				
				}
				
				
			}

		  #cat("\nAntes de young")    
			if (youngFinalFillNA != "NONE") { 
			
				if (vr %in% youngCols[!youngCols %in% varsToExcludeYoung]) {
			
					if (youngFinalFillNA=="INTER") if(nrow(dp[!is.na(dp[[vr]]),])>2) { dp[[vr]]<-na_interpolation(dp[[vr]]) }				
					if (youngFinalFillNA=="DRAG") {
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
					}		

					if (boo_updateYoung) {
						#dp$B_NAFinalfilled<-ifelse(dp$Y_youngfill1==1,paste0(dp$B_NAFinalfilled,"Y"),dp$B_NAFinalfilled)
						dp$B_NAFinalfilled<-ifelse(!is.na(dp$B_youngfill1),dp$B_NAFinalfilled,paste0(dp$B_NAFinalfilled,"Y"))
						boo_updateYoung=FALSE
					}
				
				}
				
				
			}
		  
		  #cat("\nDespues de young")    

		  #cat("\nAntes de intervenciones")  
			if (intervencionesFinalFillNA != "NONE") { 
			
				if (vr %in% intervencionesCols) {
			
					if (intervencionesFinalFillNA=="INTER") if(nrow(dp[!is.na(dp[[vr]]),])>2) { dp[[vr]]<-na_interpolation(dp[[vr]]) }				
					if (intervencionesFinalFillNA=="DRAG") {
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
						dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
					}			
					
					if (boo_updateIntervencion) {
						#dp$B_NAFinalfilled<-ifelse(dp$I_interventionfill1==1,paste0(dp$B_NAFinalfilled,"I"),dp$B_NAFinalfilled)
						dp$B_NAFinalfilled<-ifelse(!is.na(dp$B_interventionfill1),dp$B_NAFinalfilled,paste0(dp$B_NAFinalfilled,"I"))
						boo_updateIntervencion=FALSE
					}
				
				}
				
			}
		  
		  #cat("\ndespues de intervenciones")  

			
		}
		
		#cat("\nAntes de D_slowness_fill_form_no_outliers")  
		if (diarioFinalFillNA=="INTER" | diarioFinalFillNA=="DRAG") {
		  if (!sum(is.na(dp$D_slowness_fill_form_no_outliers))==length(dp$D_slowness_fill_form_no_outliers)) {
			dp$D_slowness_fill_form_no_outliers <- rm.outlier(dp$D_slowness_fill_form,fill = TRUE,)				  
			dp$D_slowness_fill_form_no_outliers_scales <- rescale(dp$D_slowness_fill_form_no_outliers, to = c(1, 5), from = range(dp$D_slowness_fill_form_no_outliers, na.rm = TRUE, finite = TRUE))
		  }
		}
		
		#cat("\nAntes del rbind")  
		if (primero) {
		dpar<-dp
		primero<-FALSE
		} else {
		dpar<-rbind(dpar,dp)
		}


	}
	
	
	dftotal$D_start <- as.POSIXct(dftotal$D_start, origin = '1970-01-01',tz="UTC")
	dftotal$D_week <- (week (dftotal$A_date)+year (dftotal$A_date)*52)
	
	dftotal<-dpar
	
}


colnamesdftotal <- sort(colnamesdftotal,decreasing =FALSE)
dftotal<-dftotal[,colnamesdftotal]



save(dftotal,file="dnormalizado7.RData")

#script_graficos<-"04_graficos.r"
#source(paste(path_scripts,script_graficos,sep=""))



#setwd("C:\\Users\\pavel\\Google Drive\\Bip4Cast-Doctorado-2018\\2018-Pavel LLamocca\\Pavel\\drive-download-20180902T092205Z-001\\Nuevos_Scripts\\graficos\\")
#
#
#for (x in unique(total_datos$id)){
#
#dp <- total_datos[total_datos$id==x,]
#if (nrow(dp)<=20) next #revisar
#dp<-subset(dp,,c(seq(1,22)))
#dp <- dp [,!colnames(dp)=='fecha']
#dp <- dp [,!colnames(dp)=='inicio']
#dp <- dp [,!colnames(dp)=='id']
#dp <- dp [,!colnames(dp)=='lentitud']
#dp <- dp [,!colnames(dp)=='lentitudsinoutliers']
#dp <- dp [,!colnames(dp)=='observ_num']


#nm_gra<-paste0(x,"_biplot_pca.jpg")
#jpeg(nm_gra,width = 8, height = 8, units = 'in', res = 800)
#
#biplot(princomp(dp),pc.biplot=TRUE,cex=0.5,expand=0.8)
#dev.off()
#
#}

#print(unique(total_datos$id))
#id<-readline("Ingrese un id\n")
#dp <- total_datos[total_datos$id==id,]


