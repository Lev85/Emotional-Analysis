paciente="CASGA"
centros=3
umbral_correlacion=0.5 # valor absoluto
numvariables=5

library(dplyr)

load("dnormalizado4.RData")
dnm<-dftotal


dnm<-filter(dnm, A_id==paciente)

vartypes<-c("D","S","Y","H","I")
vartypes<-c("D")


vrs<-colnames(dnm)
vrs<-vrs[!vrs %in% c("D_start")]

vrsel<-as.character()
for (vt in vartypes) vrsel<-c(vrsel,vrs[grepl(paste0("^",vt),vrs)])

library(gtools)

cnames<-combinations(length(vrsel),numvariables,vrsel)


for ( i in c(1:nrow(cnames)) ) {
  dnmtemp<-dnm[,c("A_date",cnames[i,],"E_dx_numeric")]
  dnmtemp<-na.omit(dnmtemp)
  #datosscalados<-scale(dnmtemp[,c(2:(numvariables+1))],scale=FALSE)
  datosscalados<-dnmtemp[,c(2:(numvariables+1))]
  datosnormalizados<-data.frame(dnmtemp[,1],datosscalados,dnmtemp[,numvariables+2])
  names(datosnormalizados)[1]<-"A_date"
  names(datosnormalizados)[numvariables+2]<-"E_dx_numeric"
  
  datos_kmeans<-kmeans(datosnormalizados[,c(2:numvariables+1)], centers = 3)
  datos_finales<-data.frame(datos_kmeans$cluster,datosnormalizados)
  datos_finales$A_date<-weekdays(datos_finales$A_date)
  
  crl<-cor(datos_finales$datos_kmeans.cluster,datos_finales$E_dx_numeric)
  
  if (abs(crl) > umbral_correlacion ) cat(crl, cnames[i,],"\n")
}



