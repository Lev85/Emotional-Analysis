
ind=1
densidad<-dftotal[,c(colKeys,colControls,"I_dx_short")]

dens1<-densidad

dens1[is.na(dens1)]<-""

if (ind==1){ #SOLO REAL

dens1$B_D_NAfilled<-ifelse(dens1$B_D_NAfilled=="D","",
                           ifelse(dens1$B_D_NAfilled=="REAL","D",""))                         
                           
dens1$B_S_NAfilled<-ifelse(dens1$B_S_NAfilled=="S","",
                           ifelse(dens1$B_S_NAfilled=="REAL","S",""))                         

dens1$B_H_NAfilled<-ifelse(dens1$B_H_NAfilled=="H","",
                           ifelse(dens1$B_H_NAfilled=="REAL","H",""))                         

dens1$B_Y_NAfilled<-ifelse(dens1$B_Y_NAfilled=="Y","",
                           ifelse(dens1$B_Y_NAfilled=="REAL","Y",""))                         

#dens1$B_I_NAfilled<-ifelse(dens1$B_I_NAfilled=="I","",
#                           ifelse(dens1$B_I_NAfilled=="REAL","I",""))                         
} else { # FILLED (REAL + FICTICIO)
  
dens1$B_D_NAfilled<-ifelse(dens1$B_D_NAfilled!="","D","")
dens1$B_S_NAfilled<-ifelse(dens1$B_S_NAfilled!="","S","")
dens1$B_H_NAfilled<-ifelse(dens1$B_H_NAfilled!="","H","")
dens1$B_Y_NAfilled<-ifelse(dens1$B_Y_NAfilled!="","Y","")
#dens1$B_I_NAfilled<-ifelse(dens1$B_I_NAfilled!="","I","")
  
  
}


#dens1$X1<-paste0(dens1$B_D_NAfilled,dens1$B_S_NAfilled,dens1$B_H_NAfilled,dens1$B_Y_NAfilled,dens1$B_I_NAfilled)
dens1$X1<-paste0(dens1$B_D_NAfilled,dens1$B_S_NAfilled,dens1$B_H_NAfilled,dens1$B_Y_NAfilled)
  
  
densagg<-aggregate(dens1,by=list(dens1$A_id,dens1$I_dx_short,dens1$X1),FUN=length)

densagg<-densagg[,c(1,2,3,12)]

colnames(densagg)<-c("id","dx","fuente","observaciones")

densagg$formulario<-ifelse(grepl("D", densagg$fuente , fixed=TRUE),"FORMULARIO","")
densagg$pulsera<-ifelse(grepl("S", densagg$fuente , fixed=TRUE),"SMARTWATCH","")
densagg$hrsd<-ifelse(grepl("H", densagg$fuente , fixed=TRUE),"HRSD","")
densagg$young<-ifelse(grepl("Y", densagg$fuente , fixed=TRUE),"YOUNG","")
#densagg$intervenciones<-ifelse(grepl("I", densagg$fuente , fixed=TRUE),"INTERVENCIONES","")

densagg<-densagg[order(densagg$id,densagg$dx,densagg$fuente),]

densagg<-densagg[densagg$fuente!="",]

#densagg<-densagg[,c("id","dx","formulario","pulsera","hrsd","young","intervenciones","observaciones")]
densagg<-densagg[,c("id","dx","formulario","pulsera","hrsd","young","observaciones")]

write.csv(densagg,"densidad.csv")