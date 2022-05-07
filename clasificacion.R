rm(list = ls())

library(dplyr)
library(DataExplorer)
library(class)
source('C:/Users/pavel/Desktop/PHD Documentos/Pavel_MoodDisorders/ml_algorithms.R')
load("dnormalizado4.RData")
dnm<-dftotal
var_diagnosis<-"Z_dx_numeric"
var_id<-"A_id"

boo_algunos<-F

boo_soloReales<-F
pts<-"MAPI"
#Solo observaciones Reales o interpoladas del Diario y que tegan
#también información REAL de Pulsera

mlalgs<-c("svm","dt","rf","reglogmn","knn","reglogbay")
#mlalgs<-c("reglogbay")
ptss<-c("CASGA","DOSI","ECSA","GIUS","GOMA","LABA","MAPI","PRFE","RIVI","SAPE","SEOR")


for (pts in  ptss) {
  cat(paste0(pts,"\n"))
  
  if (boo_soloReales) {
    
    dnm<-dftotal[dftotal[[var_id]]==pts &
                   dftotal$B_D_NAfilled %in% c("REAL") & 
                   dftotal$B_S_NAfilled %in% c("REAL"),]
    
    
  } else {
     dnm<-dftotal[dftotal[[var_id]]==pts &
                    dftotal$B_D_NAfilled %in% c("REAL","D") & 
                    dftotal$B_S_NAfilled %in% c("REAL","S"),]  
    
    # dnm<-dftotal[
    #                dftotal$B_D_NAfilled %in% c("REAL","D") & 
    #                dftotal$B_S_NAfilled %in% c("REAL","S"),]  
    
    
  }
  
  
  
  # En caso de quedarnos solo con algunos que hayan tenido mania
  # y que hayan estado irritados.
  if (boo_algunos) {
    usrvalid<-as.character()
    
    for (vt in unique(dnm$A_id)){
      dfx<-dnm[dnm$A_id==vt,]
      if ((length(dfx[dfx[[var_diagnosis]]==3,c("A_id")])>0 | 
           length(dfx[dfx[[var_diagnosis]]==4,c("A_id")])>0 ) &
          length(dfx[dfx$I_dx_short %in% c("Manic","Mixed"),c("A_id")])>0
      ){
        usrvalid<-c(usrvalid,vt)
      }
      
    }
    
    dnm<-dnm[dnm$A_id %in% usrvalid,]
    
  }
  

  # Guardamos el nombre de todas las variables del Diario y Pulsera
  namevars<-colnames(dnm)
  vartypes<-c("D_","S_")
  vrs<-as.character()
  for (vt in vartypes) vrs<-c(vrs,namevars[grepl(paste0("^",vt),namevars)])
  
  # Quitamos algunas variables que dan problemas
  colsToExclude<-c("S_boutcriter.mvpa","S_acc_available","S_boutcriter.in","S_bout.metric","S_boutcriter.lig","S_boutdur.mvpa","S_sleeplog_used","S_night_number","S_window_number","S_week")
  vrs<-vrs[!vrs %in% colsToExclude]
  
  # Keeping only numeric variables
  vrs<-names(which(sapply(dnm,is.numeric)))
  
  # Añadimos la variable que queremos clasificar
  vrs<-c(vrs,var_diagnosis)
  vrs<-unique(vrs)
  
  # Seleccionamos del dataset todas las variables del diario, pulsera y la 
  # variable a clasificar
  dnm<-dnm[,vrs]  
  
  # Quitamos variables que tengan solo un único valor (o ún unico valor más NA)  
  colduplicadas<-as.character()
  for (i in colnames(dnm))
    if (length(unique(na.omit(dnm[[i]])))==1) colduplicadas<-c(colduplicadas,i)
  
  dnm<-dnm[, !names(dnm) %in% c(colduplicadas)]
  
  

  #######
  # La siguiente parte es para "partir" en datos de entrenamiento y test
  # Se hace que sean  más equitativas posibles
  # 1. Se escoge el 70% de todos los 0
  # 2. Se escoge el 70% de todos los 1
  # y todos estos se van como datos de entrenamiento datos.entreno
  # Lo demás como datos de test datos.test
  ####
  data.ind<-dnm
  rownames(data.ind) <- NULL
  
  data.ind[[var_diagnosis]]<-as.factor(data.ind[[var_diagnosis]])
  
  in_ent<-as.numeric()
  in_tst<-as.numeric()
  
  for (pt2 in unique(data.ind[[var_diagnosis]])){
    
    dfdg<-data.ind[data.ind[[var_diagnosis]]==pt2,]
    ind.class<-as.numeric(rownames(dfdg))
    tt.class<-length(ind.class)
    
    if (tt.class>2){
      te.class <- round(tt.class*0.7)
      ind.ent.class <- sample(ind.class , size=te.class) 
      in_ent<-c(in_ent,ind.ent.class)
      in_tst<-c(in_tst,ind.class[!ind.class %in% ind.ent.class])
    }
    else if (tt.class==2) {
      in_ent<-c(in_ent,ind.class)
      in_tst<-c(in_tst,ind.class[1])	
    }
    else if (tt.class==1) {
      in_ent<-c(in_ent,rep(ind.class,2))
      in_tst<-c(in_tst,ind.class)	
    }
    
  }
  
  
  
  
  datos.entreno.t <- data.ind[in_ent,colnames(dnm)]
  datos.test.t <- data.ind[in_tst,colnames(dnm)]  
  
  vrs<-c("S_wakeup","S_dur_day_min","S_dur_spt_min")
  datos.entreno<-datos.entreno.t[,c(vrs,var_diagnosis)]
  datos.test<-datos.test.t[,c(vrs,var_diagnosis)]
  
  cat("SOLO PULSERA")
  
  
  for (alg in mlalgs ) {

    fun_name<-paste0("fun_",tolower(alg))
    get(fun_name)(data.train=datos.entreno,data.test=datos.test,var_diagnosis,pt=pts)

  }
  vrs<-c("S_wakeup","S_dur_day_min","S_dur_spt_min","S_sleep_efficiency","D_irritability","D_motivation")
  datos.entreno<-datos.entreno.t[,c(vrs,var_diagnosis)]
  datos.test<-datos.test.t[,c(vrs,var_diagnosis)]
  
  cat("PULSERA + DIARIO")
  
  
  for (alg in mlalgs ) {
    
    fun_name<-paste0("fun_",tolower(alg))
    get(fun_name)(data.train=datos.entreno,data.test=datos.test,var_diagnosis,pt=pts)
    
  }  
  
}