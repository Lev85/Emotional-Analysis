rm(list = ls())
load("dnormalizado_NoInterpoatioin.RData")
dnm<-dftotal
var_diagnosis<-"Z_dx_numeric"
var_id<-"A_id"
namevars<-colnames(dnm)
vartypes<-c("D_","S_")
vrs<-as.character()
for (vt in vartypes) vrs<-c(vrs,namevars[grepl(paste0("^",vt),namevars)])

# Añadimos la variable que queremos clasificar
vrs<-c(vrs,var_diagnosis)
vrs<-unique(vrs)


# Solo variables numericas
vrs_num <- names(which(sapply(dnm[,vrs],is.numeric)))

ptss<-c("CASGA","DOSI","ECSA","GIUS","GOMA","LABA","MAPI","PRFE","RIVI","SAPE","SEOR","ALPA","DOGE","BEPR","PELA","ROCU","MARA")
#ptss<-c("CASGA")

df_cf<-data.frame()
df_tot<-data.frame()
#df_cf<-cbind(df_cf,as.data.frame(c("SD","MEAN","COEF")))

for (pts in  ptss) {
  
  v_vsd<-as.numeric()
  v_vmn<-as.numeric()
  v_vcf<-as.numeric()
  
  for (v in vrs_num) {
    
    vsd<-round(sd(dnm[dnm[[var_id]]==pts,v],na.rm = T),3)
    vmn<-round(abs(mean(dnm[dnm[[var_id]]==pts,v],na.rm = T)),3)
    vcf<-round(vsd/vmn,3)
    
    v_vsd<-c(v_vsd,vsd)
    v_vmn<-c(v_vmn,vmn)
    v_vcf<-c(v_vcf,vcf)
    
    #cls<-c(vsd,vnm,vcf)
    
  }
  
  #df_cf<-cbind(df_cf,cls)
  
  
  #df_cf<-rbind(df_cf,c(pts,"DSV",v_vsd))
  #df_cf<-rbind(df_cf,c(pts,"MED",v_vmn))
  #df_cf<-rbind(df_cf,c(pts,"COE",v_vcf))
  
  df_cf<-rbind(c(pts,"DSV",v_vsd),c(pts,"MED",v_vmn),c(pts,"COE",v_vcf))
  
  df_tot<-rbind(df_tot,df_cf)

}



colnames(df_tot)<-c("paciente","metrica",vrs_num)
