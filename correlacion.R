version_corr<-3
boo_reales<-FALSE

if (version_corr==2) {
  load("dnormalizado3.RData")
  ind_var<-"E_final_diagnosis_numeric"
  ds<-"dnormalizado"
  var_id<-"D_id"
  var_date<-"D_date"
  archivo<-"fetselv3.csv"
  var1ejemplo<-"D_caffeine"
} else if (version_corr==1) {
  load("dnormalizado.RData")
  ds<-"dnormalizado"
  ind_var<-"final_diagnosis_numeric"
  var_id<-"id"
  var_date<-"date"
  archivo<-"fetselv2.csv"
  var1ejemplo<-"caffeine"
} else { # version_corr==3
  load("dnormalizado4.RData")
  ds<-"dftotal"
  ind_var<-"Z_dx_numeric"
  var_id<-"A_id"
  var_date<-"A_date"
  archivo<-"fetselv4.csv"
  var1ejemplo<-"D_caffeine"
}

#assign(ds,d) 
d<-get(ds)
#stop()

#d<-dnormalizado

if (boo_reales){
  d<-d[d$B_D_NAfilled %in% ("REAL") &
         d$B_S_NAfilled %in% ("REAL") , ]
}

# Posibles valores: cor, chi , anova , all
algoritmos<-c("cor","chi","anova")


d$mes<-as.numeric(format(d[[var_date]], "%y%m"))



datos<-d[, c(var_id,names(which(sapply(d,is.numeric)))) ]


nms<-colnames(datos)

if (version_corr==2) {

  vardpnd<-nms[2]
  vardiario<-nms[startsWith(nms,"D_")]
  vardiario<-vardiario[ ! vardiario %in% c(var_id)]
  varpulsera<-nms[startsWith(nms,"S_")]
  varinter<-nms[startsWith(nms,"I_")]
  varyoung<-nms[startsWith(nms,"Y_")]
  varhrsd<-nms[startsWith(nms,"H_")]
  
  datos$depression<-ifelse(datos[[ind_var]]==-1,1,0)
  datos$euthymia<-ifelse(datos[[ind_var]]==0,1,0)
  datos$hypomania<-ifelse(datos[[ind_var]]==1,1,0)
  datos$mixed<-ifelse(datos[[ind_var]]==2,1,0)
  datos$hospitalized<-ifelse(datos[[ind_var]]==3,1,0)
  
  tabla_corr<-data.frame(
    id=character(), 
    origen.x=character(),
    origen.y=character(),
    var.x=character(),
    E_final_diagnosis_numeric_cor=numeric(),
    E_final_diagnosis_numeric_chi=numeric(),
    E_final_diagnosis_numeric_anova=numeric(),
    depression_cor=numeric(),
    depression_chi=numeric(),
    depression_anova=numeric(),
    euthymia_cor=numeric(),
    euthymia_chi=numeric(),
    euthymia_anova=numeric(),
    hypomania_cor=numeric(),
    hypomania_chi=numeric(),
    hypomania_anova=numeric(),
    mixed_cor=numeric(),
    mixed_chi=numeric(),
    mixed_anova=numeric(),
    hospitalized_cor=numeric(),
    hospitalized_chi=numeric(),
    hospitalized_anova=numeric())  
  
    
} else if (version_corr==1) {
  vardpnd<-colnames(datos)[2]
  vardiario<-colnames(datos)[3:21]
  varpulsera<-colnames(datos)[22:130]
  varinter<-c(colnames(datos)[131:136],colnames(datos)[166])
  varyoung<-c(colnames(datos)[137:147],colnames(datos)[166])
  varhrsd<-c(colnames(datos)[1487:164],colnames(datos)[165])
  
  datos<-datos[,c(var_id,vardiario,varpulsera,ind_var)]
  
  # Splitting variables
  
  datos$depression<-ifelse(datos$final_diagnosis_numeric==-1,1,0)
  datos$euthymia<-ifelse(datos$final_diagnosis_numeric==0,1,0)
  datos$hypomania<-ifelse(datos$final_diagnosis_numeric==1,1,0)
  datos$mixed<-ifelse(datos$final_diagnosis_numeric==2,1,0)
  datos$hospitalized<-ifelse(datos$final_diagnosis_numeric==3,1,0)
  
  

  tabla_corr<-data.frame(
    id=character(), 
    origen.x=character(),
    origen.y=character(),
    var.x=character(),
    final_diagnosis_numeric_cor=numeric(),
    final_diagnosis_numeric_chi=numeric(),
    final_diagnosis_numeric_anova=numeric(),
    depression_cor=numeric(),
    depression_chi=numeric(),
    depression_anova=numeric(),
    euthymia_cor=numeric(),
    euthymia_chi=numeric(),
    euthymia_anova=numeric(),
    hypomania_cor=numeric(),
    hypomania_chi=numeric(),
    hypomania_anova=numeric(),
    mixed_cor=numeric(),
    mixed_chi=numeric(),
    mixed_anova=numeric(),
    hospitalized_cor=numeric(),
    hospitalized_chi=numeric(),
    hospitalized_anova=numeric())  
  
} else { #version_corr==3
  
  vardiario<-nms[startsWith(nms,"D_")]
  #vardiario<-vardiario[ ! vardiario %in% c(var_id)]
  varpulsera<-nms[startsWith(nms,"S_")]
  varinter<-nms[startsWith(nms,"I_")]
  varyoung<-nms[startsWith(nms,"Y_")]
  varhrsd<-nms[startsWith(nms,"H_")]
  
  datos<-datos[,c(var_id,vardiario,varpulsera,ind_var)]
  
  datos$depression<-ifelse(datos[[ind_var]]==-1,1,0)
  datos$euthymia<-ifelse(datos[[ind_var]]==0,1,0)
  datos$hypomania<-ifelse(datos[[ind_var]]==1,1,0)
  datos$mixed<-ifelse(datos[[ind_var]]==2,1,0)
  datos$hospitalized<-ifelse(datos[[ind_var]]==3,1,0)
  
  
  
  tabla_corr<-data.frame(
    id=character(), 
    origen.x=character(),
    origen.y=character(),
    var.x=character(),
    Z_dx_numeric_cor=numeric(),
    Z_dx_numeric_chi=numeric(),
    Z_dx_numeric_anova=numeric(),
    depression_cor=numeric(),
    depression_chi=numeric(),
    depression_anova=numeric(),
    euthymia_cor=numeric(),
    euthymia_chi=numeric(),
    euthymia_anova=numeric(),
    hypomania_cor=numeric(),
    hypomania_chi=numeric(),
    hypomania_anova=numeric(),
    mixed_cor=numeric(),
    mixed_chi=numeric(),
    mixed_anova=numeric(),
    hospitalized_cor=numeric(),
    hospitalized_chi=numeric(),
    hospitalized_anova=numeric())  
  
  
}



datos[is.na(datos$depression),c("depression")] <- 0
datos[is.na(datos$euthymia),c("euthymia")] <- 0
datos[is.na(datos$hypomania),c("hypomania")] <- 0
datos[is.na(datos$mixed),c("mixed")] <- 0
datos[is.na(datos$hospitalized),c("hospitalized")] <- 0

dpndvars<-c(ind_var,"depression","euthymia","hypomania","mixed","hospitalized")



i<-1 

options(stringsAsFactors = FALSE)

#for ( m in unique(datos$mes)) {

for ( u in unique(datos[[var_id]])) {
#for ( u in c("GIUS")) {

  
  cat(u,"\n")
  #dat<-datos[datos$idnum==u & datos$mes== m,]
  dat<-datos[datos[[var_id]]==u,]

  pnt<-character()

  for (x in colnames(dat) ) {
  #for (x in c(var1ejemplo) ) {

    if (x==var_id | x=="mes") {next}

    
    if (x %in% dpndvars) {next}
 
    
    
    #for (y in colnames(dat) ) {
    

    for (y in dpndvars ) {
    #for (y in c(ind_var) ) {
      
      if (y==var_id | x=="mes" ) {next}      

      f1<-dat[!is.na(dat[,x]) & !is.na(dat[,y]),c(x,y)]
      if (nrow(f1)==0) {next}   
      
      
      x_origen <- ifelse(x %in% vardiario,"Diario",		
                         ifelse(x %in% varpulsera,"Pulsera",
                                ifelse(x %in% varinter,"Intervenciones",
                                       ifelse(x %in% varyoung,"Young",
                                              ifelse(x %in% varhrsd,"HRSD","TargetVar")
                                       ))))    
      
      
      y_origen <- ifelse(y %in% vardiario,"Diario",		
                         ifelse(y %in% varpulsera,"Pulsera",
                                ifelse(y %in% varinter,"Intervenciones",
                                       ifelse(y %in% varyoung,"Young",
                                              ifelse(y %in% varhrsd,"HRSD","TargetVar")
                                       ))))  
      tabla_corr[i,]$id<-u
      tabla_corr[i,]$origen.x<-x_origen
      tabla_corr[i,]$var.x<-x         
      tabla_corr[i,]$origen.y<-y_origen

      


      # To do not perform the same for the same variable pairs
      if (paste0(y,x) %in% pnt) {next}
 
        
      #if (length(unique(f1[,x])) <= 2 ) {next}
      
      # In case of very few observations for each diagnostic state, proceed next
      #if (y=="final_diagnosis_numeric") {
      #  if (length(unique(f1[,y])) <= 2 ) {next}		
      #} else {  
      #  if (length(unique(f1[,y])) == 1 ) {next}
      #}        
    


      	
      for (featsel in algoritmos){
        
        if (featsel=="cor") {
          fc<-cor(f1,use="complete.obs")
          val<-fc[1,2]
        }
        if (featsel=="chi") {
          if (length(unique(f1[,y])) > 1 & length(unique(f1[,x])) > 1 ) {
            cq<-chisq.test(f1[,x],f1[,y])
            val<-cq$p.value
          }
        }
        if (featsel=="anova") {
          if (length(unique(f1[,y])) > 1) {
            saov<-summary(aov(as.formula(paste0(y,"~",x)),f1))
            val<-saov[[1]][["Pr(>F)"]][[1]]
          }
        }
        
        if (is.null(val)) val<-NA
        tabla_corr[i,paste0(y,"_",featsel)]<-round(val,4)
        
      }	
      

      #if(fc[1,2]>=0.50 & fc[1,2]<=0.95){	
      #if(fc[1,2]>=0.5 | cq$p.value > 0.05 | aov_pvalue > 0.05 ){	
        
        #tabla<-rbind(tabla,list(u,x_origen,y_origen,m,x,y,fc[1,2]))
        #tabla_corr<-rbind(tabla_corr,list(u,x_origen,y_origen,x,y,fc[1,2],cq$p.value,aov_pvalue))
        #tabla_corr<-rbind(tabla_corr,c(u,x_origen,y_origen,x,y,featsel_vals))
        
        #print(fc)
      #}
      # To do not perform the same for the same variable pairs
      pnt<-c(pnt,paste0(x,y))
      
    }


    i<-i+1
    
  }
  
}

tabla_final<-tabla_corr[ 
  tabla_corr[[paste0(ind_var,"_cor")]] >= 0.8 | tabla_corr[[paste0(ind_var,"_cor")]] <= -0.8
  | tabla_corr$depression_cor >= 0.8 | tabla_corr$depression_cor <= -0.8
  | tabla_corr$euthymia_cor >= 0.8 |  tabla_corr$euthymia_cor <= -0.8
  | tabla_corr$hypomania_cor >= 0.8 | tabla_corr$hypomania_cor <= -0.8
  | tabla_corr$mixed_cor >= 0.8 | tabla_corr$hypomania_cor <= -0.8
  | tabla_corr$hospitalized_cor >= 0.8 | tabla_corr$hospitalized_cor <= -0.8
  | tabla_corr[[paste0(ind_var,"_chi")]] > 0.05
  | tabla_corr$depression_chi > 0.05
  | tabla_corr$euthymia_chi > 0.05
  | tabla_corr$hypomania_chi > 0.05
  | tabla_corr$mixed_chi > 0.05
  | tabla_corr$hospitalized_chi > 0.05
  | tabla_corr[[paste0(ind_var,"_anova")]] > 0.05
  | tabla_corr$depression_anova > 0.05
  | tabla_corr$euthymia_anova > 0.05
  | tabla_corr$hypomania_anova > 0.05
  | tabla_corr$mixed_anova > 0.05
  | tabla_corr$hospitalized_anova > 0.05 ,
  
  ]

tabla_final<-tabla_final[rowSums(is.na(tabla_final)) != ncol(tabla_final),]


write.csv(file=archivo,tabla_final)

