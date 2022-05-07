library(zoo)

na.interpolation3<-function(x){
  if (all(is.na(x))) {
    x<-0		
  } else {
    x<-na.interpolation(x)
  }
  
}


load("dnormalizado.RData")
dnm<-dnormalizado

#dnm2<-dnm[,1:41]

patients<-c("CASGA","DOSI","ECSA","GIUS","MAPI","SAPE")
patients<-c("GIUS")


for (pt in patients){
  cat(pt)
  dfetsel<-dnm[dnm$id==pt,]

  # Keeping only numeric variables
  dfetsel<-dfetsel[,names(which(sapply(dfetsel,is.numeric)))]
  
  # Removing columns with all NA values
  dfetsel<-dfetsel[, colSums(is.na(dfetsel)) != nrow(dfetsel)]
  
  # Removing columns with all the same values
  dfetsel<-Filter(var,dfetsel)

  
  
  # Interpolation
  dfetsel<-data.frame(lapply(dfetsel,na.interpolation))
  
  

  # Remove columns with any NA values
  # dfetsel<-dfetsel[ , colSums(is.na(dfetsel)) == 0]
  
  # Remove rows with any NA values
  # dfetsel<-dfetsel[complete.cases(dfetsel), ]
  
  
  
  # Removing some irrelevant variables
  dfetsel<-dfetsel[, !names(dfetsel) %in% c("week","night_number")]
  
  
  
}