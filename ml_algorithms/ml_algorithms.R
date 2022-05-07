library(dplyr)
library(arm)
library(DataExplorer)
library(class)
library(rpart)
library(glmnet)
library(e1071)
library(nnet)


fun_rf<-function(data.train,data.test,var_diagnosis,pt){
  formula<-as.formula(paste0(var_diagnosis,"~."))  
  cat("\nRANDOM FOREST")
  data.train[[var_diagnosis]]<-as.factor(data.train[[var_diagnosis]])
  data.test[[var_diagnosis]]<-as.factor(data.test[[var_diagnosis]])
  
  modelo_rf<-randomForest(formula,data=data.train)
  predictions <- predict(modelo_rf, data.test)
  mc<-table(predictions,data.test[[var_diagnosis]])
  confidence<-100 * sum(diag(mc)) / sum(mc)
  
  print(mc)
  print(confidence)
  
  
}

fun_svm<-function(data.train,data.test,var_diagnosis,pt){
  formula<-as.formula(paste0(var_diagnosis,"~."))
  cat("\nSVM")
  modelo_svm<-svm(formula,data=data.train,type='C-classification',kernel='linear')
  predictions <- predict(modelo_svm, data.test)
  mc<-table(predictions,data.test[[var_diagnosis]])
  confidence<-100 * sum(diag(mc)) / sum(mc)
  
  print(mc)
  print(confidence)
  
  
}

fun_dt<-function(data.train,data.test,var_diagnosis,pt){
  formula<-as.formula(paste0(var_diagnosis,"~."))
  cat("\nDECISSION TREE")
  #data.train[[var_diagnosis]]<-as.factor(data.train[[var_diagnosis]])
  #data.test[[var_diagnosis]]<-as.factor(data.test[[var_diagnosis]])
  
  modelo_dt<-rpart(formula,data=data.train,method='class')
  predicciones <- predict(modelo_dt, data.test,type='class')
  mc<-table(predicciones,data.test[[var_diagnosis]])
  confidence<-100 * sum(diag(mc)) / sum(mc)
  
  print(mc)
  print(confidence)

}

fun_reglogmn<-function(data.train,data.test,var_diagnosis,pt){
  formula<-as.formula(paste0(var_diagnosis,"~."))
  cat("\nREGRESION LOGISTICA (MULTINOMIAL)")
  modelo_rl<-multinom(formula,data.train,trace=F)
  predicciones<-predict(modelo_rl, newdata = data.test, "class")
  
  mc<-table(predicciones,data.test[[var_diagnosis]])
  confidence<-100 * sum(diag(mc)) / sum(mc)
  
  print(mc)
  print(confidence)

}

fun_knn<-function(data.train,data.test,var_diagnosis,pt){

  cat("\nK NEAREST NEIGBOUR")
  
  datos.entreno.alg.knn<-data.train[ , !(names(data.train) %in% var_diagnosis)]
  datos.test.alg.knn<-data.test[ , !(names(data.test) %in% var_diagnosis)]
  
  train_category <- data.train[[var_diagnosis]]
  
  modelo_knn <- knn(datos.entreno.alg.knn,datos.test.alg.knn,cl=train_category,k=3)
  
  
  mc<-table(modelo_knn,data.test[[var_diagnosis]])
  confidence<-100 * sum(diag(mc)) / sum(mc)
  
  print(mc)
  print(confidence)
  
}


fun_reglogbay<-function(data.train,data.test,var_diagnosis,pt){
  formula<-as.formula(paste0(var_diagnosis,"~."))
  cat("\nBayessian Logistic Regression\n")
  
  if(pt=="MAPI") wToClsfy<-1 # 1 -> Mania
  if(pt=="ECSA") wToClsfy<-1 # 1 -> Mania
  if(pt=="DOSI") wToClsfy<-2 # 1 -> Mixed
  if(pt=="CASGA") wToClsfy<-1 # 1 -> Mania
  if(pt=="GIUS") wToClsfy<--1 # 1 -> Depression
  if(pt=="SAPE") wToClsfy<-1 # 1 -> Mania
  if(pt=="ALPA") wToClsfy<-2 # 1 -> Mixed **
  if(pt=="DOGE") wToClsfy<-2 # 1 -> Mixed **
  if(pt=="GOMA") wToClsfy<--1 # 1 -> Depression
  if(pt=="LABA") wToClsfy<-1 # 1 -> Mania
  if(pt=="PRFE") wToClsfy<--1 # 1 -> Depression
  if(pt=="RIVI") wToClsfy<-2 # 1 -> Mixed
  if(pt=="SEOR") wToClsfy<-1 # 1 -> Mania
  
  data.train[[var_diagnosis]]<-ifelse(data.train[[var_diagnosis]]==wToClsfy,1,0)
  data.test[[var_diagnosis]]<-ifelse(data.test[[var_diagnosis]]==wToClsfy,1,0)  
  
  modelo_glm<-bayesglm(formula,data=data.train,family=binomial)
  
  options(scipen=999)
  predicciones <- predict(modelo_glm, data.test,type="response")
  
  data.test$prediccion_glm <- predicciones
  realvspredict<-data.test[,c(var_diagnosis,"prediccion_glm")]
  
  realvspredict$prediccion_glm<-round(realvspredict$prediccion_glm)
  realvspredict$correct<-ifelse(realvspredict$prediccion_glm==realvspredict[[var_diagnosis]],"YES","NO")
  
  confidence<-(nrow(realvspredict[realvspredict$correct=="YES",])/nrow(realvspredict))*100
  
  print(realvspredict)
  print(confidence)
  
}