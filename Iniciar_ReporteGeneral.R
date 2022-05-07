rm(list = ls())

ind_mode<-1

if (ind_mode==1){
  vgn_short_diagnosis<-"I_dx_short"
  vgn_numeric_diagnosis<-"Z_diagnosis"
  ind_var<-"Z_dx_numeric"
} else if (int_mode==2){
  vgn_short_diagnosis<-"D_irritability"
  vgn_numeric_diagnosis<-"Z_irritability"
  ind_var<-"Z_dx_irritability"
}

boo_soloReales<-FALSE
vartypes<-c("D","S")


var_plots<-         FALSE;


var_ml_allvars<-    FALSE;
var_ml_fsvars<-     FALSE;
var_ml_svm<-        TRUE; # Support Vector Machines
var_ml_rf<-         TRUE; # Random Forest
var_ml_knn<-        TRUE; # K nearest neighbour
var_ml_dt<-         TRUE; # Decision Tree
var_ml_ld<-         TRUE; # Logistic Discriminant
var_ml_lda<-        TRUE; # Linear Discriminant Analysis
var_ml_qda<-        TRUE; # Quadratic Discriminant Analysis
var_ml_reglin<-     TRUE; # Linear Regression
var_ml_reglog<-     TRUE; # Logistic Regression
var_ml_reglogbay<-  TRUE; # Bayessian Logistic Regression

var_featselProc<-   FALSE;
var_featselSumm<-   FALSE;

var_pca<-           TRUE;



featselalg<-c("CORR","CHI","ANOVA","BORUTA","VARIMP","LASSO","STEPWISE","RELIMP","RFE","GENALG","SIMANN","INFVAL","DALEX")
featselalg<-c("BORUTA","VARIMP","LASSO","STEPWISE","RELIMP","RFE","GENALG","SIMANN","INFVAL","DALEX")
#featselalg<-c("LASSO","STEPWISE","RFE","DALEX")
#featselalg<-c("LASSO")



patients<-c("CASGA","DOSI","ECSA","GIUS","GOMA","LABA","MAPI","PRFE","RIVI","SAPE","SEOR","ALPA","DOGE","BEPR","PELA","ROCU","MARA")
patients<-c("GIUS")

#list of selected features by patient
list_featsel<-vector(mode = "list")

#list of selected features by patient in data.frame (sorted)
list_featselPatient<-vector(mode = "list")


if (var_featselProc) source('D:/Proyectos/Pavel_MoodDisorders/FeatureSelection/FeatSelFunctions.R')



#rmarkdown::render('ReporteGeneral.Rmd',  "word_document")
rmarkdown::render('ReporteGeneral_v2.Rmd',  "word_document")

if (var_featselProc) save(list_featsel,list_featselPatient,file="featSel.RData")