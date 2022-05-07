library(dplyr)
library(tidyr)
library(lubridate)
library(boot)


#pt<-c("CASGA","DOSI","ECSA","GIUS","GOMA","LABA","MAPI","PRFE","RIVI","SAPE","SEOR","ALPA","DOGE","BEPR","PELA","ROCU","MARA")
pt<-"GIUS"
#vars<-c("D_motivation","D_irritability","D_sleep_duration","D_sleep_time","D_wakeup_time","Z_dx_numeric")
vars<-"D_motivation"



load("dnormalizado4.RData")
dnm<<-dftotal

wd<-weekdays(ISOdate(1, 1, 1:7))
bootstraping<-FALSE


Sys.setlocale("LC_ALL", "English")


byweek<-function(patient,varName){

varNameAbr=substr(varName,3,5)

varDiagnosis="Z_dx_numeric"
dateVar="A_date"
idVar="A_id"

dnm1<-dftotal[dftotal[[idVar]]==patient,c(idVar,dateVar,varName,varDiagnosis)]

dnm1<-dnm1[!duplicated(dnm1[[dateVar]]),]


dnmVar <- dnm1 %>%
  mutate(day = format(!!sym(dateVar), paste0("%a_",varNameAbr)), 
         firstDate = floor_date(!!sym(dateVar), 'week'), 
         firstDate = if_else(firstDate == !!sym(dateVar), !!sym(dateVar) - weeks(1), firstDate) + days(1), 
         lastDate = ceiling_date(firstDate, 'week')) %>% 
  pivot_wider(c(!!sym(idVar), firstDate, lastDate), names_from = day, values_from = !!sym(varName)) %>% 
  as.data.frame()

dnmDx <- dnm1 %>%
  mutate(day = format(!!sym(dateVar), paste0("%a_Dx")), 
         firstDate = floor_date(!!sym(dateVar), 'week'), 
         firstDate = if_else(firstDate == !!sym(dateVar), !!sym(dateVar) - weeks(1), firstDate) + days(1), 
         lastDate = ceiling_date(firstDate, 'week')) %>% 
  pivot_wider(c(!!sym(idVar), firstDate, lastDate), names_from = day, values_from = !!sym(varDiagnosis)) %>% 
  as.data.frame()

dnmFin<-merge(dnmVar,dnmDx,by=c(idVar,"firstDate","lastDate"))

cols<-c(idVar,"firstDate","lastDate",
        paste0("Mon","_",varNameAbr),
        paste0("Tue","_",varNameAbr),
        paste0("Wed","_",varNameAbr),
        paste0("Thu","_",varNameAbr),
        paste0("Fri","_",varNameAbr),
        paste0("Sat","_",varNameAbr),
        paste0("Sun","_",varNameAbr),
        paste0("Mon","_Dx"),
        paste0("Tue","_Dx"),
        paste0("Wed","_Dx"),
        paste0("Thu","_Dx"),
        paste0("Fri","_Dx"),
        paste0("Sat","_Dx"),
        paste0("Sun","_Dx"))

# Primera Entrega        
dnmFin<-dnmFin[,cols]

return(dnmFin)

}

nboot<-100  
boot.f <- function(data, indices){
  # data[indices] será la muestra bootstrap
  round(mean(data[indices]))
}


#' A script comment that includes **markdown** formatting.

yy<-data.frame()


#+ results='asis', echo=TRUE, tidy=TRUE
for (patient in pt){

  for (variable in vars) {
    
    
    cat("## dddd  \n")    
    datost<-byweek(patient,variable);
    datos<-datost[,c(4:10)]


    for (i in c(1:7)) {
      
      vls<-datos[,i]
      
      if (bootstraping){
      # En el vector por cada día quitamos los valores nulos
      vls<-vls[!is.na(vls)]
      stat.boot <- boot(vls, boot.f, nboot)
      vls<-c(vls,as.numeric(stat.boot$t))
      }
      
      xx<-as.data.frame(table(round(vls)))
      xx<-xx[order(xx$Var1),]
      day<-wd[i]
      
      xx$day<-day
      xx$patient<-patient
      xx$variable<-variable
      
      yy<-rbind(yy,xx)
      
    }    

    
    boxplot(datost[,4])
    cat("\n\n")
    
  }
}



colnames(yy)<-c("valor","frequencia","dia","paciente","variable")

yy<-yy[,c("paciente","variable","dia","valor","frequencia")]

frecuencias<-yy
