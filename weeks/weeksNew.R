library(dplyr)
library(tidyr)
library(lubridate)
library(boot)

# Pacientes: "CASGA","DOSI","ECSA","GIUS","GOMA","LABA","MAPI","PRFE","RIVI","SAPE","SEOR","ALPA","DOGE","BEPR","PELA","ROCU","MARA"
pt<-c("DOSI")

# Variables: "D_motivation","D_irritability","D_sleep_duration","D_sleep_time","D_wakeup_time"
vars<-c("D_motivation","D_irritability","D_sleep_duration","D_sleep_time")
vars<-c("D_irritability")



#Leyendo el csv
dftotal<-read.csv("dnormalizado4.csv")
dftotal$A_date<-as.Date(dftotal$A_date)

#Leyendo el Rdata
#load("dnormalizado4.RData")

#+ results='hide', message=FALSE
Sys.setlocale("LC_ALL", "English")

#+ echo=TRUE, tidy=TRUE
# Generamos un vector con los nombres de los días
wd<-weekdays(ISOdate(1, 1, 1:7))
wd

# Variable para activar/desactivar el bootstraping. El bootlimit es la longitud máxima a extender un vector en caso de activar el bootstrapping
bootstraping<-FALSE
bootlimit<-100



#+ results='asis', echo=FALSE, tidy=TRUE
cat("### Función que devuele la información de paciente/variable por semanas (incluye también la variable diagnositco):\n")

#+ results='asis', echo=TRUE, tidy=TRUE
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


#+ results='asis', echo=FALSE, tidy=TRUE
cat("### Por ejemplo:\n")

#+ results='hold',fig.height = 3, fig.width = 3

head(byweek("GIUS","D_motivation"))

#+ results='asis', echo=FALSE, tidy=TRUE
cat("### Función estadistica a utilizar cuando este activo el bootstraping: \n")

#+ results='asis', echo=TRUE, tidy=TRUE
# La función estadistica a utilizar es la media : mean
boot.f <- function(data, indices){
  round(mean(data[indices]))
}

#+ results='asis', echo=TRUE, tidy=TRUE

# Aquí se puede ir escogiendo los pacientes y las variables


#+ results='asis', echo=FALSE, tidy=TRUE
cat("### Loop para ir pintando los boxplots de los pacientes y variables seleccionados \n")

#+ results='asis', echo=TRUE, tidy=TRUE

for (patient in pt){

  for (variable in vars) {

    cat(paste0("## Paciente : ",pt,". Variable : ",variable,"  \n"))    
    
    # Utilizamos la función byweek previamente creada,y solo nos quedamos con las columnas de cada día
    datost<-byweek(patient,variable);
    datos<-datost[,c(4:10)]

    nn<-data.frame()
      
    # Recorremoss los valores de cada uno de los 7 días de la semana
    for (i in c(1:7)) {
      
      # Almacenamos cada vector de cada en un vector
      vls<-datos[,i]
      
      # Ejecutamos el bootstraping si es que esta activado
      if (bootstraping){
        
        # En el vector de cada día quitamos los valores nulos
        vls<-vls[!is.na(vls)]
        # Calculamos cuantos más vamos a extender el vector
        nboot<-bootlimit-length(vls)
        # Ejecutamos el bootstraping
        stat.boot <- boot(vls, boot.f, nboot)
        # Los nuevos valores generados los agregamos a los valores iniciales
        vls<-c(vls,as.numeric(stat.boot$t))
        
      }
      
      # Almacenamos los vectores en un dataframe y los vamos acumulando con cbind.
      dw<-as.data.frame(vls)
      colnames(dw)<-wd[i] # El nombre del día.
      
      if (i==1) 
        nn<-dw
      else 
        nn<-cbind(nn,dw)
      
    } 
    
    par(cex.axis=0.8)
    boxplot(nn)
    cat("\n\n")
    
  }
  cat("\n\n")
}







