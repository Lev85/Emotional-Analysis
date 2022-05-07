
paciente<-"CASGA"
variable<-"D_wakeup_time"


# Esto se usa solo si ejecutamos desde fuera masivamente
if (exists("cArgs")) {
  paciente<-cArgs[1]
  variable<-cArgs[2]
  rm(cArgs)
}


# Variables fijas
var_diagnosis<-"I_dx_short"
var_date<-"A_date"




library(dplyr)
library(ggplot2)

#Leyendo el csv
#dftotal<-read.csv("dnormalizado4.csv")
#asignamos el tipo fecha
#dftotal$A_date<-as.Date(dftotal$A_date)

load("datosIntegrados/dnormalizado7.RData")

#Generamos variables que usaremos en el plot
pltpaciente<-switch(paciente,
                    "ALPA"="P01",
                    "BEPR"="P02",
                    "CASGA"="P03",
                    "DOSI"="P04",
                    "ECSA"="P05",
                    "GIUS"="P06",
                    "GOMA"="P07",
                    "LABA"="P08",
                    "MAPI"="P09",
                    "PELA"="P10",
                    "PRFE"="P11",
                    "RIVI"="P12",
                    "ROCU"="P13",
                    "SAPE"="P14",
                    "SEOR"="P15",
                    "DOGE"="P16",
                    "UNKNOWN")

pltvariable<-substr(variable,3,nchar(variable))
pltdate<-substr(var_date,3,nchar(var_date))


# Filtramos el paciente a analizar y las variables de interes
datos_paciente<-filter(dftotal, A_id==paciente)

mindate<-min(
  min(datos_paciente[datos_paciente$B_S_NAfilled=="REAL",c(var_date)]),
  min(datos_paciente[datos_paciente$B_D_NAfilled=="REAL",c(var_date)]))

maxdate<-max(
  max(datos_paciente[datos_paciente$B_S_NAfilled=="REAL",c(var_date)]),
  max(datos_paciente[datos_paciente$B_D_NAfilled=="REAL",c(var_date)]))

#Filtramos las fechas de datos reales
datos_paciente<-datos_paciente[datos_paciente[[var_date]]>=mindate &
                                 datos_paciente[[var_date]]<=maxdate, ]


# SELECCIONAMOS LAS VARIABLES DE INTERES
datos_paciente<-datos_paciente[,c(var_date,variable,var_diagnosis)]

# Cambiamos nombres de variables a las que se usaran finalmente en el plot
names(datos_paciente)<-c(pltdate,pltvariable,var_diagnosis)

# Añado una columna con el día de la semana
datos_paciente$weekday<-weekdays(datos_paciente[[pltdate]])

# Creamos la columna del número de semana
#datos_paciente$nw<-strftime(datos_paciente$date,format="%V")

#datos_paciente<-datos_paciente[as.numeric(datos_paciente$nw)>=6 & 
#                               as.numeric(datos_paciente$nw)<=40,]   

# Quitamos posibles duplicados
datos_paciente<-distinct(datos_paciente)

datos_paciente$weekday<-factor(datos_paciente$weekday,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"))

# Generamos labels del eje X
break.vec <- c(seq(from = min(datos_paciente[[pltdate]]), to = max(datos_paciente[[pltdate]]),
                   by = "week"))
labdates<-strftime(break.vec,format="%V")

plt<-ggplot(datos_paciente,aes_string(pltdate))
plt<-plt + ggtitle(paste0(pltvariable," for patient ",pltpaciente)) 
plt<-plt + geom_smooth(data = datos_paciente,aes_string(x=pltdate,y=pltvariable,colour="weekday"),span=0.23,se=F)
plt<-plt + scale_x_date(breaks = break.vec,labels=labdates)
plt<-plt + xlab("week")

# Añadimos los cuadrantes de las crisis
df1<-datos_paciente[,c(pltdate,var_diagnosis)]
#df1<-datos_paciente
names(df1)[names(df1) ==  var_diagnosis] <-"Dx"
df1$start<-df1[[pltdate]]
df1$end<-df1$start+1
df1<-unique(df1)    

minval=max(datos_paciente[[pltvariable]],na.rm=T)
maxval=min(datos_paciente[[pltvariable]],na.rm=T)

plt<-plt + geom_rect(data = df1, 
                     aes(xmin = start, 
                         xmax = end, 
                         ymin = minval, 
                         ymax = maxval, 
                         fill = Dx),
                     alpha = 0.2)

plt<-plt + theme(plot.title = element_text(size = 24, face = "bold"),
                 axis.text=element_text(size=12,face="bold"),
                 #axis.text.x = element_text(angle = 90, hjust = 1),
                 #axis.text.x = element_text(hjust = 1),
                 axis.title.x = element_text(size=20,margin = margin(t = 10, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(size=20),
                 legend.text=element_text(size=24,face="bold"),
                 legend.position="bottom",
                 legend.box="vertical",
                 legend.title=element_blank())
plt<-plt + guides(color = guide_legend(order=1))

