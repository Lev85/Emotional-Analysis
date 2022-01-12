
paciente<-"GIUS"
variable<-"D_motivation"

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
library(DataExplorer)

load("dnormalizado7.RData")
source("Funciones.R")



wd<-weekdays(ISOdate(1, 1, 1:7))
bootstraping<-FALSE


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

Sys.setlocale("LC_ALL", "English")

datost<-byweek(paciente,variable)
datos<-datost[,c(4:10)]
colnames(datos)<-wd

plt<-plot_correlation(datos,
                 cor_args = list( 'use' = 'complete.obs'),
                 geom_text_args = list(size=6,fontface="bold"),
                 title=paste0("Correlation Matrix for ",pltpaciente," for ",pltvariable),
                 theme_config = list(axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     title = element_text(size=23,face="bold"),
                                     axis.text.x = element_text(size=22,angle = 90,face="bold"),
                                     axis.text.y = element_text(size=22,face="bold"),
                                     #legend.position="none",
                                     legend.text=element_text(size=15,face="bold"),
                                     legend.title=element_text(size=15,face="bold"))
)

