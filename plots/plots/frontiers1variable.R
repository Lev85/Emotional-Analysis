paciente<-"DOSI"
variable<-"S_sleep_efficiency"

#S_sleeponset
#S_wakeup
#S_dur_spt_min
#S_sleep_efficiency

# Esto se usa solo si ejecutamos desde fuera masivamente
if (exists("cArgs")) {
  paciente<-cArgs[1]
  variable<-cArgs[2]
  rm(cArgs)
}

datosReales<-TRUE

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

library(ggplot2)
load("datosIntegrados/dnormalizado7.RData")
dnm<-dftotal

Sys.setlocale("LC_TIME", "English")

pt<-paciente


int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}


var_id<-"A_id"
var_date<-"A_date"
var_diagnosis<-"E_diagnosis"
var_prev_diagnosis<-"I_dx_short"

dnm[[var_diagnosis]]<-dnm[[var_prev_diagnosis]]
dnm[[var_prev_diagnosis]]<-NULL

datos.paciente<-dnm[dnm$A_id==pt,]

vsl<-c(variable)
vmax<-as.Date(integer(),origin = "1970-01-01")
vmin<-as.Date(integer(),origin = "1970-01-01")
maxval<-as.numeric()
minval<-as.numeric()

for (vt in vsl) 
  vmax<-c(vmax,max(datos.paciente[!is.na(datos.paciente[[vt]]),c(var_date)]))

mx<-max(vmax)

dplot<-datos.paciente[datos.paciente[[var_date]]<=mx+2 ,]  

# dfscale<-dplot[,vsl]
# dfscale<-scale(dfscale)
# dfscale<-as.data.frame(dfscale)
# colnames(dfscale)<-vsl
# dplot[,vsl]<-NULL
# dplot<-cbind(dplot,dfscale)

df1<-dplot[,c(var_diagnosis,var_date,vsl)]



if (datosReales) { 
  
  for (vt in vsl) dplot[[vt]]<-ifelse(dplot$B_D_NAfilled!="REAL",NA,dplot[[vt]])
  #dplot[[vrs[2]]]<-ifelse(dplot$B_S_NAfilled=="S",NA,dplot[[vrs[2]]])
}


names(df1)[names(df1) ==  var_diagnosis] <-"Dx"
df1$start<-df1[[var_date]]
df1$end<-df1$start+1
df1<-unique(df1)    
#stop()


#stop()

plt<-ggplot(dplot,aes_string(var_date))	

for (i in 1:length(vsl) ){
  maxval<-c(maxval,max(dplot[[vsl[i]]],na.rm=TRUE))
  minval<-c(minval,min(dplot[[vsl[i]]],na.rm=TRUE))
  
  plt<-plt+geom_line(aes_string(y=vsl[i],colour=paste0("vsl[",i,"]")),size=0.1)
  
  
  
  plt<-plt+geom_point(aes_string(y=vsl[i],colour=paste0("vsl[",i,"]")),size=0.7)   
  plt<-plt+geom_smooth(data = dplot,aes_string(x=var_date,y=vsl[i],colour=paste0("vsl[",i,"]")),span=0.23,se=F)
}

if (datosReales){
  tipodatos="(Real Data)"
} else {
  tipodatos="(Interpolated Data)"
}

ctitle<-paste0("Patient: ",pltpaciente," ",tipodatos)
  
#dplot[[vsl[1]]]
#int_breaks=round(quantile(dplot[[vsl[1]]],na.rm=TRUE,names=FALSE,probs = c(0, 0.25, 0.5, 0.75, 1)),2)
ib<-pretty(dplot[[vsl[1]]],5)
#plt<-plt + scale_y_continuous(breaks = c(0,4,8,12))
plt<-plt + scale_y_continuous(breaks = ib)
plt<-plt + ggtitle(ctitle) +
labs(x = "Monitoring dates") +
labs(y = pltvariable)
maxval<-max(maxval,na.rm = TRUE)
minval<-min(minval,na.rm = TRUE)
plt<-plt + geom_rect(data = df1, aes(xmin = start, xmax = end, ymin = minval, ymax = maxval, fill = Dx), alpha = 0.2)
plt<-plt +	scale_x_date(date_breaks = "1 month",date_labels = "%b %y")  +
  theme(legend.title=element_blank(),
        plot.title = element_text(size = 24, face = "bold"),
        axis.text=element_text(size=24,face="bold"),
        legend.text=element_text(size=24,face="bold"),
        legend.position="bottom",
        legend.box="vertical",
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size=20,margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size=20),
        axis.title = element_text(size=24,face="bold"))

print(plt)

#library(ggpubr)
#plt04 <- plt04 + theme(plot.margin = margin(1,1,1,1, "cm"))
#plt02 <- plt + theme(plot.margin = margin(1,1,1,1, "cm")) 


#ggarrange(plt01,plt02,labels = c("(a)","(b)"),nrow = 2, ncol = 1,font.label = list(size = 28),hjust=-14,vjust=21)

#library(ggpubr)
# plt01 <- plt + theme(plot.margin = margin(1,1,1,1, "cm")) 
# plt02 <- plt + theme(plot.margin = margin(1,1,1,1, "cm"))
# plt03 <- plt + theme(plot.margin = margin(1,1,1,1, "cm"))
# plt04 <- plt + theme(plot.margin = margin(1,1,1,1, "cm")) 
# 
#ggarrange(plt01,plt02,plt03,plt04,labels = c("(A)","(B)","(C)","(D)"),font.label = list(size = 17),hjust=-15,vjust=43.5)
