paciente<-"CASGA"
variable1<-"D_irritability"
variable2<-"S_sleep_efficiency"

# Esto se usa solo si ejecutamos desde fuera masivamente
if (exists("cArgs")) {
  paciente<-cArgs[1]
  variable1<-cArgs[2]
  variable2<-cArgs[3]
  rm(cArgs)
}

library(ggplot2)
load("dnormalizado4.RData")
dnm<-dftotal

Sys.setlocale("LC_TIME", "English")

var_id<-"A_id"
var_date<-"A_date"
var_diagnosis<-"E_diagnosis"
var_prev_diagnosis<-"I_dx_short"
 
pt<-paciente

#Generamos variables que usaremos en el plot
pltpaciente<-switch(pt,
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

dnm[[var_diagnosis]]<-dnm[[var_prev_diagnosis]]
dnm[[var_prev_diagnosis]]<-NULL

datosReales<-F


datos.paciente<-dnm[dnm[[var_id]]==pt,]
namevars<-colnames(datos.paciente)

vartypes<-c("D_","S_")


repeat{
  
  vrs<-as.character()
  vds<-as.character()
  vmax<-as.Date(integer(),origin = "1970-01-01")
  boo_break<-TRUE  
  
  for (vt in vartypes) {
    
    vr<-namevars[grepl(paste0("^",vt),namevars)]
    vrd<-sample(vr,1)
    
    
    vmax<-c(vmax,max(datos.paciente[!is.na(datos.paciente[[vrd]]),c(var_date)]))
    vrs<-c(vrs,vrd)
  }
  
  #vrs<-c("D_irritability","S_sleep_efficiency")
  vrs<-c(variable1,variable2)
  
  mx<-max(vmax)
  
  # dataFrame que contiene la variable a graficar
  dplot<-datos.paciente[datos.paciente[[var_date]]<=mx+0 
                        ,]  
  
  for (vt in vrs) {
    # Si dentro de la variable solo HAY UN valor diferente de NA, entonces
    # sigo buscando, porque el scale pondrá NaN a todos los NO son NA, 
    # y perdemos datos.
    # Por eso busco las variables que tenga más de un valor distinto para 
    # hacer el "break"
    if (length(unique(na.exclude(dplot[[vt]]))) > 1 ) {
      boo_break<-boo_break & TRUE
    } else {
      boo_break<-boo_break & FALSE
      break
    }
    #vds<-c(vds,vardesc[vardesc$Variable==vt,c("eng_desc")])
  }  
  
  if(boo_break)  break
  
}

dfscale<-dplot[,vrs]
dfscale<-scale(dfscale)
dplot[,vrs]<-NULL
dplot<-cbind(dplot,dfscale)
dplot$S_sleep_efficiency<-ifelse(-5>dplot$S_sleep_efficiency,-5,dplot$S_sleep_efficiency)

df1<-dplot[,c(var_diagnosis,var_date,vrs)]
#df1<-datos.paciente[,c(var_diagnosis,var_date,vrs)]

if (datosReales) { 
  dplot[[vrs[1]]]<-ifelse(dplot$B_D_NAfilled=="D",NA,dplot[[vrs[1]]])
  dplot[[vrs[2]]]<-ifelse(dplot$B_S_NAfilled=="S",NA,dplot[[vrs[2]]])
}

names(df1)[names(df1) ==  var_diagnosis] <-"Dx"
df1$start<-df1[[var_date]]
df1$end<-df1$start+1
df1<-unique(df1)


  plt<-ggplot(dplot,aes_string(var_date)) + 	
  ggtitle(paste0("Patient: ",pltpaciente)) +
  #geom_line(aes_string(y=vrs[1],colour="vrs[1]"),size=0.6) + 
  geom_point(aes_string(y=vrs[1],colour="vrs[1]"),size=0.5) +
  #geom_line(aes_string(y=vrs[2],colour="vrs[2]"),size=0.6) + 
  geom_point(aes_string(y=vrs[2],colour="vrs[2]"),size=0.5) +
  labs(x = "Monitoring dates") +
  labs(y = "Variable values scaled [-5,+5]") +
  scale_color_discrete(
    name = "Variables", 
    labels = c(vrs[1],vrs[2])
  ) +
  #stat_smooth(data = dplot,aes_string(x=var_date,y=vrs[1],colour="vrs[1]"),level=0) +
  #stat_smooth(data = dplot,aes_string(x=var_date,y=vrs[2],colour="vrs[2]"),level=0) +
  geom_smooth(data = dplot,aes_string(x=var_date,y=vrs[1],colour="vrs[1]"),span = 0.2,se=F) +
  geom_smooth(data = dplot,aes_string(x=var_date,y=vrs[2],colour="vrs[2]"),span = 0.2,se=F) +
  geom_rect(
    data = df1, 
    aes(xmin = start, 
        xmax = end, 
        #ymin = min(c(dplot[[vrs[1]]],dplot[[vrs[2]]]),na.rm=T), 
        #ymax = max(c(dplot[[vrs[1]]],dplot[[vrs[2]]]),na.rm=T), 
        ymin=-5,
        ymax=5,
        fill = Dx), 
    alpha = 0.25
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%d %b %y"
  )  +
  theme(
    legend.position="bottom",
    legend.box="vertical", 
    legend.title=element_text(size=15,face="bold"),
    legend.text=element_text(size=13),
    axis.text.y = element_text(face="bold", size=13), 
    axis.text.x = element_text(angle = 90, hjust = 1,face="bold", size=13),
    axis.title.x= element_text(size=13),
    axis.title.y= element_text(size=13)) +
  guides(color = guide_legend(order=1))

#print (plt)


#library(ggpubr)
# plt03 <- plt + theme(plot.margin = margin(1,1,1,1, "cm")) 
# plt04 <- plt + theme(plot.margin = margin(1,1,1,1, "cm"))
# plt06 <- plt + theme(plot.margin = margin(1,1,1,1, "cm"))
# plt09 <- plt + theme(plot.margin = margin(1,1,1,1, "cm")) 
# 
# ggarrange(plt03,plt04,plt06,plt09,labels = c("(A)","(B)","(C)","(D)"),font.label = list(size = 17),hjust=-15,vjust=43.5)
