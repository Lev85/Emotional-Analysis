paciente<-"CASGA"
variable<-"D_wakeup_time"

# Esto se usa solo si ejecutamos desde fuera masivamente
if (exists("cArgs")) {
  paciente<-cArgs[1]
  variable<-cArgs[2]
  rm(cArgs)
}

Sys.setlocale("LC_TIME", "English")

load("datosIntegrados/dnormalizado4.RData")
dnm<-dftotal


var_id<-"A_id"
var_date<-"A_date"
var_diagnosis<-"E_diagnosis"
var_prev_diagnosis<-"I_dx_short"

dnm[[var_diagnosis]]<-dnm[[var_prev_diagnosis]]
dnm[[var_prev_diagnosis]]<-NULL

dft<-data.frame()


datx<-data.frame()
datosReales<-T
for (pt in c(paciente)){

  datos.paciente<-dnm[dnm$A_id==pt,]
  cx<-nrow(datos.paciente[datos.paciente$B_S_NAfilled %in% c("REAL","S")  & 
                            datos.paciente$B_D_NAfilled %in% c("REAL","D") &
                            !is.na(datos.paciente[[var_diagnosis]]),
                          c("E_diagnosis","B_S_NAfilled")])
  datx<-rbind(datx,data.frame(pt,cx))
  
}

oldn<-variable
newn<-variable
names(datos.paciente)[names(datos.paciente) == oldn] <- newn
vsl<-c(newn)
vmax<-as.Date(integer(),origin = "1970-01-01")
vmin<-as.Date(integer(),origin = "1970-01-01")
maxval<-as.numeric()
minval<-as.numeric()

for (vt in vsl) 
  vmax<-c(vmax,max(datos.paciente[!is.na(datos.paciente[[vt]]),c(var_date)]))

mx<-max(vmax)


dplot<-datos.paciente[datos.paciente[[var_date]]<=mx+2 ,]  

dfscale<-dplot[,vsl]
dfscale<-scale(dfscale)
dfscale<-as.data.frame(dfscale)
colnames(dfscale)<-vsl
dplot[,vsl]<-NULL
dplot<-cbind(dplot,dfscale)

df1<-dplot[,c(var_diagnosis,var_date,vsl)]

if (datosReales) { 
  
  for (vt in vsl) dplot[[vt]]<-ifelse(dplot$B_S_NAfilled!="REAL",NA,dplot[[vt]])
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
  
  plt<-plt+geom_line(aes_string(y=paste0("`",vsl[i],"`"),colour=paste0("'",vsl[i],"'")),size=0.1)
  
  plt<-plt+geom_smooth(data = dplot,
                       aes_string(x=var_date,paste0("`",vsl[i],"`"),
                                  colour=paste0("'",vsl[i],"'")),
                      span=0.2,size=2
                       )
}

maxval<-max(maxval,na.rm = TRUE)
minval<-min(minval,na.rm = TRUE)
plt<-plt + geom_rect(data = df1, aes(xmin = start, xmax = end, ymin = minval, ymax = maxval, fill = Dx), alpha = 0.25)
plt<-plt +	scale_x_date(date_breaks = "1 month",date_labels = "%d %b %y")  +
  theme(legend.title=element_blank(),
        axis.text=element_text(size=18,face="bold"),
        legend.text=element_text(size=30,face="bold"),
        legend.position="bottom",
        legend.box="vertical",
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title=element_text(size=24,face="bold"))  
plt<-plt + guides(color = guide_legend(order=1))

print(plt)
