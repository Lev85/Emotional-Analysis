

#script_intervenciones<-"02_01_Carga_intervenciones.r"
#source(paste0(path_scripts,script_intervenciones))

script_young<-"02_02_Carga_young.r"
source(paste0(path_scripts,script_young))

script_hrsd<-"02_01_Carga_hrsd.r"
source(paste0(path_scripts,script_hrsd))


if (foj_discretos) {
discretos<-merge(hrsd,young,by=c("fecha","id"),all=T)
} else {
discretos<-merge(hrsd,young,by=c("fecha","id"))

}
#stop()
#colnamesDiscretos<-colnames(discretos)
#colnamesToExclude<-colnamesDiscretos[grepl('^NAfilled',colnamesDiscretos)]

#discretos<-discretos[ , !names(discretos) %in% colnamesToExclude]

# colnames(discretos)[3]<-"I_alivio"
# colnames(discretos)[4]<-"I_terapeuta"
# colnames(discretos)[5]<-"I_programada"
# colnames(discretos)[6]<-"I_dx"
# colnames(discretos)[7]<-"I_tipo_intervencion"
# colnames(discretos)[8]<-"I_cambio_significativo"
# colnames(discretos)[9]<-"I_intervencionesfill1"
# colnames(discretos)[10]<-"Y_euforia"
# colnames(discretos)[11]<-"Y_hiperactividad"
# colnames(discretos)[12]<-"Y_impulso_sexual"
# colnames(discretos)[13]<-"Y_suenho"
# colnames(discretos)[14]<-"Y_irritabilidad"
# colnames(discretos)[15]<-"Y_expresion_verbal"
# colnames(discretos)[16]<-"Y_curso"
# colnames(discretos)[17]<-"Y_contenido"
# colnames(discretos)[18]<-"Y_agresividad"
# colnames(discretos)[19]<-"Y_apariencia"
# colnames(discretos)[20]<-"Y_conciencia"
# colnames(discretos)[21]<-"Y_puntaje"
# colnames(discretos)[22]<-"Y_dx"
# colnames(discretos)[23]<-"Y_youngfill1"
# colnames(discretos)[24]<-"H_depresion"
# colnames(discretos)[25]<-"H_culpa"
# colnames(discretos)[26]<-"H_suicidio"
# colnames(discretos)[27]<-"H_iprecoz"
# colnames(discretos)[28]<-"H_imedio"
# colnames(discretos)[29]<-"H_itardio"
# colnames(discretos)[30]<-"H_actividad"
# colnames(discretos)[31]<-"H_inhibicion"
# colnames(discretos)[32]<-"H_agitacion"
# colnames(discretos)[33]<-"H_an_psiq"
# colnames(discretos)[34]<-"H_an_som"
# colnames(discretos)[35]<-"H_an_gastro"
# colnames(discretos)[36]<-"H_somatic"
# colnames(discretos)[37]<-"H_sexo"
# colnames(discretos)[38]<-"H_conciencia"
# colnames(discretos)[39]<-"H_hipocondria"
# colnames(discretos)[40]<-"H_peso"
# colnames(discretos)[41]<-"H_puntaje"
# colnames(discretos)[42]<-"H_dx"
# colnames(discretos)[43]<-"H_hrsdfill1"

