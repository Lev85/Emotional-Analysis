cat("Empiezo a cargar diario\n")
script_diario<-"01_01_Carga_diario.r"
source(paste0(path_scripts,script_diario))
cat("Empiezo a cargar pulsera\n")
script_pulsera<-"01_02_Carga_pulsera.r"
source(paste0(path_scripts,script_pulsera))


if (foj_continuos) {
continuos<-merge(diario,pulsera,by=c("fecha","id"),all=T)
} else {
continuos<-merge(diario,pulsera,by=c("fecha","id"),all.y=T)
}

#colnamesContinuos<-colnames(continuos)
#colnamesToExclude<-colnamesContinuos[grepl('^NAfilled',colnamesContinuos)]

#continuos<-continuos[ , !names(continuos) %in% colnamesToExclude]
