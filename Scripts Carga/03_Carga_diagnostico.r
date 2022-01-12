script_intervenciones<-"03_01_Carga_intervenciones.r"
source(paste0(path_scripts,script_intervenciones))
cat("Termino de cargar intervenciones\n")

script_eventos<-"03_02_Carga_eventos.r"
source(paste0(path_scripts,script_eventos))
cat("Termino de cargar eventos\n")



if (foj_diagnostico) {
diagnosticos<-merge(intervenciones,eventos,by=c("fecha","id"),all=T)
} else {
diagnosticos<-merge(intervenciones,eventos,by=c("fecha","id"),all.x=T)
}
