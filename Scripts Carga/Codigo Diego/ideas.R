########### COSAS PARA HACER

#SCORES Teóricos: actividad y energía....

#ALARMAS
# de cenit y de tiempo de sueño con varianzas
# de cambio de patron de sueño
# de NA en campos clave como dormir y despertar procesados... (para evitar errores)
# de aumento de actividad: duración del sueño, irritabilidad, ansiedad,subri animo, subir motivación, rapidez,
# de no rellenar el diario


#mandador automático de datos a pacientes, una gráfica. por correo, a médico: que mande las observaciones tambiEn


#añadir farmacos, dosis, niveles


`r a <- 3`
```{asis, eval=(a == 2), echo=TRUE}
texto condicional
```

#GRAFICOS



unirdatosantiguo <- function(directorioggir1,individuo) {
  ficheroscsv <- dir (directorioggir1, pattern = "csv")
  ggirdatatotal <- read.csv(paste (directorioggir1,ficheroscsv[1],sep ="")) #inicializo este ficher
  colnames(ggirdatatotal)[4]<-"fecha"                      #cambio nombre para que coincida
  ggirdatatotal$fecha<- dmy(ggirdatatotal$fecha)                #cambio formato de fecha
  # y le añado el resto de csv's
  for (archivo in ficheroscsv) {
    ggirdata <- read.csv(paste (directorioggir1,archivo,sep =""))
    colnames(ggirdata)[4]<-"fecha"                      #cambio nombre para que coincida
    ggirdata$fecha<- dmy(ggirdata$fecha)                #cambio formato de fecha
    ggirdatatotal <- full_join (ggirdatatotal,ggirdata, all = TRUE)
  }
  individual (individuo)->diarioindividual            #genero individual
  # y hago un join con los csv's (ggirdatatotal)
  datospaciente <- full_join (x = diarioindividual,y = ggirdatatotal, by = 'fecha' , copy=FALSE)   #lo uno todo
  return (datospaciente)
}
