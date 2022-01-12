autoggir <- function (directorio1){

#Automatizar conversión
# le decimos el directorio
# hace un directorio temporal
# copia allí el primer archivo
# ejecuta el script...
# copia los archivos de interes (2)
# borra temporal


#pongo el directorio a procesar
#directorio1 <- "GENEACTIV UBIP/GIUS"
  
listabin <- dir (directorio1) #lista de archivos a procesar
message (" voy a procesar ", listabin)
message ("__________________")
for(archivo in listabin) {
  message("     PROCESO ",archivo)
  message ("__________________")
  # creo directorio temporal
  dir.create(paste (directorio1,"/temporal",sep = ""))
  # copio a temporal el archivo
  origen <- paste (directorio1,"/",archivo, sep = "")
  fin <- paste (directorio1,"/temporal/","trabajo.bin", sep = "")
  file.copy (origen, fin)
  
  
  #### LO PROCESO 
  {
    
    directorio <- paste (directorio1,"/temporal/", sep ="")
    getwd()
    #setwd("") # Directorio en el que se encuentra el .bin
    # Es importante poner la misma ruta en "datadir" y "outputdir"
    g.shell.GGIR(#=======================================
                 # INPUT NEEDED:
                 mode=c(1,2,3,4,5),
                 datadir=directorio,
                 outputdir=directorio,
                 f0=1, f1=2,
                 #-------------------------------
                 # Part 1:
                 #-------------------------------
                 # Key functions: reading file, auto-calibration, and extracting features
                 do.enmo = TRUE,             do.anglez=TRUE,
                 chunksize=1,                printsummary=TRUE,
                 #-------------------------------
                 # Part 2:
                 #-------------------------------
                 strategy = 2,               ndayswindow=7,
                 hrs.del.start = 0,          hrs.del.end = 0,
                 maxdur = 9,                 includedaycrit = 16,
                 winhr = c(5,10),
                 qlevels = c(c(1380/1440),c(1410/1440)),
                 qwindow=c(0,24),
                 ilevels = c(seq(0,400,by=50),8000),
                 mvpathreshold =c(100,120), #Umbral de ejercicio MVPT!!!
                 bout.metric = 4,
                 closedbout=FALSE,
                 #-------------------------------
                 # Part 3:
                 #-------------------------------
                 # Key functions: Sleep detection
                 timethreshold= c(5),        anglethreshold=5,
                 ignorenonwear = TRUE,
                 #-------------------------------
                 # Part 4:
                 #-------------------------------
                 # Key functions: Integrating sleep log (if available) with sleep detection
                 # storing day and person specific summaries of sleep
                 excludefirstlast = TRUE,
                 includenightcrit = 16,
                 def.noc.sleep = c(),
                 outliers.only = TRUE,
                 criterror = 4,
                 relyonsleeplog = FALSE,
                 sleeplogidnum = TRUE,
                 colid=1,
                 coln1=2,
                 do.visual = TRUE,
                 nnights = 9,
                 #-------------------------------
                 # Part 5:
                 # Key functions: Merging physical activity with sleep analyses
                 #-------------------------------
                 threshold.lig = c(30), threshold.mod = c(100),  threshold.vig = c(400),
                 boutcriter = 0.8,      boutcriter.in = 0.9,     boutcriter.lig = 0.8,
                 boutcriter.mvpa = 0.8, boutdur.in = c(1,10,30), boutdur.lig = c(1,10),
                 boutdur.mvpa = c(1),   timewindow = c("WW"),
                 #-----------------------------------
                 # Report generation
                 #-------------------------------
                 # Key functions: Generating reports based on meta-data
                 do.report=c(2,4,5),
                 visualreport=TRUE,     dofirstpage = TRUE,
                 viewingwindow=1)
    
  }
  
 archivopdf <- paste (directorio1,"/temporal/output_temporal/results/file summary reports/Report_trabajo.bin.pdf", sep = "")
 archivop5 <- paste (directorio1,"/temporal/output_temporal/results/part5_daysummary_WW_L30M100V400_T5A5.csv", sep = "")
 pdffin <- paste (directorio1,"/",archivo,".pdf",sep="")
 csvfin <- paste (directorio1, "/",archivo,"_p5.csv",sep="")
 file.copy (from = archivopdf, to = pdffin,overwrite = TRUE )
 file.copy (from = archivop5, to = csvfin,overwrite = TRUE  )
#borrar temporaldir.
 unlink (paste (directorio1,"/temporal",sep = ""), recursive = TRUE)

  
 
}
}
#le damos el directorio y procesa los bin
#deja los csv y los pdf en el mismo directorio



  
  

