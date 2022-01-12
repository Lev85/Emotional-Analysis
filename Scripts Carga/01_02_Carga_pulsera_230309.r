
path_bin<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN"
path_bin_Car<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN/Cargados"
path_bin_Err<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN/Errores"


destino_pdf<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/2018-Pavel LLamocca/Pavel/datos_pulsera/vis_sleep_pdf"
destino_csv<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/2018-Pavel LLamocca/Pavel/datos_pulsera/csv"

c<-0
for (archivo in dir(path_bin,pattern = "\\.bin$")){


    tcout<-tryCatch({
	c<-c+1
	archivosinext<-gsub(".bin","",archivo)

	cat(paste0("Fichero : ",archivo,"\n"))
	dirtemp<-paste0(path_bin_Car,"/temp",c)
	cat(paste0("Carpeta temp es ",dirtemp,"\n"))
	
	options(warn=2)
	if(dir.exists(dirtemp)) { 
		cat("Existe carpeta temporal, entonces borro carpeta Temporal\n")
		unlink (dirtemp, recursive = TRUE,force=TRUE)
	}
	options(warn=1) 
	cat("Creo carpeta Temporal\n")
    dir.create(dirtemp)	
	
	
    # Mueve a temporal el archivo
    origen <- paste0 (path_bin,"/",archivo)
    fin <- paste0 (dirtemp,"/",archivo)
	cat("Muevo fichero a carpeta Temporal\n")
    file.rename (origen, fin)
	cat("Empiezo g.shell.GGIR\n")
	
	      g.shell.GGIR(#=======================================
                   # INPUT NEEDED:
                   mode=c(1,2,3,4,5),
                   datadir=dirtemp,
                   outputdir=dirtemp,
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
                   # strategy antes 2, ndays antes 7, maxdur antes 9
                   strategy = 3,               ndayswindow=300,
                   hrs.del.start = 0,          hrs.del.end = 0,
                   maxdur = 900,                 includedaycrit = 16,
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
                   
                   ##nnights antes 9, ahora 200, outliers only antes T, ahora F
                   excludefirstlast = TRUE,
                   includenightcrit = 16,
                   def.noc.sleep = c(),
                   outliers.only = FALSE,
                   criterror = 4,
                   relyonsleeplog = FALSE,
                   sleeplogidnum = TRUE,
                   colid=1,
                   coln1=2,
                   do.visual = TRUE,
                   nnights = 200 ,
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
				   
	
				
    origen_p5csv <- paste0 (dirtemp,"/output_temp",c,"/results/part5_daysummary_WW_L30M100V400_T5A5.csv")
    origen_sleeppdf <- paste0(dirtemp,"/output_temp",c,"/results/visualisation_sleep.pdf")

	destino_p5csv<-paste0(destino_csv,"/",archivosinext,".csv")
	destino_sleeppdf<-paste0(destino_pdf,"/",archivosinext,".pdf")
	
    file.rename (from = origen_p5csv, to = destino_p5csv )
    file.rename (from = origen_sleeppdf, to = destino_sleeppdf  )

	
	
	#cat("Todo OK, por eso borro temporal\n")
	#unlink (dirtemp, recursive = TRUE,force=TRUE)
	#Sys.sleep(2)
	#cat("Muevo fichero a carpeta Cargados\n")		
	cat("Todo OK, Regreso Fichero\n")
	fin_carg<-paste0(path_bin_Car,"/",archivo)
	file.rename(fin,fin_carg)
	
	write.table( data.frame(archivo=archivo,fecha=date(),error="OK"),paste0(path_bin,"/estado.txt"),row.names=FALSE,col.names=FALSE,append=TRUE)
	
	},
	error = function(e){ 

		fin_err <- paste0(path_bin_Err,"/",archivo)
		#cat("Muevo fichero a carpeta Errores\n")
		#file.rename (origen, finErr)
		cat("Error, Regreso Fichero\n")
		file.rename(fin,fin_err)

		write.table( data.frame(archivo=archivo,fecha=date(),error=as.character(e)),paste0(path_bin,"/estado.txt"),row.names=FALSE,col.names=FALSE,append=TRUE)

	}
	)	
	
	if(inherits(tcout, "error")) {	
	next
	}
    

}