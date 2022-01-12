
path_bin<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN"
path_bin_Car<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN/Cargados"
path_bin_Err<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN/Errores"


destino_pdf<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/2018-Pavel LLamocca/Pavel/datos_pulsera/vis_sleep_pdf"
destino_csv<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/2018-Pavel LLamocca/Pavel/datos_pulsera/csv"


for (archivo in dir(path_bin,pattern = "\\.bin$")){

    tcout<-tryCatch({
	 


	cat(paste0("Fichero : ",archivo,"\n"))
	dirtemp<-paste0(path_bin,"/temporal")
	
	if(dir.exists(dirtemp)) { 
		cat("Existe carpeta temporal, entonces borro carpeta Temporal\n")
		unlink (dirtemp, recursive = TRUE,force=TRUE)
	}
	 
	cat("Creo carpeta Temporal\n")
    dir.create(dirtemp)	
	
	log("aa")
	
    # copio a temporal el archivo
    origen <- paste0 (path_bin,"/",archivo)
    fin <- paste0 (dirtemp,"/",archivo)
	cat("Copio fichero a carpeta Temporal\n")
    file.copy (origen, fin)
	cat("Empiezo g.shell.GGIR\n")
	
	      
	
	
	cat("Todo OK, por eso borro temporal\n")
	unlink (dirtemp, recursive = TRUE,force=TRUE)
	cat("Muevo fichero a carpeta Cargados\n")		
	file.rename(origen,paste0(path_bin_Car,"/",archivo))
	
	},
	error = function(e){ 

		finErr <- paste0(path_bin_Err,"/",archivo)
		cat("Muevo fichero a carpeta Errores\n")
		file.rename (origen, finErr)

		
		write.table( data.frame(archivo=archivo,fecha=date(),error=as.character(e)),paste0(path_bin_Err,"/errores.txt"),row.names=FALSE,col.names=FALSE,append=TRUE)

	}
	)	
	
	if(inherits(tcout, "error")) {
	Sys.sleep(2)
	next
	}
    Sys.sleep(2)

    

}