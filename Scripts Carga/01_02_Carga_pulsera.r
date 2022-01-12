path_bin<-"D:/Datos/Bip4Cast/pulsera/bin/"
path_bin_Proc<-"D:/Datos/Bip4Cast/pulsera/bin/Procesando/"
path_bin_Err<-"D:/Datos/Bip4Cast/pulsera/bin/Errores/"

destino_pdf<-"D:/Datos/Bip4Cast/pulsera/vis_sleep_pdf/"
destino_csv<-"D:/Datos/Bip4Cast/pulsera/csv/"

autoggir<-function() {
library(GGIR)

fichero_estado<-paste0(path_bin,"/estado.txt")

borrar_output<-TRUE

if (file.exists(fichero_estado)) 
  file.remove(fichero_estado)
  
file.create(fichero_estado)

for (archivo in dir(path_bin,pattern = "\\.bin$")){

    
    tcout<-tryCatch({

	cat(paste0(archivo,"\n"))
	archivosinext<-gsub(".bin","",archivo)

	
	# Cada BIN lo vamos a ir colocando en la carpeta "Procesando" (temporal) para que el GGIR lo vaya cogiendo de ahi
	cat(paste0("Fichero : ",archivo,"\n"))
	cat(paste0("Carpeta temp es ",path_bin_Proc,"\n"))
	
	options(warn=2)
	if(dir.exists(path_bin_Proc)) { 
		cat("Existe carpeta temporal, entonces borro carpeta Temporal\n")
		unlink (path_bin_Proc, recursive = TRUE,force=TRUE)
	}
	options(warn=1) 
	cat("Creo carpeta Temporal\n")
    dir.create(path_bin_Proc)	
    # Mueve a temporal el archivo
    origen <- paste0 (path_bin,"/",archivo)
    fin <- paste0 (path_bin_Proc,"/",archivo)
	cat("Muevo fichero a carpeta Temporal\n")
    file.rename (origen, fin)
	cat("Empiezo g.shell.GGIR\n")	
	
	
	# Creamos la carpeta de salida de cada BIN
	dirtemp_output<-paste0(destino_csv,"/","ggir_",archivosinext,"",sep="")
	options(warn=2)
	if(dir.exists(dirtemp_output)) { 
		cat("Existe carpeta temporal output, entonces la borro\n")
		unlink (dirtemp_output, recursive = TRUE,force=TRUE)
	}
	options(warn=1) 
	cat("Creo carpeta Temporal\n")
    dir.create(dirtemp_output)	
	

	
	      g.shell.GGIR(#=======================================
                   # INPUT NEEDED:
                   mode=c(1,2,3,4,5),
                   datadir=path_bin_Proc,
                   outputdir=dirtemp_output,
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
				   
	# Se crea carpeta donde se dejaran todos los csvs de cada BIN
	destino_csv_bin<-paste0(destino_csv,"/",archivosinext,sep="")
	
	options(warn=2)
	if(dir.exists(destino_csv_bin)) { 
		cat("Existe carpeta temporal output, entonces la borro\n")
		unlink (destino_csv_bin, recursive = TRUE,force=TRUE)
	}
	options(warn=1) 
	cat("Creo carpeta Temporal\n")
    dir.create(destino_csv_bin)	
				   

	
	
	origen_csvs<-paste0 (dirtemp_output,"/output_Procesando/results")
	cat(paste0("La carpeta donde GGIR genera los csvs es : ",origen_csvs,"\n"))
	
	
	for (archivo_csv in dir(origen_csvs,pattern = "\\.csv$")){
	
		origen_parteX_csv<-paste0(origen_csvs,"/",archivo_csv,sep="")
		destino_parteX_csv<-paste0(destino_csv_bin,"/",archivo_csv,sep="")
		cat(paste0("origen :",origen_parteX_csv))
		cat(paste0("destino :",destino_parteX_csv))		
		file.rename (from = origen_parteX_csv, to = destino_parteX_csv )
	
	}
				

	if (borrar_output) {
	
		options(warn=2)
		if(dir.exists(dirtemp_output)) { 
			cat("Borramos carpeta salida GGIR \n")
			unlink (dirtemp_output, recursive = TRUE,force=TRUE)
		}
		options(warn=1) 		
	}	
	
	
	cat("Todo OK, Regreso Fichero\n")
	fin_carg<-paste0(path_bin,"/",archivo)
	file.rename(fin,fin_carg)
	
	#Borramos temporal
	options(warn=2)
	if(dir.exists(path_bin_Proc)) { 
		cat("Borramos carpeta Temporal\n")
		unlink (path_bin_Proc, recursive = TRUE,force=TRUE)
	}
	options(warn=1) 	
	
	
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

}

if (binExecute) autoggir

dfs<-c("part2_daysummary","part2_summary","part4_nightsummary_sleep_cleaned","part4_summary_sleep_cleaned","part5_daysummary_WW_L30M100V400_T5A5","part5_personsummary_WW_L30M100V400_T5A5","part5_daysummary_WW_L30M100V400_T5A5.Old")



rm(list=dfs)

for (dfname in dfs) {
  
  c<-0
  l<-list()
  for (filepath in dir(destino_csv,recursive = T,pattern=paste0(dfname,"\\.csv$"))) {
    
    filename<-sub(".*/","",filepath)
    filetotalpath<-paste0(destino_csv,"/",filepath)
    
    l[[paste0(c,"")]]<-read.csv(filetotalpath,header=T,stringsAsFactors=F)
    c<-c+1
    
  }
  
  assign(dfname,as.data.frame(rbindlist(l, use.names=TRUE, fill=TRUE)))
  
  
  
}


rm(l)

for (dfname in dfs) {
  
  dftemp<-get(dfname)

  dftemp$ID<-ifelse(is.na(dftemp$ID) | dftemp$ID=="not stored in header",
                    toupper(substr(dftemp$filename,1,4)),
                    toupper(substr(dftemp$ID,1,4)))

  dftemp$ID<-ifelse(dftemp$ID=="CASG","CASGA",dftemp$ID)
  
  assign(dfname,dftemp)
}

rm(dftemp)



pulori<-part5_daysummary_WW_L30M100V400_T5A5

pulori<-pulori[!duplicated(pulori[c(1,4)]),]

colnames(pulori)[4]<-"fecha"
pulori$fecha<-as.Date(pulori$fecha)

colnames(pulori)[111]<-"id"

pulori<-pulori[!duplicated(pulori[c(1,4)]),]

pulori$S_NAfilled<-"1"

if (pulseraFillNA!="NONE") {

	pulseraColNames<-colnames(pulori)

	pulsera<-data.frame()
	###########################
	# AÃ±adimos fechas e interpolamos
	##############################
	#print("segundos unique")

	for (x in unique(pulori$id)){
	#for (x in c("GOGA")){
		dp <- pulori[pulori$id==x,]
		#if (nrow(dp)<=2) next #revisar


		tiempo.min <- min(dp$fecha)
		tiempo.max <- max(dp$fecha)

		#print(tiempo.min)
		#print(tiempo.max)
		rango.fechas <- as.Date (seq(tiempo.min, tiempo.max, by="day"))
		todas.fechas <- data.frame(list(fecha=rango.fechas))

		# Ordenamos sin dplyr. Ordenamos descendentemente por variable inicio
		dp<-dp[order(dp$fecha,decreasing=TRUE),]

		# Quitamos duplicados sin dplyr
		dp<-dp[!duplicated(dp$fecha),]

		dp <- merge(todas.fechas, dp, all.x=T)
		dp$S_NAfilled<-ifelse(is.na(dp$S_NAfilled),"S","REAL")
		dp$id <- x


		varsToExclude<-c("S_NAfilled","id","fecha","filename","weekday","sleeponset_ts","wakeup_ts","guider","L5TIME","M5TIME","L10TIME","M10TIME","boutdur.in","boutdur.lig","daytype")
		
		for (vr in pulseraColNames[!pulseraColNames %in% varsToExclude])	{	
			if (pulseraFillNA=="INTER") if(nrow(dp[!is.na(dp[[vr]]),])>2) { dp[[vr]]<-na_interpolation(dp[[vr]]) }				
			if (pulseraFillNA=="DRAG") {
				dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE) 
				dp[[vr]]<-na.locf((dp[[vr]]),na.rm=FALSE,fromLast = TRUE) 			
			}
		}

		# if(nrow(dp[!is.na(dp$window_number                  ),])>2){ dp$window_number                   <-na_interpolation(dp$window_number                 )}
		# if(nrow(dp[!is.na(dp$sleeponset                     ),])>2){ dp$sleeponset                      <-na_interpolation(dp$sleeponset                    )}
		# #if(nrow(dp[!is.na(dp$sleeponset_ts                  ),])>2){ dp$sleeponset_ts                   <-na_interpolation(dp$sleeponset_ts                 )}
		# if(nrow(dp[!is.na(dp$wakeup                         ),])>2){ dp$wakeup                          <-na_interpolation(dp$wakeup                        )}
		# #if(nrow(dp[!is.na(dp$wakeup_ts                      ),])>2){ dp$wakeup_ts                       <-na_interpolation(dp$wakeup_ts                     )}
		# if(nrow(dp[!is.na(dp$night_number                   ),])>2){ dp$night_number                    <-na_interpolation(dp$night_number                  )}
		# if(nrow(dp[!is.na(dp$daysleeper                     ),])>2){ dp$daysleeper                      <-na_interpolation(dp$daysleeper                    )}
		# if(nrow(dp[!is.na(dp$cleaningcode                   ),])>2){ dp$cleaningcode                    <-na_interpolation(dp$cleaningcode                  )}
		# #if(nrow(dp[!is.na(dp$guider                         ),])>2){ dp$guider                          <-na_interpolation(dp$guider                        )}
		# if(nrow(dp[!is.na(dp$sleeplog_used                  ),])>2){ dp$sleeplog_used                   <-na_interpolation(dp$sleeplog_used                 )}
		# if(nrow(dp[!is.na(dp$acc_available                  ),])>2){ dp$acc_available                   <-na_interpolation(dp$acc_available                 )}
		# if(nrow(dp[!is.na(dp$nonwear_perc_day               ),])>2){ dp$nonwear_perc_day                <-na_interpolation(dp$nonwear_perc_day              )}
		# if(nrow(dp[!is.na(dp$nonwear_perc_spt               ),])>2){ dp$nonwear_perc_spt                <-na_interpolation(dp$nonwear_perc_spt              )}
		# if(nrow(dp[!is.na(dp$nonwear_perc_day_spt           ),])>2){ dp$nonwear_perc_day_spt            <-na_interpolation(dp$nonwear_perc_day_spt          )}
		# if(nrow(dp[!is.na(dp$dur_spt_sleep_min              ),])>2){ dp$dur_spt_sleep_min               <-na_interpolation(dp$dur_spt_sleep_min             )}
		# if(nrow(dp[!is.na(dp$dur_spt_wake_IN_min            ),])>2){ dp$dur_spt_wake_IN_min             <-na_interpolation(dp$dur_spt_wake_IN_min           )}
		# if(nrow(dp[!is.na(dp$dur_spt_wake_LIG_min           ),])>2){ dp$dur_spt_wake_LIG_min            <-na_interpolation(dp$dur_spt_wake_LIG_min          )}
		# if(nrow(dp[!is.na(dp$dur_spt_wake_MOD_min           ),])>2){ dp$dur_spt_wake_MOD_min            <-na_interpolation(dp$dur_spt_wake_MOD_min          )}
		# if(nrow(dp[!is.na(dp$dur_spt_wake_VIG_min           ),])>2){ dp$dur_spt_wake_VIG_min            <-na_interpolation(dp$dur_spt_wake_VIG_min          )}
		# if(nrow(dp[!is.na(dp$dur_day_IN_unbt_min            ),])>2){ dp$dur_day_IN_unbt_min             <-na_interpolation(dp$dur_day_IN_unbt_min           )}
		# if(nrow(dp[!is.na(dp$dur_day_LIG_unbt_min           ),])>2){ dp$dur_day_LIG_unbt_min            <-na_interpolation(dp$dur_day_LIG_unbt_min          )}
		# if(nrow(dp[!is.na(dp$dur_day_MOD_unbt_min           ),])>2){ dp$dur_day_MOD_unbt_min            <-na_interpolation(dp$dur_day_MOD_unbt_min          )}
		# if(nrow(dp[!is.na(dp$dur_day_VIG_unbt_min           ),])>2){ dp$dur_day_VIG_unbt_min            <-na_interpolation(dp$dur_day_VIG_unbt_min          )}
		# if(nrow(dp[!is.na(dp$dur_day_MVPA_bts_1_min         ),])>2){ dp$dur_day_MVPA_bts_1_min          <-na_interpolation(dp$dur_day_MVPA_bts_1_min        )}
		# if(nrow(dp[!is.na(dp$dur_day_IN_bts_30_min          ),])>2){ dp$dur_day_IN_bts_30_min           <-na_interpolation(dp$dur_day_IN_bts_30_min         )}
		# if(nrow(dp[!is.na(dp$dur_day_IN_bts_10_30_min       ),])>2){ dp$dur_day_IN_bts_10_30_min        <-na_interpolation(dp$dur_day_IN_bts_10_30_min      )}
		# if(nrow(dp[!is.na(dp$dur_day_IN_bts_1_10_min        ),])>2){ dp$dur_day_IN_bts_1_10_min         <-na_interpolation(dp$dur_day_IN_bts_1_10_min       )}
		# if(nrow(dp[!is.na(dp$dur_day_LIG_bts_10_min         ),])>2){ dp$dur_day_LIG_bts_10_min          <-na_interpolation(dp$dur_day_LIG_bts_10_min        )}
		# if(nrow(dp[!is.na(dp$dur_day_LIG_bts_1_10_min       ),])>2){ dp$dur_day_LIG_bts_1_10_min        <-na_interpolation(dp$dur_day_LIG_bts_1_10_min      )}
		# if(nrow(dp[!is.na(dp$dur_day_total_IN_min           ),])>2){ dp$dur_day_total_IN_min            <-na_interpolation(dp$dur_day_total_IN_min          )}
		# if(nrow(dp[!is.na(dp$dur_day_total_LIG_min          ),])>2){ dp$dur_day_total_LIG_min           <-na_interpolation(dp$dur_day_total_LIG_min         )}
		# if(nrow(dp[!is.na(dp$dur_day_total_MOD_min          ),])>2){ dp$dur_day_total_MOD_min           <-na_interpolation(dp$dur_day_total_MOD_min         )}
		# if(nrow(dp[!is.na(dp$dur_day_total_VIG_min          ),])>2){ dp$dur_day_total_VIG_min           <-na_interpolation(dp$dur_day_total_VIG_min         )}
		# if(nrow(dp[!is.na(dp$dur_day_min                    ),])>2){ dp$dur_day_min                     <-na_interpolation(dp$dur_day_min                   )}
		# if(nrow(dp[!is.na(dp$dur_spt_min                    ),])>2){ dp$dur_spt_min                     <-na_interpolation(dp$dur_spt_min                   )}
		# if(nrow(dp[!is.na(dp$dur_day_spt_min                ),])>2){ dp$dur_day_spt_min                 <-na_interpolation(dp$dur_day_spt_min               )}
		# if(nrow(dp[!is.na(dp$N_atleast5minwakenight         ),])>2){ dp$N_atleast5minwakenight          <-na_interpolation(dp$N_atleast5minwakenight        )}
		# if(nrow(dp[!is.na(dp$sleep_efficiency               ),])>2){ dp$sleep_efficiency                <-na_interpolation(dp$sleep_efficiency              )}
		# if(nrow(dp[!is.na(dp$ACC_spt_sleep_mg               ),])>2){ dp$ACC_spt_sleep_mg                <-na_interpolation(dp$ACC_spt_sleep_mg              )}
		# if(nrow(dp[!is.na(dp$ACC_spt_wake_IN_mg             ),])>2){ dp$ACC_spt_wake_IN_mg              <-na_interpolation(dp$ACC_spt_wake_IN_mg            )}
		# if(nrow(dp[!is.na(dp$ACC_spt_wake_LIG_mg            ),])>2){ dp$ACC_spt_wake_LIG_mg             <-na_interpolation(dp$ACC_spt_wake_LIG_mg           )}
		# if(nrow(dp[!is.na(dp$ACC_spt_wake_MOD_mg            ),])>2){ dp$ACC_spt_wake_MOD_mg             <-na_interpolation(dp$ACC_spt_wake_MOD_mg           )}
		# if(nrow(dp[!is.na(dp$ACC_spt_wake_VIG_mg            ),])>2){ dp$ACC_spt_wake_VIG_mg             <-na_interpolation(dp$ACC_spt_wake_VIG_mg           )}
		# if(nrow(dp[!is.na(dp$ACC_day_IN_unbt_mg             ),])>2){ dp$ACC_day_IN_unbt_mg              <-na_interpolation(dp$ACC_day_IN_unbt_mg            )}
		# if(nrow(dp[!is.na(dp$ACC_day_LIG_unbt_mg            ),])>2){ dp$ACC_day_LIG_unbt_mg             <-na_interpolation(dp$ACC_day_LIG_unbt_mg           )}
		# if(nrow(dp[!is.na(dp$ACC_day_MOD_unbt_mg            ),])>2){ dp$ACC_day_MOD_unbt_mg             <-na_interpolation(dp$ACC_day_MOD_unbt_mg           )}
		# if(nrow(dp[!is.na(dp$ACC_day_VIG_unbt_mg            ),])>2){ dp$ACC_day_VIG_unbt_mg             <-na_interpolation(dp$ACC_day_VIG_unbt_mg           )}
		# if(nrow(dp[!is.na(dp$ACC_day_MVPA_bts_1_mg          ),])>2){ dp$ACC_day_MVPA_bts_1_mg           <-na_interpolation(dp$ACC_day_MVPA_bts_1_mg         )}
		# if(nrow(dp[!is.na(dp$ACC_day_IN_bts_30_mg           ),])>2){ dp$ACC_day_IN_bts_30_mg            <-na_interpolation(dp$ACC_day_IN_bts_30_mg          )}
		# if(nrow(dp[!is.na(dp$ACC_day_IN_bts_10_30_mg        ),])>2){ dp$ACC_day_IN_bts_10_30_mg         <-na_interpolation(dp$ACC_day_IN_bts_10_30_mg       )}
		# if(nrow(dp[!is.na(dp$ACC_day_IN_bts_1_10_mg         ),])>2){ dp$ACC_day_IN_bts_1_10_mg          <-na_interpolation(dp$ACC_day_IN_bts_1_10_mg        )}
		# if(nrow(dp[!is.na(dp$ACC_day_LIG_bts_10_mg          ),])>2){ dp$ACC_day_LIG_bts_10_mg           <-na_interpolation(dp$ACC_day_LIG_bts_10_mg         )}
		# if(nrow(dp[!is.na(dp$ACC_day_LIG_bts_1_10_mg        ),])>2){ dp$ACC_day_LIG_bts_1_10_mg         <-na_interpolation(dp$ACC_day_LIG_bts_1_10_mg       )}
		# if(nrow(dp[!is.na(dp$ACC_day_total_IN_mg            ),])>2){ dp$ACC_day_total_IN_mg             <-na_interpolation(dp$ACC_day_total_IN_mg           )}
		# if(nrow(dp[!is.na(dp$ACC_day_total_LIG_mg           ),])>2){ dp$ACC_day_total_LIG_mg            <-na_interpolation(dp$ACC_day_total_LIG_mg          )}
		# if(nrow(dp[!is.na(dp$ACC_day_total_MOD_mg           ),])>2){ dp$ACC_day_total_MOD_mg            <-na_interpolation(dp$ACC_day_total_MOD_mg          )}
		# if(nrow(dp[!is.na(dp$ACC_day_total_VIG_mg           ),])>2){ dp$ACC_day_total_VIG_mg            <-na_interpolation(dp$ACC_day_total_VIG_mg          )}
		# if(nrow(dp[!is.na(dp$ACC_day_mg                     ),])>2){ dp$ACC_day_mg                      <-na_interpolation(dp$ACC_day_mg                    )}
		# if(nrow(dp[!is.na(dp$ACC_spt_mg                     ),])>2){ dp$ACC_spt_mg                      <-na_interpolation(dp$ACC_spt_mg                    )}
		# if(nrow(dp[!is.na(dp$ACC_day_spt_mg                 ),])>2){ dp$ACC_day_spt_mg                  <-na_interpolation(dp$ACC_day_spt_mg                )}
		# if(nrow(dp[!is.na(dp$quantile_mostactive60min_mg    ),])>2){ dp$quantile_mostactive60min_mg     <-na_interpolation(dp$quantile_mostactive60min_mg   )}
		# if(nrow(dp[!is.na(dp$quantile_mostactive30min_mg    ),])>2){ dp$quantile_mostactive30min_mg     <-na_interpolation(dp$quantile_mostactive30min_mg   )}
		# #if(nrow(dp[!is.na(dp$L5TIME                         ),])>2){ dp$L5TIME                          <-na_interpolation(dp$L5TIME                        )}
		# if(nrow(dp[!is.na(dp$L5VALUE                        ),])>2){ dp$L5VALUE                         <-na_interpolation(dp$L5VALUE                       )}
		# #if(nrow(dp[!is.na(dp$M5TIME                         ),])>2){ dp$M5TIME                          <-na_interpolation(dp$M5TIME                        )}
		# if(nrow(dp[!is.na(dp$M5VALUE                        ),])>2){ dp$M5VALUE                         <-na_interpolation(dp$M5VALUE                       )}
		# if(nrow(dp[!is.na(dp$L5TIME_num                     ),])>2){ dp$L5TIME_num                      <-na_interpolation(dp$L5TIME_num                    )}
		# if(nrow(dp[!is.na(dp$M5TIME_num                     ),])>2){ dp$M5TIME_num                      <-na_interpolation(dp$M5TIME_num                    )}
		# #if(nrow(dp[!is.na(dp$L10TIME                        ),])>2){ dp$L10TIME                         <-na_interpolation(dp$L10TIME                       )}
		# if(nrow(dp[!is.na(dp$L10VALUE                       ),])>2){ dp$L10VALUE                        <-na_interpolation(dp$L10VALUE                      )}
		# #if(nrow(dp[!is.na(dp$M10TIME                        ),])>2){ dp$M10TIME                         <-na_interpolation(dp$M10TIME                       )}
		# if(nrow(dp[!is.na(dp$M10VALUE                       ),])>2){ dp$M10VALUE                        <-na_interpolation(dp$M10VALUE                      )}
		# if(nrow(dp[!is.na(dp$L10TIME_num                    ),])>2){ dp$L10TIME_num                     <-na_interpolation(dp$L10TIME_num                   )}
		# if(nrow(dp[!is.na(dp$M10TIME_num                    ),])>2){ dp$M10TIME_num                     <-na_interpolation(dp$M10TIME_num                   )}
		# if(nrow(dp[!is.na(dp$Nbouts_day_MVPA_bts_1          ),])>2){ dp$Nbouts_day_MVPA_bts_1           <-na_interpolation(dp$Nbouts_day_MVPA_bts_1         )}
		# if(nrow(dp[!is.na(dp$Nbouts_day_IN_bts_30           ),])>2){ dp$Nbouts_day_IN_bts_30            <-na_interpolation(dp$Nbouts_day_IN_bts_30          )}
		# if(nrow(dp[!is.na(dp$Nbouts_day_IN_bts_10_30        ),])>2){ dp$Nbouts_day_IN_bts_10_30         <-na_interpolation(dp$Nbouts_day_IN_bts_10_30       )}
		# if(nrow(dp[!is.na(dp$Nbouts_day_IN_bts_1_10         ),])>2){ dp$Nbouts_day_IN_bts_1_10          <-na_interpolation(dp$Nbouts_day_IN_bts_1_10        )}
		# if(nrow(dp[!is.na(dp$Nbouts_day_LIG_bts_10          ),])>2){ dp$Nbouts_day_LIG_bts_10           <-na_interpolation(dp$Nbouts_day_LIG_bts_10         )}
		# if(nrow(dp[!is.na(dp$Nbouts_day_LIG_bts_1_10        ),])>2){ dp$Nbouts_day_LIG_bts_1_10         <-na_interpolation(dp$Nbouts_day_LIG_bts_1_10       )}
		# if(nrow(dp[!is.na(dp$Nblocks_spt_sleep              ),])>2){ dp$Nblocks_spt_sleep               <-na_interpolation(dp$Nblocks_spt_sleep             )}
		# if(nrow(dp[!is.na(dp$Nblocks_spt_wake_IN            ),])>2){ dp$Nblocks_spt_wake_IN             <-na_interpolation(dp$Nblocks_spt_wake_IN           )}
		# if(nrow(dp[!is.na(dp$Nblocks_spt_wake_LIG           ),])>2){ dp$Nblocks_spt_wake_LIG            <-na_interpolation(dp$Nblocks_spt_wake_LIG          )}
		# if(nrow(dp[!is.na(dp$Nblocks_spt_wake_MOD           ),])>2){ dp$Nblocks_spt_wake_MOD            <-na_interpolation(dp$Nblocks_spt_wake_MOD          )}
		# if(nrow(dp[!is.na(dp$Nblocks_spt_wake_VIG           ),])>2){ dp$Nblocks_spt_wake_VIG            <-na_interpolation(dp$Nblocks_spt_wake_VIG          )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_IN_unbt            ),])>2){ dp$Nblocks_day_IN_unbt             <-na_interpolation(dp$Nblocks_day_IN_unbt           )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_LIG_unbt           ),])>2){ dp$Nblocks_day_LIG_unbt            <-na_interpolation(dp$Nblocks_day_LIG_unbt          )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_MOD_unbt           ),])>2){ dp$Nblocks_day_MOD_unbt            <-na_interpolation(dp$Nblocks_day_MOD_unbt          )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_VIG_unbt           ),])>2){ dp$Nblocks_day_VIG_unbt            <-na_interpolation(dp$Nblocks_day_VIG_unbt          )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_MVPA_bts_1         ),])>2){ dp$Nblocks_day_MVPA_bts_1          <-na_interpolation(dp$Nblocks_day_MVPA_bts_1        )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_IN_bts_30          ),])>2){ dp$Nblocks_day_IN_bts_30           <-na_interpolation(dp$Nblocks_day_IN_bts_30         )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_IN_bts_10_30       ),])>2){ dp$Nblocks_day_IN_bts_10_30        <-na_interpolation(dp$Nblocks_day_IN_bts_10_30      )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_IN_bts_1_10        ),])>2){ dp$Nblocks_day_IN_bts_1_10         <-na_interpolation(dp$Nblocks_day_IN_bts_1_10       )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_LIG_bts_10         ),])>2){ dp$Nblocks_day_LIG_bts_10          <-na_interpolation(dp$Nblocks_day_LIG_bts_10        )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_LIG_bts_1_10       ),])>2){ dp$Nblocks_day_LIG_bts_1_10        <-na_interpolation(dp$Nblocks_day_LIG_bts_1_10      )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_total_IN           ),])>2){ dp$Nblocks_day_total_IN            <-na_interpolation(dp$Nblocks_day_total_IN          )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_total_LIG          ),])>2){ dp$Nblocks_day_total_LIG           <-na_interpolation(dp$Nblocks_day_total_LIG         )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_total_MOD          ),])>2){ dp$Nblocks_day_total_MOD           <-na_interpolation(dp$Nblocks_day_total_MOD         )}
		# if(nrow(dp[!is.na(dp$Nblocks_day_total_VIG          ),])>2){ dp$Nblocks_day_total_VIG           <-na_interpolation(dp$Nblocks_day_total_VIG         )}
		# if(nrow(dp[!is.na(dp$boutcriter.in                  ),])>2){ dp$boutcriter.in                   <-na_interpolation(dp$boutcriter.in                 )}
		# if(nrow(dp[!is.na(dp$boutcriter.lig                 ),])>2){ dp$boutcriter.lig                  <-na_interpolation(dp$boutcriter.lig                )}
		# if(nrow(dp[!is.na(dp$boutcriter.mvpa                ),])>2){ dp$boutcriter.mvpa                 <-na_interpolation(dp$boutcriter.mvpa               )}
		# #if(nrow(dp[!is.na(dp$boutdur.in                     ),])>2){ dp$boutdur.in                      <-na_interpolation(dp$boutdur.in                    )}
		# #if(nrow(dp[!is.na(dp$boutdur.lig                    ),])>2){ dp$boutdur.lig                     <-na_interpolation(dp$boutdur.lig                   )}
		# if(nrow(dp[!is.na(dp$boutdur.mvpa                   ),])>2){ dp$boutdur.mvpa                    <-na_interpolation(dp$boutdur.mvpa                  )}
		# if(nrow(dp[!is.na(dp$bout.metric                    ),])>2){ dp$bout.metric                     <-na_interpolation(dp$bout.metric                   )}
		# #if(nrow(dp[!is.na(dp$daytype                        ),])>2){ dp$daytype                         <-na_interpolation(dp$daytype                       )}

		pulsera<-rbind(pulsera,dp)
	}

} else {
	pulori$S_NAfilled<-"REAL"
	pulsera<-pulori
}


pulsera$pulserafill1<-1