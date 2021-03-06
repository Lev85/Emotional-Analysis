path_bin<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN"
path_bin_Car<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN/Cargados"
path_bin_Err<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN/Errores"

destino_pdf<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/2018-Pavel LLamocca/Pavel/datos_pulsera/vis_sleep_pdf"
destino_csv<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/2018-Pavel LLamocca/Pavel/datos_pulsera/csv"


autoggir<-function() {

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

}

#autoggir()
setwd(destino_csv)

pulori<-data.frame()
for (archivo in dir(destino_csv,pattern = "\\.csv$")){
  cat(paste0("last1 ",archivo,"\n"))
	pulori<-rbind(pulori,read.csv(archivo))
}
cat("last2\n")

pulori$id<-as.character(pulori$id)
colnames(pulori)[4]<-"fecha"
pulori$fecha<-as.Date(pulori$fecha)

pulori<-pulori[!duplicated(pulori[c(1,4)]),]

if (interpolarPulsera) {


	pulsera<-data.frame()
	###########################
	# A??adimos fechas e interpolamos
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
	dp$id <- x


	if(nrow(dp[!is.na(dp$acc_onset                  ),])>2){dp$acc_onset                  <-na_interpolation(dp$acc_onset                  			)} #	: num  22.3 31 24 25.5 24.8 ...
	if(nrow(dp[!is.na(dp$acc_wake                   ),])>2){dp$acc_wake                   <-na_interpolation(dp$acc_wake                   			)} #	: num  34 35.7 35.7 34.5 36 ...
	if(nrow(dp[!is.na(dp$sleeplog_onset             ),])>2){dp$sleeplog_onset             <-na_interpolation(dp$sleeplog_onset             			)} #	: num  22.2 25.2 23.8 26 25.6 ...
	if(nrow(dp[!is.na(dp$sleeplog_wake              ),])>2){dp$sleeplog_wake              <-na_interpolation(dp$sleeplog_wake              			)} #	: num  34.2 37.2 35.8 38 37.6 ...
	if(nrow(dp[!is.na(dp$window_length_in_hours     ),])>2){dp$window_length_in_hours     <-na_interpolation(dp$window_length_in_hours     			)} #	: num  24 25.7 24.1 22.8 25.5 ...
	if(nrow(dp[!is.na(dp$nonwear_hours              ),])>2){dp$nonwear_hours              <-na_interpolation(dp$nonwear_hours              			)} #	: num  24 25.7 24.1 22.8 25.5 ...
	if(nrow(dp[!is.na(dp$dur_nightsleep_min         ),])>2){dp$dur_nightsleep_min         <-na_interpolation(dp$dur_nightsleep_min       			)} #	: num  533 223 347 503 504 ...
	if(nrow(dp[!is.na(dp$dur_nightwak_and_IN30_min  ),])>2){dp$dur_nightwak_and_IN30_min  <-na_interpolation(dp$dur_nightwak_and_IN30_min  			)} #	: num  165 32.1 327.7 41.3 124.2 ...
	if(nrow(dp[!is.na(dp$dur_nightwak_LIG30_100_min ),])>2){dp$dur_nightwak_LIG30_100_min <-na_interpolation(dp$dur_nightwak_LIG30_100_min 			)} #	: num  3.92 24.83 30.08 0 46 ...
	if(nrow(dp[!is.na(dp$dur_nightwak_MOD100_400_min),])>2){dp$dur_nightwak_MOD100_400_min<-na_interpolation(dp$dur_nightwak_MOD100_400_min			)} #	: num  0 0 0.167 0 0.75 ...
	if(nrow(dp[!is.na(dp$dur_nightwak_VIG400_min    ),])>2){dp$dur_nightwak_VIG400_min    <-na_interpolation(dp$dur_nightwak_VIG400_min    			)} #	: num  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$dur_day_SIB_min            ),])>2){dp$dur_day_SIB_min            <-na_interpolation(dp$dur_day_SIB_min            			)} #	: num  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$dur_day_OIN30_min          ),])>2){dp$dur_day_OIN30_min          <-na_interpolation(dp$dur_day_OIN30_min          			)} #	: num  76.2 89.2 88.6 76 82.6 ...
	if(nrow(dp[!is.na(dp$dur_day_LIG30_100_min      ),])>2){dp$dur_day_LIG30_100_min      <-na_interpolation(dp$dur_day_LIG30_100_min      			)} #	: num  65.1 81.4 78.7 68.2 74.2 ...
	if(nrow(dp[!is.na(dp$dur_day_MOD100_400_min     ),])>2){dp$dur_day_MOD100_400_min     <-na_interpolation(dp$dur_day_MOD100_400_min     			)} #	: num  0.75 1.08 1.08 1 1.25 ...
	if(nrow(dp[!is.na(dp$dur_day_VIG400_min         ),])>2){dp$dur_day_VIG400_min         <-na_interpolation(dp$dur_day_VIG400_min         			)} #	: num  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$dur_MVPA_D1T100_min        ),])>2){dp$dur_MVPA_D1T100_min        <-na_interpolation(dp$dur_MVPA_D1T100_min        			)} #	: num  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$dur_INB_D30T30_min         ),])>2){dp$dur_INB_D30T30_min         <-na_interpolation(dp$dur_INB_D30T30_min         			)} #	: num  107.8 546.1 92.9 255 226.4 ...
	if(nrow(dp[!is.na(dp$dur_INB_D10T30_min         ),])>2){dp$dur_INB_D10T30_min         <-na_interpolation(dp$dur_INB_D10T30_min         			)} #	: num  128.1 134.9 105.4 58.2 115.8 ...
	if(nrow(dp[!is.na(dp$dur_INB_D1T30_min          ),])>2){dp$dur_INB_D1T30_min          <-na_interpolation(dp$dur_INB_D1T30_min          			)} #	: num  137 159 136 146 139 ...
	if(nrow(dp[!is.na(dp$dur_LIGB_D10T30_100_min    ),])>2){dp$dur_LIGB_D10T30_100_min    <-na_interpolation(dp$dur_LIGB_D10T30_100_min    			)} #	: num  86.8 104.3 96.8 87.2 80.9 ...
	if(nrow(dp[!is.na(dp$dur_LIGB_D1T30_100_min     ),])>2){dp$dur_LIGB_D1T30_100_min     <-na_interpolation(dp$dur_LIGB_D1T30_100_min     			)} #	: num  136 145 139 133 133 ...
	if(nrow(dp[!is.na(dp$dur_TSIBday_min            ),])>2){dp$dur_TSIBday_min            <-na_interpolation(dp$dur_TSIBday_min            			)} #	: num  181 171.4 38.8 250.5 240.9 ...
	if(nrow(dp[!is.na(dp$dur_TOINday_min            ),])>2){dp$dur_TOINday_min            <-na_interpolation(dp$dur_TOINday_min            			)} #	: num  276 762 391 294 326 ...
	if(nrow(dp[!is.na(dp$dur_TLIGday_min            ),])>2){dp$dur_TLIGday_min            <-na_interpolation(dp$dur_TLIGday_min            			)} #	: num  278 324 306 278 283 ...
	if(nrow(dp[!is.na(dp$dur_TMODday_min            ),])>2){dp$dur_TMODday_min            <-na_interpolation(dp$dur_TMODday_min            			)} #	: num  2.67 3.42 3.17 3.08 2.75 ...
	if(nrow(dp[!is.na(dp$dur_TVIGday_min            ),])>2){dp$dur_TVIGday_min            <-na_interpolation(dp$dur_TVIGday_min            			)} #	: num  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$dur_TINday_min             ),])>2){dp$dur_TINday_min             <-na_interpolation(dp$dur_TINday_min             			)} #	: num  457 934 430 544 567 ...
	if(nrow(dp[!is.na(dp$dur_day_min                ),])>2){dp$dur_day_min                <-na_interpolation(dp$dur_day_min                			)} #	: num  738 1261 739 825 852 ...
	if(nrow(dp[!is.na(dp$dur_night_min              ),])>2){dp$dur_night_min              <-na_interpolation(dp$dur_night_min              			)} #	: num  702 280 705 545 675 ...
	if(nrow(dp[!is.na(dp$dur_nightandday_min        ),])>2){dp$dur_nightandday_min        <-na_interpolation(dp$dur_nightandday_min        			)} #	: num  1440 1541 1444 1369 1527 ...
	if(nrow(dp[!is.na(dp$N_atleast5minwakenight     ),])>2){dp$N_atleast5minwakenight     <-round(na_interpolation(dp$N_atleast5minwakenight)		)} #	: int  5 1 4 1 4 3 7 3 3 1 ...
	if(nrow(dp[!is.na(dp$nonwear_perc_day           ),])>2){dp$nonwear_perc_day           <-na_interpolation(dp$nonwear_perc_day           			)} #	: num  100 100 100 100 100 100 100 100 100 100 ...
	if(nrow(dp[!is.na(dp$nonwear_perc_night         ),])>2){dp$nonwear_perc_night         <-na_interpolation(dp$nonwear_perc_night         			)} #	: num  100 100 100 100 100 100 100 100 100 100 ...
	if(nrow(dp[!is.na(dp$nonwear_perc_nightandday   ),])>2){dp$nonwear_perc_nightandday   <-na_interpolation(dp$nonwear_perc_nightandday   			)} #	: num  100 100 100 100 100 100 100 100 100 100 ...
	if(nrow(dp[!is.na(dp$sleep_efficiency           ),])>2){dp$sleep_efficiency           <-na_interpolation(dp$sleep_efficiency           			)} #	: num  0.759 0.797 0.492 0.924 0.747 ...
	if(nrow(dp[!is.na(dp$ACC_nightsleep_mg          ),])>2){dp$ACC_nightsleep_mg          <-na_interpolation(dp$ACC_nightsleep_mg          			)} #	: num  7.08 12.7 8.89 7.18 6.99 ...
	if(nrow(dp[!is.na(dp$ACC_nightwak_and_IN30_mg   ),])>2){dp$ACC_nightwak_and_IN30_mg   <-na_interpolation(dp$ACC_nightwak_and_IN30_mg   			)} #	: num  7.06 18.16 6.88 4.31 10.12 ...
	if(nrow(dp[!is.na(dp$ACC_nightwak_LIG30_100_mg  ),])>2){dp$ACC_nightwak_LIG30_100_mg  <-na_interpolation(dp$ACC_nightwak_LIG30_100_mg  			)} #	: num  38.3 44.2 42.6 NA 45.2 ...
	if(nrow(dp[!is.na(dp$ACC_nightwak_MOD100_400_mg ),])>2){dp$ACC_nightwak_MOD100_400_mg <-na_interpolation(dp$ACC_nightwak_MOD100_400_mg 			)} #	: num  NA NA 114 NA 145 ...
	if(nrow(dp[!is.na(dp$ACC_nightwak_VIG400_mg     ),])>2){dp$ACC_nightwak_VIG400_mg     <-na_interpolation(dp$ACC_nightwak_VIG400_mg     			)} #	: num  NA NA NA NA NA NA NA NA NA NA ...
	if(nrow(dp[!is.na(dp$ACC_day_SIB_mg             ),])>2){dp$ACC_day_SIB_mg             <-na_interpolation(dp$ACC_day_SIB_mg             			)} #	: num  NA NA NA NA NA NA NA NA NA NA ...
	if(nrow(dp[!is.na(dp$ACC_day_OIN30_mg           ),])>2){dp$ACC_day_OIN30_mg           <-na_interpolation(dp$ACC_day_OIN30_mg           			)} #	: num  22.1 22.3 22.2 22.4 22.2 ...
	if(nrow(dp[!is.na(dp$ACC_day_LIG30_100_mg       ),])>2){dp$ACC_day_LIG30_100_mg       <-na_interpolation(dp$ACC_day_LIG30_100_mg       			)} #	: num  39.9 40.1 39.9 39.9 40.1 ...
	if(nrow(dp[!is.na(dp$ACC_day_MOD100_400_mg      ),])>2){dp$ACC_day_MOD100_400_mg      <-na_interpolation(dp$ACC_day_MOD100_400_mg      			)} #	: num  146 149 155 152 166 ...
	if(nrow(dp[!is.na(dp$ACC_day_VIG400_mg          ),])>2){dp$ACC_day_VIG400_mg          <-na_interpolation(dp$ACC_day_VIG400_mg          			)} #	: num  NA NA NA NA NA NA NA NA NA NA ...
	if(nrow(dp[!is.na(dp$ACC_MVPA_D1T100_mg         ),])>2){dp$ACC_MVPA_D1T100_mg         <-na_interpolation(dp$ACC_MVPA_D1T100_mg         			)} #	: num  NA NA NA NA NA NA NA NA NA NA ...
	if(nrow(dp[!is.na(dp$ACC_INB_D30T30_mg          ),])>2){dp$ACC_INB_D30T30_mg          <-na_interpolation(dp$ACC_INB_D30T30_mg          			)} #	: num  26.76 7.73 15.18 13.93 21.35 ...
	if(nrow(dp[!is.na(dp$ACC_INB_D10T30_mg          ),])>2){dp$ACC_INB_D10T30_mg          <-na_interpolation(dp$ACC_INB_D10T30_mg          			)} #	: num  26.5 24.5 21.5 23.3 22.9 ...
	if(nrow(dp[!is.na(dp$ACC_INB_D1T30_mg           ),])>2){dp$ACC_INB_D1T30_mg           <-na_interpolation(dp$ACC_INB_D1T30_mg           			)} #	: num  21.5 20.2 19.2 21.7 19.5 ...
	if(nrow(dp[!is.na(dp$ACC_LIGB_D10T30_100_mg     ),])>2){dp$ACC_LIGB_D10T30_100_mg     <-na_interpolation(dp$ACC_LIGB_D10T30_100_mg     			)} #	: num  43.3 44.7 44.6 44.5 43.2 ...
	if(nrow(dp[!is.na(dp$ACC_LIGB_D1T30_100_mg      ),])>2){dp$ACC_LIGB_D1T30_100_mg      <-na_interpolation(dp$ACC_LIGB_D1T30_100_mg      			)} #	: num  47 46.3 46.3 46.6 46.4 ...
	if(nrow(dp[!is.na(dp$ACC_TSIBday_mg             ),])>2){dp$ACC_TSIBday_mg             <-na_interpolation(dp$ACC_TSIBday_mg             			)} #	: num  31.2 23 30.5 17.6 24.1 ...
	if(nrow(dp[!is.na(dp$ACC_TOINday_mg             ),])>2){dp$ACC_TOINday_mg             <-na_interpolation(dp$ACC_TOINday_mg             			)} #	: num  18.9 11.2 17.7 18.3 18.6 ...
	if(nrow(dp[!is.na(dp$ACC_TLIGday_mg             ),])>2){dp$ACC_TLIGday_mg             <-na_interpolation(dp$ACC_TLIGday_mg             			)} #	: num  45 44.8 44.8 45.1 44.5 ...
	if(nrow(dp[!is.na(dp$ACC_TMODday_mg             ),])>2){dp$ACC_TMODday_mg             <-na_interpolation(dp$ACC_TMODday_mg             			)} #	: num  147 153 155 149 151 ...
	if(nrow(dp[!is.na(dp$ACC_TVIGday_mg             ),])>2){dp$ACC_TVIGday_mg             <-na_interpolation(dp$ACC_TVIGday_mg             			)} #	: num  NA NA NA NA NA NA NA NA NA NA ...
	if(nrow(dp[!is.na(dp$ACC_TINday_min             ),])>2){dp$ACC_TINday_min             <-na_interpolation(dp$ACC_TINday_min             			)} #	: num  23.8 13.4 18.9 17.9 20.9 ...
	if(nrow(dp[!is.na(dp$ACC_day_mg                 ),])>2){dp$ACC_day_mg                 <-na_interpolation(dp$ACC_day_mg                 			)} #	: num  32.2 21.8 30.2 27.6 29.1 ...
	if(nrow(dp[!is.na(dp$ACC_night_mg               ),])>2){dp$ACC_night_mg               <-na_interpolation(dp$ACC_night_mg               			)} #	: num  7.25 16.12 9.42 6.96 10.32 ...
	if(nrow(dp[!is.na(dp$ACC_nightandday_mg         ),])>2){dp$ACC_nightandday_mg         <-na_interpolation(dp$ACC_nightandday_mg         			)} #	: num  20 20.8 20.1 19.4 20.8 ...
	if(nrow(dp[!is.na(dp$quantile_mostactive60min_mg),])>2){dp$quantile_mostactive60min_mg<-na_interpolation(dp$quantile_mostactive60min_mg			)} #	: num  57.5 59.4 57.5 55.5 59.3 ...
	if(nrow(dp[!is.na(dp$quantile_mostactive30min_mg),])>2){dp$quantile_mostactive30min_mg<-na_interpolation(dp$quantile_mostactive30min_mg			)} #	: num  66.8 68.2 66.8 65.3 68.2 ...
	if(nrow(dp[!is.na(dp$L5VALUE                    ),])>2){dp$L5VALUE                    <-na_interpolation(dp$L5VALUE                    			)} #	: num  4.58 4.58 4.58 4.56 4.56 ...
	if(nrow(dp[!is.na(dp$M5VALUE                    ),])>2){dp$M5VALUE                    <-na_interpolation(dp$M5VALUE                    			)} #	: num  35.8 35.8 35.8 35.7 35.7 ...
	if(nrow(dp[!is.na(dp$L5TIME_num                 ),])>2){dp$L5TIME_num                 <-na_interpolation(dp$L5TIME_num                 			)} #	: num  25 25 25 24.9 24.9 ...
	if(nrow(dp[!is.na(dp$M5TIME_num                 ),])>2){dp$M5TIME_num                 <-na_interpolation(dp$M5TIME_num                 			)} #	: num  12.5 12.5 12.5 12.6 12.5 ...
	if(nrow(dp[!is.na(dp$L10VALUE                   ),])>2){dp$L10VALUE                   <-na_interpolation(dp$L10VALUE                   			)} #	: num  5.36 5.36 5.36 5.36 5.37 ...
	if(nrow(dp[!is.na(dp$M10VALUE                   ),])>2){dp$M10VALUE                   <-na_interpolation(dp$M10VALUE                   			)} #	: num  33.6 33.6 32.5 32.4 33.6 ...
	if(nrow(dp[!is.na(dp$L10TIME_num                ),])>2){dp$L10TIME_num                <-na_interpolation(dp$L10TIME_num                			)} #	: num  23.7 23.7 23.7 23.6 23.5 ...
	if(nrow(dp[!is.na(dp$M10TIME_num                ),])>2){dp$M10TIME_num                <-na_interpolation(dp$M10TIME_num                			)} #	: num  34.5 34.5 12.5 35.7 34.5 ...
	if(nrow(dp[!is.na(dp$Nbouts_MVPA_D1T100         ),])>2){dp$Nbouts_MVPA_D1T100         <-round(na_interpolation(dp$Nbouts_MVPA_D1T100)  			)} #	: int  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$Nbouts_INB_D30T30          ),])>2){dp$Nbouts_INB_D30T30          <-round(na_interpolation(dp$Nbouts_INB_D30T30)  			)} #	: int  2 2 1 2 3 4 3 3 2 3 ...
	if(nrow(dp[!is.na(dp$Nbouts_INB_D10T30          ),])>2){dp$Nbouts_INB_D10T30          <-round(na_interpolation(dp$Nbouts_INB_D10T30)          	)} #	: int  8 10 8 5 8 9 7 7 10 4 ...
	if(nrow(dp[!is.na(dp$Nbouts_INB_D1T30           ),])>2){dp$Nbouts_INB_D1T30           <-round(na_interpolation(dp$Nbouts_INB_D1T30)           	)} #	: int  61 76 63 64 64 42 70 64 64 70 ...
	if(nrow(dp[!is.na(dp$Nbouts_LIGB_D10T30_100     ),])>2){dp$Nbouts_LIGB_D10T30_100     <-round(na_interpolation(dp$Nbouts_LIGB_D10T30_100)     	)} #	: int  6 7 6 6 6 4 6 6 4 8 ...
	if(nrow(dp[!is.na(dp$Nbouts_LIGB_D1T30_100      ),])>2){dp$Nbouts_LIGB_D1T30_100      <-round(na_interpolation(dp$Nbouts_LIGB_D1T30_100)      	)} #	: int  59 62 59 54 56 45 66 65 61 68 ...
	if(nrow(dp[!is.na(dp$Nblocks_nightsleep         ),])>2){dp$Nblocks_nightsleep         <-round(na_interpolation(dp$Nblocks_nightsleep)         	)} #	: int  20 5 14 9 15 12 16 12 6 13 ...
	if(nrow(dp[!is.na(dp$Nblocks_nightwak_and_IN30  ),])>2){dp$Nblocks_nightwak_and_IN30  <-round(na_interpolation(dp$Nblocks_nightwak_and_IN30)  	)} #	: int  50 67 107 9 118 14 97 49 36 55 ...
	if(nrow(dp[!is.na(dp$Nblocks_nightwak_LIG30_100 ),])>2){dp$Nblocks_nightwak_LIG30_100 <-round(na_interpolation(dp$Nblocks_nightwak_LIG30_100) 	)} #	: int  34 66 95 0 111 2 82 38 30 42 ...
	if(nrow(dp[!is.na(dp$Nblocks_nightwak_MOD100_400),])>2){dp$Nblocks_nightwak_MOD100_400<-round(na_interpolation(dp$Nblocks_nightwak_MOD100_400)	)} #	: int  0 0 2 0 8 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$Nblocks_nightwak_VIG400    ),])>2){dp$Nblocks_nightwak_VIG400    <-round(na_interpolation(dp$Nblocks_nightwak_VIG400)    	)} #	: int  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$Nblocks_day_SIB        	),])>2){dp$Nblocks_day_SIB        	  <-round(na_interpolation(dp$Nblocks_day_SIB)        	  	)} #	: int  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$Nblocks_day_OIN30      	),])>2){dp$Nblocks_day_OIN30      	  <-round(na_interpolation(dp$Nblocks_day_OIN30)      	  	)} #	: int  320 379 379 329 351 253 345 383 320 374 ...
	if(nrow(dp[!is.na(dp$Nblocks_day_LIG30_100  	),])>2){dp$Nblocks_day_LIG30_100  	  <-round(na_interpolation(dp$Nblocks_day_LIG30_100)  	  	)} #	: int  328 397 394 342 368 267 354 394 330 374 ...
	if(nrow(dp[!is.na(dp$Nblocks_day_MOD100_400 	),])>2){dp$Nblocks_day_MOD100_400 	  <-round(na_interpolation(dp$Nblocks_day_MOD100_400) 	  	)} #	: int  8 10 11 10 11 5 10 13 5 11 ...
	if(nrow(dp[!is.na(dp$Nblocks_day_VIG400     	),])>2){dp$Nblocks_day_VIG400     	  <-round(na_interpolation(dp$Nblocks_day_VIG400)     	  	)} #	: int  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$Nblocks_MVPA_D1T100    	),])>2){dp$Nblocks_MVPA_D1T100    	  <-round(na_interpolation(dp$Nblocks_MVPA_D1T100)    	  	)} #	: int  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$Nblocks_INB_D30T30     	),])>2){dp$Nblocks_INB_D30T30     	  <-round(na_interpolation(dp$Nblocks_INB_D30T30)     	  	)} #	: int  2 2 1 2 3 3 2 3 2 3 ...
	if(nrow(dp[!is.na(dp$Nblocks_INB_D10T30     	),])>2){dp$Nblocks_INB_D10T30     	  <-round(na_interpolation(dp$Nblocks_INB_D10T30)     	  	)} #	: int  8 10 8 5 8 9 7 6 9 4 ...
	if(nrow(dp[!is.na(dp$Nblocks_INB_D1T30      	),])>2){dp$Nblocks_INB_D1T30      	  <-round(na_interpolation(dp$Nblocks_INB_D1T30)      	  	)} #	: int  61 76 63 64 63 42 70 64 64 69 ...
	if(nrow(dp[!is.na(dp$Nblocks_LIGB_D10T30_100	),])>2){dp$Nblocks_LIGB_D10T30_100	  <-round(na_interpolation(dp$Nblocks_LIGB_D10T30_100)	  	)} #	: int  6 7 6 6 6 4 6 6 4 8 ...
	if(nrow(dp[!is.na(dp$Nblocks_LIGB_D1T30_100 	),])>2){dp$Nblocks_LIGB_D1T30_100 	  <-round(na_interpolation(dp$Nblocks_LIGB_D1T30_100) 	  	)} #	: int  59 62 59 54 56 45 66 65 61 68 ...
	if(nrow(dp[!is.na(dp$Nblocks_TSIBday        	),])>2){dp$Nblocks_TSIBday        	  <-round(na_interpolation(dp$Nblocks_TSIBday)        	  	)} #	: int  15 16 6 11 11 16 16 16 16 8 ...
	if(nrow(dp[!is.na(dp$Nblocks_TOINday        	),])>2){dp$Nblocks_TOINday        	  <-round(na_interpolation(dp$Nblocks_TOINday)        	  	)} #	: int  659 798 780 663 720 552 725 793 657 822 ...
	if(nrow(dp[!is.na(dp$Nblocks_TLIGday        	),])>2){dp$Nblocks_TLIGday        	  <-round(na_interpolation(dp$Nblocks_TLIGday)        	  	)} #	: int  678 821 801 683 739 575 750 815 684 842 ...
	if(nrow(dp[!is.na(dp$Nblocks_TMODday        	),])>2){dp$Nblocks_TMODday        	  <-round(na_interpolation(dp$Nblocks_TMODday)        	  	)} #	: int  28 33 30 31 27 21 31 31 27 31 ...
	if(nrow(dp[!is.na(dp$Nblocks_TVIGday        	),])>2){dp$Nblocks_TVIGday        	  <-round(na_interpolation(dp$Nblocks_TVIGday)        	  	)} #	: int  0 0 0 0 0 0 0 0 0 0 ...
	if(nrow(dp[!is.na(dp$Nblocks_TINday         	),])>2){dp$Nblocks_TINday         	  <-round(na_interpolation(dp$Nblocks_TINday)         	  	)} #	: int  654 796 779 661 718 556 726 792 662 818 ...
	if(nrow(dp[!is.na(dp$boutcriter.in          	),])>2){dp$boutcriter.in          	  <-round(na_interpolation(dp$boutcriter.in)          	  	)} #	: num  0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 ...
	if(nrow(dp[!is.na(dp$boutcriter.lig         	),])>2){dp$boutcriter.lig         	  <-round(na_interpolation(dp$boutcriter.lig)         	  	)} #	: num  0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 ...
	if(nrow(dp[!is.na(dp$boutcriter.mvpa        	),])>2){dp$boutcriter.mvpa        	  <-round(na_interpolation(dp$boutcriter.mvpa)        	  	)} #	: num  0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 ...
	if(nrow(dp[!is.na(dp$boutdur.mvpa           	),])>2){dp$boutdur.mvpa           	  <-round(na_interpolation(dp$boutdur.mvpa)    	  			)} #	: int  1 1 1 1 1 1 1 1 1 1 ...
	if(nrow(dp[!is.na(dp$bout.metric            	),])>2){dp$bout.metric            	  <-round(na_interpolation(dp$bout.metric)     	  			)} #	: int  4 4 4 4 4 4 4 4 4 4 ...

	 

	pulsera<-rbind(pulsera,dp)

	}


} else {
pulsera<-pulori
}

