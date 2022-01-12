cargar<-FALSE

path_scripts<-"C:\\Users\\pavel\\Google Drive\\Bip4Cast-Doctorado-2018\\2018-Pavel LLamocca\\Pavel\\drive-download-20180902T092205Z-001\\Nuevos_Scripts\\"

if(cargar) source(paste0(path_scripts,"00_iniciar.r"))

d$idnum<-as.numeric(as.factor(d$id))
d$mes<-as.numeric(format(d$fecha, "%y%m"))

datos<-d[,names(which(sapply(d,is.numeric)))]

vardiario<-colnames(datos)[1:20]
varpulsera<-colnames(datos)[21:128]
varinter<-c(colnames(datos)[129:134],colnames(datos)[165])
varyoung<-c(colnames(datos)[135:145],colnames(datos)[164])
varhrsd<-c(colnames(datos)[146:162],colnames(datos)[163])




tabla<-data.frame()
options(stringsAsFactors = FALSE)

for ( m in unique(datos$mes)) {

	for ( u in unique(datos$idnum)) {


		dat<-datos[datos$idnum==u & datos$mes== m,]

		pnt<-character()

		for (x in colnames(dat) ) {

			if (x=="idnum" | x=="mes") {next}

			for (y in colnames(dat) ) {

				if (y=="idnum" | x=="mes" | x==y) {next}
				
				f1<-dat[!is.na(dat[,x]) & !is.na(dat[,y]),c(x,y)]
				
				if (length(unique(f1[,x])) <= 1 ) {next}
				if (length(unique(f1[,y])) <= 1 ) {next}		
				
				if (paste0(y,x) %in% pnt) {next}
				
				fc<-cor(f1)
				
									
						
				
				#if(fc[1,2]>=0.50 & fc[1,2]<=0.95){	
				if(fc[1,2]>=0.5 | fc[1,2] <=-0.5){	
				
				x_origen <- ifelse(x %in% vardiario,"Diario",		
								ifelse(x %in% varpulsera,"Pulsera",
									ifelse(x %in% varinter,"Intervenciones",
										ifelse(x %in% varyoung,"Young","HRSD"))))
										
				y_origen <- ifelse(y %in% vardiario,"Diario",		
								ifelse(y %in% varpulsera,"Pulsera",
									ifelse(y %in% varinter,"Intervenciones",
										ifelse(y %in% varyoung,"Young","HRSD"))))				
				
				tabla<-rbind(tabla,list(u,x_origen,y_origen,m,x,y,fc[1,2]))
				
				pnt<-c(pnt,paste0(x,y))
				
				
				#print(fc)
				}
			}
		}

	}

}