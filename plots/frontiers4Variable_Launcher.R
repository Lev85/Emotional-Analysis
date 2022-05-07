pacientes<-c("GIUS","DOSI","ECSA","ALPA","BEPR","CASGA","GOMA","LABA","MAPI","PELA","PRFE","RIVI","ROCU","SAPE","SEOR","DOGE")
pacientes<-c("SAPE")
variables<-c("D_motivation","D_irritability","D_wakeup_time","D_sleep_quality","D_sleep_duration")
variables<-c("S_sleeponset","S_wakeup","S_dur_spt_min","S_sleep_efficiency")
variables1<-c("S_wakeup")
variables2<-c("D_motivation")
script<-"/frontiers4variables.R"

projectpath<-"D:\\Proyectos\\Pavel_MoodDisorders\\plots"
plotfolder<-"\\plots"

scriptPath<-paste0(projectpath,plotfolder,script)

for (p in pacientes) {
  for (v1 in variables1) {
    for (v2 in variables2) {
    
		cArgs <- c(p,v1,v2)
		source(scriptPath)
		
		folderpaciente<-paste0("/",p)     
		
		
		if (!dir.exists(paste0(projectpath,plotfolder,folderpaciente))) {
		  dir.create(paste0(projectpath,plotfolder,folderpaciente))
		}
		
		
		jpgfullpath<-paste0(projectpath,
							plotfolder,
							folderpaciente,"/",p,"_",substr(v1,3,nchar(v1)),"_",substr(v2,3,nchar(v2)),"_FRONT4VAR.jpg")
		
		jpeg(
		  jpgfullpath,
		  units="px",
		  width = 800,
		  height = 561,
		  quality=100
		)
		
		#boxplot(nn,main=vtitle)
		print(plt)
		
		dev.off()
    
    }
  }
}