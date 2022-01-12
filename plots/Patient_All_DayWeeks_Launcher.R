pacientes<-c("GIUS","DOSI","ECSA","ALPA","BEPR","CASGA","GOMA","LABA","MAPI","PELA","PRFE","RIVI","ROCU","SAPE","SEOR","DOGE")
pacientes<-c("CASGA")
variables<-c("D_motivation","D_irritability","D_wakeup_time","D_sleep_quality","D_sleep_duration")
variables<-c("S_sleeponset","S_wakeup","S_dur_spt_min","S_sleep_efficiency")
variables<-c("D_wakeup_time")
script<-"/Patient_All_DayWeeks.R"

projectpath<-"D:\\Proyectos\\Pavel_MoodDisorders\\plots"
plotfolder<-"\\plots"

scriptPath<-paste0(projectpath,plotfolder,script)


for (p in pacientes) {
  for (v in variables) {
    
    cArgs <- c(p,v)
    source(scriptPath)
    
    folderpaciente<-paste0("/",p)     
    
    
    if (!dir.exists(paste0(projectpath,plotfolder,folderpaciente))) {
      dir.create(paste0(projectpath,plotfolder,folderpaciente))
    }
    
    
    jpgfullpath<-paste0(projectpath,
                        plotfolder,
                        folderpaciente,"/",p,"_",substr(v,3,nchar(v)),"_ALLDAYSONEPLOT.jpg")
    
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


