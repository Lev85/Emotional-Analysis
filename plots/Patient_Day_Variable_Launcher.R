pacientes<-c("ALPA","BEPR","CASGA","DOSI","ECSA",
             "GIUS","GOMA","LABA","MAPI","PELA",
             "PRFE","RIVI","ROCU","SAPE","SEOR","DOGE")

pacientes<-c("DOGE")

variables<-c("D_motivation","D_irritability","D_wakeup_time","D_sleep_quality","D_sleep_duration")
variables<-c("D_motivation","D_irritability","D_wakeup_time","D_sleep_quality","D_sleep_duration","S_sleeponset","S_wakeup","S_dur_spt_min","S_sleep_efficiency")
variables<-c("D_sleep_time")

dias<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

script<-"/Patient_Day_Variable.R"

projectpath<-"D:\\Proyectos\\Pavel_MoodDisorders\\plots"
plotfolder<-"\\plots"

scriptPath<-paste0(projectpath,plotfolder,script)


for (p in pacientes) {
  for (v in variables) {
    for (d in dias) {
      
      cArgs <- c(p,v,d)
      source(scriptPath)
      
      folderpaciente<-paste0("/",p)     
      
      
      if (!dir.exists(paste0(projectpath,plotfolder,folderpaciente))) {
        dir.create(paste0(projectpath,plotfolder,folderpaciente))
      }
      
      
      jpgfullpath<-paste0(projectpath,
                          plotfolder,
                          folderpaciente,"/",p,"_",substr(v,3,nchar(v)),"_",d,".jpg")
      
      jpeg(
        jpgfullpath,
        units="px",
        width = 800,
        height = 561,
        quality=100
      )
      
      print(plt)
      
      dev.off()
      
      
    }
  }
}





