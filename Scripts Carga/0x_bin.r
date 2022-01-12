
path_bin<-"C:/Users/pavel/Google Drive/Bip4Cast-Doctorado-2018/bip4cast-datasets/BIN/temporal"

      g.shell.GGIR(#=======================================
                   # INPUT NEEDED:
                   mode=c(1,2,3,4,5),
                   datadir=path_bin,
                   outputdir=path_bin,
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
      
 
