library(ggplot2)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

print(unique(d$id))

usuario=readline("escoja usuario: ")
dfu<-d[d$id==usuario,]

setwd("C:\\Users\\pavel\\Google Drive\\Bip4Cast-Doctorado-2018\\2018-Pavel LLamocca\\Pavel\\drive-download-20180902T092205Z-001\\Nuevos_Scripts\\graficos\\")


name_pdf<-paste0("plots_",usuario,"_Ev_Diario.pdf")
pdf(name_pdf)
p1<-ggplot(dfu,aes(fecha,animo)) + geom_point() + geom_smooth()
p2<-ggplot(dfu,aes(fecha,ansiedad)) + geom_point() + geom_smooth()
p3<-ggplot(dfu,aes(fecha,concentracion)) + geom_point() + geom_smooth()
p4<-ggplot(dfu,aes(fecha,motivacion)) + geom_point() + geom_smooth()

p5<-ggplot(dfu,aes(fecha,animo,colour=dx2)) + geom_point()
p6<-ggplot(dfu,aes(fecha,ansiedad,colour=dx2)) + geom_point()
p7<-ggplot(dfu,aes(fecha,concentracion,colour=dx2)) + geom_point()
p8<-ggplot(dfu,aes(fecha,motivacion,colour=dx2)) + geom_point()

#p9<-ggplot(dfu,aes(fecha,cafeina)) + geom_point() + geom_smooth()
#p10<-ggplot(dfu,aes(fecha,cafeina,colour=dx2)) + geom_point()


multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=2)
dev.off()



name_pdf<-paste0("plots_",usuario,"_PulseraVSDx.pdf")
pdf(name_pdf)
p1<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = sleeplog_onset,color = "b") ) +
  geom_line(data = dfu, aes(x = fecha, y = nonwear_perc_nightandday, color = "c")) + 
  geom_line(data = dfu, aes(x = fecha, y = quantile_mostactive60min_mg, color = "d")) + 
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="green","c"="blue","d"="yellow"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","sleeplog_onset","nonwear_perc_nightandday","quantile_mostactive60min_mg")) +
  xlab('Fecha') +
  ylab('Variables')   
  
p2<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = boutdur.mvpa,color = "b") ) +
  geom_line(data = dfu, aes(x = fecha, y = N_atleast5minwakenight, color = "c")) + 
  geom_line(data = dfu, aes(x = fecha, y = nonwear_perc_day, color = "d")) + 
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="green","c"="blue","d"="yellow"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","boutdur.mvpa","N_atleast5minwakenight","nonwear_perc_day")) +
  xlab('Fecha') +
  ylab('Variables')   
  
p3<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = Nblocks_INB_D1T30,color = "b") ) +
  geom_line(data = dfu, aes(x = fecha, y = Nblocks_day_MOD100_400, color = "c")) + 
  geom_line(data = dfu, aes(x = fecha, y = Nblocks_nightwak_LIG30_100, color = "d")) + 
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="green","c"="blue","d"="yellow"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","Nblocks_INB_D1T30","Nblocks_day_MOD100_400","Nblocks_nightwak_LIG30_100")) +
  xlab('Fecha') +
  ylab('Variables')   
  
p4<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = dur_TSIBday_min,color = "b") ) +
  geom_line(data = dfu, aes(x = fecha, y = dur_TOINday_min, color = "c")) + 
  geom_line(data = dfu, aes(x = fecha, y = dur_TLIGday_min, color = "d")) + 
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="green","c"="blue","d"="yellow"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","dur_TSIBday_min","dur_TOINday_min","dur_TLIGday_min")) +
  xlab('Fecha') +
  ylab('Variables')   

  
multiplot(p1,p2,p3,p4,cols=1)
dev.off()

name_pdf<-paste0("plots_",usuario,"_DiarioVSDx.pdf")
pdf(name_pdf)
p1<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = irritabilidad,color = "b") ) +
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="green"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","irritabilidad")) +
  xlab('Fecha') +
  ylab('Variables')   
  
p2<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = ansiedad,color = "b") ) +
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="blue"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","ansiedad")) +
  xlab('Fecha') +
  ylab('Variables')   
  
p3<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = concentracion,color = "b") ) +
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="green"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","concentracion")) +
  xlab('Fecha') +
  ylab('Variables')   
  
p4<-ggplot(dfu) + 
  geom_line(data = dfu, aes(x = fecha, y = dx2num,color = "a") ) + 
  geom_line(data = dfu, aes(x = fecha, y = tabaco,color = "b") ) +
  geom_line(data = dfu, aes(x = fecha, y = cafeina, color = "c")) + 
  scale_color_manual(name="Legenda", 
	values=c("a"="red","b"="green","c"="blue"),labels=c("Dx(-1=D,0=E,1=H,2=Mixto,3=Ingresado)","tabaco","cafeina")) +
  xlab('Fecha') +
  ylab('Variables')   

  
multiplot(p1,p2,p3,p4,cols=1)
dev.off()



name_pdf<-paste0("plots_",usuario,"_PCA.pdf")
pdf(name_pdf)


#Solo diario
dfu_d<-subset(dfu,,c(1:23))
dfu_d <- dfu_d[,sapply(dfu_d, is.numeric)]
dfu_d<-dfu_d[!is.na(dfu_d$animo),]
#para quitar columnas con algún NA, para hacer el pca
dfu_d<-dfu_d[,colMeans(is.na(dfu_d)) == 0]
p1<-biplot(princomp(dfu_d),pc.biplot=TRUE,cex=0.5,expand=0.8,main="PCA Variables del Diario")

#Solo Pulsera
dfu_d<-subset(dfu,,c(24:144))
dfu_d <- dfu_d[,sapply(dfu_d, is.numeric)]
dfu_d<-dfu_d[!is.na(dfu_d$night_number),]
#para quitar columnas con algún NA, para hacer el pca
dfu_d<-dfu_d[,colMeans(is.na(dfu_d)) == 0]
dfu_d<-subset(dfu_d,,c(1:85))
p2<-biplot(princomp(dfu_d),pc.biplot=TRUE,cex=0.5,expand=0.8,main="PCA Variables de Pulsera")


dfu_d<-dfu[!is.na(dfu$night_number) & !is.na(dfu$animo),]
dfu_d <- dfu_d[,sapply(dfu_d, is.numeric)]
dfu_d<-dfu_d[,colMeans(is.na(dfu_d)) == 0]
dfu_d<-subset(dfu_d,,c(1:85))
p3<-biplot(princomp(dfu_d),pc.biplot=TRUE,cex=0.5,expand=0.8,main="PCA Variables TODAS")

multiplot(p1,p2,p3,cols=1)
dev.off()

#biplot(princomp(dfu_d),xlabs=rep("o",nrow(dfu_d)))






