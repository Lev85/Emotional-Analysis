#Quitamos la variable caracter, IDUSUARIO
dt<-subset(dt,,-c(53))
k<-eigen(cor(dt))


#Convertimos dt a una matriz
dt<-as.matrix(dt)

dtm %*% k$vec
# El primer componente principal se define como
dtm %*% k$vec[,1]
# El segundo componente principal se define como
dtm %*% k$vec[,2]






#La función que ya da el pca es princomp

pca<-princomp(dt,scores=TRUE)
# scores de los componentes
pca$scores



biplot(princomp(dt),pc.biplot=TRUE,cex=0.5,expand=0.8)

biplot(princomp(dt[,c("dormir","despertar")]),pc.biplot=TRUE,cex=0.5,expand=0.8)

biplot(princomp(dt[,c("dormir","despertar","motivación","alivio","lentitud","depresion","concentración")]),pc.biplot=TRUE,cex=0.5,expand=0.8)
