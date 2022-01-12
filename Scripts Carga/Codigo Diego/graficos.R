# graficos

## Correlaciones para datos totales

datosgraf <- read.csv (file = 'GENEACTIV UBIP/DOSI/dat_total_DOSI.csv')
datosgraf <- datosgraf [,!colnames(datosgraf)=='id']
datosgraf <- datosgraf [,!colnames(datosgraf)=='sleeplog_used']
datosgraf <- datosgraf [,!colnames(datosgraf)=='weekday']
datosgraf <- datosgraf [,!colnames(datosgraf)=='filename']
datosgraf <- datosgraf [,!colnames(datosgraf)=='fecha']
datosgraf <- datosgraf [,!colnames(datosgraf)=='X']
datosgraf <- datosgraf [,!colnames(datosgraf)=='L5TIME']
datosgraf <- datosgraf [,!colnames(datosgraf)=='L10TIME']
datosgraf <- datosgraf [,!colnames(datosgraf)=='M10TIME']
datosgraf <- datosgraf [,!colnames(datosgraf)=='X']

datosgraf <- datosgraf [,!colnames(datosgraf)=='alivio']
datosgraf <- datosgraf [,!colnames(datosgraf)=='terapeuta']
datosgraf <- datosgraf [,!colnames(datosgraf)=='programada']
datosgraf <- datosgraf [,!colnames(datosgraf)=='dx']
datosgraf <- datosgraf [,!colnames(datosgraf)=='tipo']
datosgraf <- datosgraf [,!colnames(datosgraf)=='cambiosignificativo']
datosgraf <- datosgraf [,!colnames(datosgraf)=='terapeuta']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Euforia.']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Hiperactividad.']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Impulso.Sexual']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Sueño']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Irritabilidad']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Expresion.verbal']
datosgraf <- datosgraf [,!colnames(datosgraf)=='curso']
datosgraf <- datosgraf [,!colnames(datosgraf)=='contenido']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Agresividad']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Apariencia']
datosgraf <- datosgraf [,!colnames(datosgraf)=='conciencia.x']
datosgraf <- datosgraf [,!colnames(datosgraf)=='depresion']
datosgraf <- datosgraf [,!colnames(datosgraf)=='culpa']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Suicidio']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Expresión.verbal']
datosgraf <- datosgraf [,!colnames(datosgraf)=='iprecoz']
datosgraf <- datosgraf [,!colnames(datosgraf)=='imedio']
datosgraf <- datosgraf [,!colnames(datosgraf)=='itardio']
datosgraf <- datosgraf [,!colnames(datosgraf)=='actividad']
datosgraf <- datosgraf [,!colnames(datosgraf)=='inhibicion']
datosgraf <- datosgraf [,!colnames(datosgraf)=='Agitación']
datosgraf <- datosgraf [,!colnames(datosgraf)=='an_psiq']
datosgraf <- datosgraf [,!colnames(datosgraf)=='an_som']
datosgraf <- datosgraf [,!colnames(datosgraf)=='gastro']
datosgraf <- datosgraf [,!colnames(datosgraf)=='M5TIME']
datosgraf <- datosgraf [,!colnames(datosgraf)=='somatic']
datosgraf <- datosgraf [,!colnames(datosgraf)=='sexo']
datosgraf <- datosgraf [,!colnames(datosgraf)=='conciencia.y']
datosgraf <- datosgraf [,!colnames(datosgraf)=='hipocondria']
datosgraf <- datosgraf [,!colnames(datosgraf)=='peso']
datosgraf <- datosgraf [,!colnames(datosgraf)=='ACC_day_SIB_mg']
datosgraf <- datosgraf [,!colnames(datosgraf)=='acc_available']
datosgraf <- datosgraf [,!colnames(datosgraf)=='acc_onset_ts']
datosgraf <- datosgraf [,!colnames(datosgraf)=='acc_wake_ts']
datosgraf <- datosgraf [,!colnames(datosgraf)=='sleeplog_onset_ts']
datosgraf <- datosgraf [,!colnames(datosgraf)=='sleeplog_wake_ts']
datosgraf$ánimo <-as.numeric (datosgraf$ánimo)
datosgraf$ansiedad <-as.numeric (datosgraf$ansiedad)
datosgraf$irritabilidad <-as.numeric (datosgraf$irritabilidad)
datosgraf$concentración <-as.numeric (datosgraf$concentración)
datosgraf$dur_day_SIB_min <-as.numeric (datosgraf$dur_day_SIB_min)
datosgraf$Nbouts_MVPA_D1T100 <-as.numeric (datosgraf$Nbouts_MVPA_D1T100)
datosgraf$Nbouts_INB_D1T30 <-as.numeric (datosgraf$Nbouts_INB_D1T30)
datosgraf$Nbouts_INB_D10T30 <-as.numeric (datosgraf$Nbouts_INB_D1oT30)
datosgraf$cafeina <-as.numeric (datosgraf$cafeina)
datosgraf$tabaco <-as.numeric (datosgraf$tabaco)
datosgraf$motivación <-as.numeric (datosgraf$motivación)
datosgraf$alcohol_num <-as.numeric (datosgraf$alcohol_num)
datosgraf$otras_drogas_num <-as.numeric (datosgraf$otras_drogas_num)
datosgraf$horatest <-as.numeric (datosgraf$horatest)
datosgraf$datoNA <-as.numeric (datosgraf$datoNA)
datosgraf$actividadsubjetiva <-as.numeric (datosgraf$actividadsubjetiva)
datosgraf$night.number <-as.numeric (datosgraf$night.number)
datosgraf$daysleeper <-as.numeric (datosgraf$daysleeper)
datosgraf$cleaningcode <-as.numeric (datosgraf$cleaningcode)

datosgraf$N_atleast5minwakenight <-as.numeric (datosgraf$N_atleast5minwakenight)
datosgraf$Nbouts_INB_D1T30 <-as.numeric (datosgraf$Nbouts_INB_D1T30)
datosgraf$Nbouts_LIGB_D1T30_100 <-as.numeric (datosgraf$Nbouts_LIGB_D1T30_100)
datosgraf$Nbouts_LIGB_D10T30_100 <-as.numeric (datosgraf$Nbouts_LIGB_D10T30_100)
datosgraf$Nbouts_INB_D10T30D1T30 <-as.numeric (datosgraf$Nbouts_INB_D10T30D1T30)
datosgraf$Nbouts_INB_D30T30 <-as.numeric (datosgraf$Nbouts_INB_D30T30)
datosgraf$N_atleast5minwakenight <-as.numeric (datosgraf$N_atleast5minwakenight)
datosgraf$observ_num <-as.numeric (datosgraf$observ_num)
datosgraf$Nbouts_INB_D10T30 <-as.numeric (datosgraf$Nbouts_INB_D10T30)
datosgraf$calidad_sueño <-as.numeric (datosgraf$calidad_sueño)

datosgraf$boutcriter.in <-as.numeric (datosgraf$boutcriter.in)
  datosgraf$boutcriter.lig <-as.numeric (datosgraf$boutcriter.lig)
datosgraf$bout.metric <-as.numeric (datosgraf$bout.metric)
datosgraf$boutdur.in <-as.numeric (datosgraf$boutdur.in)
datosgraf$boutdur.mvpa <-as.numeric (datosgraf$boutdur.mvpa)
datosgraf$boutdur.lig <-as.numeric (datosgraf$boutdur.lig)
datosgraf <- filter (datosgraf, night.number>0)

matriz <- cor (datosgraf)
matriz.lista = melt(matriz)
names(matriz.lista)=c("Variable_1","Variable_2","Correlacion")
escala = seq(-1,1,0.1)

ggplot(matriz.lista,aes(Variable_1, Variable_2, fill=Correlacion)) + geom_tile(aes(fill=Correlacion)) + scale_fill_continuous(low = "white", high = "steelblue" ,breaks=escala) + theme (axis.text.x = element_text(angle = 60, hjust = 1)) 

write.csv (x = datosgraf, file = 'correlacion.csv')



# ensure the results are repeatable
set.seed(7)
# load the library
install.packages("mlbench")
library(mlbench)
install.packages("caret")
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))