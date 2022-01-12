#plot_correlation(datacorrplot, 
# cor_args = list( 'use' = 'complete.obs'))

#plot_correlation(datacorrplot, cor_args = list( 'use' = 'complete.obs'),theme_config = list(axis.text.x = element_text(size=18,angle = 90,face="bold"),axis.text.y = element_text(size=18,face="bold")))

plot_correlation(datacorrplot,
				cor_args = list( 'use' = 'complete.obs'),
				geom_text_args = list(size=6,fontface="bold"),
				theme_config = list(axis.title.x = element_blank(),
									axis.title.y = element_blank(),
									axis.text.x = element_text(size=22,angle = 90,face="bold"),
									axis.text.y = element_text(size=22,face="bold"),
									#legend.position="none",
									legend.text=element_text(size=15,face="bold"),
									legend.title=element_text(size=15,face="bold"))
				)



library(DataExplorer)
load("dnormalizado4.RData")
paciente="GIUS"
dnm<-dftotal
datos.paciente<-dnm[dnm$A_id==paciente,]				
load("featSel.RData")
  

df_topfeatsel<-list_featselPatient[[paciente]]
df_topfeatsel<-df_topfeatsel[order(-df_topfeatsel$Tot),]  


varsToShow<-nrow(df_topfeatsel)
vToShow<-15
datacorrplot<-datos.paciente[,rownames(df_topfeatsel)[1:vToShow]]


# Contingencia 20.05
nn<-c("D_sleep_zenith",
      "S_ACC_day_LIG_unbt_mg",
      "S_Nblocks_spt_wake_MOD",
      "D_sleep_time",
      "D_wakeup_time",
      "S_ACC_day_LIG_bts_10_mg",
      "S_ACC_day_total_MOD_mg",
      "S_dur_day_total_VIG_min",
      "S_dur_day_VIG_unbt_min",
      "S_dur_spt_wake_LIG_min",
      "S_Nblocks_day_IN_unbt",
      "S_Nblocks_day_LIG_bts_10",
      "S_Nblocks_spt_sleep",
      "S_Nblocks_spt_wake_LIG",
      "S_wakeup");

datacorrplot<-datos.paciente[,nn]