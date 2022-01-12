datos<-"eventosUniformesTotales.xlsx"
eventos<- as.data.frame (read_excel (paste0(path_datos,datos)))
eventos$fecha<-as.Date(eventos$fecha)

eventos$dx_corto<-ifelse(eventos$dx_largo %in% c("F31.6 Sintomas mixtos  X","F31.6 Síntomas mixtos  X","F31.6 Síntomas mixtos X","Mixto","Mixta","Mixo"),"Mixto",
							ifelse(eventos$dx_largo %in% c("Eutimia","Eutimia 0","Eutimia  0"),"Eutimia",
								ifelse(eventos$dx_largo %in% c("F31.0 Hipomania 1","F31.0 Hipomanía 1","F31.0 Hipomanía  1","Hipomania"),"Hipomania",
									ifelse(eventos$dx_largo %in% c("Depresion Leve" ,"Depresion","F31.3 Depresion leve o moderada  -1","F31.3 Depresión leve o moderada  -1","Depresion/Mixto" ),"Depresion",
										ifelse(eventos$dx_largo %in% c("Ingreso" ),"Ingresado","No Informado")					     
									)
								)
							)				
						)
				
				
				
eventos$dx_num<-ifelse(eventos$dx_corto=="Mixto",2,
					ifelse(eventos$dx_corto=="Eutimia",0,
						ifelse(eventos$dx_corto=="Hipomania",1,
							ifelse(eventos$dx_corto=="Depresion",-1,
								ifelse(eventos$dx_corto=="Ingresado",3,
									4)))))
									
								