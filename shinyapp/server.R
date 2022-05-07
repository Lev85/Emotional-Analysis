server = function(input, output) { 
    #output$text<- reactive({
  
  var_id<-"A_id"
  var_date<-"A_date"
  var_diagnosis<-"E_diagnosis"
  var_prev_diagnosis<-"I_dx_short"
  

  
  plot_data<- reactive({
       test <- dftotal[dftotal[[var_id]] == input$patient &
                       dftotal[[var_date]] >= input$dateRange[1] &
                       dftotal[[var_date]] <= input$dateRange[2]
                       ,]
       print(test)
       test
       #c(input$patient,input$dateRange[1],input$dateRange[2],dim(test))
     })
     
     output$plot <- renderPlot({
       
       datos.paciente<-plot_data()
       
       datos.paciente[[var_diagnosis]]<-datos.paciente[[var_prev_diagnosis]]
       datos.paciente[[var_prev_diagnosis]]<-NULL          
       
       vsl<-c(input$d_vars,input$s_vars,input$i_vars)
       #print(c(1,vsl))
       
       vmax<-as.Date(integer(),origin = "1970-01-01")
       maxval<-as.numeric()
       minval<-as.numeric()
       #print(c(2,vsl))
       for (vt in vsl) 
         vmax<-c(vmax,max(datos.paciente[!is.na(datos.paciente[[vt]]),c(var_date)]))
       
       mx<-max(vmax)
       dplot<-datos.paciente[datos.paciente[[var_date]]<=mx+2 ,]  


       
       dfscale<-dplot[,vsl]
       dfscale<-scale(dfscale)
       dfscale<-as.data.frame(dfscale)
       colnames(dfscale)<-vsl
       dplot[,vsl]<-NULL
       dplot<-cbind(dplot,dfscale)
       
       #print(c(3,dim(dplot)))
       
       df1<-dplot[,c(var_diagnosis,var_date,vsl)]  
       
       print(c(3.5,dim(dplot)))
       
       names(df1)[names(df1) ==  var_diagnosis] <-"Dx"
       df1$start<-df1[[var_date]]
       df1$end<-df1$start+1
       df1<-unique(df1)  
       
       #print(c(4,dim(dplot)))
       
       plt<-ggplot(dplot,aes_string(var_date))	
       
       #print(c(5,dim(dplot)))
       
       for (i in 1:length(vsl) ){
         maxval<-c(maxval,max(dplot[[vsl[i]]],na.rm=TRUE))
         minval<-c(minval,min(dplot[[vsl[i]]],na.rm=TRUE))
         
         plt<-plt+geom_line(aes_string(y=paste0("`",vsl[i],"`"),colour=paste0("'",vsl[i],"'")),size=0.1)
         
         plt<-plt+geom_smooth(data = dplot,
                              aes_string(x=var_date,paste0("`",vsl[i],"`"),
                                         colour=paste0("'",vsl[i],"'")),
                              span=0.2,size=2
         )
       }
       
       maxval<-max(maxval,na.rm = TRUE)
       minval<-min(minval,na.rm = TRUE)
       plt<-plt + geom_rect(data = df1, aes(xmin = start, xmax = end, ymin = minval, ymax = maxval, fill = Dx), alpha = 0.25)
       plt<-plt +	scale_x_date(date_breaks = "1 month",date_labels = "%d %b %y")  +
         theme(legend.title=element_blank(),
               axis.text=element_text(size=10,face="bold"),
               legend.text=element_text(size=15,face="bold"),
               legend.position="right",
               legend.box="vertical",
               axis.text.x = element_text(hjust = 1),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               axis.title=element_text(size=14,face="bold"))  
       plt<-plt + guides(color = guide_legend(order=1))

       print(plt)

                        },height = 350,width = 700)

  
    #output$text <-plot_data()
}