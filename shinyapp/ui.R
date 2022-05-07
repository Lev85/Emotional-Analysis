library(ggplot2)

# values to show, or not show, these will be the 'choices' and 'selected' values
# for the checkboxGroupInput()
vrs<-names(dftotal)
d_vars<-vrs[grepl('^D_',vrs)]
s_vars<-vrs[grepl('^S_',vrs)]
ind_vars<-c(vrs[grepl('^Y_',vrs)],vrs[grepl('^H_',vrs)])
all_rows <- 1:25
names(all_rows) <- paste("Row", all_rows)

# data control: the checkboxes will control the data values plotted
s_controls <-
  list(h4("Smartwatch variables"), 
       tags$div(class = 'multicol',
                checkboxGroupInput(inputId  = 's_vars',
                                   label    = NULL,
                                   choices  = s_vars,
                                   selected = NULL,
                                   inline   = FALSE))
       ) 

d_controls <-
  list(h4("Self-report variables"), 
       tags$div(class = 'multicol',
                checkboxGroupInput(inputId  = 'd_vars',
                                   label    = NULL,
                                   choices  = d_vars,
                                   selected = NULL,
                                   inline   = FALSE))
  ) 

ind_controls <-
  list(h4("Medical Evaluation"), 
       tags$div(class = 'multicol',
                checkboxGroupInput(inputId  = 'i_vars',
                                   label    = NULL,
                                   choices  = ind_vars,
                                   selected = NULL,
                                   inline   = FALSE))
  ) 


pts<-unique(dftotal[,c("A_id","A_newid")])
pts<-pts[order(pts[["A_newid"]]), ]

ptchoices<-pts$A_id
names(ptchoices)<-pts$A_newid


ui <- fluidPage(titlePanel(h3("Variable visualization", align = "center")),
                tags$style(type='text/css',"
                label {font-size: 10px; }
                .form-group {margin-top: 1px; margin-bottom: 1px;}
                .nav-tabs {font-family:'arial';font-size:20px}
                input[type=checkbox] {transform: scale(1);margin-top:1px;}
                .multicol {height: auto;
                          -webkit-column-count: 5; /* Chrome, Safari, Opera */
                          -moz-column-count: 5;    /* Firefox */
                          column-count: 5;
                          -moz-column-fill: auto;
                          -column-fill: auto;}
                .btn {display:block;height: 60px;width: 40px;border-radius: 50%;}
                #modules .checkbox label span {font-weight:bold;}
                #label {color:#fff;}
                .checkbox, .radio {margin-top: 8px; margin-bottom: -10px;}
                .selectize-dropdown-content {max-height: 200px; }
                .input-daterange input {min-height: 34px;}
                "),
                #fluidRow(column(width = 12, d_controls),
                #         column(width = 12, s_controls))
                 sidebarLayout(
                     #position = "left",
                     wellPanel(
                       fluidRow( 
                                  
                                      column(
                                          width=1, 
                                          HTML('<h4>Patient</h4>')
                                        ),
                                      column(
                                        width=2,
                                        selectInput(
                                          "patient",
                                          label = NULL,
                                          choices = ptchoices
                                        )                                        
                                      ),
                                      column(
                                        width=1, 
                                        HTML('<h4></h4>')
                                      ),                                      
                                      column(
                                        width=1, 
                                        HTML('<h4>Dates</h4>')
                                      ),                                      
                                      column(
                                          width=3,
                                          dateRangeInput(
                                            'dateRange',
                                            label = NULL,
                                            start = Sys.Date() - 2, 
                                            end = Sys.Date() + 2
                                          )
                                        )
                                      ),
                                d_controls,s_controls,ind_controls,
                                style ="overflow-y:scroll;
                                        max-height: 250px;
                                        margin-left:10px;
                                        margin-right:10px"
                              ),
                     #textOutput(outputId = "plot", height = "300px")
                     fluidRow(
                       #column(width=6,align="center",
                       #  plotOutput("plot")
                       #)
                       column(width=12,
                              div(plotOutput("plot", height = "100%"), align = "center")
                       )                         
                     )
                     
                     
                     
                   
                     
                     
                     #textOutput("text")
                   )
                #basicPage(d_controls,s_controls)
                )