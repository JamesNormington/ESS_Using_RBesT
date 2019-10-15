list.of.packages <- c("RBesT", "shinyjs", "memoise", "data.table",
                      "DT", "shiny", "ggplot2", "pwr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, update = TRUE)

library(RBesT); library(shinyjs); library(ggplot2)
library(data.table); library(memoise); library(pwr)
library(DT); library(shiny)

## some gMAP calls below library this option
options(RBesT.MC.control=list(adapt_delta=0.9999))



ui = navbarPage("",
                
                tabPanel("Sample size",
                         
                         fluidRow(column(3,
                                         
                                         selectInput("endpt", "Endpoint type", choices = c("Binary", "Normal", "Poisson")),
                                         
                                         selectInput("samp", "One or two group trial?", choices = c("One", "Two")),
                                         
                                         
                                         
                                         conditionalPanel("input.endpt == 'Binary'",
                                                          
                                                          conditionalPanel("input.samp == 'One'", numericInput("p0_freq", HTML("<p> Null hypothesized event rate (p<sub>0,freq</sub>) </p>"), value = 0.2, min = 0.01, max = 0.99),
                                                                           
                                                                           numericInput("p1_alt", HTML("<p> Anticipated event rate </p>"), value = 0.35, min = 0,
                                                                                        
                                                                                        max = 1)),
                                                          
                                                          conditionalPanel("input.samp == 'Two'", numericInput("p1_trt", HTML("<p> Anticipated event rate, treatment group </p>"), value = 0.35, min = 0,
                                                                                                               
                                                                                                               max = 1),
                                                                           
                                                                           numericInput("p1_ctrl", HTML("<p> Anticipated event rate, control group </p>"), value = 0.5, min = 0,
                                                                                        
                                                                                        max = 1)
                                                                           
                                                          )),
                                         
                                         conditionalPanel("input.endpt == 'Normal'",
                                                          
                                                          numericInput("sigma", "Reference Scale (i.e., the standard deviation of the individual-level data)",
                                                                       
                                                                       value = 10, min = .Machine$double.eps),
                                                          
                                                          
                                                          
                                                          conditionalPanel("input.samp == 'One'", numericInput("mu_alt1", HTML("<p> Anticipated mean </p>"), value = 21),
                                                                           
                                                                           numericInput("mu0_freq", HTML("Null hypothesized mean (&mu;<sub>0,freq</sub>)"), value = 18)),
                                                          
                                                          conditionalPanel("input.samp == 'Two'", numericInput("mu_trt", HTML("<p> Anticipated mean, treatment group </p>"), value = 15),
                                                                           
                                                                           numericInput("mu_ctrl", "Anticipated mean, control group", value = 20))
                                                          
                                         ),
                                         
                                         # close Poisson conditionalPanel
                                         
                                         #numericInput("lambda1_alt", "Hypothesized rate, treatment group", value = 10, min = .Machine$double.xmin),
                                         
                                         #conditionalPanel("input.samp == 'One'", numericInput("lambda0_freq", HTML("1st null value, &lambda;"), value = 10)),
                                         
                                         #conditionalPanel("input.samp == 'Two'", numericInput("lambda2_alt", "Hypothesized rate, control group", value = 8, min = .Machine$double.xmin))
                                         
                                         # ),
                                         
                                         conditionalPanel("input.endpt != 'Poisson'",
                                                          
                                                          numericInput("power", "Desired power", value = 0.8, min = .Machine$double.xmin, max = 1 - .Machine$double.xmin),
                                                          
                                                          numericInput("alpha", "Desired Type I error (one-sided)", value = 0.05, min = .Machine$double.xmin, max = 1 - .Machine$double.xmin)
                                                          
                                         )
                                         
                         ), # close column
                         
                         column(3,
                                
                                selectInput("sidedness", "Lower or upper tail?", c("Upper", "Lower")),
                                
                                conditionalPanel("input.endpt == 'Binary'",
                                                 
                                                 conditionalPanel("input.samp == 'One'",
                                                                  
                                                                  HTML("<p> H<sub>0</sub>: p = p<sub>0,freq</sub> </p>"),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower'", HTML("<p> H<sub>1</sub>: p < p<sub>0,freq</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper'", HTML("<p> H<sub>1</sub>: p > p<sub>0,freq</sub> </p>"))
                                                                  
                                                 ),
                                                 
                                                 conditionalPanel("input.samp == 'Two'",
                                                                  
                                                                  HTML("<p> H<sub>0</sub>: p<sub>ctrl</sub> = p<sub>trt</sub> </p>"),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower'", HTML("<p> H<sub>1</sub>: p<sub>ctrl</sub> < p<sub>trt</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper'", HTML("<p> H<sub>1</sub>: p<sub>ctrl</sub> > p<sub>trt</sub> </p>"))
                                                                  
                                                 )
                                                 
                                ),
                                
                                conditionalPanel("input.endpt == 'Normal'",
                                                 
                                                 conditionalPanel("input.samp == 'One'",
                                                                  
                                                                  HTML("<p> H<sub>0</sub>: &mu; = &mu;<sub>0,freq</sub> </p>"),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower'", HTML("<p> H<sub>1</sub>: &mu; < &mu;<sub>0,freq</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper'", HTML("<p> H<sub>1</sub>: &mu; > &mu;<sub>0,freq</sub> </p>"))
                                                                  
                                                 ),
                                                 
                                                 conditionalPanel("input.samp == 'Two'",
                                                                  
                                                                  HTML("<p> H<sub>0</sub>: &mu;<sub>ctrl</sub> = &mu;<sub>trt</sub> </p>"),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower'", HTML("<p> H<sub>1</sub>: &mu;<sub>ctrl</sub> < &mu;<sub>trt</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper'", HTML("<p> H<sub>1</sub>: &mu;<sub>ctrl</sub> > &mu;<sub>trt</sub> </p>"))
                                                                  
                                                 )
                                                 
                                ),
                                
                                
                                
                                textOutput("hyp_samp_size")
                                
                         ) # close column
                         
                         ) # close fluidRow
                         
                ), # close tabPanel
                
                tabPanel("Compute ESS",
                         
                         sidebarLayout(
                           
                           sidebarPanel(tags$b("(1) Enter in historical meta-data."),
                                        
                                        selectInput("hist_data_option", "Enter in data manually or upload .csv", c("Manual", "Upload")),
                                        
                                        conditionalPanel("input.hist_data_option == 'Manual'",
                                                         
                                                         DT::dataTableOutput("table", width = 3),
                                                         
                                                         tags$hr(),
                                                         
                                                         checkboxInput("add_study", "Add study to table?"),
                                                         
                                                         conditionalPanel("input.add_study",
                                                                          
                                                                          textInput("study_add", "Name of added study", value = ""),
                                                                          
                                                                          numericInput("n_add", "Sample size of added study", value = 100, step = 1),
                                                                          
                                                                          conditionalPanel("input.endpt == 'Binary'",
                                                                                           
                                                                                           numericInput("r_add", "Number of events in added study", value = 0, step = 1)),
                                                                          
                                                                          conditionalPanel("input.endpt == 'Normal'",
                                                                                           
                                                                                           numericInput("m_add", "Sample mean in added study", value = 0)),
                                                                          
                                                                          conditionalPanel("input.endpt == 'Poisson'",
                                                                                           
                                                                                           numericInput("m_add_pois", "Total count in added study", value = 1000),
                                                                                           
                                                                                           numericInput("t_add", "Trial duration of added study", value = 10)),
                                                                          
                                                                          actionButton("add_btn", "Add study")),
                                                         
                                                         checkboxInput("delete_study", "Delete study from table?"),
                                                         
                                                         conditionalPanel("input.delete_study",
                                                                          
                                                                          numericInput("delete_row", "Number of row to delete", value = 1, step = 1),
                                                                          
                                                                          actionButton("delete_btn", "Delete study"))
                                                         
                                        ), # close Manual upload
                                        
                                        conditionalPanel("input.hist_data_option == 'Upload'",
                                                         
                                                         conditionalPanel("input.endpt == 'Binary'",
                                                                          
                                                                          fileInput("file1.bin", "Please upload a .csv file containing your trial meta-data. The first column should
                                                                                    
                                                                                    contain a unique name for the study. The second column should contain the size of the
                                                                                    
                                                                                    control arm. The third column should contain the number of events in the control arm. To change the
                                                                                    
                                                                                    endpoint type, go to the previous tab.",
                                                                                    
                                                                                    accept = c(
                                                                                      
                                                                                      "text/csv",
                                                                                      
                                                                                      "text/comma-separated-values,text/plain",
                                                                                      
                                                                                      ".csv"))),
                                                         
                                                         conditionalPanel("input.endpt == 'Normal'",
                                                                          
                                                                          fileInput("file1.norm", "Please upload a .csv file containing your trial meta-data. The first column should
                                                                                    
                                                                                    contain a unique name for the study. The second column should contain the size of the
                                                                                    
                                                                                    control arm. The third column should contain a normally-distributed outcome. To change the
                                                                                    
                                                                                    endpoint type, go to the previous tab.",
                                                                                    
                                                                                    accept = c(
                                                                                      
                                                                                      "text/csv",
                                                                                      
                                                                                      "text/comma-separated-values,text/plain",
                                                                                      
                                                                                      ".csv"))),
                                                         
                                                         conditionalPanel("input.endpt == 'Poisson'",
                                                                          
                                                                          fileInput("file1.pois", "Please upload a .csv file containing your trial meta-data. The first column should
                                                                                    
                                                                                    contain a unique name for the study. The second column should contain the size of the
                                                                                    
                                                                                    control arm. The third column should contain the sample mean. The fourth column is optional, and should
                                                                                    
                                                                                    contain the length of time the trial lasted. To change the
                                                                                    
                                                                                    endpoint type, go to the previous tab.",
                                                                                    
                                                                                    accept = c(
                                                                                      
                                                                                      "text/csv",
                                                                                      
                                                                                      "text/comma-separated-values,text/plain",
                                                                                      
                                                                                      ".csv"))
                                                                          
                                                         ),
                                                         
                                                         checkboxInput("header", "First row of table contains headers", TRUE)
                                                         
                                        ), # close Upload conditionalPanel
                                        
                                        tags$b(HTML("<p> (2) Specify priors for &beta; and &tau; </p>")),
                                        
                                        numericInput("seed", "Random seed (positive integer)", value=sample(1:10000, 1), min = 1),
                                        
                                        selectInput("tau_dist", HTML("<p> Specify prior for &tau; (between-trial standard deviation) </p>"),
                                                    
                                                    choices = c(`HalfNormal` = "HalfNormal", `TruncNormal` = "TruncNormal",
                                                                
                                                                `Uniform` = "Uniform", `Gamma` = "Gamma", `InvGamma` = "InvGamma",
                                                                
                                                                `LogNormal` = "LogNormal", `TruncCauchy` = "TruncCauchy",
                                                                
                                                                `Exponential` = "Exp", `Fixed` ="Fixed")),
                                        
                                        conditionalPanel("input.tau_dist == 'HalfNormal'", HTML("<p> &tau; ~ HalfNormal(&sigma;) </p>"),
                                                         
                                                         numericInput("tau_arg1_HN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                        
                                        conditionalPanel("input.tau_dist == 'TruncNormal'", HTML("<p> &tau; ~ TruncNormal(&mu;, &sigma;) </p>"),
                                                         
                                                         numericInput("tau_arg1_TN", HTML("<p> &mu; </p>"), value = 0, min = .Machine$double.xmin),
                                                         
                                                         numericInput("tau_arg2_TN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                        
                                        conditionalPanel("input.tau_dist == 'Uniform'", HTML("<p> &tau; ~ Uniform(a, b) </p>"),
                                                         
                                                         numericInput("tau_arg1_Uni", HTML("a"), value = 0),
                                                         
                                                         numericInput("tau_arg2_Uni", HTML("b"), value = 1)),
                                        
                                        conditionalPanel("input.tau_dist == 'Gamma'", HTML("<p> &tau; ~ Gamma(shape = &alpha;, rate = &beta;) </p>"),
                                                         
                                                         numericInput("tau_arg1_G", HTML("<p> &alpha; </p>"), value = 0.1),
                                                         
                                                         numericInput("tau_arg2_G", HTML("<p> &beta; </p>"), value = 0.1)),
                                        
                                        conditionalPanel("input.tau_dist == 'InvGamma'", HTML("<p> &tau; ~ InverseGamma(shape = &alpha;, scale = &beta;) </p>"),
                                                         
                                                         numericInput("tau_arg1_IG", HTML("<p> &alpha; </p>"), value = 0.001),
                                                         
                                                         numericInput("tau_arg2_IG", HTML("<p> &beta; </p>"), value = 0.001)),
                                        
                                        conditionalPanel("input.tau_dist == 'LogNormal'", HTML("<p> &tau; ~ LogNormal(&mu;, &sigma;) </p>"),
                                                         
                                                         numericInput("tau_arg1_LN", HTML("<p> &mu; </p>"), value = 0),
                                                         
                                                         numericInput("tau_arg2_LN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                        
                                        conditionalPanel("input.tau_dist == 'TruncCauchy'", HTML("<p> &tau; ~ TruncCauchy(&mu;, &sigma;) </p>"),
                                                         
                                                         numericInput("tau_arg1_TC", HTML("<p> &mu; </p>"), value = 0),
                                                         
                                                         numericInput("tau_arg2_TC", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                        
                                        conditionalPanel("input.tau_dist == 'Exp'", HTML("<p> &tau; ~ Exp(rate = &lambda;) </p>"),
                                                         
                                                         numericInput("tau_arg_exp", HTML("<p> &lambda; </p>"), value = 1)),
                                        
                                        conditionalPanel("input.tau_dist == 'Fixed'", HTML("<p> &tau; = &tau;<sub>0</sub> </p>"),
                                                         
                                                         numericInput("tau_fixed", HTML("<p> &tau;<sub>0</sub> </p>"), value = 1)),
                                        
                                        selectInput("beta_choice", HTML("<p> Specify prior for &beta; (common mean) </p>"), choices = c("Default", "Specify")),
                                        
                                        conditionalPanel("input.beta_choice == 'Specify'", HTML("<p> &beta; ~ Normal(&mu;, &sigma;) </p>"),
                                                         
                                                         numericInput("beta_mu", HTML("<p> &mu; </p>"), value = 0),
                                                         
                                                         numericInput("beta_sigma", HTML("<p> &sigma; </p>"), value = 100, min = .Machine$double.xmin)),
                                        
                                        conditionalPanel("input.beta_choice == 'Default'",
                                                         
                                                         conditionalPanel("input.endpt == 'Binary'", HTML("<p> Default prior is &beta; ~ N(0, 2) </p>")),
                                                         
                                                         conditionalPanel("input.endpt == 'Normal'", HTML("<p> Default prior is &beta; ~ N(0, 100 * &sigma;)) </p>")),
                                                         
                                                         conditionalPanel("input.endpt == 'Poisson'", HTML("<p> Default prior is &beta; ~ N(0, sd(log(y + 0.5 + log(t*n))))"))
                                                         
                                        )
                                        
                                        
                                        
                         ),
                         
                         mainPanel(actionButton("make.forest", tags$b("(3) Visualize MAP prior & compute ESS")),
                                   
                                   br(), br(),
                                   
                                   selectInput("method", "ESS method", c("ELIR (recommended)", "Moment", "Morita")),
                                   
                                   br(), br(),
                                   
                                   textOutput("unique_warn"),
                                   
                                   plotOutput("forest"),
                                   
                                   htmlOutput("plot_err"),
                                   
                                   htmlOutput("forest_plot_addendum"),
                                   tableOutput("MAP_summ"),
                                   textOutput("ESS"), br(),
                                   textOutput("ESS.rob")
                         ) # close MainPanel
                         ) # close SideBarLayout
                         ), # close tabPanel

                tabPanel("Run trial", 
                         fluidRow(column(3,
                                         numericInput("n.new", "Sample size of treatment group", value = 50, min = 1, step = 1),
                                         conditionalPanel("input.samp == 'Two'",
                                                          numericInput("n.ctrl", "Sample size of control group", value = 50, min = 1, step = 1)
                                         ),
                                         
                                         conditionalPanel(condition = "input.endpt == 'Binary'",
                                                          
                                                          numericInput("r.new", "Number of events within treatment group", value = 25, min = 1, step = 1),
                                                          
                                                          conditionalPanel(condition = "input.samp == 'Two'",
                                                                           
                                                                           numericInput("r.ctrl", "Number of events within control group", value = 50, min = 1, step = 1)
                                                                           
                                                          )),
                                         
                                         conditionalPanel(condition = "input.endpt == 'Normal'",
                                                          
                                                          numericInput("m.new", "Sample mean within treatment group", value = 20.5),
                                                          
                                                          conditionalPanel(condition = "input.samp == 'Two'",
                                                                           
                                                                           numericInput("m.ctrl", "Sample mean within control group", value = 18)
                                                                           
                                                          )),
                                         
                                         conditionalPanel(condition = "input.endpt == 'Poisson'",
                                                          
                                                          numericInput("y.new.pois", "Total count within treatment group", value = 1000, min = 0, step = 1),
                                                          
                                                          numericInput("t.new", "Trial duration", value = 12, min = .Machine$double.eps),
                                                          
                                                          conditionalPanel(condition = "input.samp == 'Two'",
                                                                           
                                                                           numericInput("y.ctrl.pois", "Total count within control group", value = 1100, min = 0, step = 1)
                                                                           
                                                          )),
                                         
                                         numericInput("wt", "Weight for robust prior", value = 0.2, min = 0, max = 1)
                                         
                         ), # close column
                         
                         column(3, selectInput("ncrit", "One or two success criteria?", c("One", "Two")),
                                
                                conditionalPanel("(input.endpt == 'Binary' & input.samp == 'Two') | (input.endpt == 'Poisson' & input.samp == 'Two')",
                                                 
                                                 selectInput("link", "Select a link function g() for the decision rule",
                                                             
                                                             choices = c("identity", "logit", "log"))),
                                
                                
                                
                                conditionalPanel("input.ncrit == 'One'",
                                                 
                                                 conditionalPanel("input.endpt == 'Binary'",
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'One'", HTML("<p> Decision rule: <br>P(p < p<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'One'", HTML("<p> Decision rule: <br>P(p > p<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>"))
                                                                  
                                                 ),
                                                 
                                                 conditionalPanel("input.endpt == 'Normal'",
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'One'", HTML("<p> Decision rule: <br>P(&mu; < &mu;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'One'", HTML("<p> Decision rule: <br>P(&mu; > &mu;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> < &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> > &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>"))
                                                                  
                                                 ),
                                                 
                                                 
                                                 
                                                 conditionalPanel("input.endpt == 'Poisson'",
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'", HTML("<p> Decision rule: <br>P(&lambda; < &lambda;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'", HTML("<p> Decision rule: <br>P(&lambda; > &lambda;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rule: <br>P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rule: <br>P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>"))
                                                                  
                                                 )
                                                 
                                ) # close one-criteria conditionalPanel
                                
                                ,
                                
                                
                                
                                conditionalPanel("input.ncrit == 'Two'" ,
                                                 
                                                 conditionalPanel("input.endpt == 'Binary'",
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(p < p<sub>0</sub>) > p<sub>crit</sub><br> (2) P(p < p<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(p > p<sub>0</sub>) > p<sub>crit</sub><br> (2) P(p > p<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>02</sub>) > p<sub>crit2</sub></p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>02</sub>) > p<sub>crit2</sub></p>"))
                                                                  
                                                 ),
                                                 
                                                 
                                                 
                                                 conditionalPanel("input.endpt == 'Binary' & input.samp == 'Two'",
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>02</sub>) > p<sub>crit2</sub></p>")),
                                                                  
                                                                  conditionalPanel("input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>02</sub>) > p<sub>crit2</sub></p>"))
                                                                  
                                                 ),
                                                 
                                                 
                                                 
                                                 conditionalPanel("input.endpt == 'Normal'",
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(&mu; < &mu;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu; < &mu;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(&mu; > &mu;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu; > &mu;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> < &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> < &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> > &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> > &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>"))
                                                                  
                                                 ),
                                                 
                                                 
                                                 
                                                 conditionalPanel("input.endpt == 'Poisson'",
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(&lambda; < &lambda;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&lambda; < &lambda;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(&lambda; > &lambda;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&lambda; > &lambda;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) > &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>"))
                                                                  
                                                                  
                                                                  
                                                 ) # close conditionalPanel Poisson
                                                 
                                ), # close two-criteria condtionalPanel
                                
                                
                                
                                conditionalPanel("input.endpt == 'Binary' & input.samp == 'One'", numericInput("p0", HTML("<p> Null value, p<sub>0</sub> </p>"), value = 0.4, min = 0.01, max = 0.99)),
                                
                                conditionalPanel("input.endpt == 'Binary' & input.samp == 'Two'", numericInput("diff0_bin", HTML("<p> Null value, &Delta;<sub>0</sub> </p>"), value = 0)),
                                
                                conditionalPanel("input.endpt == 'Normal' & input.samp == 'One'", numericInput("mu0", HTML("<p> Null value, &mu;<sub>0</sub> </p>"), value = 18, min = 0.01, max = 0.99)),
                                
                                conditionalPanel("input.endpt == 'Normal' & input.samp == 'Two'", numericInput("diff0_norm", HTML("<p> Null value, &Delta;<sub>0</sub> </p>"), value = 0)),
                                
                                conditionalPanel("input.endpt == 'Poisson' & input.samp == 'One'", numericInput("lambda0", HTML("<p> Null value, &lambda;<sub>0</sub> </p>"), value = 1, min = 0.01, max = 0.99)),
                                
                                conditionalPanel("input.endpt == 'Poisson' & input.samp == 'Two'",
                                                 
                                                 HTML("<p> &Delta;<sub>0</sub> &#8801; g(&lambda;<sub>0,ctrl</sub>) - g(&lambda;<sub>0,trt</sub>)</p>"),
                                                 
                                                 numericInput("lambda_ctrl", HTML("<p> Null value, &lambda;<sub>0,ctrl</sub> </p>"), value = 1.1),
                                                 
                                                 numericInput("lambda_trt", HTML("<p> Null value, &lambda;<sub>0,trt</sub> </p>"), value = 1)),
                                
                                
                                
                                numericInput("pc1", HTML("<p> Critical probability threshold, p<sub>crit</sub> </p>"), value = 0.975, min = .Machine$double.xmin, max = 1 - .Machine$double.xmin),
                                
                                conditionalPanel("input.ncrit == 'Two'",
                                                 
                                                 conditionalPanel("input.endpt == 'Binary' & input.samp == 'One'", numericInput("qc2_bin", HTML("<p> 2nd null value, p<sub>02</sub> </p>"), value = 0.3)),
                                                 
                                                 conditionalPanel("input.endpt == 'Normal' & input.samp == 'One'", numericInput("qc2_norm", HTML("<p> 2nd null value, &mu;<sub>02</sub> </p>"), value = 20)),
                                                 
                                                 conditionalPanel("input.endpt == 'Poisson' & input.samp == 'One'", numericInput("qc2_pois", HTML("<p> 2nd null value, &lambda;<sub>02</sub> </p>"), value = 0.8)),
                                                 
                                                 conditionalPanel("(input.endpt == 'Binary' & input.samp == 'Two') | (input.endpt == 'Normal' & input.samp == 'Two') | (input.endpt == 'Poisson' & input.samp == 'Two')", numericInput("qc2", HTML("<p> 2nd null value, &Delta;<sub>02</sub> </p>"), value = 0)),
                                                 
                                                 numericInput("pc2", HTML("<p> Critical probability threshold, p<sub>crit2</sub> </p>"), value = 0.5, min = .Machine$double.xmin, max = 1 - .Machine$double.xmin)
                                                 
                                )), # close column
                         
                         column(5,
                                
                                actionButton("do.trial", tags$b("Run the trial!")), br(),
                                
                                textOutput("warning.wt"),
                                
                                uiOutput("results2"),
                                
                                plotOutput("mixplot.H"),
                                
                                plotOutput("mixplot.noH"))
                         
                         ) # close fluidRow
                         
                ), # close tabPanel
                
                tabPanel("Help", uiOutput("link0"), br(), uiOutput("link1"), br(), uiOutput("link2"), br(), uiOutput("link3"), br(), uiOutput("link4")),
                
                tags$style(HTML("
                                
                                input[type=number] {
                                
                                -moz-appearance:textfield;
                                
                                }
                                
                                input[type=number]::{
                                
                                -moz-appearance:textfield;
                                
                                }
                                
                                input[type=number]::-webkit-outer-spin-button,
                                
                                input[type=number]::-webkit-inner-spin-button {
                                
                                -webkit-appearance: none;
                                
                                margin: 0;
                                
                                "))
                
                ) # close ui







server = function(input, output, session) {
  
  output$hyp_samp_size = renderText({
    
    if(!is.numeric(input$alpha) | input$alpha <= 0 | input$alpha >= 1) {return("Type I error rate must be a number strictly between 0 and 1.")}
    
    if(!is.numeric(input$power) | input$power <= 0 | input$power >= 1) {return("Desired power must be a number strictly between 0 and 1.")}
    
    
    
    if(input$endpt == "Binary") {
      
      if(input$samp == "One") {
        
        if(!is.numeric(input$p1_alt) | input$p1_alt < 0 | input$p1_alt > 1) {return("Anticipated event rate must be a number between 0 and 1.")}
        
        if(!is.numeric(input$p0_freq) | input$p0_freq < 0 | input$p0_freq > 1) {return("Null hypothesized event rate must be a number between 0 and 1.")}
        
        if(input$sidedness == "Lower" & input$p1_alt >= input$p0_freq) {return("Anticipated event rate must be less than null hypothesized value.")}
        
        if(input$sidedness == "Upper" & input$p1_alt <= input$p0_freq) {return("Anticipated event rate must be greater than null hypothesized value.")}
        
        
        
        samp.size = ceiling(pwr.p.test(h = 2*(asin(sqrt(input$p1_alt)) - asin(sqrt(input$p0_freq))), sig.level = input$alpha, power = input$power,
                                       
                                       alternative = ifelse(input$sidedness == "Lower", "less", "greater"))$n)
        
      } else {
        
        if(!is.numeric(input$p1_ctrl) | input$p1_ctrl < 0 | input$p1_ctrl > 1) {return("Anticipated event rates must be numbers between 0 and 1.")}
        
        if(!is.numeric(input$p1_trt) | input$p1_trt < 0 | input$p1_trt > 1) {return("Anticipated event rates must be numbers between 0 and 1.")}
        
        if(input$sidedness == "Lower" & input$p1_ctrl >= input$p1_trt) {return("Anticipated event rate in control group must be less than in treatment group.")}
        
        if(input$sidedness == "Upper" & input$p1_ctrl <= input$p1_trt) {return("Anticipated event rate in control group must be greater than in treatment group.")}
        
        samp.size = ceiling(power.prop.test(p1 = input$p1_ctrl, p2 = input$p1_trt, power = input$power,
                                            
                                            sig.level = input$alpha, alternative = "one.sided")$n)
        
      }
      
      paste0("Based on these inputs, the necessary sample size is ", samp.size,
             
             ifelse(input$samp == "Two", " per group", ""), ".")
      
    } else if(input$endpt == "Normal") {
      
      if(!is.numeric(input$sigma) | input$sigma <= 0) {return("Reference Scale must be a positive number.")}
      
      if(input$samp == "One") {
        
        if(!is.numeric(input$mu_alt1)) {return("Anticipated mean must be a number.")}
        
        if(!is.numeric(input$mu0_freq)) {return("Null hypothesized mean must be a number.")}
        
        if(input$sidedness == "Lower" & input$mu_alt1 >= input$mu0_freq) {return("Anticipated mean must be less than null hypothesized mean.")}
        
        if(input$sidedness == "Upper" & input$mu_alt1 <= input$mu0_freq) {return("Anticipated mean must be greater than null hypothesized mean.")}
        
        if(input$sidedness == "Lower") {
          
          samp.size = ceiling(power.t.test(delta = input$mu0_freq - input$mu_alt1, sd = input$sigma, sig.level = input$alpha, power = input$power,
                                           
                                           type = "one.sample", alternative = "one.sided")$n)
          
        } else{
          
          if(!is.numeric(input$mu_ctrl) | !is.numeric(input$mu_trt)) {return("Anticipated means must be numbers.")}
          
          samp.size = ceiling(power.t.test(delta = input$mu_alt1 - input$mu0_freq, sd = input$sigma, sig.level = input$alpha, power = input$power,
                                           
                                           type = "one.sample", alternative = "one.sided")$n)
          
        }
        
        
        
      } else{
        
        if(input$sidedness == "Lower" & input$mu_ctrl >= input$mu_trt) {return("Anticipated mean in control group must be less than in treatment group.")}
        
        if(input$sidedness == "Upper" & input$mu_ctrl <= input$mu_trt) {return("Anticipated mean in control group must be greater than in treatment group.")}
        
        if(input$sidedness == "Lower") {
          
          samp.size = ceiling(power.t.test(delta = input$mu_trt - input$mu_ctrl, sd = input$sigma, sig.level = input$alpha, power = input$power,
                                           
                                           type = "two.sample", alternative = "one.sided")$n)
          
        } else{
          
          samp.size = ceiling(power.t.test(delta = input$mu_ctrl - input$mu_trt, sd = input$sigma, sig.level = input$alpha, power = input$power,
                                           
                                           type = "two.sample", alternative = "one.sided")$n)
          
        }
        
        
        
      }
      
      paste0("Based on these inputs, the necessary sample size is ", samp.size,
             
             ifelse(input$samp == "Two", " per group", ""), ".")
      
    } else{
      
      "The current version of this app does not support sample size calculations for Poisson endpoints."
      
    }
    
  })
  
  
  
  this_table_bin = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), r = c(40, 50, 60), stringsAsFactors = FALSE)
  
  names(this_table_bin) = c("Study", "n", "# of events")
  
  this_table_norm = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), m = c(26, 23, 20), stringsAsFactors = FALSE)
  
  names(this_table_norm) = c("Study", "n", "Sample mean")
  
  this_table_pois = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), y = c(1113, 980, 1020), t = rep(12, 3), stringsAsFactors = FALSE)
  
  names(this_table_pois) = c("Study", "n", "Total count", "Trial duration")
  
  
  
  val = reactiveValues(mat = this_table_bin)
  
  val2 = reactiveValues(mat = this_table_norm)
  
  val3 = reactiveValues(mat = this_table_pois)
  
  
  
  which_val = function() {
    
    if(input$endpt == "Binary") {
      
      return(val) 
      
    } else if(input$endpt == "Normal") {
      
      return(val2)
      
    } else{
      
      return(val3)
      
    }
    
  }
  
  
  
  observeEvent(input$add_btn, {
    
    if(input$endpt == "Binary") {
      
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$r_add)))
      
      val$mat <<- t 
      
    } else if(input$endpt == "Normal") {
      
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$m_add)))
      
      val2$mat <<- t
      
    } else{
      
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$m_add_pois, input$t_add)))
      
      val3$mat <<- t
      
    }
    
  })
  
  
  
  observeEvent(input$delete_btn, {
    
    t = which_val()$mat
    
    if(!is.numeric(input$delete_row) | is.null(input$delete_row)) {
      
      t = t
      
    } else if(input$delete_row%%1 != 0) {
      
      t = t
      
    } else if(input$delete_row > nrow(t) | input$delete_row < 1) {
      
      t = t
      
    } else if(nrow(t) == 1) {
      
      t = t
      
    } else {
      
      t = t[-as.numeric(input$delete_row),]
      
    }
    
    if(input$endpt == "Binary") {
      
      val$mat <<- t 
      
    } else if(input$endpt == "Normal") {
      
      val2$mat <<- t
      
    } else{
      
      val3$mat <<- t
      
    }
    
  })
  
  
  
  # Create editable DataTabe of meta-data.
  
  output$table <- DT::renderDataTable({
    
    if(input$endpt == "Binary") {
      
      DT::datatable(val$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))),
                    
                    editable = TRUE, selection="single")
      
    } else if(input$endpt == "Normal") {
      
      DT::datatable(val2$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))),
                    
                    editable = TRUE, selection="single")
      
    } else{
      
      DT::datatable(val3$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))),
                    
                    editable = TRUE, selection="single")
      
    }
    
    
    
  }, server = TRUE)
  
  
  
  proxy = dataTableProxy('table')
  
  
  
  observeEvent(input$table_cell_edit, {
    
    info = input$table_cell_edit
    
    
    
    i = info$row
    
    j = info$col
    
    v = info$val
    
    
    
    if(input$endpt == "Binary") {
      
      val$mat[i,j] = DT::coerceValue(v, val$mat[i,j])
      
      replaceData(proxy, val$mat, rownames = FALSE)
      
    } else if(input$endpt == "Normal") {
      
      val2$mat[i,j] = DT::coerceValue(v, val2$mat[i,j])
      
      replaceData(proxy, val2$mat, rownames = FALSE)
      
    } else{
      
      val3$mat[i,j] = DT::coerceValue(v, val3$mat[i,j])
      
      replaceData(proxy, val3$mat, rownames = FALSE)
      
    }
    
    
    
  })
  
  
  
  meta.data = reactive({
    
    if(input$endpt == "Binary") {
      
      inFile = input$file1.bin
      
    } else if(input$endpt == "Normal") {
      
      inFile = input$file1.norm
      
    } else{
      
      inFile = input$file1.pois
      
    }
    
    if (is.null(inFile)) return(NULL)
    
    dat = read.csv(inFile$datapath, header = input$header)
    
    dat
    
  })
  
  
  
  # Error checking
  
  errCheck = function() {
    
    output$warning.wt = renderText({""})
    
    
    
    if(input$hist_data_option == "Manual") {
      
      co.data = which_val()$mat
      
    } else{
      
      co.data = meta.data()
      
    }
    
    
    
    co.data[,2] = as.numeric(co.data[,2])
    
    co.data[,3] = as.numeric(co.data[,3])
    
    
    
    # Error check historical data
    
    if(sum(co.data[,1] == "") > 0 | sum(is.na(co.data[,c(2,3)])) > 0) return("Please complete the table of historical trial data.")
    
    if(sum(co.data[,2] == "") > 0) return("Please complete the table of historical trial data.")
    
    if(sum(co.data[,3] == "") > 0) return("Please complete the table of historical trial data.")
    
    
    
    if(input$endpt == "Binary") {
      
      if(!is.numeric(input$p0) | input$p0 < 0 | input$p0 > 1){return(HTML("<p> Null value for P(event), one-sample trials must be between 0 and 1. </p>"))}
      
      if(input$ncrit == "Two" & (!is.numeric(input$qc2_bin) | input$qc2_bin < 0 | input$qc2_bin > 1)){return(HTML("<p> Null value for P(event), one-sample trials must be between 0 and 1. </p>"))}
      
      if(input$samp == "Two") {
        
        #if(!is.numeric(input$diff0_bin)) {return("Value for null hypothesis difference in two-sample trials must be a number")}
        
      }
      
      if(sum(co.data[,3]%%1 != 0) > 0) {return("Your 3rd column should contain control arm sample sizes, which are positive integers.") }
      
      if(sum(co.data[,2] < co.data[,3]) > 0) {return("Your number of events (r) cannot exceed your sample size (n).")}
      
      if(sum(co.data[,3] < 0) > 0) {return("Your 3rd column should contain # of events in control group, which are non-negative integers.")}
      
    } else if(input$endpt == "Normal") {
      
      if(!is.numeric(input$mu0)){return(HTML("<p> Null value &mu;<sub>0</sub> must be numeric! </p>"))}
      
      if(input$samp == "Two" & input$diff0_norm == "Two") {return("Value for null hypothesis difference in two-sample trials must be a number")}()
      
      if(!is.numeric(input$sigma) | input$sigma < 0) {return("Reference Scale must be a number greater than 0.")}
      
      if(input$sigma > 1000000) {return("Are you sure the Reference Scale is that high? Think about rescaling the data.")}
      
      if(input$ncrit == "Two" & !is.numeric(input$qc2_norm) ){return(HTML("<p> Null value &mu;<sub>02</sub> must be a number. </p>"))}
      
      
      
    } else{
      
      co.data[,4] = as.numeric(co.data[,4])
      
      if(sum(is.na(co.data[,4])) > 0) return("Please complete the table of historical trial data.")
      
      if(!is.numeric(input$lambda0) | input$lambda0 < 0){return(HTML("<p> Null value &lambda;<sub>0</sub> must be a non-negative number. </p>"))}
      
      if(input$ncrit == "Two" & (!is.numeric(input$qc2_pois) | input$qc2_pois < 0)){return(HTML("<p> Null value &lambda;<sub>02</sub> must be a non-negative number. </p>"))}
      
      if(sum(!is.numeric(co.data[,4])) > 0) return("Your 4th column should contain numbers.")
      
      if(((sum(co.data[,3] < 0) > 0) | (sum(co.data[,4] <= 0) > 0))) return("Your total counts (y) must be non-negative and your trial durations (t) must be positive.")
      
    }
    
    
    
    if(length(unique(co.data[,1])) != nrow(co.data)) {
      
      output$unique_warn = renderText({"Each row should uniquely define one study."})
      
    } else{
      
      output$unique_warn = renderText({""})
      
    }
    
    
    
    if(input$hist_data_option == "Manual") {
      
      if(sum(nchar(co.data[,1]) > 30)) {return("For readability, please keep the names of all studies below 30.")}
      
    }
    
    
    
    if(input$endpt == "Binary") {
      
      if(!is.numeric(input$n.new) | input$n.new <= 0) {return("Sample size in new trial must be a number greater than 0.")}
      
      if(!is.numeric(input$r.new) | input$r.new <= 0 | input$r.new >= input$n.new) {return("Number of events in new trial must be a non-negative number less than the sample size.")}
      
      if(input$samp == "Two") {
        
        if(!is.numeric(input$n.ctrl) | input$n.ctrl <= 0) {return("Sample size in new trial must be a number greater than 0.")}
        
        if(!is.numeric(input$r.ctrl) | input$r.ctrl <= 0 | input$r.new >= input$n.new) {return("Number of events in new trial must be a non-negative number less than the sample size.")}
        
      }
      
    } else if(input$endpt == "Normal") {
      
      if(!is.numeric(input$n.new) | input$n.new <= 0) {return("Sample size in new trial must be a number greater than 0.")}
      
      if(!is.numeric(input$m.new)) {return("Sample mean in new trial must be a number less than the sample size.")}
      
      if(input$samp == "Two") {
        
        if(!is.numeric(input$n.ctrl) | input$n.ctrl <= 0) {return("Sample size in new trial must be a number greater than 0.")}
        
        if(!is.numeric(input$m.ctrl)) {return("Number of events in new trial must be a number less than the sample size.")}
        
      }
      
    } else{
      
      if(!is.numeric(input$n.new) | input$n.new <= 0) {return("Sample size in new trial must be a number greater than 0.")}
      
      if(!is.numeric(input$y.new.pois) | input$y.new.pois%%1 != 0 | input$y.new.pois < 0) {return("Total count in new trial must be a non-negative integer.")}
      
      if(!is.numeric(input$t.new) | input$t.new <= 0) {return("Trial duration must be a positive number.")}
      
      if(input$samp == "Two") {
        
        if(!is.numeric(input$n.ctrl) | input$n.ctrl <= 0) {return("Sample size in new trial must be a number greater than 0.")}
        
        if(!is.numeric(input$y.ctrl.pois) | input$y.ctrl.pois <= 0) {return("Sample size in new trial must be a number greater than 0.")}
        
        
        
      }
      
    }
    
    
    
    if(sum(co.data[,2] <= 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.") 
    
    if(sum(co.data[,2]%%1 != 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.") 
    
    
    
    if(sum(!is.numeric(co.data[,2])) > 0) return("Your 2nd column should contain numbers.")
    
    
    
    if(sum(!is.numeric(co.data[,3])) > 0) return("Your 3rd column should contain numbers.")
    
    
    
    if(sum(co.data[,2] >= 100000) > 0) {
      
      output$samp_size_warn = renderText({"MAP prior is slower to compute for large sample sizes..."})
      
    } else{
      
      output$samp_size_warn = renderText({""})
      
    }
    
    
    
    if(!is.numeric(input$seed) | !input$seed%%1 == 0| input$seed < 1){return("Seed must be a positive integer.")}
    
    if(input$seed > 2147483647) {return("Seed must be less than 2147483647.")}
    
    if(!is.numeric(input$wt) | input$wt < 0 | input$wt > 1){return("Weight for non-informative prior must be between 0 and 1.")}
    
    if(input$wt > 0.7 & input$wt <= 1){
      
      output$warning.wt = renderText({
        
        ("Note: It is not recommended for the non-informative prior's weight to be greater than 0.7.")
        
      })
      
    }
    
    
    
    if(input$endpt == "Normal") {
      
      if(input$link != "identity") {return("Only identity link for Normal endpoint is sensible.")}
      
    }
    
    
    
    if(input$endpt == "Poisson") {
      
      if(input$link == "logit") {return("Logit link for Poisson endpoint is not sensible.")}
      
    }
    
    
    
    if(input$tau_dist == "HalfNormal" & (!is.numeric(input$tau_arg1_HN) | input$tau_arg1_HN <= 0)) {return(HTML(("<p> Parameter for &tau; prior must be a positive number. </p>")))}
    
    if(input$tau_dist == "TruncNormal" & (!is.numeric(input$tau_arg1_TN) | !is.numeric(input$tau_arg2_TN) | input$tau_arg1_TN < 0 | input$tau_arg2_TN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
    
    if(input$tau_dist == "Uniform" & (!is.numeric(input$tau_arg1_Uni) | !is.numeric(input$tau_arg2_Uni) | (input$tau_arg2_Uni <= input$tau_arg1_Uni) | input$tau_arg1_Uni < 0 | input$tau_arg2_Uni < 0)) {return(HTML(("<p> Parameters for &tau; prior must be numeric, with 0 <= a < b </p>")))}
    
    if(input$tau_dist == "Gamma" & (!is.numeric(input$tau_arg1_G) | !is.numeric(input$tau_arg2_G) | input$tau_arg1_G <= 0 | input$tau_arg2_G <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers. </p>")))}
    
    if(input$tau_dist == "InvGamma" & (!is.numeric(input$tau_arg1_IG) | !is.numeric(input$tau_arg2_IG) | input$tau_arg1_IG <= 0 | input$tau_arg2_IG <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers. </p>")))}
    
    if(input$tau_dist == "LogNormal" & (!is.numeric(input$tau_arg1_LN) | !is.numeric(input$tau_arg2_LN) | input$tau_arg1_LN < 0 | input$tau_arg2_LN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
    
    if(input$tau_dist == "TruncCauchy" & (!is.numeric(input$tau_arg1_TC) | !is.numeric(input$tau_arg2_TC) | input$tau_arg1_TC < 0 | input$tau_arg2_TC <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
    
    if(input$tau_dist == "Exp" & (input$tau_arg_exp <= 0 | !is.numeric(input$tau_arg_exp))) {return(HTML(("<p> &lambda; must be a positive number. </p>")))}
    
    if(input$tau_dist == "Fixed" & (input$tau_fixed <= 0 | !is.numeric(input$tau_fixed))) {return(HTML(("<p> &tau;<sub>0</sub> must be a positive number. </p>")))}
    
    
    
    if(input$beta_choice == "Specify") {
      
      if(!is.numeric(input$beta_mu)) {return(HTML("<p> Prior mean for &beta; must be a number. </p>"))}
      
      if(!is.numeric(input$beta_sigma) | input$beta_sigma <= 0) {return(HTML("<p> Prior standard deviation for &beta; must be a positive number. </p>"))}
      
    }
    
    
    
    if(!is.numeric(input$pc1) | input$pc1 < 0 | input$pc1 > 1){return("Critical probability threshold must be between 0 and 1.")}
    
    if(input$ncrit == "Two" & (!is.numeric(input$pc2) | input$pc2 < 0 | input$pc2 > 1)){return("Critical probability thresholds must be between 0 and 1.")}
    
  }
  
  
  
  # Compute gMAP prior
  
  get.gMAP = function(co.data, beta.prior, tau.prior, tau.dist) {
    
    
    
    if(sum(co.data[,1] == "") > 0 | sum(is.na(co.data[,c(2,3)])) > 0) return("Please complete the table of historical trial data.")
    
    if(sum(co.data[,2] == "") > 0) return("Please complete the table of historical trial data.")
    
    if(sum(co.data[,3] == "") > 0) return("Please complete the table of historical trial data.")
    
    if(input$endpt == "Poisson") {
      
      if(sum(is.na(co.data[,4])) > 0) return("Please complete the table of historical trial data.")
      
    }
    
    
    
    if(length(unique(co.data[,1])) != nrow(co.data)) {
      
      output$unique_warn = renderText({"Each row should uniquely define one study."})
      
    } else{
      
      output$unique_warn = renderText({""})
      
    }
    
    
    
    if(sum(co.data[,2] <= 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.") 
    
    if(sum(co.data[,2]%%1 != 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.") 
    
    
    
    if(sum(!is.numeric(co.data[,2])) > 0) return("Your 2nd column should contain numbers.")
    
    if(sum(co.data[,3] < 0) > 0 & input$endpt == "Binary") return("Your 3rd column should contain # of events in control group, which are non-negative integers.") 
    
    if(input$endpt == "Binary" & sum(co.data[,3]%%1 != 0) > 0) return("Your 3rd column should contain control arm sample sizes, which are positive integers.") 
    
    
    
    if(sum(!is.numeric(co.data[,3])) > 0) return("Your 3rd column should contain numbers.")
    
    if(input$endpt == "Binary" & (sum(co.data[,2] < co.data[,3]) > 0)) return("Your number of events (r) cannot exceed your sample size (n).")
    
    if(input$endpt == "Poisson") {
      
      if(sum(!is.numeric(co.data[,4])) > 0) return("Your 4th column should contain numbers.")
      
      if(((sum(co.data[,3] < 0) > 0) | (sum(co.data[,4] <= 0) > 0))) return("Your total counts (y) must be non-negative and your trial durations (t) must be positive.")
      
    }
    
    
    
    if(sum(co.data[,2] >= 100000) > 0) {
      
      output$samp_size_warn = renderText({"MAP prior is slower to compute for large sample sizes..."})
      
    } else{
      
      output$samp_size_warn = renderText({""})
      
    }
    
    
    
    if(input$endpt == "Binary") {
      
      names(co.data) = c("study", "n", "r")
      
      base.MAP.mc = gMAP(cbind(r, n-r) ~ 1 | study, co.data, family = binomial,
                         
                         tau.dist = input$tau_dist, tau.prior = tau.prior, beta.prior = beta.prior)
      
    } else if (input$endpt == "Normal") {
      
      names(co.data) = c("study", "n", "m")
      
      m.se = input$sigma / sqrt(co.data[,2])
      
      co.data = cbind(co.data, m.se)
      
      base.MAP.mc = gMAP(cbind(m, m.se) ~ 1 | study, family = gaussian,
                         
                         data = co.data,
                         
                         weights = n, beta.prior = beta.prior,
                         
                         tau.dist = input$tau_dist, tau.prior = tau.prior)
      
    } else {
      
      names(co.data) = c("study", "n", "y", "t")
      
      base.MAP.mc = gMAP(y ~ 1 + offset(log(t*n)) | study, family = poisson,
                         
                         data = co.data, beta.prior = beta.prior,
                         
                         tau.dist = input$tau_dist, tau.prior = tau.prior)
      
    }
    
    return(base.MAP.mc)
    
  }
  
  
  
  get.params = function() {
    
    if(input$hist_data_option == "Manual") {
      
      co.data = which_val()$mat
      
    } else{
      
      co.data = meta.data()
      
    }
    
    
    
    co.data[,2] = as.numeric(co.data[,2])
    
    co.data[,3] = as.numeric(co.data[,3])
    
    if(input$endpt == "Poisson") co.data[,4] = as.numeric(co.data[,4])
    
    
    
    if(input$beta_choice == "Default") {
      
      if(input$endpt == "Binary") {
        
        beta.prior = 2
        
      } else if(input$endpt == "Normal") {
        
        beta.prior = 100*input$sigma
        
      } else{
        
        beta.prior = sd(log(co.data[,3] + 0.5 + log(co.data[,2]*co.data[,4])))
        
      }
      
    } else{
      
      beta.prior = cbind(input$beta_mu, input$beta_sigma)
      
    }
    
    
    
    tau.dist = input$tau_dist
    
    
    
    if(tau.dist == "HalfNormal") tau.prior = cbind(0, input$tau_arg1_HN)
    
    if(tau.dist == "TruncNormal") tau.prior = cbind(input$tau_arg1_TN, input$tau_arg2_TN)
    
    if(tau.dist == "Uniform") tau.prior = cbind(input$tau_arg1_Uni, input$tau_arg2_Uni)
    
    if(tau.dist == "Gamma") tau.prior = cbind(input$tau_arg1_G, input$tau_arg2_G)
    
    if(tau.dist == "InvGamma") tau.prior = cbind(input$tau_arg1_IG, input$tau_arg2_IG)
    
    if(tau.dist == "LogNormal") tau.prior = cbind(input$tau_arg1_LN, input$tau_arg2_LN)
    
    if(tau.dist == "TruncCauchy") tau.prior = cbind(input$tau_arg1_TC, input$tau_arg2_TC)
    
    if(tau.dist == "Exp") tau.prior = input$tau_arg_exp
    
    if(tau.dist == "Fixed") tau.prior = input$tau_fixed
    
    
    
    return(list(co.data = co.data, beta.prior = beta.prior, tau.prior = tau.prior, tau.dist = tau.dist))
    
  }
  
  
  
  get.gMAP.mem = memoise(get.gMAP)
  
  
  
  # Function to make forest plot.
  
  makeForest = function() {
    error_check = errCheck()
    if(!is.null(error_check)) return(error_check)
    withProgress(message = "Computing MAP prior...", value = 0.5, {
      
      params = get.params()
      base.MAP.mc = get.gMAP.mem(params$co.data, params$beta.prior, params$tau.prior, params$tau.dist)
      MAP_summ = summary(base.MAP.mc)$theta.pred
      
      rownames(MAP_summ) = "Posterior summary of MAP prior"
      incProgress(amount = 0.25)
      setProgress(message = "Computing parametric approximation...")
      
      base.MAP = automixfit(base.MAP.mc)
      if(input$wt > 0 & input$wt < 1) {
        if(input$endpt == "Binary") {
          robust.mean = ifelse(input$samp == "One", input$p0, 0.5)
        } else if(input$endpt == "Normal") {
          robust.mean = ifelse(input$samp == "One", input$mu0, input$diff0_norm)
        } else{
          if(input$samp == "One") robust.mean = input$lambda0
        }
        
        if(input$endpt != "Poisson" | (input$endpt == "Poisson" & input$samp == "One")) {
          MAP.robust = robustify(base.MAP, weight = input$wt, mean = robust.mean)
        } else{ # Two-sample Poisson
          MAP.robust = robustify(base.MAP, weight = input$wt)
        }
      } else if(input$wt == 0) {
        MAP.robust = base.MAP
      } else {
        if(input$endpt == "Binary") {
          MAP.robust = mixbeta(c(1,1,1))
        } else if(input$endpt == "Normal") {
          MAP.robust = mixnorm(c(1, robust.mean, 100))
        } else{
          MAP.robust = mixgamma(c(1, 0.001, 0.001))
        }
      }
      if(input$method == "ELIR (recommended)") {
        ESS = ess(base.MAP, method = "elir")
        ESS.rob = ess(MAP.robust, method = "elir")
      } else if(input$method == "Moment") {
        ESS = ess(base.MAP, method = "moment")
        ESS.rob = ess(MAP.robust, method = "moment")
      } else{
        ESS = ess(base.MAP, method = "morita")
        ESS.rob = ess(MAP.robust, method = "morita")
      }
    })
  
    return(list(base.MAP.mc = base.MAP.mc, MAP_summ = MAP_summ, ESS = ESS, ESS.rob = ESS.rob))
  } # close makeForest() function
  
  # Call make forest function triggered by action button
  observeEvent(input$make.forest, {
    output$forest = renderPlot({})
    func.output = isolate(makeForest())
    
    if(class(func.output)[1] %in% c("character", "html")) {
      output$plot_err = renderText({func.output})
    } else{
      
      output$forest = renderPlot({
        
        plot(func.output$base.MAP.mc)$forest + theme(legend.position = "right") +
          labs(title=expression("Posterior medians of "*theta[1]*", ..., "*theta[H]*", "*theta*"*"*" with 95% credible intervals"))
      })
      
      output$plot_err = renderText({""})
      
      output$MAP_summ = renderTable({func.output$MAP_summ},
                                    
                                    rownames = TRUE, digits = 3)
      
      output$ESS = renderText({
        paste0("The effective sample size of the MAP prior is ", round(func.output$ESS), ".")
      })
      
      output$ESS.rob = renderText({
        paste0("The effective sample size of the MAP prior with robustification is ", round(func.output$ESS.rob), ".")
      })
    }
  }, ignoreInit = TRUE)
  
  
  
  #### Frequentist designs
  
  freqAnal = function(endpt, samp, sidedness, null = 0) {
    
    
    
    alt = ifelse(sidedness == "Lower", "less", 'greater')
    
    
    
    if(endpt == "Binary") {
      
      if(samp == "One") {
        
        freq.p = prop.test(input$r.new, input$n.new, alternative = alt, p = null)$p.
        
      } else {
        
        p.ctrl = input$r.ctrl / input$n.ctrl
        
        p.new = input$r.new / input$n.new
        
        SE = sqrt(p.ctrl * (1-p.ctrl) / input$n.ctrl + p.new*(1-p.new) / input$n.new)
        
        Z = (p.ctrl - p.new - null) / SE
        
        freq.p = pnorm(Z, lower.tail = ifelse(sidedness == "Lower", TRUE, FALSE))
        
      }
      
    } else if(endpt == "Normal") {
      
      if(samp == "One") {
        
        t = (input$m.new - null) / (input$sigma / sqrt(input$n.new))
        
        freq.p = pt(t, df = input$n.new - 1, lower.tail = ifelse(sidedness == "Lower", TRUE, FALSE))
        
      } else {
        
        t = ((input$m.ctrl - input$m.new) - null) / sqrt(input$sigma * ((1 / input$n.new + 1 / input$n.ctrl)))
        
        freq.p = pt(t, df = input$n.new + input$n.ctrl - 2, lower.tail = ifelse(input$sidedness == "Lower", TRUE, FALSE))
        
      }
      
      
      
    } else {
      
      if(samp == "One") {
        
        freq.p = poisson.test(input$y.new.pois, T = input$n.new * input$t.new,
                              
                              alternative = ifelse(sidedness == "Lower", "less", "greater"), r = null)$p.
        
      } else{
        
        freq.p = poisson.test(c(input$y.ctrl.pois, input$y.new.pois), r = input$lambda_ctrl/input$lambda_trt,
                              
                              T = c(input$n.ctrl * input$t.new, input$n.new * input$t.new),
                              
                              alternative = ifelse(sidedness == "Lower", "less", "greater"))$p.
        
      }
      
    }
    
    return(freq.p)
    
  }
  
  
  
  # Function to run the trial under hypothetical values
  
  doTrial = function() {
    
    
    
    error_check = errCheck()
    
    if(!is.null(error_check)) return(error_check)
    
    
    
    if(input$endpt == "Binary") samp = input$samp
    
    if(input$endpt == "Normal") samp = input$samp
    
    if(input$endpt == "Poisson") samp = input$samp
    
    
    
    withProgress(message = "Computing MAP prior...", value = 0.5, {
      
      
      
      set.seed(input$seed)
      
      
      
      params = get.params()
      
      base.MAP.mc = get.gMAP.mem(params$co.data, params$beta.prior, params$tau.prior, params$tau.dist)
      
      incProgress(amount = 0.25)
      
      setProgress(message = "Computing parametric approximation...")
      base.MAP = automixfit(base.MAP.mc)
      if(input$wt > 0 & input$wt < 1) {
        
        if(input$endpt == "Binary") {
          
          robust.mean = ifelse(input$samp == "One", input$p0, 0.5)
          
        } else if(input$endpt == "Normal") {
          
          robust.mean = ifelse(input$samp == "One", input$mu0, input$diff0_norm)
          
        } else{
          
          if(input$samp == "One") robust.mean = input$lambda0
          
        }
        
        if(input$endpt != "Poisson" | (input$endpt == "Poisson" & samp == "One")) {
          
          MAP.robust = robustify(base.MAP, weight = input$wt, mean = robust.mean)
          
        } else{ # Two-sample Poisson
          
          MAP.robust = robustify(base.MAP, weight = input$wt)
          
        }
        
      } else if(input$wt == 0) {
        MAP.robust = base.MAP
      } else {
        if(input$endpt == "Binary") {
          MAP.robust = mixbeta(c(1,1,1))
        } else if(input$endpt == "Normal") {
          MAP.robust = mixnorm(c(1, robust.mean, 100))
        } else{
          MAP.robust = mixgamma(c(1, 0.001, 0.001))
        }
      }
      incProgress(amount = 0.25)
    }
    )
  
    # Derive decision rule.
    
    if(samp == "One"){
      
      if(input$endpt == "Binary") qc1 = input$p0
      
      if(input$endpt == "Normal") qc1 = input$mu0
      
      if(input$endpt == "Poisson") qc1 = input$lambda0
      
    } else{
      
      if(input$endpt == "Binary") qc1 = input$diff0_bin
      
      if(input$endpt == "Normal") qc1 = input$diff0_norm
      
      if(input$endpt == "Poisson") {
        
        qc1 = ifelse(input$link == "identity", input$lambda_ctrl - input$lambda_trt, log(input$lambda_ctrl/input$lambda_trt))
        
      }
      
    }
    
    pc2 = qc2 = NULL
    
    if(input$ncrit == "Two") {
      
      if(input$endpt == "Binary") qc2 = input$qc2_bin
      
      if(input$endpt == "Normal") qc2 = input$qc2_norm
      
      if(input$endpt == "Poisson") qc2 = input$qc2_pois
      
      if(samp == "Two") qc2 = input$qc2
      
      
      
      pc2 = input$pc2
      
    }
    
    
    
    link = "identity"
    
    if(input$endpt != "Normal") {
      
      link = input$link
      
    }
    
    
    
    lower.tail = ifelse(input$sidedness == "Lower", TRUE, FALSE)
    
    if(samp == "One") {
      
      decision = decision1S(pc = c(input$pc1, pc2), qc = c(qc1, qc2), lower.tail = lower.tail)
      
    } else{
      
      decision = decision2S(pc = c(input$pc1, pc2), qc = c(qc1, qc2), lower.tail = lower.tail, link = link)
      
    }
    
    
    
    prob2.noH = prob2.H = NULL
    
    ctrl.post.H = ctrl.post.noH = NULL
    
    if(input$endpt == "Binary") {
      
      treat.prior = mixbeta(c(1, 1, 1))
      
      ## Without historical data
      
      treat.post.noH = postmix(treat.prior, n = input$n.new, r = input$r.new)
      
      
      
      if(samp == "One") {
        
        ## With historical data
        
        treat.post.H = postmix(MAP.robust, n = input$n.new, r = input$r.new)
        
        prob.noH = pmix(treat.post.noH, c(qc1,qc2), lower.tail = lower.tail)
        
        prob.H = pmix(treat.post.H, c(qc1,qc2), lower.tail = lower.tail)
        
      } else{
        
        treat.post.H = treat.post.noH
        
        ctrl.post.noH = postmix(treat.prior, n = input$n.ctrl, r = input$r.ctrl)
        
        ctrl.post.H = postmix(MAP.robust, n = input$n.ctrl, r = input$r.ctrl)
        
        prob.noH = pmixdiff(ctrl.post.noH, treat.post.noH, c(qc1,qc2), lower.tail = lower.tail)
        
        prob.H = pmixdiff(ctrl.post.H, treat.post.H, c(qc1,qc2), lower.tail = lower.tail)
        
      }
      
      
      
      
      
    } else if(input$endpt == "Normal") {
      
      treat.prior = mixnorm(c(1, 0, 100), sigma = input$sigma)
      
      
      
      ## Without historical data
      
      treat.post.noH = postmix(treat.prior, n = input$n.new, m = input$m.new)
      
      
      
      if(samp == "One") {
        
        treat.post.H = postmix(MAP.robust, n = input$n.new, m = input$m.new)
        
        prob.noH = pmix(treat.post.noH, c(qc1,qc2), lower.tail = lower.tail)
        
        prob.H = pmix(treat.post.H, c(qc1,qc2), lower.tail = lower.tail)
        
      } else{
        
        treat.post.H = treat.post.noH
        
        ctrl.post.noH = postmix(treat.prior, n = input$n.ctrl, m = input$m.ctrl)
        
        ctrl.post.H = postmix(MAP.robust, n = input$n.ctrl, m = input$m.ctrl)
        
        prob.noH = pmixdiff(ctrl.post.noH, treat.post.noH, c(qc1,qc2), lower.tail = lower.tail)
        
        prob.H = pmixdiff(ctrl.post.H, treat.post.H, c(qc1,qc2), lower.tail = lower.tail)
        
      }
      
    } else{ # Poisson
      
      treat.prior = mixgamma(c(1, 0.001, 0.001))
      
      
      
      ## Without historical data
      
      treat.post.noH = postmix(treat.prior, n = input$n.new, m = input$y.new.pois/(input$n.new*input$t.new))
      
      treat.post.noH[1] = 1
      
      
      
      if(samp == "One") {
        
        treat.post.H = postmix(MAP.robust, n = input$n.new, m = input$y.new.pois/(input$n.new*input$t.new))
        
        treat.post.H[1,] = MAP.robust[1,]
        
        prob.noH = pmix(treat.post.noH, c(qc1,qc2), lower.tail = lower.tail)
        
        prob.H = pmix(treat.post.H, c(qc1,qc2), lower.tail = lower.tail)
        
      } else{
        
        treat.post.H = treat.post.noH
        
        ctrl.post.noH = postmix(treat.prior, n = input$n.ctrl, m = input$y.ctrl.pois/(input$n.ctrl*input$t.new))
        
        ctrl.post.H = postmix(MAP.robust, n = input$n.ctrl, m = input$y.ctrl.pois/(input$n.ctrl*input$t.new))
        
        ctrl.post.noH[1] = 1
        
        ctrl.post.H[1,] = MAP.robust[1,]
        
        prob.noH = pmixdiff(ctrl.post.noH, treat.post.noH, c(qc1,qc2), lower.tail = lower.tail)
        
        prob.H = pmixdiff(ctrl.post.H, treat.post.H, c(qc1,qc2), lower.tail = lower.tail)
        
      }
      
    }
    
    
    
    if(samp == "One") {
      
      success.noH = decision(treat.post.noH)
      
      success.H = decision(treat.post.H)
      
    } else{
      
      success.noH = decision(ctrl.post.noH, treat.post.noH)
      
      success.H = decision(ctrl.post.H, treat.post.H)
      
    }
    
    
    
    
    
    freq.p = isolate(freqAnal(endpt = input$endpt, samp = samp,
                              
                              sidedness = input$sidedness, null = qc1))
    
    freq.p2 = NULL
    
    if(input$ncrit == "Two") {
      
      freq.p2 = isolate(freqAnal(endpt = input$endpt, samp = samp,
                                 
                                 sidedness = input$sidedness, null = qc2))
      
      prob2.noH = prob.noH[2]
      
      prob2.H = prob.H[2]
      
    }
    
    
    
    
    
    return(list(success.noH = success.noH, success.H = success.H, prob.H = prob.H[1], prob.noH = prob.noH[1],
                
                prob2.H = prob2.H, prob2.noH = prob2.noH, freq.p = freq.p, freq.p2 = freq.p2,
                
                mix.obj.H = treat.post.H, mix.obj.noH = treat.post.noH,
                
                mix.obj.H.ctrl = ctrl.post.H, mix.obj.noH.ctrl = ctrl.post.noH))
    
  }
  
  
  
  # Compute ESS triggered by action button.
  
  observeEvent(input$do.trial, {
    
    func.output = isolate(doTrial())
    
    
    
    if(class(func.output)[1] %in% c("html", "character")) {
      
      output$results2 = renderUI({func.output})
      
    } else{
      
      string.noH = ifelse(func.output$success.noH == 1, "success", "failure")
      
      string.H = ifelse(func.output$success.H == 1, "success", "failure")
      
      sided.string = ifelse(input$sidedness == "Lower", "less", "greater")
      
      string.freq2 = ""
      
      
      
      string2 = ifelse(input$ncrit == "One", "", paste0("&emsp; 2nd criteria: ", round(func.output$freq.p2, 3)))
      
      string3 = ifelse(input$ncrit == "One", "", "s")
      
      string4 = ifelse(input$ncrit == "One", "", paste0("&emsp; Post. prob., 2nd criteria: ", round(func.output$prob2.noH, 3), "<br />"))
      
      string5 = ifelse(input$ncrit == "One", "", paste0("&emsp; Post. prob., 2nd criteria: ", round(func.output$prob2.H, 3), "<br />"))
      
      
      
      output$results2 = renderUI({
        
        HTML(paste0("<p>(1) Using MAP prior: trial ", string.H, "<br />",
                    
                    "&emsp; Post. prob., 1st criteria: ", round(func.output$prob.H, 3), "<br />",
                    
                    string4,
                    
                    "(2) Not using MAP prior: trial ", string.noH, "<br />",
                    
                    "&emsp; Post. prob., 1st criteria: ", round(func.output$prob.noH, 3), "<br />",
                    
                    string5,
                    
                    "(3) Frequentist p-value", string3, "<br />",
                    
                    "&emsp; 1st criteria: ", round(func.output$freq.p, 3), "<br />",
                    
                    string2, "</p>"))
        
      })
      
      
      
      plot.H = plot.noH = plot.new()
      
      if(input$samp == "One") {
        
        
        
        if(input$samp == "One"){
          
          if(input$endpt == "Binary") qc1 = input$p0
          
          if(input$endpt == "Normal") qc1 = input$mu0
          
          if(input$endpt == "Poisson") qc1 = input$lambda0
          
        } else{
          
          if(input$endpt == "Binary") qc1 = input$diff0_bin
          
          if(input$endpt == "Normal") qc1 = input$diff0_norm
          
          if(input$endpt == "Poisson") {
            
            qc1 = ifelse(input$link == "identity", input$lambda_ctrl - input$lambda_trt, log(input$lambda_ctrl/input$lambda_trt))
            
          }
          
        }
        
        
        
        if(input$sidedness == "Lower") {
          
          xlims = c(qmix(func.output$mix.obj.noH, 0.001), qc1)
          
          crit1.noH = qmix(func.output$mix.obj.noH, input$pc1)
          
          crit1.H = qmix(func.output$mix.obj.H, input$pc1)
          
        } else{
          
          xlims = c(qc1, qmix(func.output$mix.obj.noH, 0.999))
          
          crit1.noH = qmix(func.output$mix.obj.noH, 1-input$pc1)
          
          crit1.H = qmix(func.output$mix.obj.H, 1-input$pc1)
          
        }
        
        
        
        cols = c("Post. prob., 1st" = "#84CA72", "1st criteria" = "dotted", "Post. prob., 2nd" = "#CA7294", "2nd criteria" = "dashed")
        
        
        
        plot.H = plot(func.output$mix.obj.H, comp = FALSE) + labs(title = expression("Posterior of "*theta*"* using MAP prior")) + xlab(expression(theta*"*")) + stat_function(fun = ~RBesT::dmix(func.output$mix.obj.H, .x), xlim = xlims, geom = "area", alpha = 0.3, aes(fill = "Post. prob., 1st")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit1.H, linetype = "1st criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
        
        plot.noH = plot(func.output$mix.obj.noH, comp = FALSE) + xlab(expression(theta*"*")) + labs(title = expression("Posterior of "*theta*"* using non-informative prior")) + stat_function(fun = ~RBesT::dmix(func.output$mix.obj.noH, .x), xlim = xlims, geom = "area", alpha = 0.3, aes(fill="Post. prob., 1st")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit1.noH, linetype = "1st criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
        
        
        
        if(input$ncrit == "Two") {
          
          if(input$endpt == "Binary") qc2 = input$qc2_bin
          
          if(input$endpt == "Normal") qc2 = input$qc2_norm
          
          if(input$endpt == "Poisson") qc2 = input$qc2_pois
          
          if(input$samp == "Two") qc2 = input$qc2
          
          
          
          if(input$sidedness == "Lower") {
            
            xlims2 = c(qmix(func.output$mix.obj.noH, 0.001), qc2)
            
            crit2.noH = qmix(func.output$mix.obj.noH, input$pc2)
            
            crit2.H = qmix(func.output$mix.obj.H, input$pc2)
            
          } else{
            
            xlims2 = c(qc2, qmix(func.output$mix.obj.noH, 0.999))
            
            crit2.noH = qmix(func.output$mix.obj.noH, 1-input$pc2)
            
            crit2.H = qmix(func.output$mix.obj.H, 1-input$pc2)
            
          }
          
          plot.H = plot.H + stat_function(fun = ~dmix(func.output$mix.obj.H, .x), xlim = xlims2, geom = "area", alpha = 0.3, aes(fill = "Post. prob., 2nd")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit2.H, linetype = "2nd criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
          
          plot.noH = plot.noH + stat_function(fun = ~dmix(func.output$mix.obj.noH, .x), xlim = xlims2, geom = "area", alpha = 0.3, aes(fill="Post. prob., 2nd")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit2.noH, linetype = "2nd criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
          
        }
        
        
        
      }
      
      
      
      if(input$samp == "Two") {
        if(input$samp == "One"){
          
          if(input$endpt == "Binary") qc1 = input$p0
          if(input$endpt == "Normal") qc1 = input$mu0
          if(input$endpt == "Poisson") qc1 = input$lambda0
          
        } else{
          if(input$endpt == "Binary") qc1 = input$diff0_bin
          if(input$endpt == "Normal") qc1 = input$diff0_norm
          if(input$endpt == "Poisson") {
            qc1 = ifelse(input$link == "identity", input$lambda_ctrl - input$lambda_trt, log(input$lambda_ctrl/input$lambda_trt))
          }
        }
        
        if(input$sidedness == "Lower") {
          xlims = c(qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, 0.001), qc1)
          crit1.noH = qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, input$pc1)
          crit1.H = qmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, input$pc1)
        } else{
          xlims = c(qc1, qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, 0.999))
          crit1.noH = qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, 1-input$pc1)
          crit1.H = qmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, 1-input$pc1)
        }
        
        cols = c("Post. prob., 1st" = "#84CA72", "1st criteria" = "dotted", "Post. prob., 2nd" = "#CA7294", "2nd criteria" = "dashed")
        bounds.H = c(qmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, c(0.001, 0.999)))
        plot.H = ggplot(data.frame(x = bounds.H), aes(x)) + labs(title = expression("Posterior of "*theta*"* using MAP prior")) + xlab(expression(theta*"*")) + stat_function(fun = ~RBesT::dmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, .x), xlim = qmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, c(0.001, 0.999))) + stat_function(fun = ~RBesT::dmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, .x), xlim = xlims, geom = "area", alpha = 0.3, aes(fill = "Post. prob., 1st")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit1.H, linetype = "1st criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
        plot.H = plot.H + stat_function(fun = ~dmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, .x), xlim = bounds.H)
        bounds.noH = c(qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, c(0.001, 0.999)))
        plot.noH = ggplot(data.frame(x = bounds.noH), aes(x)) + xlab(expression(theta*"*")) + labs(title = expression("Posterior of "*theta*"* using non-informative prior")) + stat_function(fun = ~RBesT::dmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, .x), xlim = xlims, geom = "area", alpha = 0.3, aes(fill="Post. prob., 1st")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit1.noH, linetype = "1st criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
        plot.noH = plot.noH + stat_function(fun = ~dmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, .x), xlim = bounds.noH)
        
        if(input$ncrit == "Two") {
          
          if(input$endpt == "Binary") qc2 = input$qc2_bin
          if(input$endpt == "Normal") qc2 = input$qc2_norm
          if(input$endpt == "Poisson") qc2 = input$qc2_pois
          if(input$samp == "Two") qc2 = input$qc2
          
          if(input$sidedness == "Lower") {
            xlims2 = c(qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, 0.001), qc2)
            crit2.noH = qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, input$pc2)
            crit2.H = qmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, input$pc2)
          } else{
            xlims2 = c(qc2, qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, 0.999))
            crit2.noH = qmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, 1-input$pc2)
            crit2.H = qmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, 1-input$pc2)
          }
          plot.H = plot.H + stat_function(fun = ~RBesT::dmixdiff(func.output$mix.obj.H.ctrl, func.output$mix.obj.H, .x), xlim = xlims2, geom = "area", alpha = 0.3, aes(fill = "Post. prob., 2nd")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit2.H, linetype = "2nd criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
          plot.noH = plot.noH + stat_function(fun = ~RBesT::dmixdiff(func.output$mix.obj.noH.ctrl, func.output$mix.obj.noH, .x), xlim = xlims2, geom = "area", alpha = 0.3, aes(fill = "Post. prob., 2nd")) + scale_fill_manual("Criteria", values = cols) + geom_vline(aes(xintercept = crit2.noH, linetype = "2nd criteria")) + scale_linetype_manual("Critical thresholds", values = cols)
        }
      }
      output$mixplot.H = renderPlot({
        plot.H
      })
      
      output$mixplot.noH = renderPlot({
        plot.noH
      })
    }
  }, ignoreInit = TRUE)

  URL0 = a("Click here for a walkthrough of this Shiny app (Chrome or Firefox only).", href = "https://github.com/JamesNormington/ESS_Using_RBesT/blob/master/ESS%20App%20Walkthrough.pdf")
  output$link0 = renderUI({
    tagList(URL0)
  })
  
  URL1 = a("Click here for a walkthrough of the methodology and code (Chrome or Firefox only).", href = "https://github.com/JamesNormington/ESS_Using_RBesT/blob/master/ESS%20Code%20and%20Methodology%20Walkthrough.pdf")
  output$link1 = renderUI({
    tagList(URL1)
  })
  
  URL2 = a("Click here for Yue Li's walkthrough of RBesT for a normal endpoint, including ESS calculation.", href = "https://cran.r-project.org/web/packages/RBesT/vignettes/introduction_normal.html")
  output$link2 = renderUI({
    tagList(URL2)
  })
  
  URL3 = a("Click here for RBesT documentation on CRAN.", href = "https://cran.r-project.org/web/packages/RBesT/RBesT.pdf")
  output$link3 = renderUI({
    tagList(URL3)
  })
  
  URL4 = a("Click here for Sebastian Weber et al.'s RBesT paper on arXiv.", href = "https://arxiv.org/pdf/1907.00603.pdf")
  output$link4 = renderUI({
    tagList(URL4)
  })

} # close server() function
shinyApp(ui, server)
