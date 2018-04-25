library(shiny)
library(DT)
library(dplyr)
library(purrr)
library(ISLR)
library(data.table)
library(tidyr)
library(corrplot)
library(ggplot2)
library(tools)
library(caret)
library(shinycssloaders)
library(highcharter)
library(ggcorrplot)
library(rsconnect)
library(plotly)
library(shinythemes)
library(glmnet)
library(ranger)
library(e1071)
library(xgboost)
library(stringr)

# Define UI for application that draws a histogram
ui<-shinyUI(
    navbarPage("Data Analysis and Modeling",
               theme = shinytheme("flatly"),
                       ##1.) Panel Data Upload and Overview
                       tabPanel("Data Upload and Overview",
                                
                                sidebarPanel(fileInput("file", "Select a file", accept = c("text/csv",
                                                                                           "text/comma-separated-values,text/plain",
                                                                                           ".csv")),
                                             tags$hr(),
                                             #Dataset
                                             selectInput("Dataset", "Choose Dataset:",
                                                          choices = c(Choose='',ls("package:ISLR"))
                                                          ),
                                             tags$hr(),
                                             uiOutput("MakeNumericOut"),
                                             tags$hr(),
                                             uiOutput("MakeFactorOut"),
                                             actionButton("goButton", "Change data type!")

                                             
                                             
                                             
                                             ),
                                mainPanel(
                                tabsetPanel(id="Tabset1",
                                  
                                  
                                  tabPanel(title="Top 10",DT::dataTableOutput(outputId = "table")),
                                  tabPanel(title="Summary",DT::dataTableOutput(outputId = "Summary")),
                                  tabPanel(title="Additional Information",
                                                                         uiOutput("documentation"))
                                  
                                ))),
                       ##2Panel
                       tabPanel("Data Visualisation",
                                tabsetPanel(tabPanel("CorrPlot",
                               
                                  mainPanel(actionButton("CorrButton", "Plot!"),
                                            plotOutput(outputId = "corrplot"))
                                 
                                ),
                                tabPanel("BarChart",sidebarPanel(
                                  selectInput(inputId = "SummaryStat",label = "Choose Summary Statistic",
                                              choices = c("Count"="count",
                                                          "Mean"="mean",
                                                          "Median"="median",
                                                          "Standard Deviation"="sd",
                                                          "Sum"="sum"),
                                              selected = "count"
                                                ),
                                  conditionalPanel(condition = "input.SummaryStat=='count'",
                                                   uiOutput("X_Factor_Bar_Out"),
                                                   selectInput(inputId = "BarType",label = "Choose Position",
                                                   choices=c("dodge","fill","stack"),
                                                   selected = "stack"
                                                   )
                                                   ),
                                  conditionalPanel(condition ="input.SummaryStat!='count'",
                                                   uiOutput("X_Summary_Bar_Out"),
                                                   uiOutput("Y_Numeric_Bar_Out")),
                                                   uiOutput("Col_Bar_Out"),
                                                   uiOutput("Facet_Bar_Out"),
                                                  actionButton("BarButton","Plot!")  
                                  
                                  
                                ),mainPanel(plotOutput(outputId = "bar"))
                                         
                                         ),
                                tabPanel("BoxPlot",sidebarPanel(
                                  uiOutput("X_Box_Out"),
                                  uiOutput("Y_Box_Out"),
                                  uiOutput("Col_Box_Out"),
                                  uiOutput("Facet_Box_Out"),
                                  actionButton("BoxButton","Plot!")
                                  
                                ),
                                         mainPanel(plotOutput(outputId = "box"))
                                         
                                         ),
                                
                                
                                tabPanel("DistributionPlots",sidebarPanel(
                                  uiOutput("X_Hist_Out"),
                                  uiOutput("Fill_Hist_Out"),
                                  uiOutput("Facet_Hist_Out"),
                                  sliderInput(inputId = "Bins","Number of bins",
                                              min=1,max=150,value=30),
                                  checkboxInput(inputId = "DensityBox", "Show Density",value=FALSE),
                                  actionButton("HistButton","Plot!")
                                ),
                                mainPanel(plotOutput(outputId = "hist"))
                                         ),

                                tabPanel("ScatterPlot",sidebarPanel(uiOutput("X_Numeric_Out"),uiOutput("Y_Numeric_Out"),
                                                                    uiOutput("Color_All"),
                                                                    uiOutput("Size_Out"),
                                                                    uiOutput("Facet_Out"),
                                                                    checkboxInput(inputId="LmBox",
                                                                                  label="Show Regression Line"
                                                                                  , value = FALSE),
                                                                    actionButton("ScatterButton","Plot!")),
                                         mainPanel(plotOutput(outputId = "scatter"))
                                         
                                         )
                                
                                
                                )
                                
                                
                                ),
                       tabPanel("Fitting Models",
                                tabsetPanel(
                                  tabPanel("Model Comparison",
                               sidebarPanel(
                                 selectInput(inputId = "App",label="Choose Application",
                                             choices = c("Binary Classification"="Binary",
                                                         "Multiclass Classification"="Multi"
                                                         ,"Regression"="Regression")
                                               ),
                                 conditionalPanel(condition = "input.App=='Binary'",
                                                  uiOutput("BinaryClassificationOut")),
                                 conditionalPanel(condition = "input.App=='Multi'",
                                                  uiOutput("MultiClassificationOut")),
                                 conditionalPanel(condition = "input.App=='Regression'",
                                                  uiOutput("RegressionOut")),
                                 uiOutput("PredictorsOut"),
                                 conditionalPanel(condition ="input.App=='Binary'",
                                                  checkboxGroupInput(inputId = "checkBinary",label = "Choose Model",
                                                                     choices = list("LogisticRegression"="glm",
                                                                                   "Ridge/Lasso"="glmnet",
                                                                                   "LinearDiscriminant"="lda",
                                                                                   "QuadraticDiscriminant"="qda",
                                                                                   "RandomForest"="ranger",
                                                                                   "SVM_Linear"="svmLinear",
                                                                                   "SVM_Poly"="svmPoly",
                                                                                   "xgbTree"="xgbTree"),
                                                                     selected = "glm"
                                                                     )
                                                  ),
                                 conditionalPanel(condition ="input.App=='Multi'",
                                                  checkboxGroupInput(inputId = "checkMulti",label = "Choose Model",
                                                                     choices = list("Ridge/Lasso"="glmnet",
                                                                                    "RandomForest"="ranger",
                                                                                    "SVM_Linear"="svmLinear",
                                                                                    "SVM_Poly"="svmPoly",
                                                                                    "xgbTree"="xgbTree"),
                                                                     selected = "glmnet"
                                                  )
                                 ),
                                 conditionalPanel(condition ="input.App=='Regression'",
                                                  checkboxGroupInput(inputId = "checkRegression",label = "Choose Model",
                                                                     choices = list("LinearRegression"="lm",
                                                                                    "Ridge/Lasso"="glmnet",
                                                                                    "RandomForest"="ranger",
                                                                                    "xgbTree"="xgbTree"),
                                                                     selected = "lm"
                                                  )
                                 ),
                                 h4("Preprocessing"),
                                 checkboxInput("centerscale", "Center and Scale Variables", TRUE),
                                 checkboxInput("MedianImpute", "Impute Missing Values", FALSE),
                                 h4("Cross Validation"),
                                 sliderInput(inputId = "CV",label = "Choose Folds",min=2,max=10,value = 3),
                                 actionButton("ModelButton","Run!")
                                 
                               ),mainPanel(
                                 conditionalPanel(condition = "input.checkBinary.length>1||input.checkMulti.length>1||input.checkRegression.length>1",
                                                    plotOutput("modelComparisonPlot")),
                                 conditionalPanel(condition="input.ModelButton>0",
                                 verbatimTextOutput("modelComparisonTable")%>%withSpinner(color="#0dc5c1")))
                                 ),
                               tabPanel("Choose Model and Make Predictions",
                                        sidebarPanel(uiOutput("ModelsOut"),
                                        fileInput("testset", "Provide test set", accept = c("text/csv",
                                                                                                   "text/comma-separated-values,text/plain",
                                                                                                   ".csv")),
                                        checkboxInput("includesDependent","Test set includes dependent variable",value=FALSE),
                                        actionButton(inputId = "predictButton","Make Predictions!",width="400px"),
                                        br(),
                                        hr(),
                                        downloadButton(outputId = "downloadPredictions", label = "Download Predictions"),
                                        downloadButton(outputId = "downloadModel", label = "Download Model")
                                        ),
                                        mainPanel(tabsetPanel(id="Tabset3",
                                          tabPanel("Model",verbatimTextOutput("finalmodel")),
                                                  tabPanel("Model Performance",
                                                           h4(textOutput("ErrorType")),
                                                           verbatimTextOutput("ModelPerformance"))
                                          ))
                                        
                                        
                                        )
                               
                               )
                               
                               ),tabPanel("Help",
                                          h3(strong("Manual")),
                                          p('This app can be used to do data analysis with ggplot2 and data modeling with caret. One can use its own data or data from the ISLR package. It is possible to do binary classification, multiclass classification or regression analysis.',align="Justify"),
                                          
                                          p('The data types are shown in the summary tab. It is possible to change the data types, for classification task, one should change the data type to factor. One has to do all the changes to the data set at the same time and click on the Change data type button.',align="Justify"),
                                          
                                          p('It is possible to center and scale the numeric variables or impute missing values. Numeric values are imputed by the median and factors with the most common value. Otherwise, missing values are omitted.',align="Justify"),
                                          
                                          p('One can upload a new data set to make predictions. It is required that the test set has the same column names, only the dependent variable does not need to be included. If the "Test set includes dependent variable" is checked, the test set error is calculated, otherwise training errors are calculated.',align="Justify"),
                                          
                                          p('It is also possible to start the app by running the following code in R shiny::runGitHub( "ShinyCaret", "Bastien90"). shinyapps.io limits the availability of system resources to 1024 mb in the free, hence it is better to run the app locally for larger data sets.',align="Justify"),
                                          
                                          h3(strong("Author")),
                                          p("Bastien Haller"),
                                          a(h5("LinkedIn"),href="https://www.linkedin.com/in/bastien-haller-77b53193/",target="_blank"),
                                          p(strong('The source code is available on Github')),
                                          a(h5("Code"),href="https://github.com/Bastien90/ShinyCaret",target="_blank")
                                          
                               )
               
                       
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  ##First Tab Sever Upload and data types
  ##Data Upload
  df <-reactive( {
    if(input$Dataset!=""){
    data<-eval(parse(text=input$Dataset))
    }else{
    req(input$file)
    data<-fread(input$file$datapath,data.table = FALSE,stringsAsFactors = FALSE,na.strings=c("NA"," ",""))}
    
    input$goButton
    isolate({
      if(is.null(input$MakeNumericINPUT)&is.null(input$MakeFactorINPUT)){
        data<-data
      }else if(!(any(names(data)%in%input$MakeNumericINPUT)|any(names(data)%in%input$MakeFactorINPUT))){
        data<-data
      } else if(ifelse(is.null(input$MakeNumericINPUT),"",input$MakeNumericINPUT)!=""&ifelse(is.null(input$MakeFactorINPUT),"",input$MakeFactorINPUT)!="")  { 
              
        data[,input$MakeNumericINPUT]<-(map_df(data[,input$MakeNumericINPUT,drop=FALSE],as.numeric))
        data[,input$MakeFactorINPUT]<-(map_df(data[,input$MakeFactorINPUT,drop=FALSE],as.factor))
        
        
        
      }else if(ifelse(is.null(input$MakeNumericINPUT),"",input$MakeNumericINPUT)!=""){
        data[,input$MakeNumericINPUT]<-(map_df(data[,input$MakeNumericINPUT,drop=FALSE],as.numeric))
      }else if(ifelse(is.null(input$MakeFactorINPUT),"",input$MakeFactorINPUT)!=""){
        data[,input$MakeFactorINPUT]<-(map_df(data[,input$MakeFactorINPUT,drop=FALSE],as.factor))
      }else{
        data<-data
      }
      data
            })
    }) 
  

  
  
  
  

  
    
  output$table <- DT::renderDataTable({
    
    data <- df()
    data
  })
  

  
  numeric<-reactive({data<-df()
  numeric<-names(data)[map_lgl(data,is.numeric)]
   
                      
                    })
  factors<-reactive({data<-df()
  factors<-names(data)[!map_lgl(data,is.numeric)]
   
  })
  
  binary<-reactive({
    data<-df()
    binary<-names(data)[map_lgl(data,~(length(unique(.))== 2& is.factor(.)==TRUE))]
  })
  
  multi<-reactive({
    data<-df()
    multi<-names(data)[map_lgl(data,~(length(unique(.))> 2& is.factor(.)==TRUE))]
  })
  
  
  ### Summary Stats
  
  
  output$Summary<-DT::renderDataTable({
    fun <- function(f) pmap(list(x = df()[,numeric()], na.rm = TRUE), f)
    
    param <- list(min=list(min),mean=list(mean), median=list(median),max=list(max),sd=list(sd))
    summary<-bind_rows(invoke_map(.f = fun, .x = param))
    summary<-cbind(Stats=names(param),bind_rows(summary))
    
    summary_<-summary%>%gather("Variable","Value",2:ncol(summary))%>%spread(Stats,Value)
    summary_[,2:6]<-map_df(summary_[,2:6],round,2)
    
    ##factors
    data<-df()
    allvars<-data.frame(Variable=names(data),Class=map_chr(data,class),row.names = NULL)
    allvars$SumNa<-map_int(df(),~sum(is.na(.)))
    allvars$Mode<-map_chr(data,~names(sort(table(.),decreasing = TRUE)[1]))
    allvars$ShareMode<-map_dbl(data,~sort(table(.),decreasing = TRUE)[1]/length(.))
    allvars$NumUnique<-map_int(data,~length(unique(.)))
    allvars%>%left_join(summary_,by="Variable")%>%arrange(desc(Class))
    allvars<-allvars%>%left_join(summary_,by="Variable")%>%arrange(desc(Class))%>%
      dplyr::select(Variable,Class,min,mean,median,max,sd,SumNa,NumUnique,Mode,ShareMode)
    
    datatable(allvars,options = list(paging =FALSE))%>%formatPercentage("ShareMode",2)

     
  })
  
  #Tab1 Show Additional Information
  ##Help page
  tmp <- tempfile()
  output$documentation <- renderUI({
    
    package<-paste0(input$Dataset,".Rd")
    
    Rd2HTML(Rd_db("ISLR")[[package]], tmp, no_links = TRUE, package = "ISLR")
    includeHTML(tmp)
  })
  
  observeEvent(input$Dataset, {
    if(input$Dataset==""){
      hideTab(inputId = "Tabset1", target = "Additional Information")}else{
        showTab(inputId = "Tabset1", target = "Additional Information")
      }
    
    
  })
  #Tab2
  #CorrelationPlot
  
  output$corrplot<-renderPlot({
    input$CorrButton
    isolate({
      if(input$CorrButton>0){
    corm<-  cor(df()[,numeric()],use="pairwise.complete.obs")
    #corrplot.mixed(corm,tl.pos="lt",tl.col="black")
    ggcorrplot(corm, hc.order = TRUE, type = "lower",
               lab = TRUE)}
    })  
 
  })
  ##Scatter Plot
  output$scatter<-renderPlot({
    input$ScatterButton
    isolate({
      
      
      p<-df()%>%ggplot(aes_string(x=input$X_Numeric_In,
                                  y=input$Y_Numeric_In,
                                  color=input$Color_All_In,
                                  size=input$Size_In))+geom_point()
      if(input$LmBox==TRUE){
       p<- p+geom_smooth(method ="lm",se=FALSE)
      }
      
      if(input$ScatterButton==0){
     
      }else if(input$Facet_In==""){
      p
      }else{
        p+facet_wrap(~eval(parse(text=input$Facet_In)))
      }
      
      
    })
  })
  
  ##Histogram
  
  output$hist<-renderPlot({
    
    input$HistButton
    
    isolate({
      
      if(input$DensityBox=="FALSE"){
      p<-df()%>%ggplot(aes_string(x=input$X_Hist_In,fill=input$Fill_Hist_In))+geom_histogram(bins=input$Bins)
      }else{
        p<-df()%>%ggplot(aes_string(x=input$X_Hist_In,fill=input$Fill_Hist_In))+geom_density(alpha=0.2)
        }
      if(input$HistButton==0){
        
      }else if(input$Facet_Hist_In==""){
        p
      }else{
        p+facet_wrap(~eval(parse(text=input$Facet_Hist_In)))
      }
      
      })
    
  })
  
  ## Boxplot
  
  output$box<-renderPlot({
    
    input$BoxButton
    
    isolate({
      
    
        p<-df()%>%ggplot(aes_string(x=input$X_Box_In,y=input$Y_Box_In,col=input$Col_Box_In))+geom_boxplot()

      if(input$BoxButton==0){
        
      }else if(input$Facet_Box_In==""){
        p
      }else{
        p+facet_wrap(~eval(parse(text=input$Facet_Box_In)))
      }
      
    })
    
  })
  
  ##BarChart
  
  output$bar<-renderPlot({
    
    input$BarButton
    
    isolate({
      
      

      if(input$BarButton==0){
        
      }else if(input$Facet_Bar_In=="" & input$SummaryStat=="count"){
        p<-df()%>%ggplot(aes_string(x=input$X_Factor_Bar_In,fill=input$Col_Bar_In))+geom_bar(position = input$BarType)
        p
      }else if(input$Facet_Bar_In!="" & input$SummaryStat=="count"){
        p<-df()%>%ggplot(aes_string(x=input$X_Factor_Bar_In,fill=input$Col_Bar_In))+geom_bar(position = input$BarType)
        p+facet_wrap(~eval(parse(text=input$Facet_Bar_In)))
      }else if(input$Facet_Bar_In=="" & input$SummaryStat!="count"){
        p<-df()%>%ggplot(aes_string(x=input$X_Summary_Bar_In,y=input$Y_Numeric_Bar_In,fill=input$Col_Bar_In))+
          geom_bar(position = "dodge",stat="summary",fun.y=input$SummaryStat)
        p
      }else if(input$Facet_Bar_In!="" & input$SummaryStat!="count"){
        p<-df()%>%ggplot(aes_string(x=input$X_Summary_Bar_In,y=input$Y_Numeric_Bar_In,fill=input$Col_Bar_In))+
          geom_bar(position = "dodge",stat="summary",fun.y=input$SummaryStat)
        p+facet_wrap(~eval(parse(text=input$Facet_Bar_In))) 
      }
      
    })
    
  })
  
##Tab3
  ##excludes Y variable from df or try trainX or create preProc_Test
  preProc<-reactive({
    input$ModelButton
    isolate({
      
      if(input$ModelButton==0){
        preProc=""
      }else if(input$MedianImpute==TRUE & input$centerscale==TRUE){
        preProc<-preProcess(df(),method = c("medianImpute","center","scale"))
      }else if(input$MedianImpute==FALSE & input$centerscale==TRUE){
        preProc<-preProcess(df(),method = c("center","scale"))
      }else if(input$MedianImpute==TRUE & input$centerscale==FALSE){
        preProc<-preProcess(df(),method = c("medianImpute"))
      }else{
        prep<-"NoPreProc"
      }
    })
    
  })
  ##replace factors only and na.omit
  data_prep<-reactive({
    input$ModelButton
    isolate({
      
      if(input$ModelButton==0){
        prep=""
      }else if(input$MedianImpute==TRUE & input$centerscale==TRUE){
        #prep<-preProcess(df(),method = c("medianImpute","center","scale"))
        prep<-predict(preProc(),df())
        prep[,factors()]<-map_df(prep[,factors(),drop=FALSE],~ifelse(is.na(.),as.character(names(sort(table(.),decreasing = TRUE)[1])),as.character(.)))
        prep[,factors()]<-map_df(prep[,factors(),drop=FALSE],as.factor)
        prep
      }else if(input$MedianImpute==FALSE & input$centerscale==TRUE){
        #prep<-preProcess(df(),method = c("center","scale"))
        prep<-predict(preProc(),df()) 
        prep<-na.omit(prep)
        prep
      }else if(input$MedianImpute==TRUE & input$centerscale==FALSE){
       # prep<-preProcess(df(),method = c("medianImpute"))
        prep<-predict(preProc(),df()) 
        prep[,factors()]<-map_df(prep[,factors(),drop=FALSE],~ifelse(is.na(.),as.character(names(sort(table(.),decreasing = TRUE)[1])),as.character(.)))
        prep[,factors()]<-map_df(prep[,factors(),drop=FALSE],as.factor)
        prep
        
      }else{
        prep<-na.omit(df())
        prep
      }
      #na.omit(prep)
    })
    
  })
  #trainings data matrix
  trainX<-reactive({
    input$ModelButton
    
    isolate({
      
      if(input$ModelButton==0){
        ##length>1
      }else if(input$App=="Binary" & input$PredictorsIn[1]=="All"){
        fm<-as.formula(paste0(input$BinaryClassificationIn,"~."))
        trainX<-model.matrix(fm,data=data_prep())[,-1,drop=FALSE]
      }else if(input$App=="Multi" & input$PredictorsIn[1]=="All"){
        fm<-as.formula(paste0(input$MultiClassificationIn,"~."))
        trainX<-model.matrix(fm,data=data_prep())[,-1,drop=FALSE]
      }else if(input$App=="Regression" & input$PredictorsIn[1]=="All"){
        fm<-as.formula(paste0(input$RegressionIn,"~."))
        trainX<-model.matrix(fm,data=data_prep())[,-1,drop=FALSE]
      }else if(input$App=="Binary" & input$PredictorsIn[1]!="All"){
        fm<-as.formula(paste0(input$BinaryClassificationIn,"~."))
        trainX<-model.matrix(fm,data=data_prep()[,c(input$BinaryClassificationIn,input$PredictorsIn),drop=FALSE])[,-1,drop=FALSE]
      }else if(input$App=="Multi" & input$PredictorsIn[1]!="All"){
        fm<-as.formula(paste0(input$MultiClassificationIn,"~."))
        trainX<-model.matrix(fm,data=data_prep()[,c(input$MultiClassificationIn,input$PredictorsIn),drop=FALSE])[,-1,drop=FALSE]
      }else if(input$App=="Regression" & input$PredictorsIn[1]!="All"){
        fm<-as.formula(paste0(input$RegressionIn,"~."))
        trainX<-model.matrix(fm,data=data_prep()[,c(input$RegressionIn,input$PredictorsIn),drop=FALSE])[,-1,drop=FALSE]
      }
      
      
    })
  } )
  
  
  
  trainY<-reactive({
    
    input$ModelButton
    
    isolate({
      
      if(input$ModelButton==0){
        
      }else if(input$App=="Regression" & input$MedianImpute==TRUE){
        trainY<-df()[,input$RegressionIn]
      }else if(input$App=="Regression" & input$MedianImpute==FALSE){
        trainY<-na.omit(df())[,input$RegressionIn]
      }else if(input$App=="Binary" & input$MedianImpute==TRUE){
        trainY<-make.names(df()[,input$BinaryClassificationIn])
      }else if(input$App=="Binary" & input$MedianImpute==FALSE){
        trainY<-make.names(na.omit(df())[,input$BinaryClassificationIn])
      } else if(input$App=="Multi"& input$MedianImpute==TRUE){
        trainY<-make.names(df()[,input$MultiClassificationIn])
        }else if (input$App=="Multi"& input$MedianImpute==FALSE){
        trainY<-make.names(na.omit(df())[,input$MultiClassificationIn])
        }
      
      
    })
  })
  
  
  modelList<-reactive({
    
    input$ModelButton
    
    isolate({
      
      
      if(input$ModelButton==0){
        
        
      }else if(input$App=="Binary"){
        myFolds<-createFolds(trainY(),k=input$CV)
        myControl <- trainControl(
          summaryFunction = twoClassSummary,
          classProbs = TRUE, # IMPORTANT!
          verboseIter = FALSE,
          savePredictions = TRUE,
          method = "cv",
          index = myFolds
        )
        method<-as.list(input$checkBinary)
        names(method)<-input$checkBinary
        models<-map(method,caret::train,x=trainX(),y=trainY(),trControl = myControl,metric="ROC")
        models
      }else if(input$App=="Multi"){
        myFolds<-createFolds(trainY(),k=input$CV)
        myControl <- trainControl(
          summaryFunction = multiClassSummary,
          classProbs = TRUE, # IMPORTANT!
          savePredictions = TRUE,
          verboseIter = FALSE,
          index = myFolds,
          method="cv"
        )
        method<-as.list(input$checkMulti)
        names(method)<-input$checkMulti
        models<-map(method,caret::train,x=trainX(),y=trainY(),trControl = myControl,metric="prAUC")
        models
      }else if(input$App=="Regression"){
        myFolds<-createFolds(trainY(),k=input$CV)
        myControl <- trainControl(
          summaryFunction = defaultSummary,
          savePredictions = TRUE,
          verboseIter = FALSE,
          index = myFolds,
          method="cv"
        )
        method<-as.list(input$checkRegression)
        names(method)<-input$checkRegression
        models<-map(method,caret::train,x=trainX(),y=trainY(),trControl = myControl,metric="RMSE")
        models
      }
      
      
      
    })
    
    
    
  })
  
  output$modelComparisonTable<-renderPrint({
    
    input$ModelButton
    
    isolate({
    
    if(input$ModelButton==0){
      
    }else if(length(input$checkBinary)>1&length(modelList())>1&input$App=="Binary"){
    re<-resamples(modelList())
    summary(re)
    }else if (length(input$checkBinary)==1&input$App=="Binary"){
      modelList()[[input$checkBinary]]
    }else if(length(input$checkMulti)>1&length(modelList())>1&input$App=="Multi"){
      re<-resamples(modelList())
      summary(re)
    }else if (length(input$checkMulti)==1&input$App=="Multi"){
      modelList()[[input$checkMulti]]
    }else if(length(input$checkRegression)>1&length(modelList())>1&input$App=="Regression"){
      re<-resamples(modelList())
      summary(re)
    }else if (length(input$checkRegression)==1&input$App=="Regression"){
      modelList()[[input$checkRegression]]
    }
      
      
      
    })
    
  })
  
  output$modelComparisonPlot<-renderPlot({
    input$ModelButton
    
    isolate({
    if(input$ModelButton==0){
      
    }else if(length(input$checkBinary)>1&length(modelList())>1&input$App=="Binary"){
      re<-resamples(modelList())
      bwplot(re,metric="ROC")
    }else if(length(input$checkMulti)>1&length(modelList())>1&input$App=="Multi"){
      re<-resamples(modelList())
      bwplot(re,metric="prAUC")
    }else if(length(input$checkRegression)>1&length(modelList())>1&input$App=="Regression"){
      re<-resamples(modelList())
      bwplot(re,metric="RMSE")
    }
      
      
    
    })
    
  })
  ##test data
  
  testset<-reactive({
    
      req(input$App)
     if(input$App=="Regression"){
      dependent<-input$RegressionIn
    }else if(input$App=="Binary"){
      dependent<-input$BinaryClassificationIn
      
    }else if(input$App=="Multi"){
      dependent<-input$MultiClassificationIn}
      
      
    if(input$includesDependent==TRUE){
    type<-map_chr(df(),class)
    }else{
    type<-map_chr(df(),class) 
    type<-type[names(type)!=dependent]
    }
    
    
    
    req(input$testset)
    data<-fread(input$testset$datapath,colClasses = type,data.table = FALSE,stringsAsFactors = FALSE,na.strings=c("NA"," ",""))
    
    if(input$includesDependent==TRUE){
      data
    }else{
    data$dummy<-df()[1,dependent]
    colnames(data)[ncol(data)]="Survived"
    data
    }
  })
  
  testsetprep<-reactive({
    data<-testset()
    input$predictButton
    isolate({
      
    if(input$predictButton==0){
      
    }else if(input$MedianImpute==TRUE & input$centerscale==TRUE){
      prep<-predict(preProc(),data)
      prep[,factors()]<-map_df(df()[,factors(),drop=FALSE],~ifelse(is.na(.),as.character(names(sort(table(.),decreasing = TRUE)[1])),as.character(.)))
      prep[,factors()]<-map_df(df()[,factors(),drop=FALSE],as.factor)
      prep
    }else if(input$MedianImpute==FALSE & input$centerscale==TRUE){
      prep<-predict(preProc(),data)
      prep<-na.omit(prep)
      prep
    }else if(input$MedianImpute==TRUE & input$centerscale==FALSE){
      prep<-predict(preProc(),data)
      prep[,factors()]<-map_df(df()[,factors(),drop=FALSE],~ifelse(is.na(.),as.character(names(sort(table(.),decreasing = TRUE)[1])),as.character(.)))
      prep[,factors()]<-map_df(df()[,factors(),drop=FALSE],as.factor)
      prep
      
    }else{
      prep<-na.omit(data)
      prep
    }
    })
  })
  
  
  ##testX
  testX<-reactive({
    
    input$predictButton
    isolate({
    if(input$predictButton==0){
      
    }else if(input$App=="Binary" & input$PredictorsIn[1]=="All"){
      fm<-as.formula(paste0(input$BinaryClassificationIn,"~."))
      testX<-model.matrix(fm,data=testsetprep())[,-1,drop=FALSE]
    }else if(input$App=="Multi" & input$PredictorsIn[1]=="All"){
      fm<-as.formula(paste0(input$MultiClassificationIn,"~."))
      testX<-model.matrix(fm,data=testsetprep())[,-1,drop=FALSE]
    }else if(input$App=="Regression" & input$PredictorsIn[1]=="All"){
      fm<-as.formula(paste0(input$RegressionIn,"~."))
      testX<-model.matrix(fm,data=testsetprep())[,-1,drop=FALSE]
    }else if(input$App=="Binary" & input$PredictorsIn[1]!="All"){
      fm<-as.formula(paste0(input$BinaryClassificationIn,"~."))
      testX<-model.matrix(fm,data=testsetprep()[,c(input$BinaryClassificationIn,input$PredictorsIn),drop=FALSE])[,-1,drop=FALSE]
    }else if(input$App=="Multi" & input$PredictorsIn[1]!="All"){
      fm<-as.formula(paste0(input$MultiClassificationIn,"~."))
      testX<-model.matrix(fm,data=testsetprep()[,c(input$MultiClassificationIn,input$PredictorsIn),drop=FALSE])[,-1,drop=FALSE]
    }else if(input$App=="Regression" & input$PredictorsIn[1]!="All"){
      fm<-as.formula(paste0(input$RegressionIn,"~."))
      testX<-model.matrix(fm,data=testsetprep()[,c(input$RegressionIn,input$PredictorsIn),drop=FALSE])[,-1,drop=FALSE]
    }
    })
    
  } )
  
  
  #testY
  
  testY<-reactive({
    
    input$predictButton
    isolate({
    if(input$predictButton==0){
      
    }else if(input$App=="Regression" & input$MedianImpute==TRUE){
      testY<-testset()[,input$RegressionIn]
    }else if(input$App=="Regression" & input$MedianImpute==FALSE){
      testY<-na.omit(testset())[,input$RegressionIn]
    }else if(input$App=="Binary" & input$MedianImpute==TRUE){
      testY<-make.names(testset()[,input$BinaryClassificationIn])
    }else if(input$App=="Binary" & input$MedianImpute==FALSE){
      testY<-make.names(na.omit(testset())[,input$BinaryClassificationIn])
    } else if(input$App=="Multi"& input$MedianImpute==TRUE){
      testY<-make.names(testset()[,input$MultiClassificationIn])
    }else if (input$App=="Multi"& input$MedianImpute==FALSE){
      testY<-make.names(na.omit(testset())[input$MultiClassificationIn])
    }
    })
  })
  
  output$finalmodel<-renderPrint({
    
    req(modelList())
    req(input$ModelsIn)
    modelList()[[input$ModelsIn]]
    
  })
  
  
  predictions<-reactive({
    

    input$predictButton
    isolate({
      req(modelList())
      req(input$ModelsIn)
      
    if(input$App!="Regression"& (is.null(input$testset)|input$includesDependent==FALSE)){
      predict(modelList()[[input$ModelsIn]])
    }else if (input$App=="Regression"& (is.null(input$testset)|input$includesDependent==FALSE)){
      predict(modelList()[[input$ModelsIn]])
    }else if(input$App!="Regression" & !is.null(input$testset) & input$includesDependent==TRUE){
      predict(modelList()[[input$ModelsIn]],testX())
    }else if(input$App=="Regression" & !is.null(input$testset)&input$includesDependent==TRUE){
      predict(modelList()[[input$ModelsIn]],testX())
    }
      
    
    })
  })
  
  
  output$ModelPerformance<-renderPrint({
    
    input$predictButton
    
    isolate({
    
    req(modelList())
    req(input$ModelsIn)
    req(predictions())
    
    if(input$App!="Regression"& (is.null(input$testset)|input$includesDependent==FALSE)){
      confusionMatrix(predictions(),trainY())
    }else if (input$App=="Regression"& (is.null(input$testset)|input$includesDependent==FALSE)){
      paste("RMSE:",RMSE(predictions(),trainY()))
    }else if(input$App!="Regression" & !is.null(input$testset)& input$includesDependent==TRUE){
      confusionMatrix(predictions(),testY())
    }else if(input$App=="Regression" & !is.null(input$testset)& input$includesDependent==TRUE){
      paste("RMSE:",RMSE(predictions(),testY()))
    }
    
    })
    
  })
  
  output$ErrorType<-renderText({
    
    input$predictButton
    
    isolate({
      
      req(modelList())
      req(input$ModelsIn)
      req(predictions())
      
      if(input$App!="Regression"& (is.null(input$testset)|input$includesDependent==FALSE)){
        paste("Training Error:" ,input$ModelsIn)
      }else if (input$App=="Regression"& (is.null(input$testset)|input$includesDependent==FALSE)){
        paste("Training Error:" ,input$ModelsIn)
      }else if(input$App!="Regression" & !is.null(input$testset)& input$includesDependent==TRUE){
        paste("Test Error:" ,input$ModelsIn)
      }else if(input$App=="Regression" & !is.null(input$testset)& input$includesDependent==TRUE){
        paste("Test Error:" ,input$ModelsIn)
      }
      
    })
    
  })
  
  output$downloadPredictions <- downloadHandler(
    filename =function(){ "predictions.csv"},
    content = function(file) {
      
      data<-df()
      data$prediction<-str_replace(predictions(),"^X[0-9]",substr(predictions(),2,length(predictions())))
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$downloadModel <- downloadHandler(
    filename =function(){ 
      paste0(input$ModelsIn,".rds")
      },
    content = function(file) {
      
      model<-modelList()[[input$ModelsIn]]
      
      saveRDS(model, file)
    }
  )

  
  
  
  ###UI
  ##Tab1
  output$MakeNumericOut<-renderUI({
    selectInput(
      inputId = "MakeNumericINPUT", label = "Change Class to Numeric", 
      choices = c("Choose"="",names(df())),
      multiple = TRUE)
  })
  
  output$MakeFactorOut<-renderUI({
    selectInput(
      inputId = "MakeFactorINPUT", label = "Change Class to Factor", 
      choices = c("Choose"="",names(df())),
      multiple = TRUE)
  })

  #Tab2 Data Visualisation  LM Plot
  output$X_Numeric_Out<-renderUI({
    selectInput(
      inputId = "X_Numeric_In", label = "Select X Variable", 
      choices = c("Choose"="",numeric()),
      multiple = FALSE)
  })
  
  
  output$Y_Numeric_Out<-renderUI({
    selectInput(
      inputId = "Y_Numeric_In", label = "Select Y Variable", 
      choices = c("Choose"="",numeric()),
      multiple = FALSE)
  })
  
  
  output$Color_All<-renderUI({
    selectInput(
      inputId = "Color_All_In", label = "Select Color Variable", 
      choices = c("None"="NULL",names(df())),
      multiple = FALSE,
      selected = "None")
  })
  
  output$Size_Out<-renderUI({
    selectInput(
      inputId = "Size_In", label = "Select the size Variable", 
      choices = c("None"="NULL",numeric()),
      multiple = FALSE,
      selected = "None")
  })
  
  
  output$Facet_Out<-renderUI({
    selectInput(
      inputId = "Facet_In", label = "Select the Variable to Facet", 
      choices = c("Choose"="",factors()),
      multiple = FALSE)
  })
  
  
  ### Inputs Histogram
  
  output$X_Hist_Out<-renderUI({
    selectInput(
      inputId = "X_Hist_In", label = "Select X Variable", 
      choices = c("Choose"="",numeric()),
      multiple = FALSE)
  })
  
  
  output$Facet_Hist_Out<-renderUI({
    selectInput(
      inputId = "Facet_Hist_In", label = "Select the Variable to Facet", 
      choices = c("Choose"="",factors()),
      multiple = FALSE)
  })
  
  
  output$Fill_Hist_Out<-renderUI({
    selectInput(
      inputId = "Fill_Hist_In", label = "Select the Fill Variable", 
      choices = c("None"="NULL",factors()),
      multiple = FALSE,
      selected = "None")
  })
  
  ## Inputs Boxplot
  output$X_Box_Out<-renderUI({
    selectInput(
      inputId = "X_Box_In", label = "Select X Variable", 
      choices = c("Choose"="",factors()),
      multiple = FALSE)
  })
  
  
  output$Y_Box_Out<-renderUI({
    selectInput(
      inputId = "Y_Box_In", label = "Select Y Variable", 
      choices = c("Choose"="",numeric()),
      multiple = FALSE)
  })
  
  
  output$Facet_Box_Out<-renderUI({
    selectInput(
      inputId = "Facet_Box_In", label = "Select the Variable to Facet", 
      choices = c("Choose"="",factors()),
      multiple = FALSE)
  })
  
  
  output$Col_Box_Out<-renderUI({
    selectInput(
      inputId = "Col_Box_In", label = "Select the Color Variable", 
      choices = c("None"="NULL",factors()),
      multiple = FALSE,
      selected = "None")
  })
  
  # Inputs BarChart
  output$Facet_Bar_Out<-renderUI({
    selectInput(
      inputId = "Facet_Bar_In", label = "Select the Variable to Facet", 
      choices = c("Choose"="",factors()),
      multiple = FALSE)
  })
  
  
  output$Col_Bar_Out<-renderUI({
    selectInput(
      inputId = "Col_Bar_In", label = "Select the Fill Variable", 
      choices = c("None"="NULL",factors()),
      multiple = FALSE,
      selected = "None")
  })
  
  ##Count
  
  output$X_Factor_Bar_Out<-renderUI({
    selectInput(
      inputId = "X_Factor_Bar_In", label = "Select the X Variable", 
      choices = c("None"="NULL",factors()),
      multiple = FALSE,
      selected = "None")
  })
  
  ## sum and mean
  
  output$Y_Numeric_Bar_Out<-renderUI({
    selectInput(
      inputId = "Y_Numeric_Bar_In", label = "Select the Y Variable", 
      choices = c("None"="NULL",numeric()),
      multiple = FALSE,
      selected = "None")
  })
  
  output$X_Summary_Bar_Out<-renderUI({
    selectInput(
      inputId = "X_Summary_Bar_In", label = "Select the X Variable", 
      choices = c("None"="NULL",factors()),
      multiple = FALSE,
      selected = "None")
  })
  
##Modeling Inputs

  output$BinaryClassificationOut<-renderUI({
    selectInput(
      inputId = "BinaryClassificationIn", label = "Select Dependent Variable", 
      choices = c("Choose"="",binary()),
      multiple = FALSE)
  }) 
  
  
  output$MultiClassificationOut<-renderUI({
    selectInput(
      inputId = "MultiClassificationIn", label = "Select Dependent Variable", 
      choices = c("Choose"="",multi()),
      multiple = FALSE)
  }) 
  
output$RegressionOut<-renderUI({
    selectInput(
      inputId = "RegressionIn", label = "Select Dependent Variable", 
      choices = c("Choose"="",numeric()),
      multiple = FALSE)
  })
  
  output$PredictorsOut<-renderUI({
    selectInput(
      inputId = "PredictorsIn", label = "Select Predictors", 
      choices = c("All Predictors"="All",names(df())),
      multiple = TRUE,
      selected = "All")
  })
  
  output$ModelsOut<-renderUI({
    selectInput(
      inputId = "ModelsIn", label = "Select Model", 
      choices = c("Choose"="",names(modelList())),
      multiple = FALSE)
  })
  
  
  
  
  




}

# Run the application 
shinyApp(ui = ui, server = server)

