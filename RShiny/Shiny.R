library(shiny)
library(stringr)
library(shinyWidgets)
library(plotly)
library(PLSDA)

ui <- fluidPage(
  
  navbarPage("PLS Regression",id="nvPage",
             
             tabPanel("ImportData",
                      sidebarLayout(
                        # sidebar for file inputs
                        sidebarPanel(
                          # select file
                          textInput("txtSheet","Enter sheet name if excel file",value="sheet1"),
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          # horizontal line
                          tags$hr(),
                          # header or not
                          radioButtons("cbHeader", "Header",
                                       choices = c("Yes" = TRUE,
                                                   "No" = FALSE),
                                       selected = TRUE,inline=T),
                          # separator
                          radioButtons("rbSeparator", "Separator",
                                       choices = c("Comma" = ",",
                                                   "Semicolon" = ";",
                                                   "Tab" = "t"),
                                       selected = ',',inline=T),
                          # quotes
                          radioButtons("rbQuote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"',inline=T),
                          tags$hr(),
                          
                        ),
                        
                        mainPanel(
                          # output
                          dataTableOutput("contents")
                        )
                      )     
             ),
             #panel for fit function
             tabPanel("Fit",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("ncTrain","Choose percentage of value in train Data Set",value="66",min="1",max="99"),
                          actionButton("abSep","Do train test split"),
                          tags$hr(),
                          numericInput("ncFit","Choose the numbers of composants",value="2",min="1"),
                          #uiOutput to get x and y variables in a list
                          uiOutput("xvar"),
                          uiOutput("yvar"),
                          actionButton("abFit","Fit the Data")
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel(verbatimTextOutput("ptrain"),width=12), #number of lines in train data set
                          tabPanel(verbatimTextOutput("ptest"),width=12), #number of lines in test data set
                          tabPanel(tableOutput("showFit")) #coefficients of the model
                        )
                        )
                      )
             ),
             #panel for predict function
             tabPanel("Predict",
                      sidebarLayout(
                        sidebarPanel(actionButton("abPred","Do the prediction")),
                        mainPanel(tabsetPanel(
                          tabPanel(tableOutput("showPred")), #classification report
                          tabPanel(verbatimTextOutput("showf"),width=12) #global f1 score
                        )
                        )
                      )
             ),
             #panel for graphics
             tabPanel("Graphics",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "inpGraph",
                            label = "Choose graphic to plot",
                            choices = c(ScatterPlot="scatterplot",
                                        CirclePlot="circleplot",
                                        IndividualsPlot="indivplot",
                                        ScreePlot="screeplot")),
                          tags$hr(),
                          h2("Scatter plot"),
                          uiOutput("var1"),
                          uiOutput("var2"),
                          tags$hr(),
                          h2("Composants to analyse"),
                          numericInput("ncComp1","First composant",value="1",min="1"),
                          numericInput("ncComp2","Second composant",value="2",min="1"),
                          actionButton("abPlot","View/update graphic")
                          
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel(plotlyOutput("graph")) #show graph selected 
                        ))
                      )
             )
  )    
)


# Define server logic to read selected file ----
server <- function(input, output) {
  ####IMPORT VALUES DATA####
  data=reactive({
    if (is.null(input$file1)){return(NULL)}
    else{
      tryCatch(
        {
          if(stringr::str_ends(input$file1$datapath, "csv")){
            read.csv(file=input$file1$datapath,
                     header = as.logical(input$cbHeader),
                     sep = input$rbSeparator,
                     quote = input$rbQuote)
          }else if(stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
            readxl::read_excel(input$file1$datapath,
                               col_names = as.logical(input$cbHeader),
                               sheet=input$txtSheet)
          }
          
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    }
  })
  
  output$contents=renderDataTable({
    head(data(),50)},options=list(scrollX = TRUE, dom='t')
  )
  ####FIT DATA#### 
  output$xvar=renderUI({
    choiceX=colnames(data())
    selectInput(inputId = "varx",
                label = "Select your X variables",
                choices = choiceX,
                multiple=TRUE)
  })
  
  output$yvar=renderUI({
    choiceY=colnames(data())
    selectInput(inputId = "vary",
                label = "Select your Y variable",
                choices = choiceY,
                multiple=FALSE)
    
  })
  #do train test split
  newData=eventReactive(input$abSep,{
    df=train_test_split(data(),input$ncTrain)
  })
  #creation train
  train=reactive({
    train=newData()$Train
    return(train)
  })
  
  test=reactive({
    test=newData()$Test
    return(test)
  })
  
  output$ptrain=renderText({
    paste("Number of lines in Train set :",dim(train())[1])})
  output$ptest=renderText({
    paste("Number of lines in Test set :",dim(test())[1])})
  
  #do fit function
  resFit=eventReactive(input$abFit,{
    formul=as.formula(paste(input$vary,"~",".",sep=""))
    if(is.null(input$varx)){
      newtrain=train()
    }else{
      newtrain=train()[,c(input$varx,input$vary)]
    }
    res=fit(formula=formul,data=newtrain,ncomp=input$ncFit)
    return(res)
  })
  #show fit coefficients
  output$showFit=renderTable({
    resFit()$coef
  },rownames=TRUE) 
  
  ####TRAIN DATA####
  resPred=eventReactive(input$abPred,{
    if(is.null(input$varx)){
      n=which(colnames(test())==input$vary) #remove selected y from dataframe
      newtest=test()[,-n]
    }else{
      newtest=test()[,input$varx] #keep columns of selected x 
    }
    respred=predict(resFit(),newtest)
    return(respred)
    
  })
  
  
  summary=reactive({
    sum=classification_report(test()[,input$vary],resPred())
    return(sum)
  })
  
  output$showPred=renderTable({
    summary()$report
  },rownames=TRUE)
  
  output$showf=renderText({
    paste("Global f1-score :",summary()$f1_score)})
  
  ####PLOTS DATA####
  output$var1=renderUI({
    choiceX=colnames(data())
    selectInput(inputId = "var1",
                label = "first variable for scatterplot",
                choices = choiceX,
                multiple=FALSE)
  })
  
  output$var2=renderUI({
    choiceX=colnames(data())
    selectInput(inputId = "var2",
                label = "2nd variable for scatterplot",
                choices = choiceX,
                multiple=FALSE)
  })
  
  #choose which graph to plot
  choicegraph <- eventReactive(input$abPlot,{
    if(input$inpGraph=="scatterplot"){
      graph=PLSDA::explanatory_variables_plot(var=data()[,input$var1],var2=data()[,input$var2],color=data()[,input$vary])
    }else if(input$inpGraph=="screeplot"){
      graph=scree_plot(resFit())
    }else if(input$inpGraph=="indivplot"){
      graph=indiv_plot(resFit(),axe1=input$ncComp1,axe2=input$ncComp2)
    }else if(input$inpGraph=="circleplot"){
      graph=circle_plot(resFit(),axe1=input$ncComp1,axe2=input$ncComp2)
    }
    return(graph)
  })
  
  
  output$graph <- renderPlotly({
    choicegraph()
  })
  
  
}



# Create Shiny app ----
shinyApp(ui, server)

