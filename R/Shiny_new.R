library(shiny)
library(reactable)
library(readxl)
library(stringr)
library(shinyWidgets)

ui <- fluidPage(
  
  navbarPage("PLS Regression",id="nvPage",
             
      tabPanel("ImportData",
               sidebarLayout(
                 # sidebar for file inputs
                 sidebarPanel(
                   # select file
                   textInput("txtSheet","Enter sheet name if csv file",value="sheet1"),
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
      
      tabPanel("Fit",
               sidebarLayout(
                 sidebarPanel(
                   numericInput("ncTrain","Choose percentage of value in train Data Set",value="66",min="1",max="99"),
                   actionButton("abSep","Do train test split"),
                   tags$hr(),
                   numericInput("ncFit","Choose the numbers of composants",value="2",min="1"),

                   uiOutput("xvar"),
                   uiOutput("yvar"),
                   actionButton("abFit","Fit the Data")
               ),
                 mainPanel(tabsetPanel(
                   tabPanel(verbatimTextOutput("ptrain"),width=12),
                   tabPanel(verbatimTextOutput("ptest"),width=12),
                   tabPanel(verbatimTextOutput("txtcoef"),width=12),
                   tabPanel(tableOutput("showFit"))
                 )
                   )
               )
               ),
      
      tabPanel("Predict",
               sidebarLayout(
                 sidebarPanel(actionButton("abPred","Do the prediction")),
                 mainPanel(tabsetPanel(
                   tabPanel(tableOutput("showPred")),
                   tabPanel(verbatimTextOutput("showf"),width=12)
                   )
                 )
                 )
               ),
      
      tabPanel("Dashboard",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("var1"),
                   uiOutput("var2"),
                   tags$hr()
                   
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel(plotlyOutput("scatter_plot"))
                 ))
               )
               )
      
      
      
  )    
)


# Define server logic to read selected file ----
server <- function(input, output) {
####IMPORT VALUES DATA####
  data=reactive({
    #inFile=input$file1
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
                   sheet=input$sheet1)
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

newData=eventReactive(input$abSep,{
  df=pls.train_test_split(data(),input$ncTrain)
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


resFit=eventReactive(input$abFit,{
  formul=as.formula(paste(input$vary,"~",".",sep=""))
  if(is.null(input$varx)){
    newtrain=train()
  }else{
    newtrain=train()[,c(input$varx,input$vary)]
  }
  res=plsda.fit(formula=formul,data=newtrain,ncomp=input$ncFit)
  return(res)
})

output$showFit=renderTable({
  resFit()$coef
},rownames=TRUE) 

####TRAIN DATA####
resPred=eventReactive(input$abPred,{
  if(is.null(input$varx)){
    n=which(colnames(test())==input$vary) #remove selected y from dataframe
    newtest=test()[,-n]
  }else{
    newtest=test()[,input$varx] #keep col of selected x 
  }
  respred=plsda.predict(resFit(),newtest)
  return(respred)
  
})


summary=reactive({
  sum=plsda_Classification_report(test()[,input$vary],resPred())
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

output$scatter_plot <- renderPlotly({
  #explanatory_variables(var=data()$input$var1,var2=data()$input$var2,color=data()$input$vary)
  explanatory_variables(var=data()[,input$var1],var2=data()[,input$var2],color=data()[,input$vary])
})

}



# Create Shiny app ----
shinyApp(ui, server)
