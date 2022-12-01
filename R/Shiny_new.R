library(shiny)
library(reactable)
library(readxl)
library(stringr)
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
                   # display head or all file
                   radioButtons("rbDisplay", "Display",
                                choices = c("head" = "head",
                                            "All" = "all"),
                                selected = "head",inline=T)
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
                   
                   numericInput("ncFit","Choose the numbers of composants",value="2",min="1"),
                   uiOutput("xvar"),
                   uiOutput("yvar"),
                   selectInput(inputId = "inpFit", "Results of fit",
                               choices = c(Coefficients = "coef",
                                           weights = "weights",
                                           Xscores ="X_scores",
                                           Yscores ="Y_scores",
                                           Xloadings = "X_loadings",
                                           Yloadings ="Y_loadings"),
                               selected="Coefficients"),
                   actionButton("abFit","Fit the Data")
               ),
                 mainPanel(reactableOutput("showFit"))
               )
               ),
      
      
      tabPanel("Predict"),
      tabPanel("Dashboard")
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

ncomp=reactive({
  input$ncFit
})





resFit=eventReactive(input$abFit,{
  print(input$varx)
  if(input$varx==NULL){
    res=plsda.fit(input$vary~.,data(),input$ncFit)
  }else{
    res=plsda.fit(input$vary~input$varx,data(),input$ncFit)
  }
  return(res$calc$coef)
  print(res$calc$coef)
})

output$showFit=renderReactable({
  reactable(resFit())
}) 

  
}

# Create Shiny app ----
shinyApp(ui, server)
