data(mtcars)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyjs)


ui <- dashboardPage(
  dashboardHeader(title = "PLS Regression"),
  dashboardSidebar(
    sidebarMenu(id = "menu",
                menuItem("Import Menu", tabName = "import"),
                         
                menuItem("Fit", tabName = "fit"),
                menuItem("Predict", tabName = "predict"),
                menuItem("VIP", tabName = "vip"),
                menuItem("Dashboard", tabName = "dashboard")
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(
        "import",
        box(
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        tags$hr(),
        checkboxInput("Header", "Header", TRUE),
        radioButtons("Separator", "Separator",
                     choices = c("Comma" = ",",
                                 "Semicolon" = ";",
                                 "Tab" = "\t"),
                     selected = ','),
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),
        actionButton("submitFile", "Submit file")
        ),
          tableOutput("contents")
      ),
      tabItem(
        "fit",
        verbatimTextOutput("fitplsda")
      ),
      tabItem(
        "predict",
        verbatimTextOutput("predplsda")
      ),
      tabItem(
        "vip",
        verbatimTextOutput("vipplsda")
      ),
      tabItem(
        "dashboard",
        box(
          plotlyOutput("scree_plot")
        ),
        box(
          plotlyOutput("corr_plot")
        ),
        box(
          plotlyOutput("indiv_plot")
        ),
        box(
          width = 4,
          selectInput("ville", "Var 1", 
                      choices = c("Toutes les villes", unique(txhousing$city))),
          selectInput("ville", "Var 2", 
                      choices = c("Toutes les villes", unique(txhousing$city))),
          plotlyOutput("scartter_plot")
        )
      )
    )
  ),
  title = "Titre dans le navigateur",
  skin = "red"
)



server <- function(input, output, session) {
  output$contents <- renderTable({
   req(input$file1)
    df = read.csv(input$file1$datapath,
                   header = input$Header,
                   sep = input$Separator)
    if(input$Header == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  output$scree_plot <- renderPlotly({
    plsda_scree_plot(res)
  })
  output$corr_plot <- renderPlotly({
    circle.plot(res)
  })
  output$indiv_plot <- renderPlotly({
    plsda_plot_indiv(res)
  })
  output$scartter_plot <- renderPlotly({
    explanatory_variables(var=res$X[,input$var1],var2=res$X[,input$var2],color=res$Y)
  })
  output$fitplsda <- renderPrint({
    plsda.fit(Species~.,df,2) 
  })
  output$predplsda <- renderPrint({
    plsda.predict(Species~.,iris,2) 
  })
  output$vipplsda <- renderPrint({
    plsda.vip(res)
  })
  
}

shinyApp(ui, server)
