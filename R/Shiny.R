data(mtcars)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyjs)


ui <- dashboardPage(
  
  dashboardHeader(title = "PLS Regression"),
  
  dashboardSidebar(
    sidebarMenu(id = "menu",
                menuItem("Import Menu", tabName = "import",
                         fileInput("file1", "Choose CSV File",
                                   multiple = FALSE,
                                   accept = c(".csv")),
                         checkboxInput("cbHeader", "Header", TRUE),
                         radioButtons("rbSeparator", "Separator",
                                      choices = c("Comma" = ",",
                                                  "Semicolon" = ";",
                                                  "Tab" = "\t"),
                                      selected = ','),
                         radioButtons("rbDisplay", "Display",
                                      choices = c("head" = "head",
                                                  "All" = "all"),
                                      selected = "head"),
                         actionButton("submitFile", "Submit file")
                ),
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
  
  observeEvent(input$submitFile, {
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    tableout <- read.csv(inFile$datapath, header = input$cbHeader, sep = input$rbSeparator)
  })
  output$contents <- renderTable({
    (tableout)
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
    plsda.fit(Species~.,iris,2) 
  })
  output$predplsda <- renderPrint({
    plsda.predict(Species~.,iris,2) 
  })
  output$vipplsda <- renderPrint({
    plsda.vip(res)
  })
  
}

shinyApp(ui, server)
