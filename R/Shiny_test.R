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
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "import",
        tableOutput("contents")
      )
    )
  )
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
  
}

shinyApp(ui, server)
