library(shiny)
library(plotly)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabPanel("Importation fichier",
              fileInput("file",
                         label="fichier à importer",
                        accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")
               ),
              plotOutput("plot1", click = "plot_click"),
              verbatimTextOutput("info")
              ),
      selectInput("var1","Choix 1 ", choices = colnames(res$X)),
      selectInput("var2","Choix 2 ", choices = colnames(res$X)),
    ),
    mainPanel(
     tabsetPanel(
       tabPanel('Selection des variables',plotlyOutput("scree_plot")),
        tabPanel('Corrélation',plotlyOutput("corr_plot")),
        tabPanel('Individus',plotlyOutput("indiv_plot")),
        tabPanel('Nuage de points ',plotlyOutput("scartter_plot"))
      )
    )
  )
)

server <- function(input, output, session){
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
  
}

shinyApp(ui, server)
