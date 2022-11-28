shiny <- function(res){

ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Partial Least Square Regression : ",
      sidebarLayout(
        sidebarPanel(
          tabPanel("Importation fichier",
                   fileInput("file",
                             label="Choix du data set",
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                             ),
                   checkboxInput("Entete", "Entête", TRUE),
                   radioButtons("Sep", "Separateur",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),
                   radioButtons("Aff", "Affichage",
                                choices = c(Head = "Entête",
                                            "100" = "100",
                                            All = "Tout"),
                                selected = "head"),
                   fluidRow(
                     column(1,
                            actionButton(
                              inputId = "voirdonnees",
                              label = "Voir les données",
                              ),
                            ),
                     plotOutput("plot1", click = "plot_click"),
                     verbatimTextOutput("info"),
                     ),
                   ),
          ),
        mainPanel(
          tabsetPanel(
            tabPanel('Selection des variables',plotlyOutput("scree_plot")),
            tabPanel('Corrélation',plotlyOutput("corr_plot")),
            tabPanel('Individus',plotlyOutput("indiv_plot")),
            tabPanel('Nuage de points ',
                     selectInput("X_Variables", 
                                 label = "Variable X", 
                                 choices = c(),
                                 selected = 1),
                     selectInput("Y_Variables",
                                 label = "Variable Y",
                                 choices = c(), 
                                 selected = 2),
                     plotlyOutput("scartter_plot"))
          )
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
    explanatory_variables(var=res$X[,input$Xvar],var2=res$X[,input$Yvar],color=res$Y)
  })
}

shinyApp(ui, server)
}
shiny(res)
