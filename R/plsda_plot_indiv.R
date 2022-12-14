#' Individuals plots for PLSDA
#'
#' @param PLSDA PLSDA object (returned with the fit function)
#' @param axe1 1st component to observe the individuals
#' @param axe2 2nd component to observe the individuals
#'
#' @return a plot with the 2 choosen components and the individuals on it
#' @export

indiv_plot<-function(PLSDA,axe1=1,axe2=2){
  
  library(plotly)
  
  x1 = PLSDA$X_scores[,axe1]
  x2 = PLSDA$X_scores[,axe2]
  fig <- plot_ly(x = x1, y = x2, color=PLSDA$Y, type = 'scatter', mode = 'markers') %>% layout(title="Graph of individuals",
                                                                                             hovermode='closest',dragmode= 'select',
                                                                                             legend=list(title=list(text='Color')),
                                                                                             xaxis = list(title=paste0("Comp ",axe1)),
                                                                                             yaxis = list(title=paste0("Comp ",axe2)))
  fig
}


