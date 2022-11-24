#library(plotly)

plsda_plot_indiv<-function(acp,axe1=1,axe2=2){
  

  #RAJOUTER CAS D'ERREUR : ex = axes doivent entre compris dans le nb de col
  
  x1 = acp$X_scores[,axe1]
  x2 = acp$X_scores[,axe2]
  fig <- plot_ly(x = x1, y = x2, color=acp$Y, type = 'scatter', mode = 'markers') %>% layout(title="Graph of individuals",
                                                                                             hovermode='closest',dragmode= 'select',
                                                                                             legend=list(title=list(text='Color')),
                                                                                             xaxis = list(title=paste0("Comp ",axe1)),
                                                                                             yaxis = list(title=paste0("Comp ",axe2)))
  fig
}



