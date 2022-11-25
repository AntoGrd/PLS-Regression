
install.packages("plotly")
library(plotly)

plsda_scree_plot=function(obj){
  
  # Calcul des corrélations 
  ncomp=obj$ncomp
  X=obj$X
  corX=cor(X)
  
  # Calcul des valeurs propres
  eigenvalues=eigen(corX)$values
  test=eigenvalues > 1
  cols <- ifelse(test, "rgba(255, 0, 0, 0.5)", "rgba(0, 0, 255, 0.5)")
  lab=paste("Comp.",1:ncomp,sep = "")
  # Scree plot
  plot_ly(x=lab, y = eigenvalues,type = "bar",color = I(cols))%>%
    layout(
      xaxis=list(title="Composantes"),
      yaxis=list(title="Valeurs propres"),
      title="Sélection des composantes"
    )
  # points(x = scree, y = eigenvalues, type = "o", pch = 16)
  
}

# data(iris)
# mod=plsda.fit(Species~.,data=iris,ncomp=4)
# plsda_scree_plot(mod)