install.packages("plotly")
library(plotly)

plsda_scree_plot=function(obj){
  
  # Calcul des corrélations 
  ncomp=obj$ncomp
  X=obj$X
  corX=cor(X)
  
  # Calcul des valeurs propres
  eigenvalues=eigen(corX)$values
  cols <- ifelse(eigenvalues > 1, "purple", "yellow")
  
  # Scree plot
  plot_ly(x=1:ncomp, y = eigenvalues,type = "bar",color = cols)
  # points(x = scree, y = eigenvalues, type = "o", pch = 16)
  
}

# data(iris)
# mod=plsda.fit(Species~.,data=iris,ncomp=4)
# plsda_scree_plot(mod)

