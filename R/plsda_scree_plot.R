plsda_scree_plot=function(obj){
  
  # Calcul des corrélations 
  
  X=obj$X
  corX=cor(X)
  
  # Calcul des valeurs propres
  eigenvalues=eigen(corX)$values
  cols <- ifelse(eigenvalues > 1, "purple", "yellow")
  
  # Scree plot
  scree <- barplot(eigenvalues, col = cols, ylab="Valeurs propres", xlab="Composantes", main="Scree plot")
  points(x = scree, y = eigenvalues, type = "o", pch = 16)
  
}
#data(iris)
#mod=plsda.fit(Species~.,data=iris,ncomp=4)
#plsda_scree_plot(mod)

