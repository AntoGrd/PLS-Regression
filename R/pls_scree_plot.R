plsda_scree_plot=function(obj){
  
  # Nombre de composantes
  
  ncomp=obj$ncomp
  
  # Calcul des corrélations 
  
  x=obj$X
  corX=cor(X)
  
  # Calcul des valeurs propres
  
  eigenvalues=eigen(corX)
  eigenvalues=eigenvalues$values[1:ncomp]
  
  for (i in 1:ncol(X)){
    
  }
    

  plot(1:ncomp,val.propres,type="b",ylab="Valeurs propres",xlab="Composante",main="Scree plot")
}