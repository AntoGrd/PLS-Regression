plsda_scree_plot=function(obj){
  
  # Nombre de composantes
  
  ncomp=obj$ncomp
  
  # Calcul des corrélations 
  
  X=obj$X
  corX=cor(X)
  
  # Calcul des valeurs propres
  
  eigenvalues=eigen(corX)
  eigenvalues1=eigenvalues$values[1:ncomp]
  scree=plot(1:ncomp,eigenvalues1,type="b",ylab="Valeurs propres",xlab="Composantes", main="Scree plot")
  
  cor.factors <- NULL
  
  for (j in 1:ncol(X)){
    rf <- sqrt(eigenvalues$values[j])*eigenvalues$vectors[,j]
    cor.factors <- cbind(cor.factors,rf)
  }
  
  rownames(cor.factors)=colnames(X)
  colnames(cor.factors)=paste("F",1:ncol(X),sep="")
  plot(1:ncomp,cor.factors,type="b",ylab="Valeurs propres",xlab="Composante",main="Scree plot")
  
}
data(iris)
mod=plsda.fit(Species~.,data=iris,ncomp=4)
plsda_scree_plot(mod)

