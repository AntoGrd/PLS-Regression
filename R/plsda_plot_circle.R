plsda_plot_circle<-function(acp,axe1=1,axe2=2){
  
  c1=acp$X_loadings[,axe1]*sqrt(eigen(cor(scale(acp$X)))$values[axe1])
  c2=acp$X_loadings[,axe2]*sqrt(eigen(cor(scale(acp$X)))$values[axe2])

  correlation <- cbind(c1,c2)
  print(correlation,digits=2)
  
  print(correlation^2,digits=2)
  
  print(t(apply(correlation^2,1,cumsum)),digits=2)

  plt=plot(c1,c2,xlim=c(-1,+1),ylim=c(-1,+1),type="n")
  +abline(h=0,v=0)
  +text(c1,c2,cex=0.5)
  +symbols(0,0,circles=1,inches=F,add=T)
  return(plt)

}