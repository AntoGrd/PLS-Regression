plsda.vip<-function(PLS,threshold=0.8){
  
  q=PLS$Y_loadings
  t=PLS$X_scores
  w=PLS$weights
  
  p=nrow(w)
  h=ncol(w)
  
  ssy=diag(t(t)%*%t%*%t(q)%*%q)
  tot_ssy=sum(ssy)
  
  weigth=(w/sqrt(colSums(w^2)))^2
  vip = sqrt(p*(ssy*weigth)/tot_ssy)
  
  PLS$vip=vip
  PLS$tresholds=threshold
  return(PLS)
}
