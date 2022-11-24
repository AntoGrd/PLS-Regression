plsda.vip<-function(PLS,threshold=0.8){
  
  # Récupération des valeurs nécéssaires à l'algorithme 
  
  q=PLS$Y_loadings
  t=PLS$X_scores
  w=PLS$weights
  X=PLS$X
  y=PLS$y
  ncomp=PLS$ncomp
  
  p=nrow(w)
  h=ncol(w)
  
  ssy=diag(t(t)%*%t%*%t(q)%*%q)
  tot_ssy=sum(ssy)
  
  weigth=(w/sqrt(colSums(w^2)))^2
  vip = sqrt(p*(ssy*weigth)/tot_ssy)
  
  #détermination des variables importantes
  variable_importante=rownames(vip)[which(vip[,ncomp]>threshold)]
  
  
  # Si une seule variable importante, on sélectionne les 2 variables avec le plus haut VIP
  if (length(variable_importante)<2){
    vip_sorted = vip[order(-vip[,ncomp]),]
    variable_importante=rownames(vip_sorted)[1:2]
  }
  # Création d'un nouveau dataset avec uniquement les variables importantes
  newX = X[,variable_importante]
  
  PLS$vip=vip
  PLS$tresholds=threshold
  return(newX)
  
}

plsda.vip(res)
