#' Title
#'
#' @param 
#' A PLSDA type model
#' @param threshold 
#' criterion of variable selection 0.8 by default)
#' @return
#' \code{newX} New dataset with variable selection
#' \cr
#' \code{vip} Selected variable in the model
#' \cr
#' @export
#'
#' @examples
#' res=plsda.vip=(objPLDS)
#' res=plsda.vip=(objPLDS,threshold=1)
plsda.vip<-function(PLS,threshold=0.8){
  
  # R√©cup√©ration des valeurs nÈcÈssaires ‡ l'algorithme 

  q=PLS$Y_loadings
  t=PLS$X_scores
  w=PLS$weights
  X=PLS$X
  y=as.data.frame(PLS$Y)
  print(y)
  colnames(y)=PLS$yname
  ncomp=PLS$ncomp
  
  p=nrow(w)
  h=ncol(w)
  
  ssy=diag(t(t)%*%t%*%t(q)%*%q)
  tot_ssy=sum(ssy)
  
  weigth=(w/sqrt(colSums(w^2)))^2
  vip = sqrt(p*(ssy*weigth)/tot_ssy)
  
  #d√©termination des variables importantes
  variable_importante=rownames(vip)[which(vip[,ncomp]>threshold)]
  
  
  # Si une seule variable importante, on s√©lectionne les 2 variables avec le plus haut VIP
  if (length(variable_importante)<2){
    vip_sorted = vip[order(-vip[,ncomp]),]
    variable_importante=rownames(vip_sorted)[1:2]
  }
  # Cr√©ation d'un nouveau dataset avec uniquement les variables importantes
  newX = X[,variable_importante]
  
  newX=data.frame(newX,y)
  return(VIP = list(newX,
                    vip))
}

