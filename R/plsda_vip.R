#' Title
#'
#' @param 
#' A PLSDA type model
#' @param threshold 
#' criterion of variable selection 0.8 by default
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
  
  # Get variables

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
  
  # Determination of importants variables
  variable_importante=rownames(vip)[which(vip[,ncomp]>threshold)]
  
  
  # If only 1 variable is simportant, we select the 2 most importants variables
  if (length(variable_importante)<2){
    vip_sorted = vip[order(-vip[,ncomp]),]
    variable_important=rownames(vip_sorted)[1:2]
  }
  # New dataset with important variables only
  newX = X[,variable_important]
  
  newX=data.frame(newX,y)
  return(VIP = list("newX"=newX,
                    "vip"=vip))
}
