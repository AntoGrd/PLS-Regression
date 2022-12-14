#' Variable Importance in the Projection
#'
#' @param PLSDA
#' A PLSDA type model
#' @param threshold 
#' criterion of variable selection 0.8 by default
#' @return
#' \code{newX} New dataset with variable selection
#' \cr
#' \code{vip} Importance of each variable for each composants
#' \cr
#' @export
#'
#' @examples
#' res=PLSDA::vip=(PLSDA)
#' res=PLSDA::vip=(PLSDA,threshold=1)
vip<-function(PLSDA,threshold=0.8){
  
  # Get variables

  q=PLSDA$Y_loadings
  t=PLSDA$X_scores
  w=PLSDA$weights
  X=PLSDA$X
  y=as.data.frame(PLSDA$Y)
  print(y)
  colnames(y)=PLSDA$yname
  ncomp=PLSDA$ncomp
  
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
    variable_importante=rownames(vip_sorted)[1:2]
  }
  # New dataset with important variables only
  newX = X[,variable_importante]
  
  newX=data.frame(newX,y)
  return(VIP = list("newX"=newX,
                    "vip"=vip))
}
