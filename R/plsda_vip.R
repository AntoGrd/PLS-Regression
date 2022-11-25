#' Variable Importance in the Projection function for PLSDA
#'
#' @param PLS 
#' @param threshold 
#'
#' @return
#' @export
#'
#' @examples
plsda.vip<-function(PLS,threshold=0.8){
  
  # Recovery of the necessary values for the algorithm

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
  
  #identification of important variables
  variable_importante=rownames(vip)[which(vip[,ncomp]>threshold)]
  
  
  #  If only one variable is important, we select the 2 variables with the highest VIP
  if (length(variable_importante)<2){
    vip_sorted = vip[order(-vip[,ncomp]),]
    variable_importante=rownames(vip_sorted)[1:2]
  }
  # Creation of a new dataset with only the important variables
  newX = X[,variable_importante]
  
  newX=data.frame(newX,y)
  return(VIP = list(newX,
                    vip))
}

