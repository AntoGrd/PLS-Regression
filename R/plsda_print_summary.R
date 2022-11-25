#' print function for PLSDA
#'
#' @param PLS 
#'
#' @return
#' @export
#'
#' @examples
#' 
plsda.print=function(PLS){

 # Creation of a matrix with the coefficients 
  
 res=PLS$coef
 
 # Modification of colnames and rownames
 rownames(res)=c(colnames(PLS$X))
 colnames(res)=c(colnames(PLS$Y_dummies))
 
 # add intercept in res
 intercept=PLS$intercept
 res=rbind(res,intercept)

 return(res)
  
}

#' Summary function for PLSDA
#'
#' @param PLS 
#' @param Xtest 
#' @param ytest 
#'
#' @return
#' @export
#'
#' @examples

plsda.summary=function(PLS,Xtest,ytest){
  
  #coef and intercept
  coef=PLS$coef
  intercept=PLS$intercept
  coefficients=rbind(coef,intercept)
  #prediction with Xtest and showing classification report with ytest
  pred=plsda.predict(PLS,Xtest)
  classification_report=plsda_Classification_report(ytest,pred)
  list=list()
  list$coefficients = coefficients
  list$classification_report = classification_report
  return(list)
}



