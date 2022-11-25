#' print function for PLSDA
#'
#' @param PLSDA PLSDA object (returned with fit function)
#'
#' @return a print function to observe the coefficients and intercept
#' obtained with the fit function
#' @export

plsda.print=function(PLSDA){

 # Creation of a matrix with the coefficients 
  
 res=PLSDA$coef
 
 # Modification of colnames and rownames
 rownames(res)=c(colnames(PLSDA$X))
 colnames(res)=c(colnames(PLSDA$Y_dummies))
 
 # add intercept in res
 intercept=PLSDA$intercept
 res=rbind(res,intercept)

 return(res)
  
}

#' Summary function for PLSDA
#'
#' @param PLSDA PLSDA object (obtained with fit function)
#' @param Xtest dataframe with the sames variables of PLSDA$X to predict the values
#' @param ytest classes that correspond to the values of Xtest, to compare with the predicted values
#'
#' @return a print function to observe 
#' - the coefficients and intercept obtained with the fit function
#' - a classification report including Confusion matrix,
#'   Report with precision, recall and f1-score for each class,
#'   global f1 score 
#' @export


plsda.summary=function(PLSDA,Xtest,ytest){
  
  #coef and intercept
  coef=PLSDA$coef
  intercept=PLSDA$intercept
  coefficients=rbind(coef,intercept)
  #prediction with Xtest and showing classification report with ytest
  pred=PLSDAda.predict(PLSDA,Xtest)
  classification_report=PLSDAda_Classification_report(ytest,pred)
  list=list()
  list$coefficients = coefficients
  list$classification_report = classification_report
  return(list)
}