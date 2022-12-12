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
#' @examples
#' PLSDA::summary.plsda(res)
#' @export


summary.PLSDA=function(PLSDA,Xtest,ytest){
  
  #coef and intercept
  coef=PLSDA$coef
  intercept=PLSDA$intercept
  coefficients=rbind(coef,intercept)
  #prediction with Xtest and showing classification report with ytest
  pred=predict(PLS,Xtest)
  classification_report=PLSDA::classification_report(ytest,pred)
  list=list()
  list$coefficients = coefficients
  list$classification_report = classification_report
  print(list)
}
