#' print function for PLSDA
#'
#' @param PLSDA PLSDA object (returned with fit function)
#'
#' @return a print function to observe the coefficients and intercept
#' obtained with the fit function
#' @examples
#' PLSDA::print(res)
#' @export


print.plsda=function(PLS){

 # Creation of a matrix with the coefficients 
  
 res=PLS$coef
 
 # Modification of colnames and rownames
 rownames(res)=c(colnames(PLS$X))
 colnames(res)=c(colnames(PLS$Y_dummies))
 
 # add intercept in res
 intercept=PLS$intercept
 res=rbind(res,intercept)

 print(res)
  
}


