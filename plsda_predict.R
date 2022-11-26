#' Predict y for a given x with a PLSDA model
#'
#' @param PLSDA 
#' PLSDA type model
#' @param newdata 
#' Dataset for which we want to make predictions
#' @param type 
#' class (by default) if we want to return the class variable 
#' posterior if we want to know the probabily of belonging of each class for the new dataset
#'
#' @return
#' \code{Ypred} if type = "posterior". Probabily of belonging of each class for the new dataset
#' \cr
#' \code{pred} if type = "class". Return the class variable 
#' \cr
#' @export
#'
#' @examples
#' 
#' pred=plsda.predict(mod,newdata)
#' pred=plsda.predict(mod,newdata,type="posterior")
#' 
plsda.predict <- function(PLSDA,newdata,type="class"){

  
  if (class(PLSDA) != "PLSDA"){ #check if object plsda is given
    stop("obj_pls is not a PLSDA object")
  }
  
  if (!is.data.frame(newdata)){ #check if newdata is a dataframe
    stop("newdata is not a dataframe")
  }
  
  l = length(intersect(colnames(newdata),colnames(PLSDA$X)))
  
  if(l != ncol(newdata) || l != ncol(PLSDA$X)){ #check if colnames are the same 
    stop("colnames are not the same")
  }

  if(type != "class" && type != "posterior"){
    stop("you didn't enter a valid type")
  }
  
  
  X = (t(newdata) - colMeans(PLSDA$X))/apply(PLSDA$X,2,sd) #normalize
  Ypred = t(X) %*% PLSDA$coef + PLSDA$intercept 
  
  #softmax to have probabilities
  Ypred = apply(Ypred,1,exp)
  Ypred = t(Ypred/colSums(Ypred))
  
  if(type == "posterior"){
    return(Ypred) #probabilities
  }else{
    #return classes
    colmax=apply(Ypred,1,which.max)
    pred=colnames(PLSDA$Y_dummies)[colmax]
    return(pred)
  }
}

