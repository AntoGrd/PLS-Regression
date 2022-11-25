#' Title
#'
#' @param PLSDA 
#' @param newdata 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
plsda.predict <- function(PLSDA,newdata,type="class"){

  
  if (class(obj_pls) != "PLSDA"){ #check if object plsda is given
    stop("obj_pls is not a PLSDA object")
  }
  
  if (!is.data.frame(newdata)){ #check if newdata is a dataframe
    stop("newdata is not a dataframe")
  }
  
  l = length(intersect(colnames(newdata),colnames(obj_pls$X)))
  
  if(l != ncol(newdata) || l != ncol(obj_pls$X)){ #check if colnames are the same 
    stop("colnames are not the same")
  }

  if(type != "class" && type != "posterior"){
    stop("you didn't enter a valid type")
  }
  
  
  X = (t(newdata) - colMeans(obj_pls$X))/apply(obj_pls$X,2,sd) #normalize
  Ypred = t(X) %*% obj_pls$coef + obj_pls$intercept 
  
  #softmax to have probabilities
  Ypred = apply(Ypred,1,exp)
  Ypred = t(Ypred/colSums(Ypred))
  
  if(type == "posterior"){
    return(Ypred) #probabilities
  }else{
    #return classes
    colmax=apply(Ypred,1,which.max)
    pred=colnames(obj_pls$Y_dummies)[colmax]
    return(pred)
  }
}

