plsda.predict <- function(obj_pls,newdata,type="class"){

  #vérifier si var toutes numerique
  
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
  print(type)
  if(type != "class" && type != "posterior"){
    stop("you didn't enter a valid type")
  }
  
  X = (t(newdata) - colMeans(obj_pls$X))/apply(obj_pls$X,2,sd)
  Ypred = t(X) %*% obj_pls$coef + obj_pls$intercept
  
  #softmax to have probabilities
  Ypred = apply(Ypred,1,exp)
  Ypred = t(Ypred/colSums(Ypred))
  
  if(type == "posterior"){
    return(Ypred) #proba
  }else{
    #retourner les classes
    colmax=apply(Ypred,1,which.max)
    pred=colnames(obj_pls$Y_dummies)[colmax]
    return(pred)
  }
}

