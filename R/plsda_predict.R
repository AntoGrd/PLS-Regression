plsda.predict <- function(obj_pls,newdata,type="class"){
  #VERIFIER SI X EST UNE MATRICE/DF
  #var toutes numerique
  #mêmes colonnes dans newdata et plsda$X
  
  #X = (newdata - colMeans(obj_pls$X))/apply(obj_pls$X,2,sd)
  
  X = (t(newdata) - colMeans(obj_pls$X))/apply(obj_pls$X,2,sd)
  Ypred = t(X) %*% obj_pls$coef + obj_pls$intercept
  
  #ypred = val renvoyées par le modele
  #softmax pour calcul pro
  Ypred = apply(Ypred,1,exp)
  Ypred = t(Ypred/colSums(Ypred))
  
  #définir colnames qq part
  
  if(type == "posterior"){
    return(Ypred) #proba
  }else{
    #retourner les classes
    colmax=apply(Ypred,1,which.max)
    pred=colnames(obj_pls$Y_dummies)[colmax]
    return(pred)
  }
}

