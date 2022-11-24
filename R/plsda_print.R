plsda.print=function(objet){

 # Creation of a matrix with the coefficients 
  
 res=objet$coef
 
 # Modification of colnames and rownames
 rownames(res)=c(colnames(objet$X))
 colnames(res)=c(colnames(objet$Y_dummies))
 
 # add intercept in res
 intercept=objet$intercept
 res=rbind(res,intercept)

 return(res)
  
}

plsda.summary=function(objet){
  
  coef=objet$coef
  intercept=objet$intercept
  coefficients=rbind(coef,intercept)
  
  Classif_report=plsda_Classification_report()
  
  res <- list()
  res$coefficients <- coefficients
  res$Y <- Y
  return(res)
}
plsda.summary(res)
