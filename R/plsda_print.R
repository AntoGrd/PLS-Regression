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

plsda.summary=function(PLS){
  
  coef=PLS$coef
  intercept=PLS$intercept
  coefficients=rbind(coef,intercept)

  pred=plsda.predict(PLS,PLS$X)
  return(pred)
}

res=plsda.fit(Species~.,iris,2) 
res$X
plsda.predict(res,res$X)
plsda.print(res)
plsda.summary(res)
