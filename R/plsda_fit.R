plsda.fit <-function(formula, data, ncomp){
  
  #AJOUTER LES MESSAGES D'ERREURS
  X = model.matrix(formula,data=data)[,-1]
  Y = model.response(model.frame(formula, data = data))
  
  plsda = plsda.nipals(X,Y,data,ncomp)
  return(plsda)
}

rm(plsda.fit)
rm(plsda.nipals)

iris=iris
res=plsda.fit(seed~.,data$train,3)
res
res$Y
res=plsda.fit(Species~.,iris,2)  
res
res$X_scores
res$weights
plsda.fit(Species~.,iris,2)  
