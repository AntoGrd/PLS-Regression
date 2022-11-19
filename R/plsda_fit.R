plsda.fit <-function(formula, data, ncomp){
  
  #AJOUTER LES MESSAGES D'ERREURS
  X = model.matrix(formula,data=data)[,-1]
  Y = model.response(model.frame(formula, data = data))
  
  plsda = plsda.nipals(formula,data,ncomp)
  return(plsda)
}


iris
res=plsda.fit(seed~.,data$train,3)
res
res=plsda.fit(Species~.,iris,2) 
res
res$intercept
ypred=plsda.predict(res,iris[1:4],type="posterior")
ypred
y=res$Y_dummies
colMeans(y)
res$X

iris2=iris[1:4]
colnames(iris2)=c("a","b","c","d")

df[, sapply(df, is.numeric)]

iris[,sapply(iris,is.numeric)]
ncol(iris[,sapply(iris,is.numeric)])
ncol(iris)

df=data.frame(iris,rep(c("a","b"),150))
df
ncol(df[,sapply(df,is.numeric)])
