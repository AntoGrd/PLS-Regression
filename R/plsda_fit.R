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

q=res$Y_loadings
t=res$X_scores
w=res$weights

p=nrow(w)
h=ncol(w)
###
b <- c(q)[1:h]
T <- t[,1:h, drop = FALSE]
SS <- b^2 * colSums(T^2)
W <- w[,1:h, drop = FALSE]
Wnorm2 <- colSums(W^2)
sqrt(nrow(W) * sum(SS * W[1,]^2 / Wnorm2) / sum(SS))

###
s=diag(t(t)%*%t%*%t(q)%*%q)
tot_s=sum(s)

weigth=(w/sqrt(colSums(w^2)))^2
vip = sqrt(p*(s*weigth)/tot_s)
vip

weigth[,1]*s[1]

###

SS <- c(q)^2 * colSums(t^2)
Wnorm2 <- colSums(w^2)
SSW <- sweep(w^2, 2, SS / Wnorm2, "*")
sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))

SS
Wnorm2
SSW
c(q)^2 * colSums(t^2)

SSY = q^2%*%t(t)%*%t
Wnorm2 <- colSums(w^2)
(p*SSY%*%Wnorm2)/SSY

SSY[,1]

SSY%*%w^2










