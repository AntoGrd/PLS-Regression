pls.cv = function(formula,data,nfold=10){
  
  # Vérification que l'utilsaiteur ait bien saisi une formule de type Y~X
  
  if(plyr::is.formula(formula)==F){
    stop("formula must be R formula !")
  }
  
  # Récupération du X et du Y
  
  X <- model.matrix(formula, data = data)
  X <- X[,-1] #suppression de l'intercept
  Y <- model.response(model.frame(formula, data = data))
  
  ncomposantes=qr(X)$rank
  PRESS <- NULL
  for(j in 1:ncomp){
    
    press <- NULL
    
    samp<-sample(1:nrow(X),nrow(X))
    nsample<-trunc(nrow(X)/nfold)
    nX <- X[samp,]
    nY <- Y[samp]
    
    for(i in 1:nfold){
      #index du iÃ¨me Ã©chantillon
      idx<-c((1+(i-1)*nsample):(nsample*(i)))
      
      #on divise les donnÃ©es test et entraÃ®nement
      
      X.train <- nX[-idx,]
      X.test <- nX[idx,]
      Y.train <- nY[-idx]
      Y.test <- plsda.dummies(nY)
      Y.test <- Y.test[idx,]
      train <- data.frame("Y"=Y.train, X.train)
  }
}