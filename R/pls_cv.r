
plsda.cross_validation <- function(formula, data, ncomp, nfolds = 5){
  
  n <- nrow(data)
  
  # Random sample
  data <- data[sample(1:n),]
  
  # Index of each indiv in fold 
  fold <- list()
  
  # Size of the folds 
  foldSize <- n / nfolds

  # Loop which returns all indiv by fold
  for(i in 1:nfolds){
    ind <- rep(TRUE, n)
    hb <- i * foldSize
    bb <- hb - foldSize + 1
    ind[bb:hb] <- FALSE
    fold[[i]] <- ind
  }
  
  FscoreVectorglobal <- c()
  models <- list()
  for(j in 1:cv){
    #Get the cols of X
    Xnames <- colnames(model.matrix(Species~.,data=iris)[,-1])
    yname <- toString(formula[[2]])
    
    #Get fold
    ind <- fold[[j]]
    train <- data[ind,]
    test <- data[!ind,]
    
    #Keep only X cols on test
    Xtest <- data.frame(test[, Xnames])
    Ytest <- test[, yname]
    
    #Fit the model on train and predict on test
    plsTrain <- plsda.fit(formula,train,ncomp)
    predTest <- plsda.predict(plsTrain, Xtest)
    
    globalFscore <- plsda_Classification_report(Ytest, predTest)$f1_score
    FscoreVectorglobal <- append(FscoreVectorglobal, globalFscore)
    models[[k]] <- plsTrain
  }
  bestmodel <- models[[which.max(FscoreVectorglobal)]]
  bestfscore <- FscoreVectorglobal[which.max(FscoreVectorglobal)]
  res <- list("model" = bestmodel,"fscore" = bestfscore)
  return(res)
}

