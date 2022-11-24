cross_validation <- function(formula, data, ncomp, cv = 5){
  
  n <- nrow(data)
  data <- data[sample(1:n),]
  fold <- list()


  foldSize <- n / cv
  for(i in 1:cv){
    ind <- rep(TRUE, n)
    hb <- i * foldSize
    bb <- hb - foldSize + 1
    ind[bb:hb] <- FALSE
    fold[[i]] <- ind
  }
  
  globalFscoreVector <- c()
  models <- list()
  for(k in 1:cv){
    #Get the cols of X
    Xnames <- colnames(model.matrix(Species~.,data=iris)[,-1])
    yname <- toString(formula[[2]])

    #Get fold
    ind <- fold[[k]]
    train <- data[ind,]
    test <- data[!ind,]
    
    #Keep only X cols on test
    Xtest <- data.frame(test[, Xnames])
    Ytest <- test[, yname]
    
    #Fit the model on train and predict on test
    plsTrain <- plsda.fit(formula,train,ncomp)
    predTest <- plsda.predict(plsTrain, Xtest)
    
    globalFscore <- plsda_Classification_report(Ytest, predTest)$f1_score
    globalFscoreVector <- append(globalFscoreVector, globalFscore)
    models[[k]] <- plsTrain
  }
  model <- models[[which.max(globalFscoreVector)]]
  fscore <- globalFscoreVector[which.max(globalFscoreVector)]
  res <- list("model" = model,"fscore" = fscore)
  return(res)
}

