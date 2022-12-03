#' Make a cross-validation on on dataset to select the best sample 
#' 
#' @description 
#' Make a cross-validation on on dataset to select the best sample
#' 
#' @param formula 
#' formula an object of class formula" (or one that can be coerced to that class): a symbolic description of the model
#' to be fitted.
#' @param data 
#' dataframe containing the variables in the model.
#' @param ncomp
#' Number of components extracted in NIPALS algorithm.
#' @param nfold
#' Number of folds in the cross-validation (5 by default)
#' 
#' @return
#' \code{bestmodel} the best model with the cross-validation
#' \cr
#' \code{bestfscore} the fscore of the best model
#' \cr
#' @export
#'
#' @examples
#' 
#' cv=plsda.cross_validation(Species~.,data=iris,2)
#' pred=plsda.predict(Species~.,data=iris,2,nfolds=10)


cross_validation <- function(formula, data, ncomp, nfolds = 5){
  
  # Random sample
  data <- data[sample(1:nrow(data)),]
  
  # Index of each indiv in fold 
  fold <- list()
  
  # Size of the folds 
  foldSize <- nrow(data) / nfolds

  # Loop which returns all indiv by fold
  for(i in 1:nfolds){
    ind <- rep(TRUE, nrow(data))
    hb <- i * foldSize
    bb <- hb - foldSize + 1
    ind[bb:hb] <- FALSE
    fold[[i]] <- ind
  }
  
  FscoreVectorglobal <- c()
  models <- list()
  for(j in 1:nfolds){
    #Get the cols of X and Y
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
    models[[j]] <- plsTrain
  }
  bestmodel <- models[[which.max(FscoreVectorglobal)]]
  bestfscore <- FscoreVectorglobal[which.max(FscoreVectorglobal)]
  res <- list("model" = bestmodel,"fscore" = bestfscore)
  return(res)
}
