#' Classification report function for PLSDA
#'
#' @param observed observed values (ytest)
#' @param predict predicted values (values returned with predict function)
#'
#' @return 
#' \code{Confusion_matrix} Confusion matrix
#' \cr
#' \code{report} Report with precision, recall and f1-score for each class
#' \cr
#' \code{f1_score} global f1 score 
#' \cr
#' @export
#' @examples
#' res=plsda_Classification_report(ytrain,ypred)

plsda_Classification_report <- function(observed,predict){
  
  # Transform observed in vector is this is not the case
  
  if (is.vector(observed==F)){
    observed=as.vector(observed)
  }
  
  df=data.frame(observed,predict)
  restable=table(df[,1],df[,2]) 
  n=nrow(restable)
  w = rowSums(restable)/length(df[,1]) #weight for each values 
  MC = matrix(nrow=n, ncol=3)
  colnames(MC)=c("precision","recall","f1-score")
  rownames(MC)=rownames(restable)
  for(i in 1:n){
    MC[i,1]=restable[i,i]/sum(restable[,i]) #precision
    MC[i,2]=restable[i,i]/sum(restable[i,]) #recall
    MC[i,3]=2*MC[i,1]*MC[i,2]/(MC[i,1]+MC[i,2]) #f1-score
  }
  f1_score = sum(MC[,3]*w) #global f1 score
  
  return(list(Confusion_matrix = restable,
              report = MC,
              f1_score=f1_score))
}

tt=pls.train_test_split(iris)
tt$Train

res=plsda.fit(Species~.,tt$Train,3)
res
pred=plsda.predict(res,tt$Test[,1:4])

mat=plsda_Classification_report(tt$Test[5],pred)
mat$Confusion_matrix
mat$report

df=data.frame(tt$Test[5],pred)
table(df[,1],df[,2])



