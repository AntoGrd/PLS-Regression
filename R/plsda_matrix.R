plsda_Classification_report <- function(observed,predict){
  
  df=data.frame(observed,predict)

  table=table(df[,1],df[,2]) 
  n=nrow(table)
  w = rowSums(table)/length(df[,1]) #weight for each values 
  MC = matrix(nrow=n, ncol=3)
  colnames(MC)=c("precision","recall","f1-score")
  rownames(MC)=rownames(table)
  for(i in 1:n){
    MC[i,1]=table[i,i]/sum(table[,i]) #precision
    MC[i,2]=table[i,i]/sum(table[i,]) #recall
    MC[i,3]=2*MC[i,1]*MC[i,2]/(MC[i,1]+MC[i,2]) #f1-score
  }
  f1_score = sum(MC[,3]*w) #global f1 score
  
  return(list(Confusion_matrix = table,
              report = MC,
              f1_score=f1_score))
}


