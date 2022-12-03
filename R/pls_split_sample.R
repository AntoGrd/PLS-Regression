#' Make a train and a test sample. 
#' 
#' @description 
#' 'This function makes a train and test samples based on a proportion chosen by the user
#' 
#' @param data
#' dataset chosen by the user
#' 
#' @param prop 
#' Proportion of the whole dataset which is going to be in the train split. 
#' For exemple, if the user write prop=66, it means that an amount of 66% of the dataset will be for the train data. 
#' prop=66 is the parameter by default
#'
#' @return Train and test data
#' 
#' @export
#'
#' @examples
#' train_test=PLSDA::train_test_split(iris)
#' train_test=PLSDA::train_test_split(iris,prop=70)

train_test_split<-function(data,prop=66){
  
  # Checking the eigen value entered by the user
  
  if(prop>100 | prop<0){
    stop("Proportion not between 0 and 100")
  }
  
  # get y col
  prop=prop/100
  
  n <- nrow(data)
  
  # Selection of the indices of the individuals in the apprenticeship sample
  
  i_sample<-sample(1:n,trunc(n*prop))
  
  # retourn res
  
  res<-list("Train"=data[i_sample,],
            "Test"=data[-i_sample,]
  )
  
  return(res)
}
