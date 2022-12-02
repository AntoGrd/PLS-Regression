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
#' train_test=pls.train_test_split(iris)
#' train_test=pls.train_test_split(iris,prop=70)

pls.train_test_split<-function(data,prop=66){
  
  # VÃ©rification de la valeur de prop saisie par l'utilsiateur 
  
  if(prop>100 | prop<0){
    stop("Proportion non comprise entre 0 et 100")
  }
  
  # RÃ©cupÃ©ration de la colonne contenant y 
  prop=prop/100
  
  n <- nrow(data)
  
  # Selection des indices des individus de l'Ã©chantillon d'apprentisage
  
  i_sample<-sample(1:n,trunc(n*prop))
  
  # RÃ©sultat sous forme de liste 
  
  res<-list("Train"=data[i_sample,],
            "Test"=data[-i_sample,]
  )
  
  return(res)
}
