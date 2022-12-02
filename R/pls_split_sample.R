### Cette fonction permet de sÃ©parer le jeu de donnÃ©es en 4 parties : 

# - Train : Jeu de données d'entrainement
# - Test : Jeu de données test

## ParamÃ¨tres de la fonction : 

# data = Jeu de donnÃ©es Ã  Ã©chantillonner
# prop = proportion de l'Ã©chantillon d'apprentissage

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
