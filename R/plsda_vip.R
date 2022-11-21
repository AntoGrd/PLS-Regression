plsda.vip<-function(PLS,threshold=0.8){

  #Récupération des variables prédictives du modèle
  x = as.matrix(instance$X)
  #Récupération des poids des variables prédictives du modèle
  W = as.matrix(instance$Xloading.weights)
  #Récupération du coefficient de détermination de la PLS-DA
  r2 = cor(instance.Ycod,instance$instance$X_scores)^2

  #somme des corrélations au carré par colonne
  Somme_corr_col = colSums(r2)
  #somme des corrélations au carrée par colonne
  Somme_corr_carre = sum(Somme_corr_col)

  #calcul des vip pour chaque variable
  vip = sapply(1:ncol(x),function(x){sum(Somme_corr_carre*((W^2)[x,]))})
  VIP = sqrt((ncol(x)/Somme_corr_col)*vip)

  #Ajout des noms des colonnes dans le vip
  noms(VIP) = list(colnames(X))

  #Choix des variables importantes
  var_imp = noms(VIP)[VIP>threshold]

  #Data frame contenant uniquement les variables importantes
  X = as.data.frame(instance$X)
  newX = X[,colnames(x) %in% var_imp]


  instance <- list("newX" = newX,
                 "Variable" = var_imp,
                 "VIP" = VIP,
                 "r2" = r2,
                 "threshold" = threshold
  )

  class(instance) <- "VIP"
  return(instance)
}