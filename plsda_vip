plsda.vip<-function(PLS,threshold=0.8){

  #Récupération des variables prédictives du modèle
  x = as.matrix(PLS$X)
  #Récupération des poids des variables prédictives du modèle
  W = as.matrix(objectPLS$Xloading.weights)
  #Récupération du coefficient de détermination de la PLS-DA
  r2 = objectPLS$R2

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
  X = as.data.frame(PLS$X)
  newX = X[,colnames(x) %in% var_imp]


  object <- list("newX" = newX,
                 "Variable" = var_imp,
                 "VIP" = VIP,
                 "r2" = r2,
                 "threshold" = threshold
  )

  class(object) <- "VIP"
  return(object)
}

plot.vip<-function(VIP){
  if (class(VIP) != "VIP") {
    stop("object must be a VIP !")
  }
  titre=paste("Variable Importance in Projection ( *:VIP>",object$threshold,")")
  plot_VIP=barplot(as.matrix(t(object$VIP)),main=titre,col="blue",ylim=c(0.0,max(object$VIP)+0.3),xlab="Variable",ylab="VIP")
  seuil=NULL
  seuil[object$VIP[,1]>object$threshold]='*'
  text(plot_VIP,t(object$VIP)+0.15,seuil,cex=2)
  abline(object$threshold,0,col = "red")
}
