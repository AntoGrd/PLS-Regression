plsda.print=function(objet){

 # Création d'une matrice avec les coefficients 
  
 res=objet$coef
 
 # Modification des lignes et des colonnes 
 rownames(res)=c(colnames(objet$X))
 colnames(res)=c(colnames(objet$Y_dummies))
 
 # Rajout de l'intercept dans l'objet res
 intercept=objet$intercept
 res=rbind(res,intercept)
 
 # Retour du résultat
 return(res)
  
}

plsda.print(modele)
  
