plsda.nipals <- function(formula, data, ncomp){
  
  X = model.matrix(formula,data=data)[,-1]
  Y = model.response(model.frame(formula, data = data))

  #One hot encoding de y
  #CHANGER PAR NOTRE CODE
  fX <- as.factor(as.vector(Y))
  lx <- levels(fX)
  Y<-sapply(lx,function(x){ifelse(fX==x,1,0)})

  #centrer réduire X
  #AJOUTER MSG ERRUR ?
  X=scale(X)
  
  #nombre de lignes
  nrx=nrow(X)
  ncx=ncol(X)
  ncy=ncol(Y)
  
  #initialisation des sorties
  Tx = matrix(nrow = nrx, ncol=ncomp) #x-scores
  Uy = matrix(nrow = nrx, ncol=ncomp) #y-scores
  W = matrix(nrow = ncx, ncol=ncomp)  #weights
  Px = matrix(nrow = ncx, ncol=ncomp) #x-loadings
  Qy = matrix(nrow = ncy, ncol=ncomp) #y-loading
  
  #Algorithme NIPALS
  for(n in 1:ncomp){
    u=X[,1]
    
    w=t(X)%*%u/(t(u)%*%u)[1,1] #weight
    w=w/sqrt((t(w)%*%w)[1,1]) #normalisation
    
    repeat
    {
      w_new=w
      t=X%*%w_new #scores x
      
      q=t(Y)%*%t/(t(t)%*%t)[1,1] #loadings de y
      q=q/sqrt((t(q)%*%q)[1,1]) #normalisation
      u=Y%*%q #score y

      w=t(X)%*%u/(t(u)%*%u)[1,1] #weight
      w=w/sqrt((t(w)%*%w)[1,1]) #normalisation

      if(abs(mean(w)-mean(w_new))<1e-8){break} #test de la convergence
    }
    
    p=t(X)%*%t/(t(t)%*%t)[1,1]
    c=t(t)%*%u/(t(t)%*%t)[1,1] #coef regression
    #nouvelles valeurs matrices
    X=X-t%*%t(p)
    Y=Y-c[1,1]*t%*%t(q)
    
    #stockage des valeurs
    Tx[,n] = t
    Uy[,n] = u
    W[,n] = w
    Px[,n] = p
    Qy[,n] = q

  }
  #voir ce que je retourne

}

plsda.nipals(seed~.,data$train,3)

tbis=c[1,1]*t
t
