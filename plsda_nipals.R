#' NIPALS Algorithm for Partial Least Square Discriminant Analysis
#'
#' This function performs NIPALS algorithm for PLS-DA regression.
#'
#' @param
#' formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model
#' to be fitted. See specification of the formula in the 'Details' section.
#' @param
#' data is the dataframe containing the the variables in the model.
#' @param
#' ncomp is the number of components for X.
#' @return
#' An object of class 'PLSDA' is a list containing at least the following components :
#' @return
#' \code{X} the original dataframe of the predictors
#' \cr
#' \code{Y} the original variable to predict
#' \cr
#' \code{Yloadings} the matrix of loadings for Y.
#' \cr
#' \code{Yscores} the matrix of components for Y.
#' \cr
#' \code{Xloadings} the matrix of loadings for X.
#' \cr
#' \code{Xloading.weights} the matrix of weights of the loadings of X.
#' \cr
#' \code{Xscores}the matrix of components of X.
#' \cr
#' \code{TrainPlsData}the PLS-DA training data set.
#' \cr
#' \code{R2} the coefficient of determination of the PLS-DA.
#'
#' @examples
#'pls.t1<-plsda.pls(Species~.,data = iris, ncomp = 2)
#'pls.t1<-plsda.pls(Species~.,data = iris, ncomp = 2, center = TRUE)


plsda.nipals <- function(X,Y, data, ncomp){

  #One hot encoding de y
  ## Vérification que la variables cible soit bien un "factor" ou un "character"
  if (is.factor(Y)==FALSE & is.character(Y)==FALSE){
    stop("La variable n'est pas de type factor ou character") 
    # Si la variable n'est pas de type factor, on la transforme
  }else if (is.factor(Y)==FALSE){
    Y=as.factor(Y)
  }
  # Récupération des différentes modalités 
  levy=levels(Y)
  ## Matrice binarisée
  Ycod<-sapply(levy,function(x){ifelse(Y==x,1,0)})
  #centrer réduire X
  #AJOUTER MSG ERRUR ?
  Xs=scale(X)
  
  #nombre de lignes
  nrx=nrow(Xs)
  ncx=ncol(Xs)
  ncy=ncol(Ycod)
  
  #initialisation des sorties
  Tx = matrix(nrow = nrx, ncol=ncomp) #x-scores
  Uy = matrix(nrow = nrx, ncol=ncomp) #y-scores
  W = matrix(nrow = ncx, ncol=ncomp)  #weights
  Px = matrix(nrow = ncx, ncol=ncomp) #x-loadings
  Qy = matrix(nrow = ncy, ncol=ncomp) #y-loading
  Ycod=scale(Ycod)
  #Algorithme NIPALS
  for(n in 1:ncomp){
    u=matrix(Ycod[,1])

    w=t(Xs)%*%u/(t(u)%*%u)[1,1] #weight
    w=w/sqrt((t(w)%*%w)[1,1]) #normalisation
    
    repeat
    {
      w_new=w
      t=Xs%*%w_new #scores x
      
      q=t(Ycod)%*%t/(t(t)%*%t)[1,1] #loadings de y
      q=q/(t(q)%*%q)[1,1] #normalisation
      u=Ycod%*%q #score y

      w=t(Xs)%*%u/(t(u)%*%u)[1,1] #weight
      w=w/sqrt((t(w)%*%w)[1,1]) #normalisation

      if(abs(mean(w)-mean(w_new))<1e-6){break} #test de la convergence
    }
    
    p=t(Xs)%*%t/(t(t)%*%t)[1,1] #loadings de X
    c=t(t)%*%u/(t(t)%*%t)[1,1] 
    
    #nouvelles valeurs matrices
    Xs=Xs-t%*%t(p)
    
    q=t(t(t)%*%Ycod/(t(t)%*%t)[1,1]) #loadings de Y
    Ycod=Ycod-c[1,1]*t%*%t(q)
      
    #stockage des valeurs
    Tx[,n] = t
    Uy[,n] = u
    W[,n] = w
    Px[,n] = p
    Qy[,n] = q
  }
  #voir ce que je retourne

  instance <- list()
  instance$X <- X
  instance$Y <- Y
  instance$weights <- W
  instance$X_loadings <- Px
  instance$Y_loadings <- Qy
  instance$X_scores <- Tx
  instance$Y_scores <- Uy
  class(instance) <- "PLSDA"
  return(instance)

}
plsda.fit(Species~.,iris,2)  
plsda.nipals(seed~.,data$train,3)

res=plsda.nipals(seed~.,data$train,3)
res$weights


T1 <- Sys.time()
#plsda.nipals(seed~.,data$train,3)
#plsda.nipals(class~.,test_big_data,3)
datanew=plsda.nipals(pid~.,accelerometer,3)
#plsda.nipals(time~.,aps_failure,3)
T2 <- Sys.time()
Tdiff=T2-T1
Tdiff

lo<-nnet::multinom(Y~.,pls$TrainPlsData,trace=F)


# Coefficients de la rÃ©gression logistique (fonctions logit)
cl<-coef(lo)
lo_coef <- c(1,rep_len(0,ncol(cl)-1))
lo_coef<-rbind(lo_coef,cl)
rownames(lo_coef)<-l

# Coefficients finaux (fonction logit)
plsda_coef <- lo_coef[,-1] %*% t(pls$Xloadings)
plsda_coef<- t(cbind(plsda_coef,"Intercept"=as.vector(lo_coef[,1])))