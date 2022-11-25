#' fit function for PLSDA
#'
#' @description Using NIPALS algorithm to fit the PLS classification to data, for binary or multinomial target
#'
#' @param formula 
#' @param data a data frame where you want the fit the datas
#' @param ncomp an integer which corresponds to the number of components in the model
#'
#' @return a "PLSDA" object with the coefficients we are going to use in the prediction
#' @export
#'
#' @examples plsda.fit(Species~.,iris,2)
#' 
plsda.fit <- function(formula, data, ncomp){
  
  if (!inherits(formula,"formula")){ #check if formula is given
    stop("You didn't enter a formula")
  }
  
  if (!is.data.frame(data)){ #check if data is a dataframe
    stop("data is not a dataframe")
  }
  
  if (!is.numeric(ncomp)){ #check if ncomp is an integer
    stop("ncomp is not an integer")
  }else if (ncomp != round(ncomp)){
    stop("ncomp is not an integer")
  }
  
  X = model.matrix(formula,data=data)[,-1]
  Y = model.response(model.frame(formula, data = data))
  
  #One hot encoding y
  ## Verification that the target variable is a factor or a character
  if (is.factor(Y)==FALSE & is.character(Y)==FALSE){
    stop("y is neither a factor or character") 
    #
  }else if (is.factor(Y)==FALSE){
    Y=as.factor(Y)
  }
  # recovery of modalities
  levy=levels(Y)
  ## binarized matrix
  Ycod<-sapply(levy,function(x){ifelse(Y==x,1,0)})
  
  #X and Y colnames
  xnames=colnames(X)
  ynames=colnames(Ycod)
  
  #standardise x
  Xs=scale(X)
  Ycodsc=scale(Ycod)
  #number of lines/columns
  nrx=nrow(Xs)
  ncx=ncol(Xs)
  ncy=ncol(Ycod)
  
  #initialize outputs
  Tx = matrix(nrow = nrx, ncol=ncomp) #x-scores
  Uy = matrix(nrow = nrx, ncol=ncomp) #y-scores
  W = matrix(nrow = ncx, ncol=ncomp)  #weights
  Px = matrix(nrow = ncx, ncol=ncomp) #x-loadings (components)
  Qy = matrix(nrow = ncy, ncol=ncomp) #y-loading
  Ycodsc=scale(Ycod)
  #Algorithme NIPALS
  for(n in 1:ncomp){
    u=matrix(Ycodsc[,1])
    
    w=t(Xs)%*%u/(t(u)%*%u)[1,1] #weight
    w=w/sqrt((t(w)%*%w)[1,1]) #normalisation
    
    repeat #while w didnt converge
    {
      w_new=w
      t=Xs%*%w_new #scores x
      
      q=t(Ycodsc)%*%t/(t(t)%*%t)[1,1] #loadings de y
      q=q/(t(q)%*%q)[1,1] #normalisation
      u=Ycodsc%*%q #score y
      
      w=t(Xs)%*%u/(t(u)%*%u)[1,1] #weight
      w=w/sqrt((t(w)%*%w)[1,1]) #normalisation
      
      if(abs(mean(w)-mean(w_new))<1e-6){break} #test of convergence
    }
    
    p=t(Xs)%*%t/(t(t)%*%t)[1,1] #x loadings
    c=t(t)%*%u/(t(t)%*%t)[1,1] 
    
    #new matrix values
    Xs=Xs-t%*%t(p)
    
    q=t(t(t)%*%Ycodsc/(t(t)%*%t)[1,1]) #y loadings
    Ycodsc=Ycodsc-c[1,1]*t%*%t(q)
    
    #storage of values
    Tx[,n] = t
    Uy[,n] = u
    W[,n] = w
    Px[,n] = p
    Qy[,n] = q
  }
  
  X_rot = W %*% solve(t(Px)%*%W) #matrix rotation
  coef = X_rot %*% t(Qy)
  coef = coef * sapply(data.frame(Ycod),sd) #coef for prediction
  intercept = sapply(data.frame(Ycod),mean) #means calculation for intercept
  
  rownames(Px)=xnames
  rownames(Qy)=ynames
  rownames(W)=xnames
  rownames(coef)=xnames
  colnames(coef)=ynames
  
  instance <- list()
  instance$X <- X
  instance$Y <- Y
  instance$yname <- toString(formula[[2]])
  instance$Y_dummies <- Ycod
  instance$weights <- W
  instance$X_loadings <- Px
  instance$Y_loadings <- Qy
  instance$X_scores <- Tx
  instance$Y_scores <- Uy
  instance$coef <- coef
  instance$intercept <- intercept
  instance$ncomp <- ncomp
  class(instance) <- "PLSDA"
  return(instance)
}

plsda.fit(Species~.,iris,3)
