stochasticEMSpike <- function(y,x,z=NULL,l0 = -1.545509,burnIn=1e3,capture=5e3,captureFreq=0.1,seed=1){
  ###y = outcomes
  ###x = design matrix
  ###l0 = logit transfomred prior probability of being non-zero
  ###model <- list
  
  #seed default random seed to control behavior
  set.seed(seed)
  
  #n: number of samples
  n <- nrow(x)
  
  #m: number of features (to be selected on)
  m <- ncol(x)
  
  #p: number of fixed covariates
  
  
  
  rSpikeSlab <- function(mu,sigma2,pb){
    x <- rbinom(1,1,pb)
    if(x==1){
      y <- rnorm(1,mean = mu,sd = sqrt(sigma2))
    }else{
      y <- 0
    }
    return(y)
  }
  
  #function to intialize the covariate matrix if it has not bee initialized
  initializeFixedCovariates <- function(z,n){
    if(is.null(z)){
      z <- as.matrix(rep(1,n))
    }
    return(z)
  }
  
  z <- initializeFixedCovariates(z,n)
  
  p <- ncol(z)
  
  #function to initialize the starting condition
  initializeModelState <- function(n,m,p,l0,burnIn,capture,captureFreq,y,x,z){
    modelState <- list()
    
    #variable to track the state of beta
    modelState$beta <- rep(0,m)
    
    #variable to track the state of the fixed effect estimates
    modelState$alpha <- rep(0,p)
    
    #variable to strack the state of the error variance parameter
    modelState$sigma <- 1
    
    #variable to track the iteration of the gibbs sampler
    modelState$iteration <- 1
    
    #prior probability of being non-zero
    modelState$l0 <- l0
    
    #complete log likelihood
    modelState$logLikelihood <- 0
    
    #define the dimensions of the data frames to store
    nSamp <- floor(capture*captureFreq)
    
    #actual sampled betas to store
    modelState$betaCollect <- matrix(0,nSamp,m)
    
    #actual sampled beta means to store
    modelState$betaMuCollect <- matrix(0,nSamp,m)
    
    #actual sampled beta variance to store
    modelState$betaSigmaCollect <- matrix(0,nSamp,m)    
    
    #actual sampled beta probabilities to store
    modelState$betaPCollect <- matrix(0,nSamp,m)    
    
    #sampled fixed covariate effects to store
    modelState$alphaCollect <- matrix(0,nSamp,p)
    
    #sampled error variance estiamtes to store
    modelState$sigmaCollect <- rep(0,nSamp)
    
    #sampled complete log likelihoods to store
    modelState$logLikelihoodCollect <- rep(0,nSamp)
    
    #residual vector
    modelState$residuals <- y
    
    #capture iteration
    modelState$captureIteration <- 1
    
    #capture period
    modelState$capturePeriod <- 1/captureFreq
    
    #burnin
    modelState$burnIn <- burnIn
    
    #sum of squares
    modelState$xSumSquares <- apply(x^2,2,sum)
    
    #y - outcome vector
    modelState$y <- y
    
    #x - design matrix
    modelState$x <- x
    
    #z - covariate matrix
    modelState$z <- z
    
    #number of observations
    modelState$n <- n
    
    #nubmer of penalized variables
    modelState$m <- m
    
    #number of unpenalized variables
    modelState$p <- p

    #zhat matrix
    modelState$Zhat <- solve(t(z)%*%z)%*%t(z)
    
    return(modelState)
  }
  
  #function to update the beta parameters
  updateBeta <- function(modelState){
    for (j in 1:modelState$m){
      #muj - mean estimate
      muj <- t(modelState$x[,j])%*%modelState$residuals
      
      muj <- muj + modelState$beta[j]*modelState$xSumSquares[j]
      
      muj <- muj/modelState$xSumSquares[j]
      
      #sigmaj
      sigmaj <- modelState$sigma/modelState$xSumSquares[j]
      
      chi <- muj^2/sigmaj
      
      #pj
      
      pj <- 1/(1+exp(-0.5*(chi+modelState$l0+log(sigmaj))))
      
      #betaj
      betaj <- rSpikeSlab(muj,sigmaj,pj)

      #update residuals
      modelState$residuals <- modelState$residuals + modelState$x[,j]*(modelState$beta[j]-betaj)
            

      #set beta paramters
      modelState$beta[j] <- betaj

      
      #if collect posterior distributions over betaj's      
      if(modelState$iteration > modelState$burnIn 
         & modelState$iteration%%modelState$capturePeriod==0){
        modelState$betaCollect[modelState$captureIteration,j] <- betaj
        modelState$betaMuCollect[modelState$captureIteration,j] <- muj
        modelState$betaSigmaCollect[modelState$captureIteration,j] <- sigmaj
        modelState$betaPCollect[modelState$captureIteration,j] <- pj
      }
      
    }
    
    #return the current model state
    return(modelState)
  }
  
  #function to update the alpha parameters
  updateAlpha <- function(modelState){
    #get new fixed parameter estimates
    alpha <- modelState$Zhat%*%(modelState$residuals+modelState$z%*%modelState$alpha)
    
    #update residuals
    modelState$residuals <- modelState$residuals + modelState$z%*%(modelState$alpha-alpha)
    
    #set model alpha to new alpha
    modelState$alpha <- alpha

    #capture the alpha distribution
    if(modelState$iteration > modelState$burnIn 
       & modelState$iteration%%modelState$capturePeriod==0){
      modelState$alphaCollect[modelState$captureIteration,] <- alpha
    }
    
    return(modelState)
  }
  
  #function to update the error parameters
  updateError <- function(modelState){
    #generate estimate of error variance
    sigma <- sum(modelState$residuals^2)/modelState$n
    
    #set current state to new estimate
    modelState$sigma <- sigma
    
    #capture the posterior distribution of sigma
    if(modelState$iteration > modelState$burnIn 
       & modelState$iteration%%modelState$capturePeriod==0){
      modelState$sigmaCollect[modelState$captureIteration] <- sigma
    }
    
    return(modelState)
  }
  
  #function to update the likelihood parameters
  updateLogLikelihood <- function(modelState){
    #temporarily going to use just the log likelihood instead of full log posterior
    
    logLikelihood <- -0.5*modelState$n*(log(2*pi*modelState$sigma)+1)
    
    modelState$logLikelihood <- logLikelihood
    
    if(modelState$iteration > modelState$burnIn
       & modelState$iteration%%modelState$capturePeriod==0){
      modelState$logLikelihoodCollect[modelState$catpureIteration] <- logLikelihood
      #last update, update the capture iteration
      modelState$captureIteration <- modelState$captureIteration + 1
    }
    
    return(modelState)
  }
  
  
  modelState <- initializeModelState(n,m,p,l0,burnIn,capture,captureFreq,y,x,z)
  
  while(modelState$iteration <= burnIn+capture){
    
    #update coefficient distribution
    modelState <- updateBeta(modelState)
    
    #update fixed covariate effect distribution
    modelState <- updateAlpha(modelState)
    
    #update error variance estimate distribution
    modelState <- updateError(modelState)
    
    #update compelte log likliehod distribution
    modelState <- updateLogLikelihood(modelState)
    
    #update the iteration
    modelState$iteration <- modelState$iteration + 1
    
    if(modelState$iteration%%10==0){
      cat('iteration:',modelState$iteration,'\n')
    }
  }
  
  return(modelState)
}