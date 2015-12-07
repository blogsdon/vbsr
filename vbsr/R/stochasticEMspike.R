stochasticEMSpike <- function(y,x,z=NULL,pb,burnIn=1e3,capture=5e3,captureFreq=0.1){
  ###y = outcomes
  ###x = design matrix
  ###pb = prior probability of being non-zero
  ###model <- list
  
  #n: number of samples
  n <- nrow(x)
  
  #m: number of features (to be selected on)
  m <- ncol(x)
  
  #p: number of fixed covariates
  p <- ncol(z)
  
  #function to intialize the covariate matrix if it has not bee initialized
  initializeFixedCovariates <- function(z,n){
    if(is.null(z)){
      z <- as.matrix(rep(1,n))
    }
    return(z)
  }
  
  #function to initialize the starting condition
  initializeModelState <- function(m,p,pb,capture,captureFreq){
    modelState <- list()
    
    #variable to track the state of beta
    modelState$beta <- rep(0,m)
    
    #variable to track the state fo the beta mean estimate
    modelState$betaMu <- rep(0,m)
    
    
    #variable to track the state of the beta variance estimates
    modelState$betaSigma <- rep(0,m)
    
    #variable to track the state of the probability paremeter for the beta distributions
    modelState$betaP <- rep(0,m)
    
    #variable to track the state of the fixed effect estimates
    modelState$alpha <- rep(0,p)
    
    #variable to strack the state of the error variance parameter
    modelState$sigma <- 1
    
    #variable to track the iteration of the gibbs sampler
    modelState$iteration <- 1
    
    #prior probability of being non-zero
    modelState$pb <- pb
    
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
    
    return(modelState)
  }
  
  #function to update the beta parameters
  updateBeta <- function(modelState){}
  
  #function to update the alpha parameters
  updateAlpha <- function(modelState){}
  
  #function to update the error parameters
  updateError <- function(modelState){}
  
  #function to update the likelihood parameters
  updateLogLikelihood <- function(modelState){}
  
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
  }
  
  return(modelState)
}