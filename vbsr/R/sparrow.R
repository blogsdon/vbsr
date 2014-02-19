sparrow <- function(data,regulatorIndex=NULL,Covariates=NULL,cleanSolution=NULL,...){
  #reformat data
  extractEdges = function(index,data,Covariates=NULL,index2=NULL,...){
    #index2 is indices of regulators in X
    X <- data;
    if(is.null(index2)){
      y <- X[,index];
      G <- X[,-index]
      if(is.null(Covariates)){
        res <- vbsr(y=y,X=G,...);
        pval <- rep(1,length(res$pval)+1);
        pval[-index]<- res$pval;
      } else {
        penvec <- c(rep(0,ncol(G)),rep(1,ncol(Covariates)));
        G <- cbind(G,Covariates)
        res <- vbsr(y=y,X=G,exclude=penvec,...)
        pval <- rep(1,length(res$pval)+1);
        pval[-index] <- res$pval;
      }
    } else {
      if (index %in% index2){
        y <- X[,index];
        G <- X[,index2];
        wi <- which(index2%in%index);
        G <- G[,-wi];
        if(is.null(Covariates)){
          res <- vbsr(y=y,X=G,...)
          pval <- rep(1,length(res$pval)+1);
          pval[-wi] <- res$pval;
        }else {
          penvec <- c(rep(0,ncol(G)),rep(1,ncol(Covariates)));
          G <- cbind(G,Covariates)
          res <- vbsr(y=y,X=G,exclude=penvec,...)
          pval <- rep(1,length(res$pval)+1);
          pval[-wi] <- res$pval;
        }        
      }else{
        y <- X[,index];
        G <- X[,index2];
        if(is.null(Covariates)){
          res <- vbsr(y=y,X=G,...)
          pval <- rep(1,length(res$pval));
          pval <- res$pval;
        }else {
          penvec <- c(rep(0,ncol(G)),rep(1,ncol(Covariates)));
          G <- cbind(G,Covariates)
          res <- vbsr(y=y,X=G,exclude=penvec,...)
          pval <- rep(1,length(res$pval));
          pval <- res$pval;
        }
      }
    }
    return(pval);    
  }
  ind <- 1:nrow(data)
  print(ind[1:5])
  if(is.null(regulatorIndex)){
    if(is.null(Covariates)){
      edgeMatPval <- sapply(X=ind,extractEdges,data=data,...);
    }else{
      edgeMatPval <- sapply(ind,extractEdges,data=data,Covariates=Covariates,...)
    }
  }else{
    if(is.null(Covariates)){
      edgeMatPval <- sapply(ind,extractEdges,data=data,index2=regulatorIndex,...)
    }else{
      edgeMatPval <- sapply(ind,extractEdges,data=data,Covariates=Covariates,index2=regulatorIndex,...)      
    }
  }
  #
  return(edgeMatPval);
}