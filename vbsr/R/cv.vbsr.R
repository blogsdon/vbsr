cv.vbsr = function(y,X,nfolds=10,l0_path=NULL,...){

	n <- nrow(X);
	#print(n);
	#path_length =50;
	folds = vector("list",nfolds);
	tfolds = vector("list",nfolds);
	res_list <- vector("list",nfolds);
	fold_vec <- sample(1:n,n);
	#print(fold_vec)
	nvec <- floor(n/nfolds);
	qa <- n%%nfolds;
	n2 <- n-qa;
  if(is.null(l0_path)){
    l0min <- -(qchisq(0.05/ncol(X),1,lower.tail=FALSE)-log(n)+2*log(0.95/0.05));
    l0max <- -(qchisq(0.05/ncol(X),1,lower.tail=FALSE)-log(n)+2*log(0.05/0.95));
    l0_path <- seq(l0min,l0max,length.out=50);
  }
  path_length <- length(l0_path);
  
	res_full<-vbsr_net(y=y,X=X,l0_path=l0_path,...);
	#if(screen<1){
	#	X <- X[,res_full$screened_ind[-1]];
	#}
	#print
	#print(dim(X));

	for (i in 1:nfolds){
		folds[[i]]<-fold_vec[((i-1)*nvec+1):(i*nvec)];
		tfolds[[i]] <- (1:n2)[-folds[[i]]];
		#print(folds[[i]]);
		#print(tfolds[[i]]);
		sdv <- apply(X[tfolds[[i]],],2,sd);
		#print(length(which(sdv==0)));
		X[tfolds[[i]],which(sdv==0)]<-rnorm(length(tfolds[[i]]));
		res_list[[i]] <- vbsr_net(y=y[tfolds[[i]]],X=X[tfolds[[i]],],l0_path=l0_path,...);
	}


	sol_len <- c();
	for (i in 1:nfolds){
		sol_len <- c(sol_len,length(res_list[[i]]$l0_path));
	}
	wm <- path_length-min(sol_len)+1;
	X <- cbind(rep(1,n),X);

	
	
	#print(dim(res_list[[i]]$e_beta));
	#print(dim(X));
	
	mse_mat <- matrix(0,min(sol_len),nfolds);
	for(i in 1:nfolds){
		#mse_mat[,i] <- sd(y[folds[[i]]]-1/(1+exp(-X[folds[[i]],]%*%res_list[[i]]$e_beta)))^2;
		pl <- ncol(res_list[[i]]$beta);
		wm <- pl - min(sol_len)+1;
		#print(c(pl,wm));
		mse_mat[,i] <- colMeans((y[folds[[i]]]-X[folds[[i]],]%*%res_list[[i]]$beta[,wm:pl])^2);
		#for(j in 1:path_length){
		#		tab1 <- table(y[folds[[i]]],round(1/(1+exp(-X[folds[[i]],]%*%res_list[[i]]$e_beta[,j]))));
		#		mse_mat[j,i] <- sum(diag(tab1))/sum(tab1);
		#	}
	}
	cv.result <- list();
  
  cv.result$mse <- mse_mat;
  cv.result$full <- res_full;
  cv.result$l0 <- res_list[[nfolds]]$l0_path[wm:pl];
  return(cv.result);
	#cv.result <- list(mse_mat,res_full,res_list[[nfolds]]$l0_path[wm:pl]);
	


}


