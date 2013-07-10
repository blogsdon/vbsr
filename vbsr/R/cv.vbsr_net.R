cv.vbsr_net = function(y,X,nfolds,path_length=100,screen=1,...){

	n <- nrow(X);
	#print(n);
	#path_length =100;
	folds = vector("list",nfolds);
	tfolds = vector("list",nfolds);
	res_list <- vector("list",nfolds);
	fold_vec <- sample(1:n,n);
	#print(fold_vec)
	nvec <- floor(n/nfolds);
	qa <- n%%nfolds;
	n2 <- n-qa;


	res_full<-vbsr_net(y,X,path_length=path_length,screen=screen,...);
	if(screen<1){
		X <- X[,res_full$screened_ind[-1]];
	}
	#print
	#print(dim(X));

	for (i in 1:nfolds){
		folds[[i]]<-fold_vec[((i-1)*nvec+1):(i*nvec)];
		tfolds[[i]] <- (1:n2)[-folds[[i]]];
		#print(folds[[i]]);
		#print(tfolds[[i]]);
		sdv <- sd(X[tfolds[[i]],]);
		#print(length(which(sdv==0)));
		X[tfolds[[i]],which(sdv==0)]<-rnorm(length(tfolds[[i]]));
		res_list[[i]] <- vbsr_net(y[tfolds[[i]]],X[tfolds[[i]],],path_length=path_length,screen=1,...);
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
		pl <- ncol(res_list[[i]]$e_beta);
		wm <- pl - min(sol_len)+1;
		#print(c(pl,wm));
		mse_mat[,i] <- colMeans((y[folds[[i]]]-X[folds[[i]],]%*%res_list[[i]]$e_beta[,wm:pl])^2);
		#for(j in 1:path_length){
		#		tab1 <- table(y[folds[[i]]],round(1/(1+exp(-X[folds[[i]],]%*%res_list[[i]]$e_beta[,j]))));
		#		mse_mat[j,i] <- sum(diag(tab1))/sum(tab1);
		#	}
	}
	
	cv.result <- list(mse_mat,res_full,res_list[[nfolds]]$l0_path[wm:pl]);
	


}


