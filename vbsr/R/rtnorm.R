rtnorm <- function(n,mu,sigma,a,b){
	accept <- rep(0,n);
	x <- rep(0,n);
	u <- rep(0,n);
	if(a>mu){
		l<-a;
	} else if(b<mu){
		l<-b;
	} else{
		l<-mu;
	}
	wh <- 1:n;
	while(sum(accept)<n){
		x[wh] <- runif(length(wh),a,b);
		u[wh] <- runif(length(wh),0,1);
		#print(wh)
		#print(x)
		#print(u)
		ntrue <- which(u[wh]<dnorm(x[wh],mu,sigma)/dnorm(l,mu,sigma));
		accept[wh][ntrue]<-TRUE;
		#print(accept)
		wh<-which(accept==FALSE);
	}
	return(x);
}

