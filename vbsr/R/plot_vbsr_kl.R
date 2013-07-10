plot_vbsr_kl = function(res){
	plot(res$l0_path,rev(res$kl),'l',xlab=expression(l[0]), ylab=expression(log~KL),main=expression(log~KL~fit~along~l[0]~path),cex.axis=1,cex.lab=1);
	pl <- length(res$l0_path);
	lpat <- res$l0_path;
	const1 <-res$kl_min+res$kl_se;
	const2 <-res$kl_min;
	const3 <- res$kl_mean;
	const4 <- res$kl_mean+res$kl_se;
	lines(x=c(res$l0_path[1],res$l0_path[pl]),y=c(const1,const1),col=2,lwd=2);
	lines(x=c(res$l0_path[1],res$l0_path[pl]),y=c(const2,const2),col=4,lwd=2);
	lines(x=c(res$l0_path[1],res$l0_path[pl]),y=c(const3,const3),col=3,lwd=2);
	lines(x=c(res$l0_path[1],res$l0_path[pl]),y=c(const4,const4),col=5,lwd=2);
	legend(res$l0_path[pl],max(res$kl),c('log(KL) min','log(KL) min + 1 s.e.','E[log(KL)]','E[log(KL)] + 1 s.e.'),lty=1,lwd=2,col=c(4,2,3,5),box.lty=0,cex=1);
}
