plot_vbsr_beta_p = function(res){


	bp <- res$beta_p[-res$which_excluded,];
	m <- nrow(bp);

	matplot((matrix(rep(res$l0_path,m),length(res$l0_path),m)),t(bp),'l',xlab=expression(l[0]),ylab=expression(p[j]),col=rainbow(50),lwd=2,main=expression(beta),cex.axis=1,cex.lab=1);

}