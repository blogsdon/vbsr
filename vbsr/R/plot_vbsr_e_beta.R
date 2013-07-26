plot_vbsr_e_beta = function(res){


	bp <- res$e_beta[-res$which_excluded,];
	m <- nrow(bp);

	matplot((matrix(rep(res$l0_path,m),length(res$l0_path),m)),t(bp),'l',xlab=expression(l[0]),ylab=expression(beta[vb]),col=rainbow(50),lwd=2,main=expression(beta[vb]),cex.axis=1,cex.lab=1);

}
