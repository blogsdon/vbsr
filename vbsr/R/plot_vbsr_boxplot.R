plot_vbsr_boxplot = function(res){

	a <- boxplot(res$beta_chi[-res$which_excluded,],plot=FALSE)$stats;
	ymax <- max(a);
	ymin <- min(a);
	plot(res$l0_path,a[3,],ylim=c(ymin,ymax),pch=16,xlab=expression(l[0]),ylab=expression(z[vbsr]));
	for(i in 1:ncol(a)){
		lines(c(res$l0_path[i],res$l0_path[i]),c(a[3,i],a[1,i]));
		lines(c(res$l0_path[i],res$l0_path[i]),c(a[3,i],a[5,i]));
	}

	lines(c(res$l0_path[1],res$l0_path[ncol(a)]),c(0,0),lwd=3,col=2);

	lines(c(res$l0_path[1],res$l0_path[ncol(a)]),c(2.68,2.68),lwd=3,col=4);

	lines(c(res$l0_path[1],res$l0_path[ncol(a)]),c(-2.68,-2.68),lwd=3,col=4);


}