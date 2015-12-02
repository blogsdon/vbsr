\name{compute_KL}
\alias{compute_KL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute an empirical Kullback Leibler (KL) divergence for an observed distribution of Z-statistics}
\description{This function computes the KL divergence between an observed distribution of Z-statistics and the expected distribution, when truncating at a given percentile of the reference normal distribution.}
\usage{
compute_KL(Zmat,alpha,pval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Zmat}{Matrix of Z-statistics outputted from \code{vbsr}, where columns are Z-statistics of covariates computed at different values of the penalty parameter \code{l0_path}, and rows are covariates in the model.}
  \item{alpha}{The inner percentile of the reference normal distribution to compare to, e.g. if \code{alpha=0.99}, the KL divergence will only be computed for the inner 99\% quantile of the reference distribution.  Allows for deviations in the tails of the distribution to be ignored.}
  \item{pval}{If marginal pre-screening was performed originally, the P-value threshold used for the marginal screening.}
}
\details{This function is a vbsr internal function that computes the KL divergence for the Z-statistic distribution output by \code{vbsr} if run on a grid of \code{l0_path}, and takes as input the inner quantile to compute the KL statistic with (\code{alpha}), and if there was already marginal pre-screening performed to remove the central part of the Z-statistic distribution (\code{pval}).}
\value{
  \item{kl_vec}{This is the observed KL statistic computed along the specified path of \code{l0_path}.}
  \item{min_kl}{This is the minimum value of observed KL statistic}
  \item{mean_kl}{Random permutations are performed to determine the expected KL statistic given the number of covariates being tested, and the setting of \code{alpha}, \code{pval}.  Useful for determining if the observed distribution is well approximated by a normal distribution for a given setting of \code{l0_path} based on the KL statistic.}
  \item{se_kl}{The error in the KL statistics from the random permutations.  Good for determining the range of KL values that is reasonable given the model fits.}
}

\references{Logsdon, B.A., C.L. Carty, A.P. Reiner, J.Y. Dai, and C. Kooperberg (2012).
  \emph{A novel variational Bayes multiple locus Z-statistic for genome-wide association studies with Bayesian model averaging.}
  \url{}
  \emph{Bioinformatics, Vol. 28(13), 1738-1744}
}


\author{Benjamin A. Logsdon}

\note{This function is an internal function, and this functionality is included primarily to include the model fit functions proposed by Logsdon et al. 2012.  The regular \code{vbsr} function with \code{post=0.95}, produces very similar results to the KL statistic using a liberal cutoff, and \code{post=0.5} produces very similar results to the more conservative cutoff proposed in Logsdon et. al. 2012, and the \code{post} approaches are much more computationally efficient, since the algorithm is fit based on just a single penalty parameter.}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{\code{vbsr}}
\examples{
   n <- 100;
   m <- 500;
   ntrue <- 10;
   e <- rnorm(n);
   X <- matrix(rnorm(n*m),n,m);
   tbeta <- sample(1:m,ntrue);
   beta <- rep(0,m);
   beta[tbeta]<- rnorm(ntrue,0,.3);
   y <- X\%*\%beta;
   y <- y+e;
   res<- vbsr(y,X,family="normal",l0_path=seq(-15,-3,length.out=100),post=NULL);
   klRes <- compute_KL(res$z,0.01,1);
   
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{model fit diagnostic}
