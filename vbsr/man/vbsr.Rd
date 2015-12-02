\name{vbsr}
\alias{vbsr}
\title{fit a linear model with variational Bayes spike penalty}
\description{
    Fit a linear model via a fast coordinate variational Bayes algorithm.
	Applicable to linear and logistic regression, and solves the problem
	on either a path of the spike (l0) parameter or at a fixed value based on the data-dimensions.
  }
\usage{
vbsr(y,
  	X,
		ordering_mat=NULL,
		eps=1e-6,
		exclude=NULL,
		add.intercept=TRUE,
		maxit = 1e4,
		n_orderings = 10,
    family = "normal",
		scaling = TRUE,
		return_kl = TRUE,
		estimation_type = "BMA",
		bma_approximation = TRUE,
		screen = 1.0,
		post=0.95,
		already_screened = 1.0,
		kl = 0.99,
		l0_path=NULL,
    cleanSolution=FALSE)}

\arguments{
  \item{y}{response variable. Normally distributed errors for \code{family="normal"}.
  For \code{family="binomial"} should be coded as a vector of 0's and 1's.}
  \item{X}{Design matrix, an n x m matrix, with rows as observations}
  \item{ordering_mat}{Optionally specified coordinate update ordering matrix.  Must be
  in matrix form with columns as permutation vectors of length m, and there must be
  \code{n_orderings} columns. }
  \item{eps}{Tolerance used to determine convergence of the algorithm based on the lower bound.}
  \item{exclude}{An optional indicator vector of length m of 0's and 1's indicating whether to penalize
  a particular variable or not (0=penalize, 1=unpenalized)}
  \item{add.intercept}{A boolean variable indicating whether or not to include an unpenalized intercept variable.}
  \item{maxit}{The maximum number of iterations to run the algorithm for a given solution to a penalized
  regression problem.}
  \item{n_orderings}{The number of random starts used.}
  \item{family}{The type of error model used.  Currently supported modes are \code{family="normal"} and
  \code{family="binomial"}}
  \item{scaling}{A boolean variable indicating whether or not to scale the columns of X to have
  mean zero and variance one.}
  \item{return_kl}{A boolean variable indicating whether or not to return an analysis of the null
  distributed features in the data-set as a function of the penalty parameter.}
  \item{estimation_type}{The type of estimation to perform based on the number of unique solution identified
  to the penalized regression problem.  Valid values are \code{estimation_type="BMA"} and
  \code{estimation_type="MAXIMAL"}}.
  \item{bma_approximation}{A boolean variable indicating whether to compute a full correction to the
  \code{z} statistic. WARNING can make the algorithm very computationally intensive for highly
  multi-modal posterior surfaces.}
  \item{screen}{P-value to do marginal screening.  Default is to not do marginal prescreening (e.g marginal p-value of 1.0)}
  \item{post}{Choice of penalty parameter such that a feature will have a  posterior probability of 0.95 if it passes a Bonferroni correction in the multivariate model.  Default is \code{post=.95}.  More conservative approach would be \code{post=0.5}}
  \item{already_screened}{If features are already screened, the marginal p-value used for screening.}
  \item{kl}{The inner percentiles of the distribution to compute the Kullback-Leibler overfitting statistic.  Only works for analysis when directly specifying a path of penalization parameter (e.g. \code{l0_path}).  For default \code{kl=0.99} the KL-statistic is used for the statistics between the 1\%-99\% of the distribution.}
  \item{l0_path}{The path of penalty parameters to solve the spike regression problem.  If \code{post} is specified, this is computed automatically.}
  \item{cleanSolution}{This parameter determines whether a given solution is further filtered using an unpenalized model.  If \code{cleanSolution=TRUE}, then the features that are significant after a Bonferroni correction given the p-values from the vbsr regression model are then tested in an unpenalized linear regression model.  The p-values and z-statistics are updated using the Wald test from the unpenalized linear regression model for the features that were selected.}
}
\details{
  The solutions to the spike penalized regression model are fit with a 
  coordinate variational Bayes algorithm based on the \code{l0_path} values
  of the spike hyper-parameter.
}
\value{
A list with all the results of the vbsr analysis.
  \item{beta}{The expected value of the penalized regression coefficients.}
  \item{alpha}{The estimated value of the unpenalized regression coefficients.}
  \item{z}{The Z-statistic for each penalized regression coefficient}
  \item{pval}{The p-values based on the asymptotic normal assumption of the Z-statistics}
  \item{post}{The posterior probabilities of each of the regression coefficients}
  \item{l0}{The penalty parameters used to solve the penalized regression problem}
  \item{modelEntropy}{The entropy of the identified approximate posterior probability distribution over model space.}
  \item{modelProb}{The approximate posterior probability distribution over the identified model space.}
  \item{kl_index}{If a path solution was run with the KL diagnostic statistic then the points in the path where the KL statistic is nearest the min, the mean, the min + 1 s.e., and the mean +1 s.e.}
  \item{kl}{The KL statistic computed across the path}
  \item{kl_min}{The minimum KL statistic identified along the path}
  \item{kl_mean}{The expected KL statistic given the number of features identified}
}
\references{Logsdon, B.A, G.E. Hoffman, and J.G. Mezey (2010)
  \emph{A variational Bayes algorithm for fast and 
  accurate multiple locus genome-wide association analysis},
  \url{http://www.biomedcentral.com/1471-2105/11/58},
  \emph{BMC Bioinformatics, Vol. 11(1), 58}
  
  Logsdon, B.A., G.E. Hoffman, and J.G. Mezey, (2012).
  \emph{Mouse obesity network reconstruction with a variational Bayes algorithm to employ aggresive false positive control},
  \url{http://www.biomedcentral.com/1471-2105/13/53/},
  \emph{BMC Bioinformatics, Vol. 13(1), 53}
  
  Logsdon, B.A., C.L. Carty, A.P. Reiner, J.Y. Dai, and C. Kooperberg (2012).
  \emph{A novel variational Bayes multiple locus Z-statistic for genome-wide association studies with Bayesian model averaging.}
  \url{}
  \emph{Bioinformatics, Vol. 28(13), 1738-1744}
}
\author{Benjamin A. Logsdon}
\seealso{\code{compute_KL}}
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


   res<- vbsr(y,X,family="normal");
}
\keyword{models}
\keyword{regression}

 
