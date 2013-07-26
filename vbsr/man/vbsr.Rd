\name{vbsr_net}
\alias{vbsr_net}
\title{fit a linear model with variational Bayes spike penalty}
\description{
    Fit a linear model via a fast coordinate variational Bayes algorithm.
	Applicable to linear and logistic regression, and solves the problem
	on a path of the spike (l0) parameter.
  }
\usage{
vbsr_net(y, 
		X,
		ordering_mat=NULL,
		eps=1e-6,
		path_length=200,
		exclude=NULL,
		add.intercept=TRUE,
		maxit = 1e4,
		n_orderings = 10,
		regress = "LINEAR",
		scaling = TRUE,
		return_kl = TRUE,
		estimation_type = "BMA",
		bma_approximation = TRUE,
		screen = 1.0,
		already_screened = 1.0,
		kl = 0.99,
		l0_path=NULL)}

\arguments{
  \item{y}{response variable. Normally distributed errors for \code{regress="LINEAR"}.
  For \code{regress="LOGISTIC"} should be coded as a vector of 0's and 1's.}
  \item{X}{Design matrix, an n x m matrix, with rows as observations}
  \item{ordering_mat}{Optionally specified coordinate update ordering matrix.  Must be
  in matrix form with columns as permutation vectors of length m, and there must be
  \code{n_orderings} columns. }
  \item{eps}{Tolerance used to determine convergence of the algorithm based on the lower bound.}
  \item{path_length}{The length of the path solution used.}
  \item{exclude}{An optional indicator vector of length m of 0's and 1's indicating whether to penalize
  a particular variable or not (0=penalize, 1=unpenalized)}
  \item{add.intercept}{A boolean variable indicating whether or not to include an unpenalized intercept variable.}
  \item{maxit}{The maximum number of iterations to run the algorithm for a given solution to a penalized
  regression problem.}
  \item{n_orderings}{The number of random starts used.}
  \item{regress}{The type of error model used.  Currently supported modes are \code{regress="LINEAR"} and
  \code{regress="LOGISTIC"}}
  \item{scaling}{A boolean variable indicating whether or not to scale the columns of X to have
  mean zero and variance one.}
  \item{return_kl}{A boolean variable indicating whether or not to return an analysis of the null
  distributed features in the data-set as a function of the path position.}
  \item{estimation_type}{The type of estimation to perform based on the number of unique solution identified
  to the penalized regression problem.  Valid values are \code{estimation_type="BMA"} and
  \code{estimation_type="MAXIMAL"}}.
  \item{bma_approximation}{A boolean variable indicating whether to compute a full correction to the
  \code{beta_chi} statistic. WARNING can make the algorithm very computationally intensive for highly
  multi-model posterior surfaces.}
  \item{screen}{}
  \item{already_screened}{}
  \item{kl}{}
  \item{l0_path}{}
}
\details{
  The solutions to the spike penalized regression model are fit with a 
  coordinate variational Bayes algorithm based on the \code{l0_path} values
  of the spike hyper-parameter.
}
\value{
A list with all the results of the vbsr analysis.
  \item{beta_chi}{}
  \item{beta_sigma}{}
  \item{e_beta}{}
  \item{beta_p}{}
  \item{lb}{}
  \item{kl}{}
  \item{l0_path}{}
  \item{sma_beta}{}
  \item{sma_chi}{}
  \item{screened_ind}{}
  \item{kl_min}{}
  \item{kl_mean}{}
  \item{kl_se}{}
  \item{ordering_mat}{}
  \item{which_excluded}{}
}
\references{Logsdon, B., Hoffman, G. and Mezey, J. (2010)
  \emph{A variational Bayes algorithm for fast and 
  accurate multiple locus genome-wide association analysis},
  \url{http://www.biomedcentral.com/1471-2105/11/58},
  \emph{BMC Bioinformatics, Vol. 11(1), 58 2010}
}
\author{Benjamin Logsdon}

\seealso{\code{plot_vbsr_e_beta}, \code{plot_vbsr_beta_p}, 
	\code{plot_vbsr_beta_chi}, \code{plot_vbsr_boxplot}, 
	\code{plot_vbsr_kl}, and \code{cv.vbsr_net} functions.}
\examples{
   library(MASS);
   n <- 100;
   m <- 500;
   ntrue <- 10;
   e <- rnorm(n);
   X <- matrix(rnorm(n*m),n,m);
   tbeta <- sample(1:m,ntrue);
   beta <- rep(0,m);
   beta[tbeta]<- rnorm(ntrue,0,.3);
   y <- rep(0,n);
   for(i in 1:n){
	for(j in 1:m){
		y[i] <- y[i] + X[i,j]*beta[j];
	}
   }
   y <- y+e;


   res<- vbsr_net(y,X,regress="LINEAR");
}
\keyword{models}
\keyword{regression}

 
