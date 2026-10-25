\name{cvPLANN}
\alias{cvPLANN}

\title{
Cross-validation method for a Neural Netwotk Model with the PLANN Method. 
}
\description{
Cross-validation method for the hyper-parameters for an object of class \code{sPLANN}.
}

\usage{
cvPLANN(formula, pro.time=NULL, eval.time = NULL data, cv=10, inter=365.241/12, size=32, decay=0.01,
                    maxit=100, MaxNWts=10000, metric = "ibs", stand = NULL)
}

\arguments{
  \item{formula}{The \code{formula} object.}
  \item{pro.time}{A numeric value with the maximum prognostic. If \code{NULL}, the maximum prognostic time is the last time of event observed.}
  \item{eval.time}{The time at which the metrics is evaluated. Default is \code{NULL} and if \code{NULL}, the median 
  of the observed times is chosen. No impact on metrics that does not use it}
  \item{data}{A data frame in which to look for the variables included in the \code{formula}.}
  \item{cv}{The number of splits for cross-validation. The default value is 10.}
  \item{inter}{A numeric value representing the length of the intervals. Default is set to 365.241/12}
  \item{size}{A numeric value for the number of units in the hidden layer. Default is set to 32}
  \item{decay}{A numeric value for the parameter for weight decay. Default is set to 0.01}
  \item{maxit}{A numeric value for the maximum number of iterations. Default is set to 100.}
  \item{MaxNWts}{The maximum allowable number of weights. There is no intrinsic limit in the code, but increasing MaxNWts will probably allow fits that are very slow and time-consuming. Default is set to 10000}
  \item{metric}{The metric used to select the best set of parameters from the cross-validation. See the function 'metrics' for more details. Default is set to "ibs".}
  \item{stand}{A vector of the continuous variable names to standardize in the data set. Default value is NULL.}

}

\value{
  \item{optimal}{A list giving the optimal value for each parameter.}
  \item{results}{A data frame listing the parameters combinaison and their metrics values.}
}

\examples{
data(dataK) # the database with the observed sample

tune.sPLANN <- cvPLANN(Surv(time, event)~ stade + delay, data=dataK, cv=10,
                     inter=365.241, size=c(2, 4, 8), decay=c(0.01, 0.1),
                    maxit=1000, MaxNWts=10000, metric = "ibs", stand = NULL)
                     
tune.sPLANN$optimal
}

\seealso{ \code{\link{sPLANN}} }

\keyword{Cross-validation}
