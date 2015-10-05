\name{ccPredict}
\alias{ccPredict}
\title{
Cell Clone Prediction}
\description{
Using methylation values of 2320 cell-specific genes to predict the cell subclone component in breast tissues.
}
\usage{
ccPredict(methy, p = 0.01, q = 1e-05)
}
\arguments{
  \item{methy}{methylation values of the cell-specific genes in tissue.}
  \item{p}{ significant threshold value for partial least squares, default is 0.01.}
  \item{q}{ minimum of the cell component, default is 0.00001.}
}
\details{
Obtain the methylation values of specific genes in tissues from function extractMethy, then set the threshold p value and q value. 
Then the reliable content of each component in this breast tissue can be predicted by this ccPredict function.
}
\value{
This function will return a matrix which represents the proportion of 33 cellular component in breast tissues.
}

\seealso{
	\code{\link{extractMethy}},
	\code{\link{tissueCluster}},
	\code{\link{BCheterogeneity}}
}
\examples{
## tissuegene represents methylation values of the specific genes in two breast cancer tissues.
## set p value and q value according to your requirements, here we set 0.01 and 0.00001 as default, respectively!
# result <- ccPredict(tissuegene)  
# result <- ccPredict(tissuegene, p=0.05, q=0.01)  # user-defined p and q value
}
\keyword{prediction}
\keyword{methylation}% __ONLY ONE__ keyword per line
