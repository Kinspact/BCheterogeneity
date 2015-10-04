\name{ccPredict}
\alias{ccPredict}
\title{
Cell Clone Prediction}
\description{
Using methylation values of the cell-specific genes to predict the cell subclone component in breast tissues.
}
\usage{
ccPredict(methy, p = 0.01, q = 1e-05)
}
\arguments{
  \item{methy}{methylation values of the cell-specific genes in tissue.}
  \item{p}{the threshold value of p value, default is 0.01.}
  \item{q}{the threshold value of q value, default is 0.00001.}
}
\details{
Obtaining methylation values of specific genes in tissues from function extractMethy, then set the threshold p value and q value, 
Then the content of each component in this tissue can be predicted by this ccPredict and the result is reliable.
}
\value{
This function will return a matrix which represents the content of each component in breast tissues.
}

\seealso{
	\code{\link{extractMethy}},
	\code{\link{tissueCluster}}
}
\examples{
## tissuegene represents methylation values of the specific genes in two breast cancer tissues.
## set p value and q value according to your requirements, here set 0.01 and 0.00001 as default, respectively!
# result <- ccPredict(tissuegene)
}
\keyword{prediction}
\keyword{methylation}% __ONLY ONE__ keyword per line
