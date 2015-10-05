\name{BCheterogeneity-package}
\alias{BCheterogeneity-package}
\alias{BCheterogeneity}
\docType{package}
\title{
Breast cancer heterogeneity evaluation}
\description{
This package can be used to evaluate the heterogeneity in breast tissues for DNA methylation data from Illumina Infinium HumanMethylation450 microarray.
}
\details{
\tabular{ll}{
Package: \tab BCheterogeneity\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-10-05\cr
License: \tab GPL-2\cr
}
}
\author{
Wen Yanhua and Song Li
}
\keyword{ package }
\seealso{
	\code{\link{extractMethy}},
	\code{\link{ccPredict}},
	\code{\link{tissueCluster}}
}
\examples{
# library(BCheterogeneity)
## return the methylation values of cell-type specific genes in two breast tissues.
# result1 <- extractMethy(tissuecg) 
## return the content of cell component in two tissues.
# result2 <- ccPredict(result1)   
# result2 <- ccPredict(result1, p=0.05, q=0.01)
## retrun the probabilities of two tissues belonging to nine clusters.
# result3 <- tissueCluster(result2) 
# result3 <- tissueCluster(result2, type="prob")
}
