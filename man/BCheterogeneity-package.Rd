\name{BCheterogeneity-package}
\alias{BCheterogeneity-package}
\alias{BCheterogeneity}
\docType{package}
\title{
Breast cancer heterogeneity evaluation}
\description{
This package can be used to evvalute the heterogeneity in breast tissues for DNA methylation data from Illumina Infinium HumanMethylation450 microarray.
}
\details{
\tabular{ll}{
Package: \tab BCheterogeneity\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-10-01\cr
License: \tab GPL-2\cr
}
}
\author{
Wen Yanhua and Song Li
Wen Yanhua <lovingtengfei@126.com>
}
\keyword{ package }
\seealso{
	\code{\link{extractMethy}},
	\code{\link{ccPredict}},
	\code{\link{tissueCluster}}
}
\examples{
# library(BCheterogeneity)
## load the data, which represents the cg methylation values in HMEC.
# data(tissuecg) 
##return the methylation values of feature genes in HMEC.
# result1 <- extractMethy(tissuecg) 
## return the content of cell component in HMEC.
# result2 <- ccPredict(result1) 
## retrun the probabilities of HMEC belonging to nine clusters.
# result3 <- tissueCluster(result2) 
}
