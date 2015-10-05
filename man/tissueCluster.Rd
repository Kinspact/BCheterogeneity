\name{tissueCluster}
\alias{tissueCluster}
\title{
Breast Tissue Classification
}
\description{
This function can obtain the probabilities of the given sample belonging to the nine clusters defined by consensus clustering
using RandomForest.
}
\usage{
tissueCluster(component, type = "response")
}
\arguments{
  \item{component}{the content of each cell-line component in one sample, the result from the function ccPredict}
  \item{type}{one of "response", "prob" or "vote", default value is "response"}
}
\details{
Consensus clustering method was performed by using ConcensusClusterPlus package in R to determine subgroups of breast tumors 
based on the cell component of samples. 672 TCGA samples were divided into nine clusters with different clinical characterristic.
We infer the subcluster of the given breast sample based on random forest algorithm.
}
\value{
This function will return a cluster with the greatest probability.
}

\seealso{
	\code{\link{ccPredict}},
	\code{\link{extractMethy}},
	\code{\link{BCheterogeneity}}
}
\examples{
# result1 <- ccPredict(tissuegene)
# result2 <- tissueCluster(result1)
# result3 <- tissueCluster(result1, type="prob")
}
