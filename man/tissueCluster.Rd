\name{tissueCluster}
\alias{tissueCluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Breast Tissue Classification
}
\description{
This function can give the probabilities of one given sample belonging to the nine clusters by using RandomForest.
}
\usage{
tissueCluster(component, type = "response")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{component}{the content of each cell-line component in one sample}
  \item{type}{one of "response", "prob" or "vote", default value is "response"}
}
\details{
At first, consensus clustering method was performed by using ConcensusClusterPlus package in R to determine subgroups of breast tumors 
based on the cell component of samples. 672 TCGA samples were divided into nine clusters. Then, using the the RandomForest R package
to evaluate the subcluster of the given samples.
}
\value{
This function will return a cluster with the greatest probability.
}

\seealso{
	\code{\link{ccPredict}},
	\code{\link{extractMethy}}
}
\examples{
## tissuecg represents the cg methylation values in two breast tissues.
# data(tissuecg)
## return methylation values of the feature genes in two breast tissues 
# result <- extractMethy(tissuecg)
}
