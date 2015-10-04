\name{extractMethy}
\alias{extractMethy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Methylation Values of Cell-specific Genes for breast tissues}
\description{
Extracting the methylation values of cell-specific genes in tissues or cell-lines, all the methylation data are from 450K platform.
}
\usage{
extractMethy(file)
}
\arguments{
 \item{file}{the cg methylation values in one or more tissues or cell-lines.}
}
\details{
The cell-specific genes are obtained from 32 breast cancer cell-lines and 1 HMEC by computational methods.
For each cancer cell-line, it has its specific charactistic genes, then we merged all the genes for each cell-line.
}
\value{
This function will return a matrix which represents methylation values of the feature genes in tissue or cell-line.
}
\seealso{
	\code{\link{ccPredict}},
	\code{\link{tissueCluster}}
}
\examples{
## tissuecg represents the cg methylation values in two breast tissues.
# data(tissuecg)
## return methylation values of the feature genes in two breast tissues 
# result <- extractMethy(tissuecg)
}
