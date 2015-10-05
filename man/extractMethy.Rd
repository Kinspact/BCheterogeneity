\name{extractMethy}
\alias{extractMethy}
\title{
Extract Methylation Values of Cell-specific Genes for breast tissues}
\description{
Extracting the methylation values of 2320 cell-specific genes in breast tissues or cell-lines, all the methylation data are 
measured by Infinium HumanMethylation450 BeadChip.
}
\usage{
extractMethy(file)
}
\arguments{
 \item{file}{the cg methylation values in one or more tissues or cell-lines.}
}
\details{
The cell-specific genes are obtained from 32 breast cancer cell-lines and 1 normal human mammary epithelial cell 
by computational methods.For each cancer cell-line, it has its specific DNA methylation genes, then we merged all 
the genes specific for each cell-line.
}
\value{
This function will return a matrix which represents methylation values of 2320 specific genes in tissue or cell-line.
}
\seealso{
	\code{\link{ccPredict}},
	\code{\link{tissueCluster}},
	\code{\link{BCheterogeneity}}
}
\examples{
## tissuecg represents the methylation values of cg sites from 450K platform in two breast tissues.
## return methylation values of 2320 specific genes in two breast tissues 
# result <- extractMethy(tissuecg)
}
