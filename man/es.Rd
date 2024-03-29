% Generated by roxygen2 (4.0.1): do not edit by hand
\name{es}
\alias{es}
\title{\code{es}}
\usage{
es(x, ci)
}
\arguments{
\item{x}{Univariate xts series of returns.}

\item{ci}{Confidence interval i.e. .05 or .95}
}
\value{
A numeric value.
}
\description{
Returns the Expected Shortfall risk based on a given confidence interval.
}
\examples{
data(data)
ret<-RTL:::data_ret(x=Cl(CL1),returntype=c("relative"))
RTL:::es(x=ret,ci=.95)
}
\author{
Philippe Cote <coteph@mac.com,philippe.cote@scotiabank.com>
}

