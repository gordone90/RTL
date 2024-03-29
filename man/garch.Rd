% Generated by roxygen2 (4.0.1): do not edit by hand
\name{garch}
\alias{garch}
\title{\code{garch}}
\usage{
garch(x = data, ret = "rel", roll = TRUE, cmdty = "")
}
\arguments{
\item{x}{Univariate or multivariate xts price series.}

\item{ret}{"rel" for relative returns, "abs" for absolute returns or "flatprice" if no transformation of x is require.}

\item{roll}{True if you want adjust the returns for roll.}

\item{cmdty}{commodity name in expiry_table object}
}
\value{
xts series of annualised Garch(1,1) volatilities if using relative returns.
}
\description{
Computes annualised Garch(1,1) volatilities using fGarch.
}
\examples{
data(data)
RTL:::garch(x=Cl(CL1),ret="rel",roll=TRUE,cmdty="cmewti")
RTL:::garch(x=merge(Cl(CL1),Cl(CL24)),ret="rel",roll=TRUE,cmdty="cmewti")
}
\author{
Philippe Cote <coteph@mac.com,philippe.cote@scotiabank.com>, Nima Safain <nima.safaian@gmail.com,nima.safaian@scotiabank.com>
}

