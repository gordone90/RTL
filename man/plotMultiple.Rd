% Generated by roxygen2 (4.0.1): do not edit by hand
\name{plotMultiple}
\alias{plotMultiple}
\title{\code{plotMultiple}}
\usage{
plotMultiple(numberofcharts = 2, chartsByRow = 2)
}
\arguments{
\item{numberofcharts}{A numeric value representing the otal number of charts to plots.}

\item{chartsByRow}{Number of rows you desired in the plot grid.}
}
\value{
Sets par(mfrow=c(chartsByCol,chartsByRow))
}
\description{
Functionn to automatically size multile object plots. Best used when using a loop to plot a changing number of variables.
}
\examples{
data(data)
RTL:::plotMultiple(numberofcharts=4,chartsByRow=2)
plot(Cl(CL1));plot(Cl(CL2));plot(Cl(CL3));plot(Cl(CL4))
RTL:::plotMultiple(numberofcharts=4,chartsByRow=1)
plot(Cl(CL1));plot(Cl(CL2));plot(Cl(CL3));plot(Cl(CL4))
}
\author{
Philippe Cote <coteph@mac.com,philippe.cote@scotiabank.com>
}

