% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{DTstat}
\alias{DTstat}
\title{Get data from an SDMX source as a data.table}
\usage{
DTstat(
  code,
  reshape = as.formula(... ~ TIME),
  drop = TRUE,
  labels = FALSE,
  ccode = defaultcountrycode(),
  startPeriod = NULL,
  endPeriod = NULL
)
}
\arguments{
\item{code}{a character query string with a RestFul SDMX query (such as ECB/EXR/A.GBP+CHF.EUR.SP00.A). See \code{\link{mdSdmx}} for details}

\item{reshape}{a formula combining dimension names in order to reshape the result. Default is \code{...~ TIME} which means each time period to be in a column, plus extra identifier columns at the beginning. See \code{\link[data.table]{dcast}} for details.}

\item{drop}{if TRUE, drop any singleton dimensions (see also drop.md3)}

\item{labels}{add extra columns with the descriptions for each dimension element, e.g. "Austria" for "AUT"}

\item{ccode}{If not \code{ccode==NULL}, then the function attempts to convert to ccode destination such as 'iso2c', 'EC', 'iso3c'. See \code{\link[MDcountrycode]{ccode}} for permissible values.  \code{\link[MDcountrycode]{defaultcountrycode}} for defining this value as a session-wide option.}

\item{startPeriod}{placeholder for future use}

\item{endPeriod}{placeholder for future use}
}
\value{
a \code{data.table}
}
\description{
Get data from an SDMX source as a data.table
}
\details{
This function tweaks the package \code{rsdmx}  to load data. For formulating SDMX query codes, check \code{help(\link{mdSdmx})}
}
\examples{
mdSdmx("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A")
mdSdmx("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A",as = 'zoo')

o1=mdSdmx("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A",labels=TRUE)
dimcodes(o1)
}
\seealso{
\code{\link{helpmds}}, \code{\link{DTstat}}, \code{\link[MD3]{Nomics}}, \code{\link[MDcountrycode]{defaultcountrycode}}, \code{\link{mdAmeco}}, \code{\link{mdWEO}}
}
