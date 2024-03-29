% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{CheckDataReceived}
\alias{CheckDataReceived}
\title{Countries of asylum have provided data.}
\usage{
CheckDataReceived()
}
\arguments{
\item{ctr}{country or list of countries - using alpha3 code - to pull using popdata package}

\item{sqafYear}{numeric value for the year of the check - for instance 2020}

\item{sqafIsASR}{boolean indicating if it is Annual or mid year report TRUE}

\item{Threshold_1}{}

\item{Threshold_2}{}

\item{Threshold_4}{}
}
\value{

}
\description{
Using the concatenated source data tables, checks whether or not data 
	has been received by the expected list of countries of asylum.
}
\examples{
{
}
}
\author{
Edgar Scrase
}
