% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{CheckGeneralDispacedOriginAsylum}
\alias{CheckGeneralDispacedOriginAsylum}
\title{Internationally forcibly displaced have a different country of origin 
than their country of asylum.}
\usage{
CheckGeneralDispacedOriginAsylum()
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
This checks that for refugees, asylum-seekers and VDA, their country of origin should not be the same as the country of asylum.
}
\examples{
{
}
}
\author{
Edgar Scrase
}
