% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{CheckUNHCRRSDProcedures}
\alias{CheckUNHCRRSDProcedures}
\title{Logical checks specifically relating to UNHCR procedures}
\usage{
CheckUNHCRRSDProcedures()
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
For UNHCR procedures:
  a. Applications/Decisions MUST equal persons (P) (Reason: UNHCR does not report mandate RSD in cases)
  
  b. “Decisions” MUST equal one of FI, AR, RA (historically some UNHCR operations used FA). 
       No other Decision type can be used. (Reason: UNHCR does not conduct mandate JR, IN, EO, SP or TA RSD)
     
  c. Cannot recognize under the column “Other”, all recognitions must be under 1951 Convention 
      (Reason: all UNHCR recognitions are under convention/mandate)
}
\examples{
{
}
}
\author{
Edgar Scrase
}
