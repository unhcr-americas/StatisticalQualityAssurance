

#' @name CheckDataReceived
#' @rdname CheckDataReceived
#' @title Countries of asylum have provided data.
#' 
#' @description Using the concatenated source data tables, checks whether or not data 
#' 	has been received by the expected list of countries of asylum.
#' 
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDataReceived
CheckDataReceived <- function(){
}



#' @name CheckDemographicCoverageAgeSex
#' @rdname CheckDemographicCoverageAgeSex
#' @title Demographic coverage by country of asylum and population type - sex and age disaggregation	
#' @description Checks the completeness of coverage by age and sex. 
#' Try to extend the coverage of the available demographic data through new data sources,
#'  statistical modelling or estimation.

#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDemographicCoverageAgeSex
CheckDemographicCoverageAgeSex <- function(){
}


#' @name CheckDemographicCoverageSex
#' @rdname CheckDemographicCoverageSex
#' @title Demographic coverage by country of asylum and population type - sex disaggregation	
#' @description Checks the completeness of coverage by sex.  
#' Try to extend the coverage of the available demographic data through 
#' new data sources, statistical modelling or estimation.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDemographicCoverageSex
CheckDemographicCoverageSex <- function(){
}


#' @name CheckSubNationalCoverage
#' @rdname CheckSubNationalCoverage
#' @title Sub-national coverage by country of asylum and population type	
#' @description Checks the number of locations reported for each country of asylum.
#'   Try to dissaggregate the demographic data by identifying additional regions 
#'   or location in which refugees, IDPs and other population type habitually reside.
#'   This could be by finding new data sources, statistical modelling or estimation.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckSubNationalCoverage
CheckSubNationalCoverage <- function(){
}


#' @name CheckDemographicAggregationType
#' @rdname CheckDemographicAggregationType
#' @title Aggregation type correctly describes the demographic disaggregation recorded in each row	
#' @description There are four types of valid demographic data aggregations - :
#'  'Detailed' where there is full disaggregation using the detailed age cohorts for adults,
#'   , 'M/F and 18-59' where only the 18-59 age cohort is available for adults,
#'    'M/F' where only disaggregation by sex is available and 
#'     'Total' where no disaggregation is available.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDemographicAggregationType
CheckDemographicAggregationType <- function(){
}


#' @name CheckDemographicAgeUnknown
#' @rdname CheckDemographicAgeUnknown
#' @title Demographic disaggregation by age and sex has a small percentage of unknown age cohorts	
#' @description Checks the % of the demographic data (where dissaggregation by age
#'  and sex is available) for which the male and female unknown columns are 
#'  relatively high (a smaller percentage is better).
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDemographicAgeUnknown
CheckDemographicAgeUnknown <- function(){
}


#' @name CheckDemographicAccommodationType
#' @rdname CheckDemographicAccommodationType
#' @title Demographic accommodation type classification is used	
#' @description Checks the % of the demographic data for which the accommodation
#'  type is unknown (a smaller percentage is better).
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDemographicAccommodationType
CheckDemographicAccommodationType <- function(){
}


#' @name CheckDemographicUrbanRural
#' @rdname CheckDemographicUrbanRural
#' @title Demographic urban rural breakdown classification is used	
#' @description Checks the % of the demographic data for which the urban rural 
#' classification is Various (a smaller percentage is better).
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDemographicUrbanRural
CheckDemographicUrbanRural <- function(){
}


#' @name CheckRefugeeReturns
#' @rdname CheckRefugeeReturns
#' @title The number of refugee returns reported by the country of origin is 
#' consistent with the country(ies) of former asylum
#' @description Review together the differences in the number of returns recorded 
#' by the country of origin and the country(ies) of asylum.  
#' Small differences are completely acceptable, but large differences should be rare.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckRefugeeReturns
CheckRefugeeReturns <- function(){
}


#' @name CheckDemographicTotals
#' @rdname CheckDemographicTotals
#' @title Demographic totals are consistent with the other tables	
#' @description Compares the figures for each population type and countries of 
#' origin and asylum that are presented in the demographic table with those
#'  in each of the source tables.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckDemographicTotals
CheckDemographicTotals <- function(){
}


#' @name CheckIDPDataIDMC
#' @rdname CheckIDPDataIDMC
#' @title IDMC comparison for stock and new arrivals	
#' @description Compares the IDMC and UNHCR IDP figures, highlighting significant differences.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckIDPDataIDMC
CheckIDPDataIDMC <- function(){
}


#' @name CheckRefugeeBasis
#' @rdname CheckRefugeeBasis
#' @title Refugee Basis should be from registration or census statistics	
#' @description Where possible, refugee figures should be based on government or 
#' census data rather than estimates.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckRefugeeBasis
CheckRefugeeBasis <- function(){
}


#' @name CheckUNHCRRSDProcedures
#' @rdname CheckUNHCRRSDProcedures
#' @title Logical checks specifically relating to UNHCR procedures	
#' @description For UNHCR procedures:
#'   a. Applications/Decisions MUST equal persons (P) (Reason: UNHCR does not report mandate RSD in cases)
#'   
#'   b. “Decisions” MUST equal one of FI, AR, RA (historically some UNHCR operations used FA). 
#'        No other Decision type can be used. (Reason: UNHCR does not conduct mandate JR, IN, EO, SP or TA RSD)
#'      
#'   c. Cannot recognize under the column “Other”, all recognitions must be under 1951 Convention 
#'       (Reason: all UNHCR recognitions are under convention/mandate)
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckUNHCRRSDProcedures
CheckUNHCRRSDProcedures <- function(){
}


#' @name CheckRSDAfrica
#' @rdname CheckRSDAfrica
#' @title Countries in Africa should typically not recognize individuals under "other"	
#' @description While there are exceptions, in most cases recognitions through RSD
#'  should be under UNHCR's mandate rather than complementary protection in countries in Africa.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckRSDAfrica
CheckRSDAfrica <- function(){
}


#' @name CheckRSDOverTime
#' @rdname CheckRSDOverTime
#' @title Checks the consistency of the procedure type in the RSD table over time	
#' @description If a country reports type of RSD procedure Government one year and
#'  type of procedure as Joint the following year
#'  If a country reports Government or Joint cases in the past and then reports only UNHCR cases
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckRSDOverTime
CheckRSDOverTime <- function(){
}


#' @name CheckAverageCaseSize
#' @rdname CheckAverageCaseSize
#' @title Applications and decisions reported as persons should always have an average case size of 0
#' @description 	Checks the average case size of asylum applications and decisions 
#' reported as persons (rather than cases).  
#' Those reported as persons should have an average size of 0
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckAverageCaseSize
CheckAverageCaseSize <- function(){
}


#' @name CheckStatelessOrigin
#' @rdname CheckStatelessOrigin
#' @title Displaced stateless correctly reported by their specific countries of
#'  former habitual residence	
#' @description Checks the percentage of the origin in the Stateless table that is 
#' STA/UKN versus specific countries of former habitual residence for 
#' displaced STA (e.g. the Rohingya)
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckStatelessOrigin
CheckStatelessOrigin <- function(){
}


#' @name CheckStatelessDisplacedGAZ
#' @rdname CheckStatelessDisplacedGAZ
#' @title Displaced PAL and GAZ origins should not be recorded in the Stateless table	
#' @description There are sensitivities around recording Palestinians as stateless 
#' -therefore reporting these in the Stateless table should be avoided where possible.
#'   It is recommended instead to record these as STA, VAR or UKN
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckStatelessDisplacedGAZ
CheckStatelessDisplacedGAZ <- function(){
}


#' @name CheckHostCommunity
#' @rdname CheckHostCommunity
#' @title Total reported host community population figures should be consistent 
#' with the assisted figures	
#' @description Checks that the total and assisted host community figures are equivalent -
#'  this is the most typical case for reporting on host communities
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckHostCommunity
CheckHostCommunity <- function(){
}


#' @name CheckSignificantChange
#' @rdname CheckSignificantChange
#' @title Change over time is within acceptable boundaries.	
#' @description Checks that the percentage change from previous years 
#' (+- 5-10% for refugee population of 100 or more) is within acceptable boundaries.
#'   The check is conducted by population type and countries of origin and asylum.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckSignificantChange
CheckSignificantChange <- function(){
}


#' @name CheckGeneralMissingBasis
#' @rdname CheckGeneralMissingBasis
#' @title All data should include a basis and source	
#' @description Checks for missing basis and source in the relevant source tables.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckGeneralMissingBasis
CheckGeneralMissingBasis <- function(){
}


#' @name CheckGeneralDispacedOriginAsylum
#' @rdname CheckGeneralDispacedOriginAsylum
#' @title Internationally forcibly displaced have a different country of origin 
#' than their country of asylum.	
#' @description This checks that for refugees, asylum-seekers and VDA, their country of origin should not be the same as the country of asylum.
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckGeneralDispacedOriginAsylum
CheckGeneralDispacedOriginAsylum <- function(){
}


#' @name CheckVDAInOOC
#' @rdname CheckVDAInOOC
#' @title VDA should no longer be included in the OOC table	
#' @description This checks that Venezuelans Displaced Abroad are no longer 
#' included in the OOC table, as there is now a separate table to include these
#'  populations.  There could potentially be special cases here so not an absolute error
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckVDAInOOC
CheckVDAInOOC <- function(){
}


#' @name CheckCountryCodes
#' @rdname CheckCountryCodes
#' @title Country codes are used consistently	
#' @description Checks all origin and asylum codes and ensures that all used are valid
#'
#' @param ctr country or list of countries - using alpha3 code - to pull using popdata package
#' @param sqafYear numeric value for the year of the check - for instance 2020
#' @param sqafIsASR boolean indicating if it is Annual or mid year report TRUE

#' @param Threshold_1	
#' @param Threshold_2	
#' @param Threshold_3
#' @param Threshold_4
#'
#' @return 
#' @author Edgar Scrase
#'
#' @examples
#' {
#'}
#' @export CheckCountryCodes
CheckCountryCodes <- function(){
}