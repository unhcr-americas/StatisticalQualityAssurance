#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
# A series of quality assurance and validation checks to assess UNHCR's statistics

# Version: January 2021 - v0.1
# Authors: Edgar Scrase

# Copyright © 2021 UNHCR Global Data Service, Statistics and Demographics Section
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------------
# Key parameters
sqafYear <- 2020
sqafIsASR <- TRUE


#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
##### Load the required libraries and core session variables using SetEnvironment.R #####
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------------
# sqaf.......
sqafDirectoryName <- paste0(rootOneDriveDirectory, "Data_Quality_Assurance/")

#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-----Load the source data -----------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

#-----1. Load the source data and build the data cube for the PREVIOUS year ----------------------------------------------
LoadSourceData(sqafYear-1, sqafIsASR, doCountryCodeStandardisation=FALSE)
grPrevYear <- BuildGR(sqafYear-1, sqafIsASR, FALSE, FALSE, doCountryCodeStandardisation=FALSE)

#-----2. Load the source data for the current year using the end or mid year data ----------------
LoadSourceData(sqafYear, sqafIsASR, doCountryCodeStandardisation=FALSE)

#-----3. Build the data cube for the start of the current year -----------------------------------------------------------
grStartYear <- BuildGR(sqafYear, sqafIsASR, TRUE, FALSE, doCountryCodeStandardisation=FALSE)

#-----4. Build the data cube for the current year using the end or mid year data ----------------
BuildDataCube(sqafYear, sqafIsASR, doCountryCodeStandardisation=FALSE)

# Ensure that the REF table has a PT column
dataREFROC$PT <- dataREFROC$populationType

#View(dataRET)
#View(grPrevYear)

#-----4. Load the master data---------------------------------------------------------------------------------------------
# Warning this will take a few moments!!
#LoadAllMasterData()  

#-----5. Load the demographics--------------------------------------------------------------------------------------------
# Note that we don't fix the data here as we want to identify potential issues...
dataDemographics <- LoadDemographics2020(dataPop, FALSE, FALSE, doCountryCodeStandardisation=FALSE)
# Add the index so that we can extract the relevant rows for 1.4
dataDemographics <- dataDemographics %>% mutate(Index = row_number())
#View(dataDemographics)



#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-----Load the SQAF functions and check list and reset the other data ----------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------
#--a-- Reloading this will also reset the global sqafList variable, which we want...
source(paste0(rootScriptDirectory,"StatisticalQualityAssurance/StatisticalQualityAssuranceFrameworkHelper.R"))

#--b-- Load the QA checks from the excel summary file
sqafChecks <- read_excel(paste0(sqafDirectoryName, "SQAF_ValidationChecks.xlsx"), sheet=1, range=cell_cols("A:L"), n_max=10000)

#--c-- Set the levels of the SQAF list and the list of data points based on the list of checks to run
# Note that we set the levels in reverse so that the charts look good!!
levels(sqafList$ID) <- rev(sqafChecks$ID)
levels(sqafDataPoints$ID) <- rev(sqafChecks$ID)

#--d-- ensure the ID is a character
sqafChecks$ID <- as.character(sqafChecks$ID)



#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-----Run the checks -----------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
for( i in 1: nrow(sqafChecks)) {
  print("")
  print("")
  print("------------------------------------------------------------------")
  print("------------------------------------------------------------------")
  print(paste0("Processing check: ", sqafChecks$ID[i], " (", sqafChecks$Area[i] , ") ", sqafChecks$Description[i] ))
  print("------------------------------------------------------------------")
  print("------------------------------------------------------------------")
  
  # Generate the right arguments
  argStr <- GetDatasetArgumentList(sqafChecks$Data[i])
  
  # Then run the check
  success <- eval(parse(text=paste0(sqafChecks$R_Validation_Function[i],"('",sqafChecks$ID[i],"', ", argStr, ")")))
  
  if (success == FALSE) {
    print(paste0("The process ", sqafChecks$ID[i], " did not run successfully - check for errors and warnings."))
  }
    
}

View(sqafList)

#View(dataDemographics)

#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#----- Export the SQAF list as JSON --------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

# Standardise the country names to used IDs which are compatible with the system
# Ensure that the result, severity and year are all integers
sqafList <- StandardiseSQAFList(sqafList)


# Write out the JSON data
write( toJSON(sqafList), paste0(genericDataDirectoryName, "SQAF/SQAF_Output.json"))

# Write out the JSON data for HEXIS
write( toJSON(sqafList %>% 
                mutate(PopulationType=PopulationTypeID, Origin=OriginID, Asylum=AsylumID) %>%
                select(ID, Notes, Result, Severity, Year, PopulationType, Origin, Asylum)), 
       paste0(genericDataDirectoryName, "SQAF/SQAF_Output_HEXIS.json"))


# And write out the lookup of the list of tests (ensuring the ID remains as a character string)
#y <- toJSON(sqafChecks %>% select(ID, Description), na="string", null="list")
write( toJSON(
          sqafChecks %>% 
            select(ID, Description) %>% 
            mutate_if(is.numeric, as.character)), 
       paste0(genericDataDirectoryName, "SQAF/SQAF_Checks.json"))

# Write out a system update time
write( toJSON(Sys.time()), paste0(genericDataDirectoryName, "SQAF/SQAF_UpdateDate.json"))

# Write out the severities
write( toJSON(sqafSeverityList), paste0(genericDataDirectoryName, "SQAF/SQAF_SeverityList.json"))


#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#----- visualise the results --------------------------------------------------------------------------------------------- 
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

# sqaf data points - so here we would need to know the number of datapoints that were valid (i.e. 1) versus 2, 3, 4, by test
# Some tests are by country of asylum only, others are by origin, asylum and / or population type.
# Therefore this needs to happen in each test? or at least in each of the sub tests?
# Data frame would be Asylum, Threshold_1, Threshold_2, Threshold_3, Threshold_4
sqafChartWidth <- 175
sqafChartHeight <- 250

sqafDataPoints <- StandardiseSQAFDataPoints(sqafDataPoints)

#View(sqafDataPoints)

# Test a few charts
plot(SQAFSummaryChart())
# plot(SQAFSummaryChart(NA)) # Same as above
plot(SQAFSummaryChart(""))
plot(SQAFSummaryChart("AFG"))
plot(SQAFSummaryChart("MYA"))

# Save the all chart
SaveChart(SQAFSummaryChart(), sqafChartWidth, sqafChartHeight, paste0(sqafDirectoryName, "Output/SQAF__ALL.png" ))

# Save a chart for each country
for( i in 1: length(sqafCountryList)) {
  print(sqafCountryList[i])
  SaveChart(
    SQAFSummaryChart(sqafCountryList[i]), 
    sqafChartWidth, sqafChartHeight, 
    paste0(sqafDirectoryName, "Output/SQAF_", sqafCountryList[i], ".png" )
  )
  
}




#-------------------------------------------------------------------------------------------------------------------------
# Testing .............................

CheckSignificantChange("xxx", gr, grPrevYear)

View(gr)
View(grPrevYear)
View(sqafList)
View(sqafDataPoints)

View(dataRSDFull)
View(dataDemographics)

View(dataHST)

#sqafList$Index <- NA

temp <- LoadPopulationTypesFromPopulationStatisticsReference()
View(temp)

View(dataRSD)


View(dataREFROC)
View(dataOOC)
View(dataIDP)

grTest <- PreparePopulationData(gr, TRUE, FALSE)
grTest <- PreparePopulationData(gr, TRUE, TRUE)
unique(grTest$PT)
sum(grTest$TotalPopulation[grTest$PT == "IOC"])


tempDemo <- dataDemographics %>% filter(asylum == "GAB" & PT == "ASY" & origin %in% c("EGU", "CAR"))
View(tempDemo)


#-----6. And then build the data cube to compare to the demographics -----------------------------------------------------
dataPop <- BuildPopDataCubeToCompareToDemographics(dataRSD, dataRDP, dataRET, dataIDP, dataREFROC, dataSTAFull, dataOOCFull, dataVDA, dataHST, FALSE)

View(dataRDP)
View(dataIDP)
tempDataIDPIOC <- ReadCSVTable(workingDirectoryName, fnIDP, ";") 
View(tempDataIDPIOC)
tempSum <- tempDataIDPIOC %>% filter(type == "IDPD") %>% group_by(origin) %>% summarise(Count=sum(totalMidYear))
View(tempSum)
sum(dataDemographics$total[dataDemographics$origin == "MYA" & dataDemographics$asylum == "BGD" & grep("camp", dataDemographics$location, perl=T)])
#dataDemographics <- AppendAggregationType(dataDemographics, TRUE, "AggregationType")
#View(dataDemographics)
#unique(dataDemographics$statelessStatus)