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
# Declare all the relevant folders and file names
genericDataDirectoryName <- paste0(rootDataDirectory, "Data/")

# The master data directory - within this there will be files containing the historic data on refugees, asylum seekers and IDPs
masterDataDirectoryName <- paste0(rootDataDirectory, "Data/Master_Data/")
# The current ASR data
workingDirectoryName <- paste0(rootDataDirectory, "Data_ASR/")

# The demographics foler
demographicDataDirectoryName <- paste0(rootDataDirectory, "Data/Demographics/")

# sqaf.......
sqafDirectoryName <- paste0(rootOneDriveDirectory, "Data_Quality_Assurance/")

# We will need both the live data and the historic data....


#-------------------------------------------------------------------------------------------------------------------------
# Run the ASR / MYSR loader to load the latest data in....

# Load population totals

# Load solutions

# Load demographics

# Load RSD...

# Ensure that all the tables have a PT column
dataREFROC$PT <- dataREFROC$populationType


#-------------------------------------------------------------------------------------------------------------------------
# Reloading this will also reset the global sqafList variable, which we want...
source(paste0(rootScriptDirectory,"StatisticalQualityAssuranceFrameworkHelper.R"))

# Load the QA checks from the excel summary file
sqafChecks <- read_excel(paste0(sqafDirectoryName, "SQAF_ValidationChecks.xlsx"), sheet=1, range=cell_cols("A:L"), n_max=10000)

View(sqafChecks)
#View(dataDemo)

# Filter the demoData to the latest year...
# Um no - we want the rawer output from above for this
dataDemographics <- dataDemo2020
#dataDemo <- ddAll %>% filter(Year == sqafYear)
#View(dataDemo)

# Iterate through each and evaluate the appropriate helper function...
# We can use this process to load it

for( i in 1: nrow(sqafChecks)) {
  print("")
  print("")
  print("------------------------------------------------------------------")
  print("------------------------------------------------------------------")
  print(paste0("Processing check: ", sqafChecks$ID[i], " (", sqafChecks$Area[i] , ") ", sqafChecks$Description[i] ))
  print("------------------------------------------------------------------")
  print("------------------------------------------------------------------")
  # Generate the right arguments
  argStr <- ""
  
  # Here differentiate demographics from returns....
  if (sqafChecks$Data[i] == "Demographics") {
    argStr <- "dataDemographics"
    
  } else if (sqafChecks$Data[i] == "Refugees") {
    argStr <- "dataREFROC, sqafIsASR"
    
  } else if (sqafChecks$Data[i] == "Returns") {
    # returns need the REFROC, returns and a flag as to whether or not this is the ASR
    argStr <- "dataREFROC, dataRET, sqafIsASR"
    
  } else if (sqafChecks$Data[i] == "DemoPoCs") {
    argStr <- "dataDemographics, dataPopulation"

  } else if (sqafChecks$Data[i] == "Stateless") {
    argStr <- "dataSTAUDN, sqafIsASR"
        
  } else if (sqafChecks$Data[i] == "Asylum-seekers") {
    argStr <- "dataRSD"

  } else if (sqafChecks$Data[i] == "All-Asylum-seekers") {
    argStr <- "dataRSDFull, dataRSD, sqafYear, sqafIsASR"
    
  } else if (sqafChecks$Data[i] == "AllBasis") {    
    
    argStr <- "dataREF, dataROC, dataRET, dataIDP, dataSTAUDN, dataOOC, dataVDA, dataHST, sqafIsASR"
    
  } else if (sqafChecks$Data[i] == "OOC") {    
    
    argStr <- "dataOOC"

  } else if (sqafChecks$Data[i] == "InternationallyForciblyDisplaced") {
    argStr <- "gr"
    
  } else if (sqafChecks$Data[i] == "All") {
    argStr <- "gr"
    
  } else if (sqafChecks$Data[i] == "All-Historic") {
    argStr <- "gr, dataPoCs"
    
  }
  
  
  
  success <- eval(parse(text=paste0(sqafChecks$R_Validation_Function[i],"('",sqafChecks$ID[i],"', ", argStr, ")")))
  
  if (success == FALSE) {
    print(paste0("The process ", sqafChecks$ID[i], " did not run successfully - check for errors and warnings."))
  }
    
}



unique(dataRSDFull$asylum[dataRSDFull$ApplicationDataType == "C" & dataRSDFull$Year == 2018])


#-------------------------------------------------------------------------------------------------------------------------
# Export the SQAF list as JSON

View(sqafList)
glimpse(sqafList)


# Note that even with these na and nulls specified, if a field is NA it is simply not included in the data
# This is particularly the case with the PopulationType, Asylum and Origin.  We might need to find a better approach for handling these
x <- toJSON(sqafList, na="string", null="list")
#x <- jsonlite::toJSON(sqafList, auto_unbox=TRUE)
#glimpse(x)
#cat(x)


write( toJSON(sqafList), paste0(sqafDirectoryName, "/SQAF_Output.json"))







#-------------------------------------------------------------------------------------------------------------------------
# Testing .............................

View(dataSTAUDN)
View(gr)


#print(DataFrameColumnExists(sqafList,"ID"))


unique(dataDemographics$typeOfAggregation)



unique(dataDemographics$AggregationType)


View(dataDemographics)


# https://www.markvanderloo.eu/yaRb/2016/03/25/easy-data-validation-with-the-validate-package/
tempDodgyList <- violating(dataDemographics, validator(total==(totalFemaleTotal + totalMaleTotal) &
             
             totalFemaleTotal==(totalFemale_0_4 + totalFemale_5_11 + totalFemale_12_17 + 
             totalFemale_18_24 + totalFemale_25_49 + totalFemale_50_59 + totalFemale_60 + Female_Unknown) &
             totalMaleTotal==(totalMale_0_4 + totalMale_5_11 + totalMale_12_17 + 
             totalMale_18_24 + totalMale_25_49 + totalMale_50_59 + totalMale_60 + Male_Unknown)
))
View(tempDodgyList)


tempDodgyList <- violating(dataDemographics, validator(AggregationType != '' & AggregationType %in% c("Default", "18_59", "M_F", "Total")))
unique(dataDemographics$AggregationType)
nrow(tempDodgyList)





