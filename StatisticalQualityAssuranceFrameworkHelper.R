#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
# Helper function to support the Statistics Quality Assurance Framework

# Version: January 2021 - v0.4
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

# declare our data cube as an empty data frame with the relevant column types
sqafList <- 
  data.frame(
    ID = factor(), # e.g. the SQAF ID 1.2.3
    
    Notes = character(),
    
    Result = integer(),
    Severity=integer(),    
    
    Year = integer(),
    PopulationType = character(),
    Origin = character(),
    Asylum = character(),
    stringsAsFactors=FALSE # This negates the use of factors so that the character based columns can contain anything
  )

# And this contains a summary of the data points for each check
sqafDataPoints <-
  data.frame(
    ID = factor(), # e.g. the SQAF ID 1.2.3
    
    Asylum = character(),    

    Total = integer(),
    Threshold_1 = integer(),
    Threshold_2 = integer(),
    Threshold_3 = integer(),
    Threshold_4 = integer(),
    
    stringsAsFactors=FALSE # This negates the use of factors so that the character based columns can contain anything
  )

# This is our data structure for the severity
sqafTitles <- c("OK", "Could Fix", "Should Fix", "Must Fix")
sqafKeys <- c("OK", "Could_Fix", "Should_Fix", "Must_Fix")
sqafSeverityList <- c(1, 2, 3, 4)
sqafColours <- c("#2c8ac1", "#f7bb16", "#e77b37", "#d23f67")

sqafSeverityList <- data.frame(
  # Remember to set the levels as shown in this guide.  This is what enforces the order of the elements
  # https://stackoverflow.com/questions/31638771/r-reorder-levels-of-a-factor-alphabetically-but-one
  ID = factor(sqafSeverityList),
  # Titles
  Title = factor(sqafTitles, levels=sqafTitles),
  # The keys
  Key = factor(sqafKeys, levels=sqafKeys), 
  
  # Pretty legend colours
  Legend = sqafColours
)
sqafSeverityList$ID <- as.integer(sqafSeverityList$ID)

# The list of countries of asylum for which we want to test
# We ignore the "Non-submissions" from these: "AND", "SMA", "VAT"
sqafCountryList <- c("ABW","AFG","AIA","ALB","ALG","ANG","ANT","ARE","ARG", 
                 "ARM","AUL","AUS","AZE","BAH","BAR","BDI","BEL","BEN","BER",
                 "BES","BGD","BHS","BKF","BLR","BOL","BOT","BRA","BRU","BSN",
                 "BUL","BVI","BZE","CAM","CAN","CAR","CAY","CHD","CHI","CHL",
                 "CMR","COB","COD","COL","COS","CUB","CUW","CVI","CYP","CZE",
                 "DEN","DJB","DMA","DOM","ECU","ERT","EST","ETH","FIJ","FIN",
                 "FRA","FSM","GAB","GAM","GBR","GEO","GFR","GHA","GNB","GRE",
                 "GRN","GUA","GUI","GUY","HAI","HKG","HON","HRV","HUN","ICE",
                 "ICO","IND","INS","IRE","IRN","IRQ","ISR","ITA","JAM","JOR",
                 "JPN","KAZ","KEN","KGZ","KOR","KOS","KUW","LAO","LBR","LBY",
                 "LCA","LEB","LES","LIE","LKA","LTU","LUX","LVA","MAC","MAD",
                 "MAU","MCD","MCO","MDA","MEX","MLI","MLS","MLW","MNE","MNG",
                 "MOR","MOZ","MSR","MTA","MTS","MYA","NAM","NEP","NET","NGR",
                 "NIC","NIG","NOR","NRU","NZL","OMN","PAK","PAN","PAR","PER",
                 "PHI","PLW","PNG","POL","POR","QAT","ROM","RSA","RUS","RWA",
                 "SAL","SAU","SEN","SEY","SIN","SLE","SOL","SOM","SPA",
                 "SRB","SRV","SSD","STK","SUD","SUR","SVK","SVN","SWA","SWE",
                 "SWI","SXM","SYR","TAN","TCI","THA","TJK","TKM","TMP","TOG",
                 "TON","TRT","TUN","TUR","UAE","UGA","UKR","URU","USA","UZB",
                 "VAN","VCT","VEN","WES","YEM","ZAM","ZIM") 


#-------------------------------------------------------------------------------------------------------------------------
# Gets the list of datasets in a string that is used to evaluate the specific logical checks
GetDatasetArgumentList <- function(dataToken) {
  
  # Generate the right arguments
  argStr <- ""
  
  # Here differentiate demographics from returns....
  if (dataToken == "Demographics") {
    argStr <- "dataDemographics"
    
  } else if (dataToken == "Refugees") {
    argStr <- "dataREFROC, sqafIsASR"
    
  } else if (dataToken == "Returns") {
    # returns need the REFROC, returns and a flag as to whether or not this is the ASR
    argStr <- "dataREFROC, dataRET, sqafIsASR"
    
  } else if (dataToken == "DemoPoCs") {
    argStr <- "dataDemographics, gr, dataREFROC, dataRET, sqafIsASR"
    
  } else if (dataToken == "Stateless") {
    argStr <- "dataSTAUDN, sqafIsASR"
    
  } else if (dataToken == "Host-Community") {
    argStr <- "dataHST, sqafIsASR"
    
  } else if (dataToken == "Asylum-seekers") {
    argStr <- "dataRSD"
    
  } else if (dataToken == "All-Asylum-seekers") {
    argStr <- "dAllRSDFull, dataRSD, sqafYear, sqafIsASR"
    
  } else if (dataToken == "AllBasis") {    
    
    argStr <- "dataREF, dataROC, dataRET, dataIDP, dataSTAUDN, dataOOC, dataVDA, dataHST, sqafIsASR"
    
  } else if (dataToken == "OOC") {    
    
    argStr <- "dataOOC"
    
  } else if (dataToken == "VDA") {    
    
    argStr <- "dataVDA"    
    
  } else if (dataToken == "InternationallyForciblyDisplaced") {
    argStr <- "gr"
    
  } else if (dataToken == "All") {
    argStr <- "gr"

  } else if (dataToken == "All-Historic-End") {
    argStr <- "gr, grPrevYear"
    
  } else if (dataToken == "All-Historic-Start") {
    argStr <- "grStartYear, grPrevYear"
    
  }
  
  
  returnVal <- argStr
}


#-------------------------------------------------------------------------------------------------------------------------
# Append an item to the global SQAF list
AppendSQAFItem <- 
  function(sqafID, msgCheck, perc, threshold, year, populationType, origin, asylum  ) {
    
    #--1-- check for NULLs and reassign as NA
    sqafID <- ifelse(IsNull(sqafID), NA, sqafID)
    msgCheck <- ifelse(IsNull(msgCheck), NA, msgCheck)

    populationType <- ifelse(IsNull(populationType), NA, populationType)
    origin <- ifelse(IsNull(origin), NA, origin)
    asylum <- ifelse(IsNull(asylum), NA, asylum)
    
    
    #--2-- Assign the new info to the global list
    sqafList[nrow(sqafList) + 1,] <<- 
      #c
      list(as.character(sqafID), # ID
        as.character(msgCheck), # Additional Notes
        perc, # Percentage result
        threshold, # Severity
        year, # Year
        as.character(populationType), # Population type
        as.character(origin), # Origin
        as.character(asylum) # Asylum
      )
    

    
  }



#-------------------------------------------------------------------------------------------------------------------------
# Generate the threshold and messages
GenerateThresholdAndMessages <-
  function(sqafID, origin, asylum, populationType, perc, additionalMessage = "") {

    # Get the relevant sqaf check
    sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
        
    # 15-March-2021 - do not build this message as it is essentially redundant.  Just include the description of the check...
    # And the additional message (if present)
    msgCheck <- NA
    
    if(! is.na(additionalMessage) & length(additionalMessage) > 0 & additionalMessage != "") {
      msgCheck <- paste0(additionalMessage, ".")
    }
    
  
    # See where the result lies
    # 1: Looking good; 2: Could improve; 3: Should improve; 4: Must improve;
    threshold <- 0

    
    if( sqafCheck$Quality_Scale == "HighToLow" ) {
      
      # With the first category the choice is the threshold NOT being null AND the value being less than the threshold
      # With the last category the choice is the threshold being null OR the value being less than the threshold
      if (! IsNNN(sqafCheck$Threshold_4) & perc <= sqafCheck$Threshold_4) {
        threshold <- 4  

      } else if (! IsNNN(sqafCheck$Threshold_3) &  perc <= sqafCheck$Threshold_3) {
        threshold <- 3

      } else if (! IsNNN(sqafCheck$Threshold_2) &  perc <= sqafCheck$Threshold_2) {
        threshold <- 2

      } else if ( IsNNN(sqafCheck$Threshold_1) | perc <= sqafCheck$Threshold_1) {
        threshold <- 1

        # What about silly %'s > 100? Can handle by leaving the top level empty in the spreadsheet
      }        
      
    } else if ( sqafCheck$Quality_Scale == "LowToHigh" ) {

      
      if (! IsNNN(sqafCheck$Threshold_4) & perc >= sqafCheck$Threshold_4) {
        threshold <- 4  

      } else if (! IsNNN(sqafCheck$Threshold_3) &  perc >= sqafCheck$Threshold_3) {
        threshold <- 3

      } else if (! IsNNN(sqafCheck$Threshold_2) &  perc >= sqafCheck$Threshold_2) {
        threshold <- 2

      } else if ( IsNNN(sqafCheck$Threshold_1) | perc >= sqafCheck$Threshold_1) {
        threshold <- 1
      }
      
    }
    

    # Then we want to return the threshold, msgCheck and msgAction
    returnValue <- list("threshold" = threshold, "msgCheck" = msgCheck)
  }


#-------------------------------------------------------------------------------------------------------------------------
# Modifies the gr (Global report data) to make it compatible with the format needed for these checks
PreparePopulationData <-
  function(gr, doRemoveNATRST=TRUE, doRemoveRETRDP=FALSE) {
    
    #--1-- Rename some key fields in the gr dataset to support the use of the CompareDemographicsAndPopulationTotals method
    gr$PT <- gr$populationType
    gr$origin <- gr$CoO
    gr$asylum <- gr$CoA
    gr$Year <- sqafYear # from the global variables
    
    # and remove the irrelevant columns
    gr$CoA <- gr$CoO <- gr$ISO3CoO <- gr$ISO3CoA <- gr$populationType <- NULL
    
    # Strip out the IDMC and UNRWA data, totals and naturalisation and resettlement
    #popTypesToRemove <- c("Total", "IDMC", "UNRWA", "NAT", "RST")
    popTypesToRemove <- c("Total", "IDMC", "UNRWA")
    print(paste0("Preparing the population data and removing population types that are not relevant: ", paste0(popTypesToRemove, collapse=", ")))
    gr <- gr %>% filter(! PT %in% popTypesToRemove)
    
    if ( doRemoveNATRST) {
      print("Also removing NAT and RST flow data")
      gr <- gr %>% filter(! PT %in% c("NAT", "RST"))
    }
    
    if ( doRemoveRETRDP) {
      print("Also removing RET and RDP flow data")
      gr <- gr %>% filter(! PT %in% c("RDP", "RET"))
    }
    
    returnValue <- gr
  }




#-------------------------------------------------------------------------------------------------------------------------
# The rules should be a data frame with the validate structure (name, description and rule)
# These rules are 0/1 i.e. they either pass or fail
# Placeholder columns are columns whose values will be included as an additional column
RunValidationChecks <-
  function(sqafID, data, rules, includeOriginInMessage=FALSE, placeHolderColName=NA, columnNameToIncludeInMessage=NA) {
    
    
    #--0-- Append the data point summaries
    AppendDataPointRows(sqafID, data, nrow(rules))
    
    #--1-- Deal wih the additional column if it exists
    if (! IsNN(placeHolderColName)) {
      print(paste0("Trying to add the placeholder column (", placeHolderColName, ")"))
      # Create the col if it does not exist
      if (! DataFrameColumnExists(sqafList, placeHolderColName)) {
        print("Adding the placeholder column")
        sqafList[[placeHolderColName]] <<- NA
      }
    }
    
    
    #--2-- Iterate through the rules and run each on the given dataset
    for( i in 1 : nrow(rules)) {
      
      ruleListRow <- rules[i,]
      
      print(paste0("Running test '", ruleListRow$name, "' on ", nrow(data), " rows"))      
      
      if(nrow(data) > 0) {
        
        #--1-- Then test the data using the validate framework
        dodgyList <- violating(data, validator(.data = ruleListRow))
        
        print(paste("Found ", nrow(dodgyList), "records violating the test"))
        
        if (nrow(dodgyList) > 0) {
          cooExists <- DataFrameColumnExists(dodgyList,"origin")
          coaExists <- DataFrameColumnExists(dodgyList,"asylum")
          popTypeExists <- DataFrameColumnExists(dodgyList,"PT")
        
        
          #--2-- Loop through them and identify any we need to record as messages
          for( j in 1 : nrow(dodgyList)) {
            
            # lets try to get the origin, asylum and PT (some of which may not be present)
            coo <- ifelse( cooExists, dodgyList$origin[j], NA)
            coa <- ifelse( coaExists, dodgyList$asylum[j], NA)
            popType <- ifelse( popTypeExists, dodgyList$PT[j], NA)
            
            # Include the description here which is useful especially when there are multiple issues
            desc <- ruleListRow$description
            
            # Add the additional message if it exists - we need the origin to be there if the origin is invalid as it wont then
            # appear in the IDs.
            if(includeOriginInMessage == TRUE){
              desc <- paste0( desc, " (Origin: ", coo, ")" )
            }
            
            # Include the value from a specific column if given
            additionalMessage <- ""
            if (! IsNN(columnNameToIncludeInMessage)) {
              desc <- paste0( desc, " (Found: ", dodgyList[[columnNameToIncludeInMessage]][j], ")" )
            }
            
            # Do the usual generating the threshold and messages
            output <- GenerateThresholdAndMessages(
              sqafID,
              coo, 
              coa,
              popType,
              0,
              desc # Include the description here which is useful especially when there are multiple issues
              
            )
            
            # Check whether the PT, origin and asylum are set - should be handled in AppendSQAFItem?
            
            # Assign the new info to the global list
            AppendSQAFItem(sqafID, output$msgCheck, #output$msgAction, 
                           0, output$threshold, 
                           sqafYear, popType, coo, coa)
            
            
            # If there is a placeholder column defined, then add this with the relevant value to the SQAF list
            if (! IsNN(placeHolderColName)) {
              # Then update the last one with the given value
              sqafList[[placeHolderColName]][nrow(sqafList)] <<- dodgyList[[placeHolderColName]][j]
            }
            
            
            # And update the number of data points
            UpdateDataPointRow(sqafID, coa, output$threshold, 1)
            
          }
        }
        
      }
    }
  }

#-------------------------------------------------------------------------------------------------------------------------
# This counts the percentage that a given enumerator column is of the given total
RunRuleBasedOnPercentageByAsylumAndPT <- function(sqafID, data, countColName, additionalWarningNote="") {
  
  #--0-- Test for prerequisites
  if ( DataFrameColumnExists(data, "Enumerator") == FALSE) {
#  if(IsNNN(data$Enumerator)) {
    print("!!! WARNING - for the function RunRuleBasedOnPercentageByAsylumAndPT the Enumerator column needs to be present. !!!")
  }

    
  #--1a-- so this is a big group by and we calculate the total....
  dataSummary <- data %>% 
    group_by(asylum, PT) %>%
    summarise(
      Enumerator = sum(Enumerator),        
      Total = sum(!! as.name(countColName))
    )
  
  
  #--1b-- Calculate the percentage
  dataSummary <- dataSummary %>% mutate(
    Perc = Enumerator / Total * 100
  )
  
  
  #--2-- Append the data point summaries
  AppendDataPointRows(sqafID, dataSummary, 1)
  
  
  numViolating = 0
  
  #--3-- Iterate through the data summary and generate the output  
  for( i in 1 : nrow(dataSummary)) {
    
    output <- GenerateThresholdAndMessages(
      sqafID,
      NULL, 
      dataSummary$asylum[i],
      dataSummary$PT[i],
      dataSummary$Perc[i]
    )
    
    # Assign the additional warning note if it has been set and the severity threshold warrants it.
    if ( output$threshold >= 2 & ! IsNNN(additionalWarningNote) & additionalWarningNote != "") {
      output$msgCheck <- additionalWarningNote
    }
    
    # Assign the new info to the global list
    AppendSQAFItem(sqafID, output$msgCheck, 
                   dataSummary$Perc[i], output$threshold, 
                   sqafYear, dataSummary$PT[i], NA, dataSummary$asylum[i])
    
    
    # And update the number of data points
    UpdateDataPointRow(sqafID, dataSummary$asylum[i], output$threshold, 1)
    
    
    if (output$threshold >= 2) {
      numViolating <- numViolating + 1
    }
    
  }
  
  print(paste0("Found ", numViolating, " cases (countries of asylum and population type) violating the test"))
  
  
}



#-------------------------------------------------------------------------------------------------------------------------
# Automatically calculated the % occurrence of a particular categorical value by country of asylum
# Optionally also includes the option to additionally filter by PT (populationType)
RunCategoricalRuleBasedOnPercentage <- function(sqafID, data, doGroupByPT, countColName, filterColName, filterValue, additionalWarningNote="") {
  
  
  #--1-- Filter out the rows with zero as total - these are not relevant to this check
  data <- data %>% filter(!! as.name(countColName) > 0)
  
  listToGroupBy <- c("asylum")
  listToJoinBy <- c("asylum"="asylum")
  if(doGroupByPT) {
    listToGroupBy <- c(listToGroupBy, "PT")
    listToJoinBy <- c(listToJoinBy, "PT"="PT")
  }
  listToGroupByExtended <- c(listToGroupBy, filterColName)

  
  #--2a-- Calculate the total by asylum, PT and urbanRural/accommodation type etc...
  dataSummary <- data %>% 
    group_by(across(all_of(listToGroupByExtended))) %>%
    summarise(        
      Enumerator = sum(!! as.name(countColName))
    )
  
#View(dataSummary)  
  
  #--2b-- Calculate the total by asylum, PT
  dataSummary2a <- data %>% 
    group_by(across(all_of(listToGroupBy))) %>%
    summarise(        
      Total = sum(!! as.name(countColName))
    )
  
#View(dataSummary2a)
  
  #--2c-- Produce the summarised and filtered data
  dataSummary2b <- dataSummary %>% 
    filter(!! as.name(filterColName) == filterValue)
  
  if(doGroupByPT) {
    dataSummary2b <- dataSummary2b %>% select(asylum, PT, Enumerator)
  } else {
    dataSummary2b <- dataSummary2b %>% select(asylum, Enumerator)
  }
  
#View(dataSummary2b)  
  
  
  #--2d-- Then summarise by asylum and PT and calculate the % for each where the urbanRural is V, accommodationType == U etc
  # then group by asylum and join to a filtered version with just STA
  dataSummary3 <- left_join( dataSummary2a, dataSummary2b, by=listToJoinBy)
  
  #--2e-- Remove the na's and produce the percentage
  dataSummary3[is.na(dataSummary3)] <- 0
  dataSummary3$Perc <- dataSummary3$Enumerator / dataSummary3$Total * 100
  
#View(dataSummary3)  
  
  
  #--3-- Append the data point summaries
  AppendDataPointRows(sqafID, dataSummary3, 1)
  
  
  numViolating = 0
  
  #--4-- Iterate through the data summary and generate the output
  for( i in 1 : nrow(dataSummary3)) {
    
    # Quick switch to ensure the pt is OK (it should no be present if we are not grouping by it)
    pt <- NA
    
    if(doGroupByPT) {
      pt <- dataSummary3$PT[i]
    }
    
    
    output <- GenerateThresholdAndMessages(
      sqafID,
      NULL, 
      dataSummary3$asylum[i],
      pt,
      dataSummary3$Perc[i]
    )
    
    # Assign the additional warning note if it has been set and the severity threshold warrants it.
    if ( output$threshold >= 2 & ! IsNNN(additionalWarningNote) & additionalWarningNote != "") {
      output$msgCheck <- additionalWarningNote
    }
    
    
    # Assign the new info to the global list
    AppendSQAFItem(sqafID, output$msgCheck, 
                   dataSummary3$Perc[i], output$threshold, 
                   sqafYear, pt, NA, dataSummary3$asylum[i])
    
    # And update the number of data points
    UpdateDataPointRow(sqafID, dataSummary3$asylum[i], output$threshold, 1)
    
    
    if (output$threshold >= 2) {
      numViolating <- numViolating + 1
    }
  }
  
  byText <- "countries of asylum"
  if(doGroupByPT) {
    byText <- paste0(byText, " and population type")  
  } 
  
  print(paste0("Found ", numViolating, " cases (", byText, ") violating the test"))
  
}



#-------------------------------------------------------------------------------------------------------------------------
# 0.1 MISSING - Checks that data has been received - i.e. no countries are missing
CheckDataReceivedMissing <-
  function(sqafID, gr) {
    
    returnValue <- CheckDataReceived(sqafID, gr, TRUE)    
  }

#-------------------------------------------------------------------------------------------------------------------------
# 0.2 UNEXPECTED - Checks whether data has been received from unexpected countries
CheckDataReceivedUnexpected <-
  function(sqafID, gr) {
    
    returnValue <- CheckDataReceived(sqafID, gr, FALSE)
  }


#-------------------------------------------------------------------------------------------------------------------------
# Runs the checks for missing or unexpected data
CheckDataReceived <-
  function(sqafID, gr, doCheckMissing) {
    
    currentNumRows <- nrow(sqafList)

    
    #--1-- Append the data point summaries (we need to package as data frame)
    AppendDataPointRows(sqafID, data.frame(asylum = sqafCountryList), 1)
    
    
    #--2-- Strip out the IDMC and UNRWA data
    gr <- gr %>% filter(! populationType %in% c("Total", "IDMC", "UNRWA"))
    
    # get the unique list from the gr dataset
    coaList <- unique(gr$CoA)
    
    #--3-- Test - No need to do an intersect, but lets include it anyway:
    print(paste0("Found ", length(intersect(coaList, sqafCountryList)), " countries of asylum that have submitted data."))

    
    #--4-- Identify any dodgy data using setDiff
    numViolating <- 0
    
    dodgyData <- data.frame()
    additionalMessage <- ""
    
    if ( doCheckMissing ) {
      
      dodgyData <- setdiff(sqafCountryList, coaList)
      print(paste0("Found ", length(dodgyData), " countries of asylum that have MISSING data submissions."))

      additionalMessage <- paste0("No data has yet been submitted")
      
    } else {
      
      dodgyData <- setdiff(coaList, sqafCountryList)
      print(paste0("Found ", length(dodgyData), " countries of asylum that have UNEXPECTED data submissions."))
      
      additionalMessage <- paste0("Data has been submitted unexpectedly")
      
    }
    
    
    #--5-- Loop through the missing or unexpected data submissions
    if( length(dodgyData) > 0 ) {
      for( i in 1 : length(dodgyData)) {
        
        
        output <- GenerateThresholdAndMessages(
          sqafID,
          NA, 
          dodgyData[i],
          NA,
          0,
          additionalMessage
        )
        
        # Assign the new info to the global list
        AppendSQAFItem(sqafID, output$msgCheck, 
                       0, output$threshold, 
                       sqafYear, NA, NA, dodgyData[i])
        
        
        # And update the number of data points
        UpdateDataPointRow(sqafID, dodgyData[i], output$threshold, 1)
        
        if (output$threshold >= 2) {
          numViolating <- numViolating + 1
        }
        
      }    
    }          

    
    print(paste0("Found ", numViolating, " countres violating the test"))
    
    
    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success
    
  }


#-------------------------------------------------------------------------------------------------------------------------
# 1.1 Checks the demographic coverage by Country of asylum, and Population type
# Returns a data frame including the asylum codes, as well as two columns specifying the %coverage for sex and sex+age
CheckDemographicCoverageAgeSex <-
  function(sqafID, demoData) {
    
    success <- CheckDemographicCoverage(sqafID, demoData, TRUE)    
    
    # Then iterate through the data summary and create the messages given the thresholds...
    
    returnValue <- success
    
  }
#-------------------------------------------------------------------------------------------------------------------------
# 1.2 
CheckDemographicCoverageSex <-
  function(sqafID, demoData) {
    
    success <- CheckDemographicCoverage(sqafID, demoData, FALSE)    
    
    returnValue <- success
  }
#-------------------------------------------------------------------------------------------------------------------------
# returns true if the process ran successfully
CheckDemographicCoverage <-
  function(sqafID, demoData, doAgeAndSex) {
    
    #--0--
    currentNumRows <- nrow(sqafList)

    #--1-- Get the relevant sqaf check
    sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
    
        
    # Filter out the rows with zero as total - these are not relevant to this check
    demoData <- demoData %>% filter(total > 0)
    
    if ( doAgeAndSex ) {
      #--AgeAndSex-- Add in two columns that calculate the sexAndAge and the Age
      # Note that this assumes that 18_59 == 18-24, 24-49 and 50-59 for the detailed breakdown
      demoData <- demoData %>% mutate(
        Enumerator = totalFemale_0_4 + totalFemale_5_11 + totalFemale_12_17 + totalFemale_18_59 + totalFemale_60 + 
          totalMale_0_4 + totalMale_5_11 + totalMale_12_17 + totalMale_18_59 + totalMale_60)
    
    } else {
      #--Sex-- Calculate where only sex data is available
      # This would be subset of where both and and sex is available, where age_and_sex=0 & (m_total>0 | f_total>0)
      demoData <- demoData %>% mutate (
        Enumerator = totalFemaleTotal + totalMaleTotal)    
    }

#View(demoData)    


    # Then run the rule based on the % occurence of this value (population occurrence rather than rowwise)    
    RunRuleBasedOnPercentageByAsylumAndPT(sqafID, demoData, "total", sqafCheck$Explanation)

    
    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success
    
  }



#-------------------------------------------------------------------------------------------------------------------------
# 1.3  So the logical test here is that a given CoA has more than a given number of sub-locations
CheckSubNationalCoverage <-
  function(sqafID, demoData) {
    
    #--0a-- Append the data point summaries
    AppendDataPointRows(sqafID, demoData, 1)
  
    
    #--0b-- Get the current number of rows
    currentNumRows <- nrow(sqafList)
    
    #--1-- Get the relevant sqaf check
    sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
    

    # Filter out the rows with zero as total - these are not relevant to this check
    demoData <- demoData %>% filter(total > 0)
    
    
    # Summarise the demographics to only include the location and the country of asylum (and of course the population type)
    demoData <- SummariseDemographics(demoData, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
    
    
    # We then need to summarise further to just include the totals, CoA and the location
    # so this is a big group by and we calculate the total....
    dataSummary <- demoData %>% 
      group_by(asylum, location) %>%
      summarise(
        Total = sum(Total)
      )
    #View(dataSummary)    

    dataSummary <- dataSummary %>% 
      group_by(asylum) %>%
      summarise(
        NumLocations = n()
      )
    

    # Produce the output
    for( i in 1 : nrow(dataSummary)) {
      
      output <- GenerateThresholdAndMessages(
        sqafID,
        NULL, 
        dataSummary$asylum[i],
        NULL,
        dataSummary$NumLocations[i]
      )
      
      # Assign the additional warning note if it has been set and the severity threshold warrants it.
      if ( output$threshold >= 2) {
        output$msgCheck <- sqafCheck$Explanation
      }
      
      
      # Assign the new info to the global list
      AppendSQAFItem(sqafID, output$msgCheck, #output$msgAction, 
                     dataSummary$NumLocations[i], output$threshold, 
                     sqafYear, NA, NA, dataSummary$asylum[i])
      
      # And update the number of data points
      UpdateDataPointRow(sqafID, dataSummary$asylum[i], output$threshold, 1)
      
    }
    
    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success
    
  }


#-------------------------------------------------------------------------------------------------------------------------
# 1.4
CheckDemographicAggregationType <-
  function(sqafID, dd) {
    
    #--0-- Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    
    
    # These are the ones used by HEXIS, and they are converted to the ones below which are used for the validation
    #validAggregationTypes <- c("Detailed", "M/F and 18-59", "M/F", "Total")
    validAggregationTypes <- c("Default", "18_59", "M_F", "Total")
    
    #stubAction <- "The aggregation type can be one of four options and the data in each row should conform to the given aggregation type."
    
    # Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    
    # We need an additional column to store the results of this test
    #sqafList$Index <- 0
    
    
    #--1-- We use the validate package to assess this set of rules, so the first step is to build our rules
    
    # Invalid aggregation types (including blank ones) - this was the wrong way round logically previously; now corrected.
    ruleList <- data.frame( 
      name = "Empty or invalid aggregation type",
      description = "Empty or invalid aggregation type", 
      rule = paste0("AggregationType != '' & AggregationType %in% c(\"",
                    paste(validAggregationTypes, collapse="\",\"", sep=""), "\")")
      
    )
    
    #"Default", # Detailed
    r1 <- c( validAggregationTypes[1], 
             "Detailed adult age cohorts - the data provided does not match the specified aggregation type",
             
             "total==(totalFemaleTotal + totalMaleTotal) &
             
             totalFemaleTotal==(totalFemale_0_4 + totalFemale_5_11 + totalFemale_12_17 + 
             totalFemale_18_24 + totalFemale_25_49 + totalFemale_50_59 + totalFemale_60 + Female_Unknown) &
             totalMaleTotal==(totalMale_0_4 + totalMale_5_11 + totalMale_12_17 + 
             totalMale_18_24 + totalMale_25_49 + totalMale_50_59 + totalMale_60 + Male_Unknown)"
             
    )
    ruleList <- rbind(ruleList, r1)
    
    
    # M/F and 18-59
    r2 <- c( validAggregationTypes[2], # "18_59", # "M/F and 18-59"
             "Just the 18-59 adult age cohort - the data provided does not match the specified aggregation type",
             
             "total==(totalFemaleTotal + totalMaleTotal) &
             
             totalFemale_18_24 == 0 & totalFemale_25_49 == 0 & totalFemale_50_59 == 0 &
             totalMale_18_24 == 0 & totalMale_25_49 == 0 & totalMale_50_59 == 0 &
             
             (totalFemale_18_59 > 0 | totalMale_18_59 > 0) &
             
             totalFemaleTotal==(totalFemale_0_4 + totalFemale_5_11 + totalFemale_12_17 + 
             totalFemale_18_59 + totalFemale_60 + Female_Unknown) &  
             
             totalMaleTotal==(totalMale_0_4 + totalMale_5_11 + totalMale_12_17 + 
             totalMale_18_59 + totalMale_60 + Male_Unknown)"
             
    )
    ruleList <- rbind(ruleList, r2)
    
    # M/F    
    r3 <- c( validAggregationTypes[3], # "M_F", #"M/F"
             "Just disaggregation by sex - The data provided does not match the specified aggregation type (check that the totals are also included in the male and female unknown age cohort columns)", 
             
             "total==(totalFemaleTotal + totalMaleTotal) &
             
             totalFemaleTotal == Female_Unknown &
             totalMaleTotal == Male_Unknown &
             
             totalFemale_0_4 == 0 & totalFemale_5_11 == 0 & totalFemale_12_17 == 0 &
             totalFemale_18_24 == 0 & totalFemale_25_49 == 0 & totalFemale_50_59 == 0 & 
             totalFemale_18_59 == 0 & totalFemale_60 == 0 &
             
             totalMale_0_4 == 0 & totalMale_5_11 == 0 & totalMale_12_17 == 0 &
             totalMale_18_24 == 0 & totalMale_25_49 == 0 & totalMale_50_59 == 0 &
             totalMale_18_59 == 0 & totalMale_60 == 0"
    )
    ruleList <- rbind(ruleList, r3)
    
    # Total
    r4 <- c( validAggregationTypes[4], #"Total", 
             "Aggregation type of Total specified, but some age/sex cohorts have been populated",
             
             # AggregationType == 'Total' &
             "total >= 0 & totalFemaleTotal == 0 & totalMaleTotal == 0 &
             
             totalFemaleTotal == 0 & Female_Unknown == 0 &
             totalMaleTotal == 0 & Male_Unknown == 0 &
             
             totalFemale_0_4 == 0 & totalFemale_5_11 == 0 & totalFemale_12_17 == 0 &
             totalFemale_18_24 == 0 & totalFemale_25_49 == 0 & totalFemale_50_59 == 0 & 
             totalFemale_18_59 == 0 & totalFemale_60 == 0 &
             
             totalMale_0_4 == 0 & totalMale_5_11 == 0 & totalMale_12_17 == 0 &
             totalMale_18_24 == 0 & totalMale_25_49 == 0 & totalMale_50_59 == 0 &
             totalMale_18_59 == 0 & totalMale_60 == 0             
             
             "
    )
    ruleList <- rbind(ruleList, r4)
    
    
    #--2-- Now iterate through the rules and filter the data, then confront only the filtered data
    for( i in 1 : nrow(ruleList)) {
      
      ruleListRow <- as.data.frame( ruleList[i,] )
      
      # Filter the data as long as the given name is a valid aggregation type (otherwise just test the full cube)
      if ( ruleListRow$name %in% validAggregationTypes ) {
        dataSubSet <- dd %>% filter(AggregationType == ruleListRow$name)
      } else {
        dataSubSet <- dd
      }
      
      # Then run this test and log the results
      # Note that we want to include the index in the sqaf outputs below to make it possible to link between them
      RunValidationChecks(sqafID, dataSubSet, ruleListRow, placeHolderColName = "Index")
      
      
    }
    
    #--3-- To improve the message, we need to now go through all the 1.4's in the sqafList and append the actual aggregation type to the error
    # Note that the info provided is not unique - i.e. there could be multiple rows for an origin, asylum and PT... but the number
    # of rows should be consistent, so we should be able to append
    list14 <- sqafList %>% filter(ID == sqafID )
    
    if(nrow(list14) > 0) {
      print(paste0("Updating the notes for  ", nrow(list14), " logical checks." ))        
      Sys.sleep(3)
            
      
      #--3a-- Then append the aggregation type to the data
      origAggType <- "AggTypeOriginal"
      dd <- AppendAggregationType(dd, TRUE, origAggType)
      
      #--3b-- HEXIFY the aggregation type to make it relevant to PopData
      dd <- ConvertAggregationType(dd, FALSE, origAggType)
      
#View(dd)      
      #--3c-- Loop through them and update the notes
      for( i in 1 : nrow(list14)) {
        
        # Store the variables
        index <- list14$Index[i]
        ori <- list14$Origin[i]
        asy <- list14$Asylum[i]
        pt <- list14$PopulationType[i]
        note <- list14$Notes[i]

        # Give a process statement
        #print(paste0("Processing ", index, " ", ori, " ", asy, " ", pt ))        
        
        # Extract the actual aggregation type from the dd using the index
        # Note that we want the HEXIS / PopData type of aggregation not the UNHCR one.
        actualAggType <- dd$typeOfAggregation[dd$Index == index]
        
        # And build the notes field
        note <- paste0( note, " The data provided has an actual aggregation type of '", actualAggType, "'")
        
        # Then overwrite the relevant notes
        sqafList$Notes[sqafList$Index == index] <<- note
      }
    }
    
    
    #--4-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success  
    
  }




#-------------------------------------------------------------------------------------------------------------------------
# 1.5
CheckDemographicAgeUnknown <- function(sqafID, dataDemo) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  #--1-- Get the relevant sqaf check
  sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
  
  #--2-- Filter out the rows with zero as total - these are not relevant to this check
  dataDemo <- dataDemo %>% filter(total > 0)
  
  #--3-- This also only makes sense for Default and 18_59 data
  dataDemo <- dataDemo %>% filter(AggregationType %in% c("Default", "18_59"))
  
  #--4-- Calculate the total male and female unknown, and the % of these of the total
  dataDemo <- dataDemo %>% mutate(
    Enumerator = Female_Unknown + Male_Unknown
  )
  
  
  #--5-- Then run the rule to calculate the percentage by asylum and PT
  RunRuleBasedOnPercentageByAsylumAndPT(sqafID, dataDemo, "total", sqafCheck$Explanation)
  
  
  #--6-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}



#-------------------------------------------------------------------------------------------------------------------------    
# 1.6  Demographic accommodation type unknown is too great
CheckDemographicAccommodationType <- function(sqafID, dataDemo) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  #--1-- Get the relevant sqaf check
  sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
  
  #--2-- This style of test where we want to look for the % of a specific variable value is now encapsulated in this function
  RunCategoricalRuleBasedOnPercentage(sqafID, dataDemo, TRUE, "total", "accommodationType", "U", sqafCheck$Explanation )
  
  
  #--3-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}



#-------------------------------------------------------------------------------------------------------------------------
# 1.7
CheckDemographicUrbanRural <- function(sqafID, dataDemo) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  #--1-- Get the relevant sqaf check
  sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
  
  
  #--2-- Run the test - essentially this calcs and tests how much of the total urbanRural is V by asylum and PT
  RunCategoricalRuleBasedOnPercentage(sqafID, dataDemo, TRUE, "total", "urbanRural", "V", sqafCheck$Explanation )
  
  
  #--3-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}




#-------------------------------------------------------------------------------------------------------------------------
# 2.1 
CheckRefugeeReturns <-
  function(sqafID, dataREFROC, dataRET, isASR) {
    
    
    currentNumRows <- nrow(sqafList)
    # The population type will always be returns...
    pt <- "RET"  
    
    #--1-- Extract the returns   
    dataReturns <- CompareRefugeeReturns(dataREFROC, dataRET, isASR )
    
    #--0a-- Append the data point summaries correctly by origin rather than asylum
    AppendDataPointRows(sqafID, dataReturns %>% mutate(asylum = origin), 1)
    
    
    # OK here we need to catch infinite percentages - and use a special 1,000,000 number so that they still appear as errors.
    dataReturns$Perc[is.infinite(dataReturns$Perc)] <- 1000000

#View(dataReturns)    

    #--2-- Loop through them and identify any we need to record as messages
    numViolating <- 0
    
    for( i in 1 : nrow(dataReturns)) {
      
      additionalMessage <- paste0("Comparison: ",
                                  dataReturns$populationType[i], " returns total ",
                                  PrettyNum(dataReturns$RETPopulation[i]), 
                                  " by country of origin (", dataReturns$origin[i],") versus ", 
                                  PrettyNum(dataReturns$REFROCPopulation[i]), 
                                  " by country of asylum (", dataReturns$asylum[i], "), i.e. a difference of ", 
                                  PrettyNum(dataReturns$Diff[i])
                                  )
      
      output <- GenerateThresholdAndMessages(
        sqafID,
        dataReturns$origin[i], 
        dataReturns$asylum[i],
        pt,
        dataReturns$Perc[i],
        additionalMessage
      )
      
      
      # Assign two versions of the new info to the global list - so that it is visible to both countries of origin and asylum
      # Typical submission by country of former asylum
      AppendSQAFItem(sqafID, output$msgCheck,
                     dataReturns$Perc[i], output$threshold, 
                     sqafYear, pt, dataReturns$origin[i], dataReturns$asylum[i])

      # And by country of origin
      AppendSQAFItem(sqafID, output$msgCheck,
                     dataReturns$Perc[i], output$threshold, 
                     sqafYear, pt, dataReturns$origin[i], dataReturns$origin[i])
      
      # And update the number of data points - note that this is by origin not asylum
      UpdateDataPointRow(sqafID, dataReturns$origin[i], output$threshold, 1)
      
      
      # Log the numbers
      if (output$threshold >= 2) {
        numViolating <- numViolating + 1
      }
      
    }
    

    print(paste0("Found ", numViolating, " rows of data (asylum-origin pairs) violating the test"))

    
    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success  
    
  }




#-------------------------------------------------------------------------------------------------------------------------
# 2.2
CheckDemographicTotals <- function(sqafID, dataDemo, gr, dataREFROC, dataRET, isASR) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)

  #--1-- Rename some key fields in the gr dataset to support the use of the CompareDemographicsAndPopulationTotals method
  gr <- PreparePopulationData(gr)

  #--2-- Extract the returns - !!IMPORTANT!!
  # For this comparison to be effective, we need the origin returns only, not the fancy maximum calculation.
  # hence we need to strip out the fancy Returns data, and add in our gucci data...
  dataReturns <- CompareRefugeeReturns(dataREFROC, dataRET, isASR )
  
  # Then simplify the dataReturns and merge it with the gr data
  dataReturns <- dataReturns %>% select(origin, asylum, populationType, RETPopulation, RETAssisted, IsDuplicate)
  dataReturns$PT <- "RET" # always RET!! dataReturns$populationType
  dataReturns$Year <- sqafYear
  dataReturns$TotalPopulation <- dataReturns$RETPopulation
  dataReturns$TotalAssisted <- dataReturns$RETAssisted
  
  # clear out all the redundant columns  
  dataReturns$populationType <- dataReturns$RETPopulation <- dataReturns$RETAssisted <- NULL
  
#View(dataReturns)
#View(gr)
  
  # Strip out the returns
  gr <- gr %>% filter(PT != "RET")
  # then add the original ones back in
  gr <- rbind(gr, dataReturns)
  
  
  #--3-- Use the comparison function to produce a data summary comparing the two
  dataSummary <- CompareDemographicsAndPopulationTotals(dataDemographics, gr)
  
  #--0a-- Append the data point summaries
  AppendDataPointRows(sqafID, dataSummary, 1)
  
  
  #--3-- Go through the data summary and flag all non-zero differences as errors...
  # for reference the column Total is from the demographic data; TotalPopulation is from the gr data
  
  numViolating <- 0
  
  for( i in 1 : nrow(dataSummary)) {
    
    row <- dataSummary[i,]
    
    #--3a-- If the row is non-zero, we have an issue
    if (row$Diff != 0) {
      
      #--3b-- Generate the additional message
      additionalMessage <- paste0("Comparison: ", 
                                  PrettyNum(row$Total), 
                                  " in the demographic table versus ", 
                                  PrettyNum(row$TotalPopulation), 
                                  " in the population data table, i.e. a difference of ", 
                                  PrettyNum(row$Diff))
      
      #--3c-- We need to absolutify the diff so that the error messages are generated correctly
      output <- GenerateThresholdAndMessages(
        sqafID,
        row$origin, 
        row$asylum,
        row$PT,
        abs(row$Diff),
        additionalMessage
      )
      
      
      #--3d-- Assign the new info to the global list
      AppendSQAFItem(sqafID, output$msgCheck,
                     row$Diff, output$threshold, 
                     sqafYear, row$PT, row$origin, row$asylum)
      
      # And update the number of data points - note that this is by origin not asylum
      UpdateDataPointRow(sqafID, row$asylum, output$threshold, 1)
      
      
      if (output$threshold >= 2) {
        numViolating <- numViolating + 1
      }      
    }
    
  }
  
  
  print(paste0("Found ", numViolating, " rows of data (by asylum, origin and population type) violating the test"))  
  

  #--4-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}



#-------------------------------------------------------------------------------------------------------------------------
# 2.3
CheckIDPDataIDMC <- function(sqafID, dataIDP, dataIDMC) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  print("!!! NOT YET IMPLEMENTED !!!")
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  #success <- nrow(sqafList) - currentNumRows > 0
  #returnValue <- success  
  returnValue <- TRUE
}



#-------------------------------------------------------------------------------------------------------------------------
# 3.1 - flag estimations for refugee data as possible issues
CheckRefugeeBasis <- function(sqafID, dataREF, isASR) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  
  if ( isASR == FALSE) {
    print( "Skipping this check as this data is only available in the annual statistics." )
  } else {

    # Basis should be one of R, V, E, C (should not be blank and estimates less desirable)
    ruleList <- data.frame( 
      name = "Estimated basis for refugee data",
      description = "Avoid estimates if possible for the refugee data", 
      rule = paste0("basis != 'E'")
      
    )
    
    
    RunValidationChecks(sqafID, dataREF, ruleList)
  
    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
  
  }
  
  returnValue <- success  
  
}




#-------------------------------------------------------------------------------------------------------------------------
# 4.1 Logical checks relating to UNHCR RSD procedures
CheckUNHCRRSDProcedures <-
  function(sqafID, dataRSD) {
    
    #--0-- Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    
    
    #--1-- Check 1 - Decisions for UNHCR RSD procedures MUST equal one of FI, AR, RA
    ruleList <- data.frame( 
      name = "UNHCR decisions should be one of FI, AR, RA",
      description = "Decisions for UNHCR RSD procedures MUST equal one of FI, AR, RA", 
      rule = paste0("ProcedureType %in% c('G', 'J') | 
                    (ProcedureType == 'U' & DecisionType %in% c('FI', 'AR', 'RA'))")
      )
    
    # Check 2 - No UNHCR recognitions are included under other
    r2 <- c( "All UNHCR recognitions are under convention/mandate", 
             "As all UNHCR recognitions are under convention/mandate, none should be recognised as others",
             
             "ProcedureType %in% c('G', 'J') | 
             (ProcedureType == 'U' & RecognizedOther == 0)"
    )
    ruleList <- rbind(ruleList, r2)    
    
    # Checks 3 and 4 - Applications/Decisions MUST equal persons (P) (Reason: UNHCR does not report mandate RSD in cases)
    
    r3 <- c( "All UNHCR application procedures related to persons not cases", 
             "For all UNHCR procedures, Applications MUST equal persons (P) (Reason: UNHCR does not report mandate RSD in cases)",
             
             "ProcedureType %in% c('G', 'J') | 
             (ProcedureType == 'U' & ApplicationDataType == 'P')"
    )
    ruleList <- rbind(ruleList, r3)    
    
    r4 <- c( "All UNHCR decision procedures related to persons not cases", 
             "For all UNHCR procedures, Decisions MUST equal persons (P) (Reason: UNHCR does not report mandate RSD in cases)",
             
             "ProcedureType %in% c('G', 'J') | 
             (ProcedureType == 'U' & DecisionDataType == 'P')"
    )
    ruleList <- rbind(ruleList, r4)    
    
    
    #--2-- Now iterate through the rules and filter the data, then confront only the filtered data
    RunValidationChecks(sqafID, dataRSD, ruleList)
    
    
    #--3-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success  
    
  }


#-------------------------------------------------------------------------------------------------------------------------
# 4.2 If a country in Africa recognizes individuals under "other" this is most likely a data entry error/misunderstanding.
CheckRSDAfrica <-
  function(sqafID, dataRSD) {
    
    #--0-- Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    
    # Link the data to the UNSD Regions and filter it to just Africa
    dataRSD <- left_join(dataRSD, t22 %>% select(UNHCR_Country_Code, UNSD_Region_Name), by=c("asylum"="UNHCR_Country_Code"))
    dataRSD <- dataRSD %>% filter(UNSD_Region_Name == "Africa")
    
    #--1-- Build the rule
    ruleList <- data.frame( 
      name = "All recognitions in Africa are under convention/mandate",
      description = "Almost all recognitions in Africa are convention/mandate", 
      rule = paste0("RecognizedOther == 0")
    )
    
    
    #--2-- Now iterate through the rules and filter the data, then confront only the filtered data
    RunValidationChecks(sqafID, dataRSD, ruleList)
    
    
    #--3-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success  
    
  }    


#-------------------------------------------------------------------------------------------------------------------------
# 4.3 Logical checks relating to UNHCR RSD procedures
CheckRSDOverTime <-
  function(sqafID, dataRSDHistoric, dataRSD, sqafYear, isASR) {
    
    # NOTE - assumption that the column names are consistent between the historic and the latest data
    
    #--0-- Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    
    # get the col name to count
    countColName = as.name(GetCountColumnName("ASY", isASR, FALSE))
    
    #--1-- First strip any mid-year data from the historic table
    dataRSDHistoric <- dataRSDHistoric %>% filter(Year < sqafYear)
    
    #--2-- produce a summary with the type of asylum application and decision, year and country of asylum and the population count
    dataSummary1 <- dataRSDHistoric %>%
      group_by(asylum, Year, ProcedureType) %>%
      summarise(
        Count = sum(!! countColName)
      )
    
    # produce a similar summary for the current year
    dataSummary2 <- dataRSD %>%
      group_by(asylum, ProcedureType) %>%
      summarise(
        Count = sum(!! countColName)
      )
    
    # and then produce a summary by CoA that is our base (using the current data which is what we are testing)
    dataSummary3 <- dataRSD %>%
      group_by(asylum) %>%
      summarise(
        Count = sum(!! countColName)
      )
    
    
    
    #--3a-- If a country reports type of RSD procedure Government one year and type of procedure as Joint the following year
    # So for this we need to extract those countries that have previously been G, and are J in the current year
    dataSummary3 <- left_join(dataSummary3, 
                              dataSummary1 %>% 
                                filter(ProcedureType == "G") %>% 
                                group_by(asylum) %>%
                                summarise(ProcedureType = min(ProcedureType)),
                              by=c("asylum"="asylum"))
    dataSummary3 <- dataSummary3 %>% mutate(HistoricG = ProcedureType, ProcedureType=NULL)
    
    dataSummary3 <- left_join(dataSummary3, 
                              dataSummary2 %>% 
                                filter(ProcedureType == "J") %>% 
                                group_by(asylum) %>%
                                summarise(ProcedureType = min(ProcedureType)),
                              by=c("asylum"="asylum"))
    dataSummary3 <- dataSummary3 %>% mutate(CurrentJ = ProcedureType, ProcedureType=NULL)
    
    
    #--3b-- If a country reports Government or Joint cases in the past and then reports only UNHCR cases
    # So for this we need to extract those countries that have previously been G or J, and are U in the current year
    dataSummary3 <- left_join(dataSummary3, 
                              dataSummary1 %>% 
                                filter(ProcedureType %in% c("G", "J")) %>% 
                                group_by(asylum) %>%
                                summarise(ProcedureType = min(ProcedureType)),
                              by=c("asylum"="asylum"))
    dataSummary3 <- dataSummary3 %>% mutate(HistoricJG = ProcedureType, ProcedureType=NULL)
    
    dataSummary3 <- left_join(dataSummary3, 
                              dataSummary2 %>% filter(ProcedureType == "U") %>% 
                                group_by(asylum) %>%
                                summarise(ProcedureType = min(ProcedureType)),
                              by=c("asylum"="asylum"))
    dataSummary3 <- dataSummary3 %>% mutate(CurrentU = ProcedureType, ProcedureType=NULL)
    
    # Set all the NAs to "" to make the following checks easier to write
    dataSummary3[is.na(dataSummary3)] <- ""
    
    #View(dataSummary3)    
    
    
    #--4-- Build the tests
    ruleList <- data.frame( 
      name = "RSD procedure type government -> joint",
      description = "Countries are highly unlikely to report their type of RSD procedure Government one year and change this to Joint the following year", 
      rule = paste0("HistoricG == '' | CurrentJ == ''")
    )
    
    r2 <- c( "RSD procedure type government, joint -> UNHCR", 
             "Countries are highly unlikely to report the source of cases as Government or Joint in the past and then change this to report only UNHCR cases.",
             "HistoricJG == '' | CurrentU == ''"
    )
    ruleList <- rbind(ruleList, r2)    
    
    
    #--5-- Run the tests
    RunValidationChecks(sqafID, dataSummary3, ruleList)
    
    
    #--6-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success  
    
  }    


#-------------------------------------------------------------------------------------------------------------------------
# 4.4 Check average case size
CheckAverageCaseSize <-
  function(sqafID, dataRSD) {
    
    #--0-- Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    

    #--1-- Build the rules
    ruleList <- data.frame( 
      name = "Applications reported as persons should always have an average case size of 0",
      description = "Applications reported as persons should always have an average case size of 0", 
      rule = "ApplicationDataType == 'C' | (ApplicationDataType == 'P' & ApplicationAveragePersonsPerCase == 0)"
    )
    
    
    # Check 2 - No UNHCR recognitions are included under other
    r2 <- c( "Decisions reported as persons should always have an average case size of 0", 
             "Decisions reported as persons should always have an average case size of 0",
             
             "DecisionDataType == 'C' | (DecisionDataType == 'P' & DecisionAveragePersonsPerCase == 0)"
    )
    ruleList <- rbind(ruleList, r2)      
    
    #--2-- Now iterate through the rules and filter the data, then confront only the filtered data
    RunValidationChecks(sqafID, dataRSD, ruleList)
    
    
    #--3-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success  
    
  }    


#-------------------------------------------------------------------------------------------------------------------------
# 6.1
CheckStatelessOrigin <- function(sqafID, dataSTA, isASR) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  #--1-- Ensure we have a PT in the stateless table!
  dataSTA$PT <- "STA"
  
  #--2-- Then calc the % that is STA or UKN for each country of asylum
  countColName = GetCountColumnName("STA", isASR, FALSE)
  
  #--3-- Standardise STA, UKN and VAR to all be STA
  dataSTA$origin[dataSTA$origin %in% c("STA", "UKN", "VAR")] <- "STA"
  
  
  #--4-- Then run the rule ... we can group by PT as we only have one pt, so it is simple....
  RunCategoricalRuleBasedOnPercentage(sqafID, dataSTA, TRUE, countColName, "origin", "STA",
    "If possible, the origin in the Stateless table should record the specific countries of former habitual residence for displaced STA (e.g. the Rohingya) rather than the generic STA/UKN/VAR.")


  #--5-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  

}



#-------------------------------------------------------------------------------------------------------------------------
# 6.2 - Displaced PAL and GAZ origins should not be recorded in the Stateless table
CheckStatelessDisplacedGAZ <- function(sqafID, dataSTA, isASR) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  #--1-- Ensure we have a PT in the stateless table!
  dataSTA$PT <- "STA"
  
  #--2-- Add the basis rule
  ruleList <- data.frame( 
    name = "Displaced PAL and GAZ origins should not be recorded in the Stateless table",
    description = "Displaced PAL and GAZ origins should not be recorded in the Stateless table", 
    rule = paste0("! origin %in% c('PAL', 'GAZ')")
    
  )
  
  #--3-- Run the rules
  RunValidationChecks(sqafID, dataSTA, ruleList)      

    
  #--4-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0

  returnValue <- success  

}

#-------------------------------------------------------------------------------------------------------------------------
# 7.1 - Host Community
CheckHostCommunity <- function(sqafID, dataHST, isASR) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  #--1-- Get the relevant sqaf check
  sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
  
  #--2-- Ensure we have a PT in the stateless table!
  dataHST$PT <- "HST"
  
  #--3-- Get the count and assisted column names
  countColName = GetCountColumnName("HST", isASR, FALSE)
  assistedColName = GetCountColumnName("HST", isASR, TRUE)
  
  #--4-- Add the rule
  ruleList <- data.frame( 
    name = sqafCheck$Description,
    description = sqafCheck$Explanation, 
    rule = paste0(countColName, " == ", assistedColName)
    
  )
  
  #--5-- Run the rule
  RunValidationChecks(sqafID, dataHST, ruleList)      
  
  
  #--6-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}


#-------------------------------------------------------------------------------------------------------------------------
# 8.1 and 8.2 - Compares the end of the previous year with the _end_ of the current year
CheckSignificantChangeBase <- function(sqafID, data2, data1, filterThreshold=1) {
  
  #--0a-- Append the data point summaries
  AppendDataPointRows(sqafID, data2, 1)
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Then do a full join whopper!
  dc <- full_join(data1 %>% 
                    mutate(T1=TotalPopulation) %>% 
                    select(origin, asylum, PT, T1), 
                  data2 %>%
                    mutate(T2=TotalPopulation) %>% 
                    select(origin, asylum, PT, T2), by=c("origin"="origin", "asylum"="asylum", "PT"="PT"))
  
  # convert the NAs to zeros (not ideal but seems to be an OK compromise for now)
  dc[is.na(dc)] <- 0
  
  # Then strip out the small numbers defined as < 1000 in at least one of the time periods
  if ( filterThreshold > 0) {
    dc <- dc %>% filter(T1 >= filterThreshold | T2 >= filterThreshold)
  }
  
  dc$Diff <- dc$T2 - dc$T1
  dc$Perc <- dc$Diff / dc$T1 * 100
  
  
  # Catch infinite percentages
  dc$Perc <- ifelse(is.infinite(dc$Perc), 1000000, dc$Perc)
  # clean it up
  dc$Perc <- as.integer(round(dc$Perc, 0))
  
#  View(dc)    
  
  # Produce the output
  print(paste0("Processing ", nrow(dc), " records..."))
  
  numViolating <- 0
  
  for( i in 1 : nrow(dc)) {
    
    
    #--3c-- We need to absolutify the diff so that the error messages are generated correctly
    output <- GenerateThresholdAndMessages(
      sqafID,
      dc$origin[i], 
      dc$asylum[i],
      dc$PT[i],
      abs(dc$Perc[i])
    )
    
    # Lets build the custom message here
    msg <- paste0( "When comparing between the two reference points, there is a difference of ", 
                   PrettyNum(dc$Diff[i])," (", dc$Perc[i],"%) i.e. previously ", 
                   PrettyNum(dc$T1[i])," and now ", 
                   PrettyNum(dc$T2[i]),"." )
    
    if (dc$PT[i] == "STA") {
      msg <- paste0( msg, " This could be due to the reclassification of the stateless data to use the origin from the former habitual residence.")
      # Ensure the lowest level of error message is generated
      output$threshold = 2
    }
    

    # Check there has been reporting for this PT and asylum
    reportedTotalByPTAndAsylum <- sum(data2$TotalPopulation[data2$asylum == dc$asylum[i] & data2$PT == dc$PT[i] ])
    
    # If the reportedTotal is zero then lets downgrade the error (as it's just due to lack of reporting)
    if ( reportedTotalByPTAndAsylum == 0) {
      output$threshold = 1
    }
    
    # If this is not just for information lets log it in the list
    if ( output$threshold >= 2) {
      
      
      # Assign the new info to the global list - in this instance ignore the "for information" checks
      AppendSQAFItem(sqafID, msg, 
                     dc$Perc[i], output$threshold, 
                     sqafYear, dc$PT[i], dc$origin[i], dc$asylum[i])
      
      
      
      numViolating <- numViolating + 1
    }
    
    
    # And ALWAYS update the number of data points
    UpdateDataPointRow(sqafID, dc$asylum[i], output$threshold, 1)
    
  }  
  
  print(paste0("Found ", numViolating, " (by countries of origin and asylum and population type) violating the test"))
  
  
  
  
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}

#-------------------------------------------------------------------------------------------------------------------------
# 8.1 - Compares the end of the previous year with the _end_ of the current year
CheckSignificantChange <- function(sqafID, dataLatest, dataPreviousYear) {

  # So essentially we have two big gr datacubes
  # So filter out irrelevant comparisons (Total, IDMC and UNRWA) - for the comparison with the previous year we can keep all the flow data
  dataLatest <- PreparePopulationData(dataLatest, FALSE, FALSE)
  dataPreviousYear <- PreparePopulationData(dataPreviousYear, FALSE, FALSE)
  
  
  returnValue <- CheckSignificantChangeBase(sqafID, dataLatest, dataPreviousYear, 1000)
  
}

#-------------------------------------------------------------------------------------------------------------------------
# 8.2 - Compares the end of the previous year with the _start_ of the current year
CheckStartEndChange <- function(sqafID, dataStartYear, dataPreviousYear) {
  

  # So filter out irrelevant comparisons (Total, IDMC and UNRWA) - for the comparison with the start year we have to remove all the flow data
  dataPreviousYear <- PreparePopulationData(dataPreviousYear, TRUE, TRUE)
  dataStartYear <- PreparePopulationData(dataStartYear, TRUE, TRUE)
  
  # We use the default threshold of 1 for this more sensitive check
  returnValue <- CheckSignificantChangeBase(sqafID, dataStartYear, dataPreviousYear)  
}



#-------------------------------------------------------------------------------------------------------------------------
# 9.1 - note that this does not exist for the RSD data
CheckGeneralMissingBasis <- function(sqafID, dataREF, dataROC, dataRET, dataIDP, dataSTA, dataOOC, dataVDA, dataHST, isASR) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Standardise the RET population type to use RET so we can identify it
  dataRET$populationType[dataRET$typeOfPopulation == "Refugee"] <- "RET"
  dataRET$populationType[dataRET$typeOfPopulation == "Refugee-like"] <- "RET"
  
  # And also make sure we have included the stateless too
  dataSTA$populationType <- "STA"
  
  datasets <- list(dataREF, dataROC, dataRET, dataIDP, dataSTA, dataOOC, dataVDA, dataHST)
  

  if ( isASR == FALSE) {
    print( "Skipping this check as this data is only available in the annual statistics." )
  } else {
    ##################################################################
    # Source should be one of U, G, V, N (should not be blank)
    # Basis should be one of R, V, E, C (should not be blank and estimates less desirable)
    
    # Add the basis
    ruleList <- data.frame( 
      name = "Missing basis",
      description = "The basis should be included for all data", 
      rule = paste0("basis != ''")
      
    )
    
    # Add the source
    r2 <- c( "Missing source", 
             "The source should be included for all data",
             "source != ''"
             
    )
    ruleList <- rbind(ruleList, r2)
    
    # Then iterate through each dataset and run these rules against them (we might need to double check each has a PT col)
    for( i in 1: length(datasets)) {

      # double square brackets as we are accessing a list here...
      d <- datasets[[ i ]]

      if (nrow(d) > 0) {
        d$PT <- d$populationType
        
        print(paste0("Testing ", d$PT[1], "..."))      
      
        RunValidationChecks(sqafID, d, ruleList)      
      }
    }
    

    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    
  }
  
  returnValue <- success  
  
  
}



#-------------------------------------------------------------------------------------------------------------------------
# 9.2  Refugee, asylum-seekers and VDA = the country of origin should not be the same as the country of asylum.
CheckGeneralDispacedOriginAsylum <- function(sqafID, dataPopulation) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)

  # Standardise the column names
  dataPopulation$PT <- dataPopulation$populationType  
  dataPopulation$origin <- dataPopulation$CoO
  dataPopulation$asylum <- dataPopulation$CoA
    
  # Filter the population data down to just forcibly displaced internationally (REF, ASY, VDA)
  dataPopulation <- dataPopulation %>% filter(PT %in% c("REF", "ROC", "ASY", "VDA"))
  
  # Add the rule
  ruleList <- data.frame( 
    name = "Origin should not be the same asylum",
    description = "For internationally forcibly displaced (REF, ROC, ASY, VDA), the country of origin should not be the same as the country of asylum", 
    rule = paste0("origin != asylum")
    
  )
  
  RunValidationChecks(sqafID, dataPopulation, ruleList)
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  

}
    

#-------------------------------------------------------------------------------------------------------------------------
# 9.3 and 9.4 VDA should no longer be included in the OOC table
CheckVDAInOOCBase <- function(sqafID, dataOOC) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)

  #--1-- Get the relevant sqaf check
  sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
  
  #--2-- Standardise the column names
  dataOOC$PT <- dataOOC$populationType  

  
  #--3-- Add the rule
  ruleList <- data.frame( 
    name = sqafCheck$Description,
    description = sqafCheck$Explanation, 
    rule = paste0("origin != 'VEN' | (origin == 'VEN' & asylum == 'VEN')")
    
  )
  
  RunValidationChecks(sqafID, dataOOC, ruleList)  
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}

#-------------------------------------------------------------------------------------------------------------------------
# 9.3 VDA should no longer be included in the OOC table
CheckVDAInOOCAmericas <- function(sqafID, dataOOC) {
  
  #--1-- Link the data to the UNSD Regions and filter it to just Americas
  dataOOC <- left_join(dataOOC, t22 %>% select(UNHCR_Country_Code, UNSD_Region_Name), by=c("asylum"="UNHCR_Country_Code"))
  dataOOC <- dataOOC %>% filter(UNSD_Region_Name == "Americas")
  
  #--2-- Run the checks
  returnValue <- CheckVDAInOOCBase(sqafID, dataOOC)  

}


#-------------------------------------------------------------------------------------------------------------------------
# 9.4
CheckVDAinOOCGlobal <- function(sqafID, dataOOC) {
  
  #--1-- Link the data to the UNSD Regions and filter it to just Americas
  dataOOC <- left_join(dataOOC, t22 %>% select(UNHCR_Country_Code, UNSD_Region_Name), by=c("asylum"="UNHCR_Country_Code"))
  dataOOC <- dataOOC %>% filter(UNSD_Region_Name != "Americas")
  
  #--2-- Run the checks
  returnValue <- CheckVDAInOOCBase(sqafID, dataOOC)  
  
}



#-------------------------------------------------------------------------------------------------------------------------
# 9.5 - Checks that the VDA category is only used in the Americas
CheckVDAGlobal <- function(sqafID, dataVDA) {
  
  #--0-- Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  #--1-- Get the relevant sqaf check
  sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
  
  #--2-- Standardise the column names
  dataVDA$PT <- dataVDA$populationType  
  
  #--3-- Link the data to the UNSD Regions and filter it to just Americas
  dataVDA <- left_join(dataVDA, t22 %>% select(UNHCR_Country_Code, UNSD_Region_Name), by=c("asylum"="UNHCR_Country_Code"))

  #View(dataVDA)
  
  #--4-- Add the rule
  ruleList <- data.frame( 
    name = sqafCheck$Description,
    description = sqafCheck$Explanation, 
    rule = paste0("UNSD_Region_Name == 'Americas'")
    
  )
  
  #--5-- Run the rule
  RunValidationChecks(sqafID, dataVDA, ruleList)  
  
  #--6-- Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success    
  
}




#-------------------------------------------------------------------------------------------------------------------------
# 9.6 Checks that all the country codes in the origin and asylum in gr are valid
CheckCountryCodes <- function(sqafID, gr) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Standardise the column names and strip out the IDMC and UNRWA data
  gr <- PreparePopulationData(gr)
  
  
  # get the source data with the definitive list of country codes
  countryList <- read_excel(paste0(rootOneDriveDirectory, "Reporting guidelines/Country_Codes.xlsx"), 
                        sheet=1, range = cell_cols("A:B"), n_max=200000)
  
#View(countryList)  
  
  # Get the full list of country codes from our official list
  # (we can do this in the same way for the missing country data.)
  # Add the rule
  ruleList <- data.frame( 
    name = "Invalid country of asylum",
    description = "Invalid country of asylum", 
    rule = paste0("asylum %in% c('", paste0(c(countryList$UNHCR_code), collapse= "','"), "')")
    
  )
  
  r2 <- c( "Invalid country of origin", 
           "Invalid country of origin",
           paste0("origin %in% c('", paste0(c(countryList$UNHCR_code), collapse= "','"), "')")
  )
  ruleList <- rbind(ruleList, r2)    
  
  
  # Then run the checks...
  RunValidationChecks(sqafID, gr, ruleList, TRUE)  
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  

}




#-------------------------------------------------------------------------------------------------------------------------
# 9.7 Checks for negative numbers in the population data
CheckNegativeNumbers <- function(sqafID, gr) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Standardise the column names and strip out the IDMC and UNRWA data
  gr <- PreparePopulationData(gr)
  
#View(gr)  
  
  # Add the rule
  ruleList <- data.frame( 
    name = "Negative numbers should not be present",
    description = "Negative numbers are invalid in the population statistics country of asylum", 
    rule = paste0("TotalPopulation >= 0")
    
  )
  

  # Then run the checks...
  RunValidationChecks(sqafID, gr, ruleList, columnNameToIncludeInMessage="TotalPopulation")  
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  
  
}



#-------------------------------------------------------------------------------------------------------------------------
# Appends the row with the total data points
AppendDataPointRows <- function(sqafID, data, numberOfSubRules) {
  
  # group by asylum and get the number of rows...
  dataSummary <- data %>% 
    group_by(asylum) %>%
    summarise(Count = n())


  for( i in 1: nrow(dataSummary)) {
    AppendDataPointRow( sqafID, dataSummary$asylum[i], numberOfSubRules, dataSummary$Count[i])
  }
  
}

#-------------------------------------------------------------------------------------------------------------------------
# Appends the row with the total data points
# Note that this will only include data points where there are countries of asylum.  This makes sense in the context
AppendDataPointRow <- function(sqafID, asylum, numberOfSubRules, totalRows) {
  
  # Check that it does not already exist ...
  matchingRows <- sqafDataPoints[sqafDataPoints$ID == sqafID & sqafDataPoints$Asylum == asylum, ]
  
#View(matchingRows)  
#print(IsNNN(matchingRows))  

#  print(paste(sqafID, asylum, numberOfSubRules, totalRows))
#  print(nrow(sqafDataPoints))
  
  if ( is.null(matchingRows) | nrow(matchingRows) == 0) {
    
#    print("Doing it")    
    
    totalMultiplied <- as.integer(totalRows) * as.integer(numberOfSubRules)
    
#    print(totalMultiplied)    
    
    
    # adds the sqafID, asylum code and the total...
    sqafDataPoints[nrow(sqafDataPoints) + 1,] <<- 
      # c - if we include this as a list rather than as a c, then the types are correct
      list(as.character(sqafID), # ID
        as.character(asylum), # Asylum
        totalMultiplied, # total
        0, 0, 0, 0
      )

  }
  
#  print(paste("AFTER", sqafID, asylum, numberOfSubRules, totalRows))  
}


#-------------------------------------------------------------------------------------------------------------------------
# Appends the row with the total data points
UpdateDataPointRow <- function(sqafID, asylum, threshold, count) {
  
#print(paste("Updating row ", sqafID, asylum, threshold, count))  
  
  # adds the count to the relevant column
  colName <- paste0("Threshold_", threshold)
  if (threshold == 0) {
    colName <- "Total" 
  }

  # BIG Assumption - we will always just have one entry by SQAF ID and asylum
  tempCount <- sqafDataPoints[[as.character(colName)]][sqafDataPoints$ID == sqafID & sqafDataPoints$Asylum == asylum ][ 1 ]
  
  #print(tempCount)      
  
  tempCount <- as.integer(tempCount) + as.integer(count)
  
  #print(tempCount)    
  
  sqafDataPoints[[colName]][sqafDataPoints$ID == sqafID & sqafDataPoints$Asylum == asylum ] <<- tempCount
  
}

#sqafDataPoints[[as.character("ID")]]
#sqafDataPoints[[as.character("Total")]][sqafDataPoints$ID == "1.4" & sqafDataPoints$Asylum == "ARM" ][1]


#-------------------------------------------------------------------------------------------------------------------------
# Cleans up the sqaf output to use integers for the years, results and severity, and also converts the country codes
# and population types to use the lookups included in the refugee-statistics and popdata systems
StandardiseSQAFList <-
  function( sqafList) {
    
    # Standardise the country names to use IDs which are compatible with the system
    countriesPSR <- LoadCountryListFromPopulationStatisticsReference()
    countriesPSR$AsylumID <- countriesPSR$OriginID <- countriesPSR$ID
    
    # And load the population types too
    ptPSR <- LoadPopulationTypesFromPopulationStatisticsReference()
    ptPSR$PopulationTypeID <- ptPSR$id
    
    # Then try to join the country list together so we can include the IDs
    b4 <-nrow(sqafList)
    
    # The country IDs for origin and asylum
    sqafList <- left_join(sqafList, countriesPSR %>% select(OriginID, code), by=c("Origin"="code"))
    sqafList <- left_join(sqafList, countriesPSR %>% select(AsylumID, code), by=c("Asylum"="code"))
    
    # And the population types for the PT
    sqafList <- left_join(sqafList, ptPSR %>% select(code, PopulationTypeID), by=c("PopulationType"="code"))
    
    after <- nrow(sqafList)
    
    if(b4 == after) {
      print(paste0("Good news - no duplicates added when joining the sqaf list with the country and population type codes; the total number of entries remains: ", after))
    } else {
      print(paste0("WARNING: Duplicates added by the join between the sqaf list, population types and the country codes for origin / asylum.  Before: ", b4, " and after: ", after, "."))
    }
    
    print(paste0("Origin codes that have no ID: ", paste0(unique(sqafList$Origin[!is.na(sqafList$Origin) & is.na(sqafList$OriginID)]), collapse=", ")))
    print(paste0("Asylum codes that have no ID: ", paste0(unique(sqafList$Asylum[!is.na(sqafList$Asylum) & is.na(sqafList$AsylumID)]), collapse=", ")))  
    
    
    # Ensure that the result, severity and year are all integers
    # This should no longer be required as we now append data using lists not vectors
    sqafList$Year <- as.integer(sqafList$Year)
    sqafList$Result <- as.integer(sqafList$Result)
    sqafList$Severity <- as.integer(sqafList$Severity)
    
    #    View(sqafList)
    
    returnValue <- sqafList
  }



#-------------------------------------------------------------------------------------------------------------------------
# Cleans up the sqaf output to use integers for the years, results and severity, and also converts the country codes
# and population types to use the lookups included in the refugee-statistics and popdata systems
StandardiseSQAFDataPoints <-
  function( sqafDataPoints) {
    
    # Make sure the dataCube is using integers where it should
    # This should no longer be required as we now append data using lists not vectors
    sqafDataPoints$Total <- as.integer(sqafDataPoints$Total)
    sqafDataPoints$Threshold_1 <- as.integer(sqafDataPoints$Threshold_1)
    sqafDataPoints$Threshold_2 <- as.integer(sqafDataPoints$Threshold_2)
    sqafDataPoints$Threshold_3 <- as.integer(sqafDataPoints$Threshold_3)
    sqafDataPoints$Threshold_4 <- as.integer(sqafDataPoints$Threshold_4)
    
    # Generate the delta - i.e. the missing data points that are OK as no test has flagged them.
    sqafDataPoints$Delta <- sqafDataPoints$Total - 
      sqafDataPoints$Threshold_1 - sqafDataPoints$Threshold_2 - sqafDataPoints$Threshold_3 - sqafDataPoints$Threshold_4
    
    # To Do Check here for negative numbers....
    
    
    
    # Then we add the delta to Threshold_1
    sqafDataPoints$Threshold_1 <- sqafDataPoints$Threshold_1 + sqafDataPoints$Delta
    

    # Check for bad calculations
    print("Checking for bad calculations")
    print(unique(sqafDataPoints$ID[sqafDataPoints$Total != 
                               sqafDataPoints$Threshold_1 + 
                               sqafDataPoints$Threshold_2 + 
                               sqafDataPoints$Threshold_3 + 
                               sqafDataPoints$Threshold_4]))
    
    
    returnValue <- sqafDataPoints
  }


#-------------------------------------------------------------------------------------------------------------------------
# Produces a summary stacked bar chart of the data
SQAFSummaryChart <- function(asylumFilter=NA) {
  
  #asylumFilter <- "AFG"
  #asylumFilter <- "MYA"
  #asylumFilter <- NA
  
  #--0--
  sqafTitle <- "Summary of SQAF checks"

  
  #--1-- Then we need to normalise the data by first making a copy of the global data
  sqafDataNarrow <- sqafDataPoints
  
  
  #--2-- If we have a specific asylum code, lets filter on it, and append a note to the tile
  if ( ! IsNN(asylumFilter) & asylumFilter != "") {
    sqafDataNarrow <- sqafDataNarrow %>%
      filter(Asylum == asylumFilter)
    
    sqafTitle <- paste0(sqafTitle, " - ", asylumFilter)
  }
  
  
  #--3-- Then we need to normalise the data and rename the columns
  sqafDataNarrow <- sqafDataNarrow %>%
    group_by(ID) %>%
    summarise(
#      Total = sum(Total), # No need to include the total here
      Must_Fix = sum(Threshold_4),
      Should_Fix = sum(Threshold_3),
      Could_Fix = sum(Threshold_2),
      OK = sum(Threshold_1)
      
    ) %>%
    # add the percentages
    #mutate(
      #OK = Threshold_1 / Total,
      #Could_Fix = Threshold_2 / Total,
      #Should_Fix = Threshold_3 / Total,
      #Must_Fix = Threshold_4 / Total
    #) %>%
    #select( ID, OK, Could_Fix, Should_Fix, Must_Fix ) %>%
    pivot_longer(!ID, names_to = "Severity", values_to = "Count")
  
  
  #--4-- Then calculate the percentages and the labels
  sqafDataNarrow <- CalculatePercentageWithLabel(sqafDataNarrow, "ID", NA, "Severity", "Count", 5)
  

  #--5-- join the titles to the data
  sqafDataNarrow <- left_join(sqafDataNarrow, sqafSeverityList %>% select(Key, Title, Legend), by=c("Severity"="Key") )
  
#View(sqafDataNarrow)  
  #--6-- Produce the stacked bar chart
  plotSQAF <- GenerateChartWithStackedBarChart(sqafDataNarrow, "ID", "Percent", "PercentLabel", "Title", 
                                               sqafColours, 
                                               sqafTitle, 
                                               "Source: Gucci SQAF", TRUE) 
  
  returnValue <- plotSQAF
}




