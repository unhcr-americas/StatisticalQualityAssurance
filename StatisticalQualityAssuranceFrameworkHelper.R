#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
# Helper function to support the Statistics Quality Assurance Framework

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

# declare our data cube as an empty data frame with the relevant column types
sqafList <- 
  data.frame(
    ID = character(), # e.g. the SQAF ID 1.2.3
    
    Notes = character(),
    
    # THe action is redundant and should be included only in the metadata
    #ActionDescription = character(),
    
    Result = integer(),
    Severity=integer(),    
    
    Year = integer(),
    PopulationType = character(),
    Origin = character(),
    Asylum = character(),
    stringsAsFactors=FALSE # This negates the use of factors so that the character based columns can contain anything
  )


#-------------------------------------------------------------------------------------------------------------------------
# Append an item to the global SQAF list
AppendSQAFItem <- 
  function(sqafID, msgCheck, #msgAction, 
           perc, threshold, year, populationType, origin, asylum  ) {
    
    #--1-- check for NULLs and reassign as NA
    sqafID <- ifelse(IsNull(sqafID), NA, sqafID)
    msgCheck <- ifelse(IsNull(msgCheck), NA, msgCheck)
#    msgAction <- ifelse(IsNull(msgAction), NA, msgAction)
    
    populationType <- ifelse(IsNull(populationType), NA, populationType)
    origin <- ifelse(IsNull(origin), NA, origin)
    asylum <- ifelse(IsNull(asylum), NA, asylum)
    
    
    #--2-- Assign the new info to the global list
    sqafList[nrow(sqafList) + 1,] <<- 
      
      c(as.character(sqafID), # ID
        as.character(msgCheck), # Additional Notes
#        as.character(msgAction), # Action message
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
# The stubAction is the generic action message to present. e.g. 
# Try to extend the coverage of the available demographic data through new data sources, statistical modelling or estimation.
GenerateThresholdAndMessages <-
  #function(sqafID, origin, asylum, populationType, stubAction, perc, additionalMessage = "") {
  function(sqafID, origin, asylum, populationType, perc, additionalMessage = "") {

    # Get the relevant sqaf check
    sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
        
    # 15-March-2021 - do not build this message as it is essentially redundant.  Just include the description of the check...
    # And the additional message (if present)
    #msgCheck <- paste0(sqafCheck$Description, ".")
    msgCheck <- NA
    
    if(! is.na(additionalMessage) & length(additionalMessage) > 0 & additionalMessage != "") {
      #msgCheck <- paste0(msgCheck, "  ", additionalMessage, ".")
      #msgCheck <- paste0(msgCheck, additionalMessage, ".")
      msgCheck <- paste0(additionalMessage, ".")
    }
    
    
#    msgCheck <- paste0("The check '", sqafChecks$Description[sqafChecks$ID == sqafID], "'")
    
    
    # Build the message describing the check, including the applicable parameters (PT, asylum and origin) e.g.
    # "The check 'My check' for country of asylum ASY and population type PT achieved 12%.
#    numParams <- sum(!is.null(origin), !is.null(asylum), !is.null(populationType))
    
    # If we have some parameters to present, lets start the half sentence
#    if( numParams >= 1) {
#      msgCheck <- paste0(msgCheck, " for ")  
#    }
    # Now build the list of params
#    paramString <- ""
    
    # Origin
#    if( ! IsNNN(origin)) {
#      paramString <- paste0("country of origin ", origin )
#    }
    
    # Asylum
#    if( ! IsNNN(asylum)) {
#      # Add a comma if there are more than 2 params, otherwise and if there are exactly 2
#      if( length(paramString) > 0) {
#        if ( numParams == 2 ) {
#          paramString <- paste0(paramString, " and " )
#        } else if ( numParams == 3 ) {
#          paramString <- paste0(paramString, ", " )
#        }
#      }      
#      
#      paramString <- paste0(paramString, "country of asylum ", asylum )
#    }

#    # Population type
#    if( ! IsNNN(populationType)) {
#      # Add an and as this is the last parameter, as long as there are more than one parameter
#      if( length(paramString) > 0) {
#        if ( numParams > 1 ) {
#          paramString <- paste0(paramString, " and " )
#        }
#      }
#      
#      paramString <- paste0(paramString, "population type ", populationType )
#    }
#    
#    # And then add the %
#    msgCheck <- paste0(msgCheck, paramString, " achieved ", round(perc), "%  ", additionalMessage, "." )
        


    # See where the result lies
    threshold <- 0
#    msg1 <- "Looking good: "
#    msg2 <- "Could improve: "
#    msg3 <- "Should improve: "
#    msg4 <- "Must improve: "
    
    if( sqafCheck$Quality_Scale == "HighToLow" ) {
      
      # With the first category the choice is the threshold NOT being null AND the value being less than the threshold
      # With the last category the choice is the threshold being null OR the value being less than the threshold
      if (! IsNNN(sqafCheck$Threshold_4) & perc <= sqafCheck$Threshold_4) {
        threshold <- 4  
#        msgCheck <- paste0(msg4, msgCheck)
        
      } else if (! IsNNN(sqafCheck$Threshold_3) &  perc <= sqafCheck$Threshold_3) {
        threshold <- 3
#        msgCheck <- paste0(msg3, msgCheck)
        
      } else if (! IsNNN(sqafCheck$Threshold_2) &  perc <= sqafCheck$Threshold_2) {
        threshold <- 2
#        msgCheck <- paste0(msg2, msgCheck)
        
      } else if ( IsNNN(sqafCheck$Threshold_1) | perc <= sqafCheck$Threshold_1) {
        threshold <- 1
#        msgCheck <- paste0(msg1, msgCheck)
        
        # What about silly %'s > 100?
      }        
      
    } else if ( sqafCheck$Quality_Scale == "LowToHigh" ) {

      
      if (! IsNNN(sqafCheck$Threshold_4) & perc >= sqafCheck$Threshold_4) {
        threshold <- 4  
#        msgCheck <- paste0(msg4, msgCheck)
        
      } else if (! IsNNN(sqafCheck$Threshold_3) &  perc >= sqafCheck$Threshold_3) {
        threshold <- 3
#        msgCheck <- paste0(msg3, msgCheck)
        
      } else if (! IsNNN(sqafCheck$Threshold_2) &  perc >= sqafCheck$Threshold_2) {
        threshold <- 2
#        msgCheck <- paste0(msg2, msgCheck)
        
      } else if ( IsNNN(sqafCheck$Threshold_1) | perc >= sqafCheck$Threshold_1) {
        threshold <- 1
#        msgCheck <- paste0(msg1, msgCheck)
      }
      
    }
    
    # Tweak the action if the threshold achieved was 1
#    if( threshold == 1) {
#      msgAction <- "No action required."      
#    }
    
    
    # Then we want to return the threshold, msgCheck and msgAction
#    returnValue <- list("threshold" = threshold, "msgCheck" = msgCheck, "msgAction" = msgAction)
    returnValue <- list("threshold" = threshold, "msgCheck" = msgCheck)
  }


#-------------------------------------------------------------------------------------------------------------------------
# The rules should be a data frame with the validate structure (name, description and rule)
# These rules are 0/1 i.e. they either pass or fail
RunValidationChecks <-
  function(sqafID, data, rules) {
    
    #--0-- Iterate through the rules and run each on the given dataset
    for( i in 1 : nrow(rules)) {
      
      ruleListRow <- rules[i,]
      
      print(paste0("Running test '", ruleListRow$name, "' on ", nrow(data), " rows"))      
      
      if(nrow(data) > 0) {
        
        #--1-- Then test the data using the validate framework
        #        out <- confront(dataSubSet, validator( ruleListRow$rule))
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
            
            output <- GenerateThresholdAndMessages(
              sqafID,
              coo, 
              coa,
              popType,
  #            stubAction, 
              0,
              ruleListRow$description # Include the description here which is useful especially when there are multiple issues
              #""
            )
            
            # Check whether the PT, origin and asylum are set - should be handled in AppendSQAFItem?
            
            # Assign the new info to the global list
            AppendSQAFItem(sqafID, output$msgCheck, #output$msgAction, 
                           0, output$threshold, 
                           sqafYear, popType, coo, coa)
            
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
  
  
  numViolating = 0
  
  #--2-- Iterate through the data summary and generate the output  
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
    
    
    if (output$threshold >= 2) {
      numViolating <- numViolating + 1
    }
    
  }
  
  print(paste0("Found ", numViolating, " cases (countries of asylum and population type) violating the test"))
  
  
}



#-------------------------------------------------------------------------------------------------------------------------
# Automatically calculated the % occurence of a particular categorical value by country of asylum
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
    #group_by(across(all_of(c("asylum", "PT", filterColName)))) %>%
    group_by(across(all_of(listToGroupByExtended))) %>%
#    group_by(asylum, PT, !! as.name(filterColName)) %>%
    summarise(        
      Enumerator = sum(!! as.name(countColName))
    )
  
#View(dataSummary)  
  
  #--2b-- Calculate the total by asylum, PT
#  dataSummary2a <- dataSummary %>%
    dataSummary2a <- data %>% 
#    group_by(asylum, PT) %>%
    group_by(across(all_of(listToGroupBy))) %>%
    summarise(        
      Total = sum(!! as.name(countColName))
      #Total = sum(Enumerator)
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
  dataSummary3 <- left_join(
    dataSummary2a,
    dataSummary2b, 
#    dataSummary %>% 
      #filter(!! as.name(filterColName) == as.character(filterValue)) %>%
#      filter(!! as.name(filterColName) == filterValue) %>%
      #filter(!! as.name(filterColName) == !!!rlang::parse_exprs(filterValue)) %>%
      
      #filter(!!!rlang::parse_exprs(filterExpression)) %>% 
#      select(asylum, PT, Enumerator),
    #by=c("asylum"="asylum", "PT"="PT")
    by=listToJoinBy
    
  )
  
  
  #--2e-- Remove the na's and produce the percentage
  dataSummary3[is.na(dataSummary3)] <- 0
  dataSummary3$Perc <- dataSummary3$Enumerator / dataSummary3$Total * 100
  
#View(dataSummary3)  
  
  
  numViolating = 0
  
  # Iterate through the data summary and generate the output
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
# 0.1 Checks that data has been received
CheckDataReceived <-
  function(sqafID, gr) {
    
    currentNumRows <- nrow(sqafList)
    
    # We ignore the "Non-submissions" from these: "AND", "SMA", "VAT"
    countryList <- c("ABW","AFG","AIA","ALB","ALG","ANG","ANT","ARE","ARG", 
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
    
    # Strip out the IDMC and UNRWA data
    gr <- gr %>% filter(! populationType %in% c("Total", "IDMC", "UNRWA"))
    
    # get the unique list from the gr dataset
    coaList <- unique(gr$CoA)
    
    # No need to do an intersect, but lets include it anyway:
    print(paste0("Found ", length(intersect(coaList,countryList)), " countries of asylum that have submitted data."))

    missingSubmissions <- setdiff(countryList, coaList)
    print(paste0("Found ", length(missingSubmissions), " countries of asylum that have MISSING data submissions."))
    
    incorrectSubmissions <- setdiff(coaList, countryList)
    print(paste0("Found ", length(incorrectSubmissions), " countries of asylum that have UNEXPECTED data submissions."))
    

    #--2-- Loop through them and identify any we need to record as messages
    numViolating <- 0
    
    # Loop through the missing submissions
    if( length(missingSubmissions) > 0 ) {
      for( i in 1 : length(missingSubmissions)) {
        
        additionalMessage <- paste0("No data has yet been submitted")
        
        output <- GenerateThresholdAndMessages(
          sqafID,
          NA, 
          missingSubmissions[i],
          NA,
          0,
          additionalMessage
        )
        
        # Assign the new info to the global list
        AppendSQAFItem(sqafID, output$msgCheck, 
                       0, output$threshold, 
                       sqafYear, NA, NA, missingSubmissions[i])
        
        
        if (output$threshold >= 2) {
          numViolating <- numViolating + 1
        }
        
      }    
    }
    
    # Loop through the incorrect submissions
    if( length(incorrectSubmissions) > 0 ) {
      for( i in 1 : length(incorrectSubmissions)) {
        
        additionalMessage <- paste0("Data has been submitted unexpectedly")
        
        output <- GenerateThresholdAndMessages(
          sqafID,
          NA, 
          incorrectSubmissions[i],
          NA,
          0,
          additionalMessage
        )
        
        # Assign the new info to the global list
        AppendSQAFItem(sqafID, output$msgCheck, 
                       0, output$threshold, 
                       sqafYear, NA, NA, incorrectSubmissions[i])
        
        
        if (output$threshold >= 2) {
          numViolating <- numViolating + 1
        }
        
      }  
    }
    
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
# 1.3 returns true if the process ran successfully
CheckSubNationalCoverage <-
  function(sqafID, demoData) {
    
    #--0-- Get the current number of rows
    currentNumRows <- nrow(sqafList)
    
    #--1-- Get the relevant sqaf check
    sqafCheck <- sqafChecks[sqafChecks$ID == sqafID,]
    
    
    # So the logical test here is that a given CoA has more than one location
    # Too simple right???  
    
    # Filter out the rows with zero as total - these are not relevant to this check
    demoData <- demoData %>% filter(total > 0)
    
    
    # Summarise the demographics to only include the location and the country of asylum (and of course the population type)
    #demoData <- SummariseDemographics(demoData, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
    demoData <- SummariseDemographics(demoData, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
    
    
    # We then need to summarise further to just include the totals, CoA and the location
    # so this is a big group by and we calculate the total....
    dataSummary <- demoData %>% 
      group_by(asylum, location) %>%
      summarise(
        #Age_Sex = sum(Age_Sex),
        #Sex = sum(Sex),
        Total = sum(Total)
      )
    #View(dataSummary)    

    dataSummary <- dataSummary %>% 
      group_by(asylum) %>%
      summarise(
        #Age_Sex = sum(Age_Sex),
        #Sex = sum(Sex),
        NumLocations = n()
      )
    

    # Produce the output
    for( i in 1 : nrow(dataSummary)) {
      
      output <- GenerateThresholdAndMessages(
        sqafID,
        NULL, 
        dataSummary$asylum[i],
        NULL,
#        stubAction, 
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
      
    }
    
    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
    success <- nrow(sqafList) - currentNumRows > 0
    returnValue <- success
    
  }


#-------------------------------------------------------------------------------------------------------------------------
# 1.4
CheckDemographicAggregationType <-
  function(sqafID, dataDemo) {
    
    #--0-- Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    
    
    #validAggregationTypes <- c("Detailed", "M/F and 18-59", "M/F", "Total")
    validAggregationTypes <- c("Default", "18_59", "M_F", "Total")
    
    
    
    # Get the current number of rows outputted...
    currentNumRows <- nrow(sqafList)
    
    
    stubAction <- "The aggregation type can be one of four options and the data in each row should conform to the given aggregation type."
    
    #--1-- We use the validate package to assess this set of rules, so the first step is to build our rules
    
    # Invalid aggregation types (including blank ones) - this was the wrong way round logically previously; now corrected.
    ruleList <- data.frame( 
      name = "Empty or invalid aggregation type",
      description = "Empty or invalid aggregation type", 
      rule = paste0("AggregationType != '' & AggregationType %in% c(\"",
                    paste(validAggregationTypes, collapse="\",\"", sep=""), "\")")
      
#      rule = paste0("AggregationType == '' | ! AggregationType %in% c(\"",
#                    paste(validAggregationTypes, collapse="\",\"", sep=""), "\")")
      
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
             "Just disaggregation by sex - The data provided does not match the specified aggregation type", 
             
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
    
    #    View(ruleList[1,])        
    
    # Now iterate through the rules and filter the data, then confront only the filtered data
    for( i in 1 : nrow(ruleList)) {
      
      ruleListRow <- as.data.frame( ruleList[i,] )
      
      # Filter the data as long as the given name is a valid aggregation type (otherwise just test the full cube)
      if ( ruleListRow$name %in% validAggregationTypes ) {
        dataSubSet <- dataDemo %>% filter(AggregationType == ruleListRow$name)
      } else {
        dataSubSet <- dataDemo
      }
      
      # Then run this test and log the results
      RunValidationChecks(sqafID, dataSubSet, ruleListRow)
      
      
    }
    
    # Pretty basic success criteria so far - basically that the function successfully adds some rows.
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
    
    # OK here we need to catch infinite percentages - and use a special 1,000,000 number so that they still appear as errors.
    dataReturns$Perc[is.infinite(dataReturns$Perc)] <- 1000000

#View(dataReturns)    
    
#    stubAction <- "Review together the differences in the number of returns recorded by the country of origin and the country(ies) of asylum.
#    Small differences are completely acceptable, but large differences should be rare."
        
    #--2-- Loop through them and identify any we need to record as messages
    numViolating <- 0
    
    for( i in 1 : nrow(dataReturns)) {
      
      additionalMessage <- paste0("Comparison: ", 
                                  PrettyNum(dataReturns$RETPopulation[i]), 
                                  " by country of origin versus ", 
                                  PrettyNum(dataReturns$REFROCPopulation[i]), 
                                  " by country of asylum, i.e. a difference of ", 
                                  PrettyNum(dataReturns$Diff[i])
                                  )
      
      output <- GenerateThresholdAndMessages(
        sqafID,
        dataReturns$origin[i], 
        dataReturns$asylum[i],
        pt,
#        stubAction, 
        dataReturns$Perc[i],
        additionalMessage
      )
      
      
      # Assign the new info to the global list
      AppendSQAFItem(sqafID, output$msgCheck, #output$msgAction, 
                     dataReturns$Perc[i], output$threshold, 
                     sqafYear, pt, dataReturns$origin[i], dataReturns$asylum[i])

      
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
CheckDemographicTotals <- function(sqafID, dataDemo, dataPopulation) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  print("!!! NOT YET IMPLEMENTED !!!")
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  #success <- nrow(sqafList) - currentNumRows > 0
  #returnValue <- success  
  returnValue <- TRUE
  
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
    ##################################################################
    # Source should be one of U, G, V, N (should not be blank)
    # Basis should be one of R, V, E, C (should not be blank and estimates less desirable)
    
    # Detailed
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
      name = "Applications reported as persons should always have an average case size of 0 or 1",
      description = "Applications reported as persons should always have an average case size of 0 or 1", 
      rule = "ApplicationDataType == 'C' | (ApplicationDataType == 'P' & ApplicationAveragePersonsPerCase %in% c(0,1))"
    )
    
    
    # Check 2 - No UNHCR recognitions are included under other
    r2 <- c( "Decisions reported as persons should always have an average case size of 0 or 1", 
             "Decisions reported as persons should always have an average case size of 0 or 1",
             
             "DecisionDataType == 'C' | (DecisionDataType == 'P' & DecisionAveragePersonsPerCase %in% c(0,1))"
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
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Ensure we have a PT in the stateless table!
  dataSTA$PT <- "STA"
  
  # Then calc the % that is STA or UKN for each country of asylum
  countColName = GetCountColumnName("STA", isASR, FALSE)
  
  # Standardise STA, UKN and VAR to all be STA
  dataSTA$origin[dataSTA$origin %in% c("STA", "UKN", "VAR")] <- "STA"
  
  
  # Then run the rule ... we can group by PT as we only have one pt, so it is simple....
  RunCategoricalRuleBasedOnPercentage(sqafID, dataSTA, TRUE, countColName, "origin", "STA",
    "If possible, the origin in the Stateless table should record the specific countries of former habitual residence for displaced STA (e.g. the Rohingya) rather than the generic STA/UKN/VAR.")

  
  # If the output is a warning or an error, lets include an additional message.
  # We do that by applying it directly to the SQAF list
#  sqafList$Notes[sqafList$ID == sqafID & sqafList$Severity >= 2 ] <-
    
  
  
  
#  # Summarise the STA data by asylum and origin
#  dataSummary <- dataSTA %>% 
#    group_by(asylum, origin) %>%
#    summarise(
#      Enumerator = sum(!! as.name(countColName))
#    )
#  
#  # Then summarise by asylum and get the total and the % for each origin
#  dataSummary2 <- dataSummary %>%
#    group_by(asylum) %>%
#    summarise (
#      Total = sum(Enumerator)
#    ) 

  # then group by asylum and join to a filtered version with just STA
#  dataSummary3 <- left_join(
#                      dataSummary2, 
#                      dataSummary %>% filter(origin == "STA") %>% select(asylum, Enumerator),
#                      by=c("asylum"="asylum")
#                    )
  
#  dataSummary3[is.na(dataSummary3)] <- 0
#  dataSummary3$Perc <- dataSummary3$Enumerator / dataSummary3$Total * 100
    
  #View(dataSummary3)    
  

#  numViolating = 0
  
#  for( i in 1 : nrow(dataSummary3)) {
    
#    output <- GenerateThresholdAndMessages(
#      sqafID,
#      NULL, 
#      dataSummary3$asylum[i],
#      NULL,
#      #                  stubAction, 
#      dataSummary3$Perc[i]
#    )
    
    
#    # Assign the new info to the global list
#    AppendSQAFItem(sqafID, output$msgCheck, #output$msgAction, 
#                   dataSummary3$Perc[i], output$threshold, 
#                   sqafYear, "STA", NA, dataSummary3$asylum[i])
    
#    if (output$threshold >= 2) {
#      numViolating <- numViolating + 1
#    }
    
#  }
  
#  print(paste0("Found ", numViolating, " countries of asylum violating the test"))
  

  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  

}



#-------------------------------------------------------------------------------------------------------------------------
# 6.2 - Displaced PAL and GAZ origins should not be recorded in the Stateless table
CheckStatelessDisplacedGAZ <- function(sqafID, dataSTA, isASR) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Ensure we have a PT in the stateless table!
  dataSTA$PT <- "STA"
  
  # Add the basis
  ruleList <- data.frame( 
    name = "Displaced PAL and GAZ origins should not be recorded in the Stateless table",
    description = "Displaced PAL and GAZ origins should not be recorded in the Stateless table", 
    rule = paste0("! origin %in% c('PAL', 'GAZ')")
    
  )
  
  RunValidationChecks(sqafID, dataSTA, ruleList)      

    
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0

  returnValue <- success  

}


#-------------------------------------------------------------------------------------------------------------------------
# 8.1
ChangeSignificantChange <- function(sqafID, dataLatest, dataHistoric) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  print("!!! NOT YET IMPLEMENTED !!!")
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  #success <- nrow(sqafList) - currentNumRows > 0
  #returnValue <- success  
  returnValue <- TRUE
  
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
# 9.3 VDA should no longer be included in the OOC table
CheckVDAInOOC <- function(sqafID, dataOOC) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Standardise the column names
  dataOOC$PT <- dataOOC$populationType  
  
  # Add the rule
  ruleList <- data.frame( 
    name = "VEN internationally displaced should now be recorded in the VDA table",
    description = "Previously, the Venezuelans Displaced Abroad were recorded in the OOC table.  Now, VEN internationally forcibly displaced should now be recorded in the VDA table.", 
    rule = paste0("origin != 'VEN' | (origin == 'VEN' & asylum == 'VEN')")
    
  )
  
  RunValidationChecks(sqafID, dataOOC, ruleList)  
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  

}


#-------------------------------------------------------------------------------------------------------------------------
# 9.4 Checks that all the country codes in the origin and asylum in gr are valid
CheckCountryCodes <- function(sqafID, gr) {
  
  # Get the current number of rows outputted...
  currentNumRows <- nrow(sqafList)
  
  # Standardise the column names
  gr$PT <- gr$populationType  
  gr$origin <- gr$CoO
  gr$asylum <- gr$CoA
  
  # Strip out the IDMC and UNRWA data
  gr <- gr %>% filter(! populationType %in% c("Total", "IDMC", "UNRWA"))
  
  
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
  RunValidationChecks(sqafID, gr, ruleList)  
  
  # Pretty basic success criteria so far - basically that the function successfully adds some rows.
  success <- nrow(sqafList) - currentNumRows > 0
  returnValue <- success  

}









