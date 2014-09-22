#### scrape.GTrends.R v2.0
#### New Method:
#### Phase 0: Setup
#### Phase 1: Seek a location where all keywords can be compared.
####          Normalize if needed. This is the 'base' locatoin. 
#### Phase 2: Pick a keyword and begin comparing it to each 
####          given location. Normalize each location to the 'base'
####          location. 
####
####    Everytime a new keyword is calculated for a location
####    it is bound to the existing data frame.  
####
####    Consider one time period:
####      At location A, Key X is 20, Key Y is 60
####      Key X is 25 at location A, 50 at location B
####      Key Y is 75 at location A, 100 at location B
####
####        Then the following table could be constructed:
####
####                Loc_A   Loc_B
####          Key_X    1      2
####          Key_Y    3      4
####
####      It could be validated by testing the following condition:
####        at location B, Key Y should be twice Key X, whatever 
####        Key X is.  If this holds, then the table is valid. 
####      
####    Now, let's get started...
####
####      ~Trevolution
####        trevor.grant@mindshareworld.com
####        trevor.d.grant@gmail.com


###################################################################
####  Phase 0: The setup
scrape.GTrends <- function(LOCATION_FILE,pivot_word, pivot_word_name, category, dates, keywords, ch)
{
  ############################################################################################
  key_table <- list()
  locations <- read.csv(LOCATION_FILE)[,1] 
  google.locations <- read.csv(LOCATION_FILE)
  #################################################
  ####  Here we pop the first demo off to be pivot geo
  ####  Check to make sure there are no NAs at that geo
  ####  Build a location table/make sure the pivot geo is in every run
  
  ### TODO: Sometimes you get NA for most recent week for all places.
  ### Need to be able to identify that situation and pop the last date off. 
  
  
  #### Prerun -- Searching for Pivot Geographies
  #### Later, take a list of keywords and organize keyword batches
  
  ## Each of these setups should be a method...
  print("Searching for useable pivot geography...")
  for (i in 1:length(locations)){
    print(paste("Checking ",locations[i]))
    pivot_geo = locations[i]
    master_key_vector <- query.GTrends(pivot_word, category, dates, pivot_geo,ch)[,2]
    if (!(0 %in% master_key_vector) & !(NA %in% master_key_vector)){
      ## If there are no 0's and no NA's, this can be the master_key_vector
      locations = locations[-1]
      key_geos <- rbind(pivot_geo,matrix(append(locations, rep("", 4-(length(locations)%%4))),nrow=4))
      pivot_geo_name <- google.locations[i,2] 
      break
    }
  }
  print(paste("Using", pivot_geo_name))
  
  geo_table <- list()
  for (location in key_geos[,1]){  #instead of locatoins 9/22/14, locations was always missing pivot_geo
    if (location==""){next}
    print(paste("Collecting in",location))
    skip_geo = FALSE
    for (i in 1:ncol(keywords)){
      temp_table <-  query.GTrends(keywords[,i], category, dates, location,ch)
      rownames(temp_table) <- temp_table['Date'][,1]
      temp_table <- temp_table[, -1,drop=FALSE]
      if (length(master_key_vector) == dim(temp_table)[1]){
        key_table[[i]] <- temp_table
      } else {
        print("Low volume warning...skipping geography")
        skip_geo =TRUE
        break 
      }
      
    }
    if (skip_geo) next
    
    if (length(key_table) > 1){  ## Only do this if there is more than one keyword
      temp_table <- key_table[[1]] / key_table[[1]][,pivot_word_name]
      for (i in 2:length(key_table)){
        temp_table <- cbind( temp_table , key_table[[i]] / key_table[[i]][,pivot_word_name])
      }
      temp_table <- temp_table[, -tail(which(colnames(temp_table) == pivot_word_name),-1)]
    }
    
    geo_table[[location]] <- temp_table
    
  }
  
  
  print("Data collected for keywords, collecting geographies")
  key_table <- list()
  for (i in 1:ncol(key_geos)){
    temp_table <-  query.GTrends(pivot_word, category, dates, key_geos[,i],ch)
    rownames(temp_table) <- temp_table['Date'][,1]
    temp_table <- temp_table[, -1]
    key_table[[i]] <- temp_table
  }
  
  
  temp_table <- key_table[[1]] / key_table[[1]][,pivot_geo_name] 
  if (length(key_table) > 1){
    for (i in 2:length(key_table)){
      temp_table <- cbind( temp_table , key_table[[i]] / key_table[[i]][,pivot_geo_name])
    }
  }
  
  if (length(key_table) > 1)  {
    col_to_drop = tail(which(colnames(temp_table) == pivot_geo_name),-1)
    temp_table <- temp_table[, -col_to_drop]
  }
  
  temp_table <- temp_table* master_key_vector
  
  
  #### Only need the rest in multiple keyword case
  
  for (j in 1:ncol(temp_table)){
    state_name <- colnames(temp_table)[j]
    state_code <-google.locations[google.locations[,2]==state_name,1]
    geo_table[[state_code]] <- geo_table[[state_code]]*temp_table[,pivot_geo_name]
  }


  
  return(geo_table)
}

