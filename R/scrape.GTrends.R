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
scrape.GTrends <- function(keyw, google.locations, date, category, ch)
{
  global.keywords <- keyw
  
  
  ## You can only query 4 keywords at a time in the API so we need to break up 
  ## our keywords into blocks of 4.  Also, at least one keyword from each block
  ## must be in common with the previous block, and the following block.  
  ## We need these keywords in common for reference when we merge. 
  ##
  ## v2.0 UPDATE:  This is much less critical than it was in v1.0 however,
  ##                it is still important for the finding and normalizing
  ##                the so-called 'base location'.
  
  keyword.block <- make.keyword.blocks( global.keywords )
  
  
  ###################################################################
  #### Phase 1:The Search for a complete set ########################
  
  
  breakFLAG <- FALSE
  nextFLAG <- FALSE
  cat("\nPhase1: Searching for a base locations...")
  for (local in 1:(dim(google.locations)[1]))
  {
    nextFLAG <- FALSE
    cat(paste("\nChecking on:", google.locations[local,2]))
    phase1_output <- list()
    for (block in 1:length(keyword.block)){
      keywords <- keyword.block[[block]]
      google.location.code <- google.locations[local,1]
      output <- query.GTrends(keywords,category,date, google.location.code,ch)
      
      #Sys.sleep(60)   A 60 second sleep timer is built into the 
      #query.GTrends function, no need for it here
      
      if(is.null(output) || ncol(output) < 3 ){
        cat("...not enough search volume.")
        nextFLAG <- TRUE
        break
      }
      phase1_output[[block]] <- output  
      
    }
    if (nextFLAG) next
    ### TODO:  This is really phase1b_output now... 
    ###  an easy fix.  Just gets confusing below
    phase2_output <- phase1_output[[1]][,-1]
    dates <- phase1_output[[1]][,1]
    ### So Phase1 output is a list() of data.frames
    for (next_frame in phase1_output[2:(length(phase1_output))]){
      if ((tail(colnames(phase2_output),1) == colnames(next_frame)[2]) &&
            (dim(next_frame)[2]>2)){
        phase2_output <- cbind(phase2_output/phase2_output[,dim(phase2_output)[2]], next_frame[,-1:-2]/next_frame[,2]) 
      }
    }
    if (ncol(phase2_output)< nrow(global.keywords)){
      cat("...almost, not enough data on all keywords.")
      next
    }
    base.table <- cbind(dates, phase2_output)
    ### Saving the code and name of the base.table makes code
    ### a lot easier to read in Phase3
    base.geo.code <-  google.locations[local,1]
    base.location <- google.locations[local,2]
    cat(paste("\nBase Table FOUND:", google.locations[local,2]))
    ### Hooray! Let's GTFO of here!
    break
    ### TODO: It could be our base table sucks.  It could have
    ### large patches of 0s or be in months when we know we can 
    ### get weekly or daily granularity.  
    ###
    ### This can happen because the selected base.location
    ### was either to small and didn't have enough data OR
    ### it was to big and eclipses lots of the other smaller
    ### locations.  
    ###
    ### The TODO then is to mark where we left off and maybe comeback
    ### later and get a better base.table
    ###
    ### ALTERNITIVELY: Depending on how clever we are at 'fixing'
    ### the phase2 output (filling in missing data), this might
    ### not be nessicary(sic). 
  }
  
  ###################################################################
  #### Phase 2:The Search for a complete set ########################
  
  ###  The list phase2_output will be a list such that each element
  ###  will be named with the name of the location.
  ###  That makes it nice so you can do names(phase2_output) and
  ###  it will give you a vector of the names of the geographies.
  ###
  ###  Each element will be a data.frame with a table that gives
  ###  The relative intensity.
  
  phase3_output <-list()
  ###  We can start our table with the base.table from Phase 1
  phase3_output[[base.location]] <- base.table
  
  for (keyword in colnames(base.table)[2:ncol(base.table)]){
    ### Starting at 2 skips the 'dates' column
    cat(paste("\nFinding data for keyword:", keyword))
    for (local in 1:(dim(google.locations)[1])){
      loop.location.name <- google.locations[local,2]
      cat(paste("\n",loop.location.name))
      if (loop.location.name == base.location) next  ## We don't need to do
      ## this for base.table
      
      google.location.code <- c(base.geo.code, google.locations[local,1] )
      output <- query.GTrends(keyword,category,date, google.location.code,ch)
      ### Does the specified data frame exist yet?
      ### If not create it, if so bind to it
      
      ### TODO: This is in bad need of error handling
      ###  Hopefully someone who knows what they are doing
      ###  will add some tryCatch here.  
      ###  Reason: if base.table is in Weeks and the output from
      ###  a few lines ago is in Months then shit's going to come
      ###  off the rails. Also- it could be 
      ###  that nothing is returned, e.g. output has only 2 columns
      ###  date and the geography of our base.table
      
      if (ncol(output) == 3 && nrow(output) == nrow(base.table)){
        ### Lazy fix: If there are 3 columns and the same number
        ### of rows as the base.table it MUST be OK... 
        ### You're better than that.  Use tryCatch
        old.data <- phase3_output[[ loop.location.name ]]
        if(is.null(old.data)){
          ### There was nothing in that spot on the list
          ###  put a new data.frame in
          new.data <- cbind(output[,1], base.table[,keyword] * (output[,3]/output[,2]))
          colnames(new.data) <- c("Date", keyword)
        } else {
          ### There was something in that spot on the list
          ###  bind to the existing data.frame
          new.data <- cbind(old.data, base.table[,keyword] * (output[,3]/output[,2]))
          colnames(new.data) <- c(colnames(old.data), keyword)
        }
        phase3_output[[loop.location.name]] <- new.data
      } else{
        cat("...failed.")
        ### and then report it so we come back later and
        ### try to fill it in.
      } 
      
    }
  }
  
  #######
  ### TODO: Spot Checks to make sure table works out the way you 
  ### Thought it should
  
  return(phase3_output)
}

