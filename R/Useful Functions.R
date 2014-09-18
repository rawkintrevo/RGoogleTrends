##  Query Codes.R



make.keyword.blocks <- function(keywords){
  keyword.block <- list()  
  block.index <- seq(1, dim(keywords)[1], 3)
  for (i in 1:length(block.index)){
    start.index <- block.index[i]
    if (start.index+3 > dim(keywords)[1]){ end.index <- dim(keywords)[1]
    }else end.index <- start.index+3
    keyword.block[[i]] <- keywords[start.index:end.index,1]
  }
  if(length(keyword.block[[length(block.index)]]) == 1 ) keyword.block[[length(block.index)]] <- NULL
  return (keyword.block)
}



addOnToURL <- function(v, name, fetchURL){
  if (length(v) > 0){
    fetchURL <- paste0(fetchURL, name,"=")
    for (i in v){
      fetchURL <- paste0(fetchURL, i)
      if (i != v[length(v)]) fetchURL <- paste0(fetchURL, ",")
    }
  }
  fetchURL <- paste0(fetchURL, "&")
  return(fetchURL)
}

query.builder <- function(query, category, dates="today%2012-m", geo="US"){
  if ( (length(query) > 1) && (length(geo)>1)){
    cat("\nSorry bro-cacho, multiple keywords or multiple geographies. Not both")
    return("")
  }
  fetchURL = "http://www.google.com/trends/fetchComponent?"
  mylist <- list(q= query, cat= category, date= dates, geo= geo)
  for (t in 1:length(mylist))    fetchURL <- addOnToURL(mylist[[t]], names(mylist)[t], fetchURL)
  fetchURL <- paste0(fetchURL, "cid=TIMESERIES_GRAPH_0&export=3")
  if(length(geo)>1) fetchURL <- paste0(fetchURL, "&cmpt=geo")
  return(fetchURL)  
}

query.GTrends <- function(keywords,category,date, google.location.code,ch){
  fetchURL <- query.builder(keywords, category, date, google.location.code)  
  fetchedJSON <- getURL(gsub(" ", "%20", fetchURL),curl=ch)
  splitJSON <- strsplit(fetchedJSON, "nse\\(")
  readyJSON <- substr(splitJSON[[1]][2], 1, nchar(splitJSON[[1]][2])- 2)
  if (grepl("Not enough search", readyJSON)) return(NULL)
  listedResult <- fromJSON(readyJSON)
  
  new.colnames <- vector(length= length(listedResult$table$cols))
  for (i in 1:length(listedResult$table$cols) ) new.colnames[i] <- listedResult$table$cols[[i]][2]
  output <- as.data.frame(matrix(ncol= length(new.colnames), nrow= length(listedResult$table$rows)))
  colnames(output) <-  new.colnames
  for (i in 1:length(listedResult$table$rows)) {
    for (j in 1:length(new.colnames)){
      repl.val <- unlist(listedResult$table$rows[[i]]$c[[j]][2])
      if (!is.null(repl.val)){
        if (j > 1) repl.val <- as.numeric(repl.val)
        output[i,j] <- repl.val
      } else output[i,j] <- NA
    }
  }
  Sys.sleep(20)  ### Be polite, don't blow google up with 500 requests a second
                 ### most people on the boards reporting their scraper doesn't work 
                ### anymore is because they got a lifetime ban for letting their
                ### bot go nuts.
  return(output)
}

fix.dates <- function(phase3_output){
  for (i in 1:length(phase3_output)){
    ### only works for weeklies
    phase3_output[[i]] <- as.data.frame(phase3_output[[i]])
    dates <- phase3_output[[i]][,1]
    phase3_output[[i]][,1] <- as.Date(gsub(" ", "", lapply(strsplit(dates, "\u2013",), 
                                                           function (x) {
                                                            paste(x[1],",", substr(x[2],nchar(x[2])-3, nchar(x[2])))
                                                           })), "%b%d,%Y")
    
  }
  return(phase3_output)
}


make.longfile <- function(phase3_output){
  ###########################################################
  #### Phase 3: Convert list into long file
  long.df <- NULL
  for (geo in names(phase3_output)){
    ### TODO Convert Dates
    geo.df <- phase3_output[[geo]]
    for (keyword in colnames(geo.df)[2:ncol(geo.df)]){
      long.df.to.bind <- data.frame(as.Date(geo.df[,1]), geo, keyword, geo.df[,keyword])
      colnames(long.df.to.bind) <- c('Date', 'Geography', 'Keyword', 'Relative Search Intensity')
      if (exists("long.df")){
        ### TODO: quit using rbind like a grad student
        ###  Why? rbind is the slowest sloppiest way in the known
        ###  R-verse to build a data.frame. You should make a 
        ###  data.frame big enough to hold everything you need
        ###  then use dynamic indexing to fill 'er up.
        ###
        ###  It's not to bad in this case though... Doesn't make it OK.
        long.df <- rbind(long.df, long.df.to.bind)
        cat(paste("\n", keyword, geo, "bound to output dataframe"))
      } else {
        long.df <- long.df.to.bind
        cat("\n output data frame constructed")
      }
    }
  }
  return (long.df)
}