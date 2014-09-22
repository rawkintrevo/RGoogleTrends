##### Load required libraries
library(RCurl)  
library(RJSONIO)
library(stringr)

##### Strings coming in as factors bugs me
options(stringsAsFactors=FALSE)

##### Auxilary Function Files Keep my code neat and clean
source("R/signIn.R")
source("R/Useful Functions.R")
source("R/scrape.GTrends.R")


username <- "some_guy@gmail.com"
password <- "his_password"
ch <- gLogin(username, password)

pivot_word <- "sauce"  
pivot_word_name <- 'sauce'

category <- ''  # 
dates <- '1%2F2011%2039m'

## Fun Fact: All columns need to be same length
keywords <- read.csv("keywords.csv", header=FALSE)

keywords <- rbind(keywords,pivot_word)

LOCATION_FILE <- "Google_state_codes.csv"
locations <- read.csv(LOCATION_FILE)[,1] 

#######################################################################################################



geo_table <- scrape.GTrends(LOCATION_FILE,pivot_word, pivot_word_name, category, dates, keywords, ch)

output <- make.longfile(geo_table)
write.csv(output, file.choose())
