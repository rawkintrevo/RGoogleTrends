######################################
#### Python Integration Scratch Pad


##### Load required libraries
library(RCurl)  
library(RJSONIO)
library(stringr)

##### Strings coming in as factors bugs me
options(stringsAsFactors=FALSE)

##### Auxilary Function Files Keep my code neat and clean
source("Google Login.R")
source("Useful Functions.R")
source("scrape.GTrends.R")


username <- "some_guy@gmail.com"
password <- "my_pass"
ch <- gLogin(username, password)

pivot_word <- "%2Fm%2F0539y0"  # Emmit Smith
pivot_word_name <- 'sauce'

category <- ''# Jan 2011 - Present
dates <- '1%2F2011%2039m'

## Fun Fact: All columns need to be same length
keywords <- read.csv("food keywords.csv", header=FALSE)

keywords <- rbind(keywords,pivot_word)

LOCATION_FILE <- "Google_state_codes.csv"
locations <- read.csv(LOCATION_FILE)[,1] 

scrape.GTrends(locations,pivot_word, pivot_word_name, category, dates, keywords, ch)

output <- make.longfile(geo_table)
write.csv(output, file.choose())
