########################################################
### Useage Example


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

keywords      <- read.csv("keywords.csv")

## This is the place where the location codes are loaded in. 
## When this is packaged it will probably be passed as an argument
## in the function.  If you Limeys get ahold of this script before
## then you'll point this to your csv file of location codes. 
##  FREE HINT WHEN MAKING THIS FILE:  Since it is going in the order
##  of the file looking for a base.table it would make sense
## to put the geographic locations most likely to return a good 
## base.table first. 

#google.locations <- read.csv("Google_DMA_codes.csv")
google.locations <- read.csv("Google_state_codes.csv")


date          <- c("1%2F2011 34m") # Jan 2011 + 34m (Oct 2013)
category      <- c("0-7-38")       # Insurance Category Code


output <- scrape.GTrends(keywords, google.locations, date, category, ch)
output <- fix.dates(output)
output <- make.longfile(output)

filename <- paste0( format(Sys.time(), "%y-%m-%d.%H-%M-%S"),"-Google Trends Scrape.csv")
write.csv(output,filename)



