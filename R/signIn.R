#############################################################################
## This gets the GALX cookie which we need to pass back with the login form
#############################################################################

getGALX <- function(curl) {
  loginURL   	<- "https://accounts.google.com/accounts/ServiceLogin"
  txt = basicTextGatherer()
  curlPerform( url=loginURL, curl=curl, writefunction=txt$update, header=TRUE, ssl.verifypeer=FALSE )
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  strsplit(val, "[:=;]")[[1]][3]
  
  return( strsplit( val, "[:=;]")[[1]][3]) 
}



############################################
## Function to perform Google login and get cookies ready
############################################
gLogin <- function(username, password) {
  ch <- getCurlHandle()
  authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
  
  ans <- (curlSetOpt(curl = ch,
                     ssl.verifypeer = FALSE,
                     useragent = getOption('HTTPUserAgent', "R"),
                     timeout = 60,         
                     followlocation = TRUE,
                     cookiejar = "./cookies",
                     cookiefile = ""))
  
  galx <- getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=username, Passwd=password, GALX=galx, PersistentCookie="yes", continue="http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  return(ch)
}


