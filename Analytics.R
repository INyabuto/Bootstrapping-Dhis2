library(httr)
library(jsonlite)
library(keyringr)


#'
#'Trigger Analytics resource tables
#'

#Set up ==========================================
dest <- "192.168.0.198"
dest.url <- "http://192.168.0.198/dhis/"
dest.user <- keyringr::get_kc_account(dest, type = "generic")
dest.pass <- keyringr::decrypt_kc_pw(dest, type = "generic")


#'
#'Function to login into the API
#'@param base.url, 
#'@param usr
#'@param pwd
#'@return boolean
#'
loginDhis2 <- function(base.url, usr, pwd){
  url <- paste0(base.url, "api/me")
  r <- GET(url, authenticate(usr,pwd))
  assertthat::assert_that(r$status_code == 200)
}

#login and set time

startTime <- Sys.time()
loginDhis2(dest.url, dest.user, dest.pass)


# trigger analytics
url <- paste0(dest.url,"api/resourceTables/analytics")
r <- POST(url)

# monitor the analytics
completed <- FALSE
while(completed == FALSE){
  Sys.sleep(10)
  r <- fromJSON(content(GET(paste0(dest.url, "system/tasks/ANALYTICSTABLE_UPDATE")), "text"), Encoding("UTF-8"), flatten = TRUE)
  cat("Not done yet ... please wait.")
  completed <- r[[1]]$completed
}

endTime <- Sys.time()
timeDifference <- endTime - startTime
