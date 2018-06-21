library(httr)
library(jsonlite)
#Login into the system 
dest.url <- "http://192.168.0.198/dhis/"
dest.user <- "admin"
dest.pass <- ""



loginDhis2 <- function(base.url, usr, pwd){
  url <- paste0(base.url, "api/me")
  r <- GET(url, authenticate(usr,pwd))
  assertthat::assert_that(r$status_code == 200)
}
#login
startTime <- Sys.time()
loginDhis2(dest.url, dest.user, dest.pass)


# trigger analytics
url <- paste0(dest.url,"api/resourceTables/analytics")
r <- POST(url)

completed <- FALSE
while(completed == FALSE){
  Sys.sleep(10)
  r <- fromJSON(content(GET(paste0(dest.url, "system/tasks/ANALYTICSTABLE_UPDATE")), "text"), Encoding("UTF-8"), flatten = TRUE)
  cat("Not done yet ... please wait.")
  completed <- r[[1]]$completed
}

endTime <- Sys.time()
timeDifference <- endTime - startTime
