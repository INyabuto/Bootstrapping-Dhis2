library(httr)
library(jsonlite)
library(rlist)

#Login into the system 
dest.url <- "http://192.168.1.124/dhis/"
dest.user <- "admin"
dest.pass <- "district"
source.url <- "https://hiskenya.org/"
source.user <- "INyabuto"
source.pass <- "Nyabuto12"
url3 <- ".json?paging=false&links=false"


loginDhis2 <- function(base.url, usr, pwd){
  url <- paste0(base.url, "api/me")
  r <- GET(url, authenticate(usr,pwd))
  assertthat::assert_that(r$status_code == 200)
}
#login
startTime <- Sys.time()
loginDhis2(dest.url, dest.user, dest.pass)



r <- GET("http://192.168.1.124/dhis/api/27/dataElementGroups", authenticate(dest.user,dest.pass))
d <- fromJSON(content(r,"text"))

#Extract data values from the national instance that matches. 
# dataElements
pb <- winProgressBar(title = "Kenya - dhis2 acedemy data exchange (pull & push)",
                     label = "0% done",
                     min = 0,
                     max = 5)


for (i in seq_along(d$dataElementGroups$id[1:5])){
  #Get the data elements ids
  url_dg <- paste0("http://192.168.1.124/dhis/api/27/dataElementGroups/",d$dataElementGroups$id[i])
  r_dg <- GET(url_dg)
  d_dg <- fromJSON(content(r,"text"))
  
  for (j in seq_along(1:nrow(d_dg$dataElements))){
    # build the url
    urlA <- paste0(source.url,"api/analytics.json?")
    urlB <- paste0("dimension=dx:", d_dg$dataElements$id[j])
    urlC <- paste0("dimension=pe:", "LAST_12_MONTHS")
    urlD <- paste0("dimension=ou:", paste("sANMZ3lpqGs","LEVEL-3","LEVEL-4","LEVEL-5","LEVEL-6","LEVEL-7","LEVEL-8", sep = ";"))
    urlE <- "displayProperty=NAME&skipMeta=TRUE"
    url <- paste(paste0(urlA, urlB),
                 urlC, urlD, urlE, 
                 sep="&")
    r <- GET(url, authenticate(source.user, source.pass),timeout(1200000))
    if (r$status_code!= 200){
      next
    }
    r <- content(r,"text")
    # convert the json into an R data structure
    d <- fromJSON(r, flatten = TRUE)
    # get the metadata map
    data <- as.data.frame(d$rows, stringAsFactors = TRUE)
    if (length(data) == 0){
      next
    }
    names(data) <- d$headers$column
    
    # prepare data for import
    # Remove nulls to make things faster
    data2 <- data[sapply(data, function(x)!any(is.na(x)))]
    # chnage the names
    names(data2) <- c("dataElement","period","orgUnit","value")
    # prepare url and post
    urlPost <- paste0(dest.url,"api/dataValueSets?preheatCache=TRUE&skipExistingCheck=TRUE")
    rp <- POST(urlPost, body = toJSON(list(dataValues = data2), auto_unbox = TRUE), content_type_json(),
               timeout(120000))
    rm(data2)
    rm(data)
  }
  info <- sprintf("%d%% done", round(i/5 *100))
  setWinProgressBar(pb, i, label = info)
  
}
close(pb)

endTime <- Sys.time()
timeDifference <- endTime - startTime

# trigger analytics
url <- paste0(dest.url,"api/resourceTables/analytics")
r <- POST(url)