library(httr)
library(jsonlite)
library(rlist)

#Login into the system 
dest.url <- "http://192.168.0.128/dhis/"
dest.user <- "admin"
dest.pass <- "district"
source.url <- "https://hiskenya.org/"
source.user <- "INyabuto"
source.pass <- "Nyabuto12"
url3 <- ".json?paging=false&links=false"

source("API Client.R")
# Generation of UIDs
generateUID <- function(codeSize=11){
  #Generate a random seed
  runif(1)
  allowedLetters <- c(LETTERS, letters)
  allowedChars <- c(LETTERS, letters, 1:9)
  #first character must be a letter according to dhis2 specs
  firstChar <- sample(allowedLetters,1)
  otherChars <- sample(allowedChars, codeSize - 1)
  uid <- paste(c(firstChar,paste(
    otherChars, sep = "", collapse = "" )), sep = "", collapse = "")
  return(uid)
}

startTime <- Sys.time()

loginDhis2 <- function(base.url, usr, pwd){
  url <- paste0(base.url, "api/me")
  r <- GET(url, authenticate(usr,pwd))
  assertthat::assert_that(r$status_code == 200)
}

#Login to dhis2 training
loginDhis2(dest.url,dest.user, dest.pass)

#define targets
targets <- c("dataElements",
             "indicators",
             "dataElementGroups",
             "organisationUnits")


# Extract metadata from dhis2 academy

pb <- winProgressBar(title = "dhis2 Academy Metadata Pull", label = "0% done", min = 0, max = length(targets), width = 300)
for(i in seq_along(targets)){
  url2 <- paste0("api/",targets[i])
  url <- paste0(dest.url,url2,url3)
  r <- GET(url)
  res <- content(r, "text")
  d <- fromJSON(res, simplifyVector = F)
  name <- sapply(d[[targets[i]]], "[[", "displayName")
  id <- sapply(d[[targets[i]]], "[[", "id")
  
  # bind
  temp <- cbind(name, id)
  
  # Recast as a data frame
  df <- as.data.frame(temp, stringsAsFactors = FALSE,
                      row.names = 1:nrow(temp))
  info <- sprintf("%d%% done", round(i/length(targets)*100))
  setWinProgressBar(pb, i, label = info)
  assign(targets[i], df)
  remove(df)
}
close(pb)

counties <- c("u4t9H8XyU9P","N7YETT3A9r1","HfVjCurKxh2")
#Extract data values from the national instance that matches. 
# dataElements
pb <- winProgressBar(title = "Kenya - dhis2 acedemy data exchange (pull & push)",
                     label = "0% done",
                     min = 0,
                     max = nrow(dataElements))
#for(k in seq_along(counties)){
  for (i in seq_along(1:nrow(dataElements))){
    # build the url
    urlA <- paste0(source.url,"api/analytics.json?")
    urlB <- paste0("dimension=dx:", dataElements$id[i])
    urlC <- paste0("dimension=pe:", "LAST_12_MONTHS")
    urlD <- paste0("dimension=ou:", paste("N7YETT3A9r1","LEVEL-3","LEVEL-4","LEVEL-5","LEVEL-6","LEVEL-7","LEVEL-8", sep = ";"))
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
    info <- sprintf("%d%% done", round(i/nrow(dataElements)*100))
    setWinProgressBar(pb, i, label = info)
    rm(data2)
    rm(data)
  }
  close(pb)
#}


endTime <- Sys.time()
timeDifference <- endTime - startTime

# trigger analytics
url <- paste0(dest.url,"api/resourceTables/analytics")
r <- POST(url)


